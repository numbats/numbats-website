# this script fetches the data from google sheet
# and creates the person folder and _index.md with yaml components 
# as necessary for the hugo academic theme

# the fetching is automated with github actions [PLAN]

library(googlesheets4)
library(tidyverse)
library(glue)
library(janitor)
library(rlang)

sheets_deauth()

fence <- "---"

# the minimum requirement of a person is 
# First Name, Last Name, Email, Affiliation, and Position
minimum_template <- "
                     authors:
                     - {user}
                     email: {email}
                     name: {first_name} {last_name}
                     organizations:
                     - name: {affiliation}
                     role: {position}"

# social media prescence
social_template <- function(x, type) {
  social <- switch(type,
                   website = glue("- icon: link",
                                  "  icon_pack: fas",
                                  "  link: {x}", 
                                  .trim = FALSE, .sep = "\n"),
                   email = glue("- icon: envelope",
                                "  icon_pack: fas",
                                "  link: \"mailto:{x}\"", 
                                .trim = FALSE, .sep = "\n"),
                   scholar = glue("- icon: google-scholar",
                                "  icon_pack: ai",
                                "  link: {x}", 
                                .trim = FALSE, .sep = "\n"),
                   twitter = glue("- icon: twitter",
                                  "  icon_pack: fab",
                                  "  link: https://twitter.com/{x}", 
                                  .trim = FALSE, .sep = "\n"),
                   github = glue("- icon: github",
                                 "  icon_pack: fab",
                                 "  link: https://github.com/{x}", 
                                 .trim = FALSE, .sep = "\n"))
  social[is.na(x)] <- ""
  social
} 
# the list of groups that the user belongs to
# List: 
# Current, Alumni
user_groups <- function(types) {
  group <- map_chr(types, 
                   ~switch(.x, 
                      Postdoc              = "Current",
                      Faculty              = "Current",
                      `Research Assistant` = "Current",
                      `PhD Student`        = "Current",
                      `Master Student`     = "Current",
                      Alumni               = "Alumni",
                      Visitor              = "Visitor"))
  group <- ifelse(group==types, "", paste0("- ", group)) 
  trim(paste0("- ", types, "\n", group))
}

research_interests <- function(x) {
  interests <- strsplit(x, ",") %>% 
    map(~paste0("- ", stringr::str_trim(.x)))
  out <- map_chr(interests,
      ~paste0("interests:\n", paste0(.x, collapse = "\n")))
  out[is.na(x)] <- ""
  out
} 

dat <- read_sheet("1fPfjYk2NLakqZFid12Qz_DNvuqLI5rmJWjPUpcp6g8Y")
df <- dat %>% clean_names() %>% 
  filter(!is.na(first_name)) %>% 
  mutate(user = tolower(glue("{last_name}-{gsub('_', '-', make_clean_names(first_name))}")),
         yaml = stringr::str_replace_all(glue_data(., 
                          fence,
                          trim(minimum_template),
                          "social:",
                          "{social_template(website, 'website')}",
                          "{social_template(twitter, 'twitter')}",
                          "{social_template(github, 'github')}",
                          "{social_template(google_scholar, 'scholar')}",
                          "{social_template(email, 'email')}",
                          "{research_interests(interests)}",
                          "user_groups:",
                          ("{user_groups(type)}"),
                          fence, 
                          .sep = "\n"), "(?m)^\\s*$[\n\r]{1,}", "")) 

# rewrite the _index.md for everyone
pwalk(df, function(user, yaml, ...) {
  user_folder <- here::here("content", "authors", user)
  user_info <- paste0(user_folder, "/_index.md")
  if(!dir.exists(user_folder)) dir.create(user_folder)
  cat(yaml, file = user_info)
})


### need to consider the avatar issue 

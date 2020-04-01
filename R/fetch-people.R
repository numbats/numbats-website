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

# the minimum requirement of a person is 
# First Name, Last Name, Email, Affiliation, and Position
fence <- "---"
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
                                  "  link: {x}", .trim = FALSE, .sep = "\n"),
                   email = glue("- icon: envelope",
                                "  icon_pack: fas",
                                "  link: mailto:{x}", .trim = FALSE, .sep = "\n"),
                   github = glue("- icon: envelope",
                                 "  icon_pack: fab",
                                 "  link: https://github.com/{x}", .trim = FALSE, .sep = "\n"))
  social[is.na(x)] <- ""
  social
} 
# the list of groups that the user belongs to
# List: 
# Current, Alumni
user_groups <- function(...) {
  flist <- list2(...)
  glue("user_groups:\n", paste("-", flist, collapse = "\n"))
}

null_transformer <- function(str = "NULL") {
  function(text, envir) {
    out <- glue::identity_transformer(text, envir)
    if (is.null(out)) {
      return(str)
    }
    out
  }
}


dat <- read_sheet("1fPfjYk2NLakqZFid12Qz_DNvuqLI5rmJWjPUpcp6g8Y")
df <- dat %>% clean_names() %>% 
  filter(!is.na(first_name)) %>% 
  mutate(user = tolower(glue("{last_name}-{gsub('_', '-', make_clean_names(first_name))}"))) %>% 
  mutate(create_folder = !dir.exists(here::here("content", "authors", user)))

out <- glue_data(df, 
                 fence,
                 trim(minimum_template),
                 "{social_template(website, 'website')}",
                 "{social_template(twitter, 'twitter')}",
                 "{social_template(github, 'github')}",
                 fence, 
                 .sep = "\n") %>% 
  stringr::str_replace_all("\n\n", "\n")

### need to write `out` to _index.md. 
### need to consider the avatar issue 

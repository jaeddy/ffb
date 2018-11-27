library(dplyr)
library(purrr)
library(tibble)
library(stringr)
library(tidyr)
library(feather)
library(DT)
library(networkD3)
library(RColorBrewer)
library(htmlwidgets)
library(shinydashboard)
library(flexdashboard)
library(shinycssloaders)

source("helpers.R")

current_week <- 12

standings <- tribble(
  ~team, ~division, ~wins, ~points,
  "James", "A", 6, 1033.4,
  "Tony", "A", 7, 1097.5,
  "Brad", "A", 5, 992.2,
  "Drew", "A", 5, 1071.5,
  "Toby", "A", 5, 906.7,
  "Shep", "A", 9, 1119.1,
  "Kevin", "B", 4, 857.1,
  "Keith", "B", 5, 1020.4,
  "Matt", "B", 4, 789.2,
  "Billy", "B", 4, 963.7,
  "Milf", "B", 6, 1152.5,
  "Ross", "B", 6, 1087.7
)

schedule <- tribble(
  ~week, ~home, ~away, ~matchup,
  "12", "Milf", "Keith", 1,
  "12", "Billy", "Ross", 2,
  "12", "Kevin", "Matt", 3,
  "12", "Tony", "Shep", 4,
  "12", "Toby", "Drew", 5,
  "12", "James", "Brad", 6,
  "13", "Ross", "Milf", 1,
  "13", "Matt", "Billy", 2,
  "13", "Kevin", "Keith", 3,
  "13", "Toby", "Shep", 4,
  "13", "Brad", "Drew", 5,
  "13", "Tony", "James", 6
)

matchup_key <- schedule %>% 
  filter(week == current_week) %>% 
  mutate(matchup = str_glue("match{matchup}"), 
         label = str_glue("{week}: {away} vs. {home}")) %>% 
  select(matchup, label)
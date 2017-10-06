library(readxl)
library(tidyverse)
library(glue)
library(feather)


update_pr_master <- function(year, week, data_dir = "data") {
  pr_spreadsheet_path <- glue("~/Dropbox/MyDocuments/Fantasy Football/{year} Jeff Cup Power Rankings.xlsx",
                              year = year)
  pr_master_path <- file.path(data_dir, 
                              glue("{year}_pr_master.feather", year = year))
  if (!file.exists(pr_master_path)) {
    pr_master_df <- read_xlsx(pr_spreadsheet_path, 
                              sheet = "PUBLISH", range = "C4:L16") %>% 
      mutate(WEEK = week)
  } else {
    pr_master_df <- read_feather(pr_master_path)
    if (!week %in% pr_master_df$WEEK) {
      pr_master_df <- pr_master_df %>% 
        bind_rows(read_xlsx(pr_spreadsheet_path, 
                            sheet = "PUBLISH", range = "C4:L16") %>% 
                    mutate(WEEK = week))
    }
  }
  write_feather(pr_master_df, pr_master_path)
  pr_master_df
}
  
get_powerrankings <- function(year, week, data_dir = "data") {
  update_pr_master(year, week, data_dir) %>% 
    filter(WEEK == week) %>% 
    select(-WEEK)
}

read_trends <- function(year, data_dir = "data") {
  pr_spreadsheet_path <- glue("~/Dropbox/MyDocuments/Fantasy Football/{year} Jeff Cup Power Rankings.xlsx",
                              year = year)
  list(roster_strength = "B2:S14", power_rank = "V2:AM14") %>% 
    map_df(function(table_range) {
      read_xlsx(pr_spreadsheet_path, 
                sheet = "TRENDS", range = table_range) %>% 
        set_names(c("Team", names(.)[-1])) %>% 
        gather(week, rank, -Team) %>% 
        mutate(week = ifelse(week == "PS", "PRE SEASON", week),
               week = stringr::str_replace_all(week, " ", ""),
               week = stringr::str_replace_all(week, "W", "WEEK "))
        
    }, 
    .id = "trend_type")
}
read_trends(2017)


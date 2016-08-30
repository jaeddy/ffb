library(dplyr)
library(tibble)
library(tidyr)
library(stringr)

teams <- c("James", "Brad", "Drew", "Matt", "Kevin", "Billy", "Shep", "Milf",
           "Keith", "Toby", "Tony", "Ross")
divisions <- c("A", "B", "B", "A", "B", "A", "A", "B", "B", "A", "A", "B")
rivalries <- c(1, 2, 2, 3, 4, 3, 5, 6, 4, 5, 1, 6)

randomize_schedule <- function(teams, divisions, rivalries) {
  set.seed(0)
  team_df <- tibble(team = teams, division = divisions, rivalry = rivalries)
  
  matchups <- combn(team_df$team, 2)

  matchup_df <- team_df %>%
    rename(team1 = team, 
           division1 = division, 
           rivalry1 = rivalry) %>% 
    crossing(team_df %>% 
               rename(team2 = team, 
                      division2 = division, 
                      rivalry2 = rivalry)) %>% 
    # filter(team1 < team2) %>%
    mutate(type = ifelse(division1 == division2, "intra", "inter"),
           type = ifelse(rivalry1 == rivalry2, "rival", type)) %>% 
    rowwise() %>% 
    mutate(matchup = str_c(str_sort(c(team1, team2)), collapse = "-")) %>% 
    ungroup()
  
  schedule_df <- matchup_df %>% 
    mutate(week = NA) %>% 
    slice(0) 
  
  for (schedule_team in teams) {
    schedule_team <- teams[3]
    
    intradivision_df <- 
      
      w <- c(1, 3, 4, 5)
    purrr::map(5 - c(4, 3, 3, 1, 1), function(x) {w[x:4]}) 
    %>% 
      purrr::flatten_dbl()
    iw <- c(1, 3, 4, 5, 
            3, 4, 5, 
            5, 1, 4,
               1, 3)
      
      # filter(team1 == schedule_team | team2 == schedule_team) %>% 
      x <- matchup_df %>% 
      filter(type == "intra") %>% 
      filter(division1 == "A") %>%
        filter(team1 < team2) %>% 
      arrange(rivalry1, rivalry2) %>% 
      mutate(week = iw) %>% 
      I 
        arrange(week)
        I
      I
      left_join(schedule_df)
    intradivision_df
    
    intradivision_df$week[is.na(intradivision_df$week)] <- 
      setdiff(c(1, 3, 4, 5), intradivision_df$week)
    
    (intradivision_df <- intradivision_df %>% 
      mutate(week = ifelse(week == 1, list(c(1, 12)), week)) %>% 
      unnest(week) %>% 
      distinct())

    interdivision_df <- matchup_df %>% 
      filter(team1 == schedule_team | team2 == schedule_team) %>% 
      filter(type == "inter") %>% 
      left_join(schedule_df)
    interdivision_df
    
    interdivision_df$week[is.na(interdivision_df$week)] <- 
      setdiff(6:11, interdivision_df$week)
    interdivision_df

    (rival_df <- matchup_df %>% 
      filter(team1 == schedule_team | team2 == schedule_team) %>% 
      filter(type == "rival") %>% 
      mutate(week = list(c(2, 13))) %>% 
      unnest(week))
  
    schedule_df <- schedule_df %>% 
      bind_rows(intradivision_df, interdivision_df, rival_df) %>% 
      arrange(week) %>% 
      distinct()
    
    # matchup_df <- matchup_df %>%
    #   filter(!(matchup %in% schedule_df$matchup))
    # message(nrow(matchup_df))
  }
  return(schedule_df)
}



randomize_schedule(teams, divisions, rivalries)

library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(purrr)

teams <- c("James", "Brad", "Drew", "Matt", "Kevin", "Billy", "Shep", "Milf",
           "Keith", "Toby", "Tony", "Ross")
divisions <- c("A", "B", "B", "A", "B", "A", "A", "B", "B", "A", "A", "B")
rivalry_nums <- c(1, 4, 4, 2, 5, 2, 3, 6,
                 5, 3, 1, 6)


get_schedule <- function(team, division, rivalry_num, draft_date) {
  set.seed(draft_date)
  team_df <- tibble(team, division, rivalry_num) %>% 
    group_by(division) %>% 
    mutate(rr = sample(seq(min(rivalry_num),
                           max(rivalry_num)))[dense_rank(rivalry_num)]) %>% 
    ungroup()
  
  schedule_template <- tribble(
    ~week, ~teamA, ~teamB,
    1,     3,      1,
    1,     4,      2,
    1,     6,      5,
    1,     10,     8,
    1,     7,      9,
    1,     12,     11,
    2,     4,      3,
    2,     1,      5,
    2,     2,      6,
    2,     11,     7,
    2,     9,      10,
    2,     8,      12,
    3,     2,      1,
    3,     5,      3,
    3,     6,      4,
    3,     7,      8,
    3,     11,     9,
    3,     12,     10,
    4,     5,      2,
    4,     1,      4,
    4,     3,      6,
    4,     11,     8,
    4,     7,      10,
    4,     9,      12,
    5,     6,      1,
    5,     2,      3,
    5,     4,      5,
    5,     8,      9,
    5,     10,     11,
    5,     7,      12,
    6,     12,     1,
    6,     11,     2,
    6,     9,      4,
    6,     8,      5,
    6,     7,      6,
    6,     3,      10,
    7,     2,      7,
    7,     6,      8,
    7,     3,      9,
    7,     1,      10,
    7,     5,      11,
    7,     4,      12,
    8,     8,      1,
    8,     10,     2,
    8,     12,     3,
    8,     7,      4,
    8,     9,      5,
    8,     6,      11,
    9,     8,      2,
    9,     11,     3,
    9,     10,     4,
    9,     12,     5,
    9,     9,      6,
    9,     1,      7,
    10,    9,      1,
    10,    8,      3,
    10,    10,     6,
    10,    5,      7,
    10,    4,      11,
    10,    2,      12,
    11,    12,     6,
    11,    3,      7,
    11,    4,      8,
    11,    2,      9,
    11,    5,      10,
    11,    1,      11,
    12,    3,      1,
    1,     4,      2,
    12,    6,      5,
    12,    10,     8,
    12,    7,      9,
    12,    12,     11,
    13,    4,      3,
    1,     1,      5,
    13,    2,      6,
    13,    11,     7,
    13,    9,      10,
    13,    8,      12
  )
  
  rivalry_week = schedule_template %>% 
    filter(week == 2) %>% 
    mutate(rivalry = row_number())
  
  team_df <- team_df %>% 
    group_by(rivalry_num) %>% 
    mutate(number = rivalry_week %>% 
             filter(rivalry == rr) %>% 
             select(teamA, teamB) %>% 
             flatten_dbl() %>% 
             sample())
  
  schedule_df <- schedule_template %>% 
    left_join(team_df, by = c("teamA" = "number")) %>% 
    left_join(team_df, by = c("teamB" = "number"), suffix = c("A", "B"))
  
  return(schedule_df %>% 
           select(week, 
                  teamA = teamAA, divisionA,
                  teamB = teamBB, divisionB))
}

schedule2016 <- get_schedule(teams, divisions, rivalry_nums, "20160827")

# randomize_schedule <- function(teams, divisions, rivalries) {
#   set.seed(0)
#   team_df <- tibble(team = teams, division = divisions, rivalry = rivalries)
#   
#   matchups <- combn(team_df$team, 2)
# 
#   matchup_df <- team_df %>%
#     rename(team1 = team, 
#            division1 = division, 
#            rivalry1 = rivalry) %>% 
#     crossing(team_df %>% 
#                rename(team2 = team, 
#                       division2 = division, 
#                       rivalry2 = rivalry)) %>% 
#     mutate(type = ifelse(division1 == division2, "intra", "inter"),
#            type = ifelse(rivalry1 == rivalry2, "rival", type)) %>% 
#     rowwise() %>% 
#     mutate(matchup = str_c(str_sort(c(team1, team2)), collapse = "-")) %>% 
#     ungroup()
#   
#   schedule_df <- matchup_df %>% 
#     mutate(week = NA) %>% 
#     slice(0) 
#   
#   for (schedule_team in teams) {
#     schedule_team <- teams[3]
#     
#     intradivision_df <- 
#       
#       w <- c(1, 3, 4, 5)
#     purrr::map(5 - c(4, 3, 3, 1, 1), function(x) {w[x:4]}) 
#     %>% 
#       purrr::flatten_dbl()
#     iw <- c(1, 3, 4, 5, 
#             3, 4, 5, 
#             5, 1, 4,
#                1, 3)
#       
#       # filter(team1 == schedule_team | team2 == schedule_team) %>% 
#     intradiv_weeks <- tibble(
#       r1 = c(1a, 1b, 2a, 1, 2, 3, 2a, 2b, 1a, 3a, 3b, 1a,),
#       r2 = c(2b, 3a, 3b, 1, 2, 3, 1b, 3a, 3b, 1b, 2a, 2b),
#       wk = c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#     )  
#     
#     x <- 
#         
#       matchup_df %>% 
#       filter(type == "intra" | type == "rival") %>% 
#       filter(division1 == "A") %>%
#       arrange(team1, team2) %>% 
#         filter(team1 < team2) %>% 
#         mutate(week = rep(1:5, 3)) %>% 
#       I 
#       group_by(team1) %>% 
#       mutate(week = iw) %>% 
#         arrange(week)
#         I
#       I
#       left_join(schedule_df)
#     intradivision_df
#     
#     intradivision_df$week[is.na(intradivision_df$week)] <- 
#       setdiff(c(1, 3, 4, 5), intradivision_df$week)
#     
#     (intradivision_df <- intradivision_df %>% 
#       mutate(week = ifelse(week == 1, list(c(1, 12)), week)) %>% 
#       unnest(week) %>% 
#       distinct())
# 
#     interdivision_df <- matchup_df %>% 
#       filter(team1 == schedule_team | team2 == schedule_team) %>% 
#       filter(type == "inter") %>% 
#       left_join(schedule_df)
#     interdivision_df
#     
#     interdivision_df$week[is.na(interdivision_df$week)] <- 
#       setdiff(6:11, interdivision_df$week)
#     interdivision_df
# 
#     (rival_df <- matchup_df %>% 
#       filter(team1 == schedule_team | team2 == schedule_team) %>% 
#       filter(type == "rival") %>% 
#       mutate(week = list(c(2, 13))) %>% 
#       unnest(week))
#   
#     schedule_df <- schedule_df %>% 
#       bind_rows(intradivision_df, interdivision_df, rival_df) %>% 
#       arrange(week) %>% 
#       distinct()
#     
#     # matchup_df <- matchup_df %>%
#     #   filter(!(matchup %in% schedule_df$matchup))
#     # message(nrow(matchup_df))
#   }
#   return(schedule_df)
# }
# 
# randomize_schedule(teams, divisions, rivalries)
# 
# 
# combn(LETTERS[1:6], 2) %>% t() %>% as_tibble() %>% 
#   group_by(V1) %>% 
#   mutate(week = 1:length(V1))

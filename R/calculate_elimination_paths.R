
# Load packages -----------------------------------------------------------

library(plyr)
library(igraph)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(dplyr)

# Set color palette -------------------------------------------------------

# modifying the colorblind palette from ggthemes 
my_cb_pal <- colorblind_pal()(8)
my_cb_pal[c(1, 3, 6)] <- my_cb_pal[c(6, 1, 3)]
my_cb_pal[3] <- "#666666"


# Set up data -------------------------------------------------------------

# I need to enter a bunch of stuff manually, because I don't have an easy way
# of pulling data from the ESPN league site

teams <- c("James", "Brad", "Drew", "Matt", "Kevin", "Billy", "Shep", "Milf",
           "Keith", "Toby", "Tony", "Ross")
divisions <- c("A", "A", "B", "A", "B", "B", "B", "B", "A", "A", "A", "B")

# CURRENT RECORDS
weeks_completed <- 10
record_df <- data_frame(team = teams, 
                        division = divisions,
                        wins = c(4, 7, 8, 3, 5, 1, 7, 4, 3, 5, 6, 7))

# REMAINING SCHEDULE

# week 10 matchups
w10_df <- data_frame(home = c("Brad", "Tony", "Keith", "Kevin", "Shep", "Ross"),
                     away = c("Drew", "Milf", "Billy", "Toby", "James", "Matt"),
                     week = "week10")

# week 11 matchups
w11_df <- data_frame(home = c("Keith", "Kevin", "Milf", "Drew", "Billy", "Shep"),
                     away = c("Ross", "Tony", "James", "Matt", "Toby", "Brad"),
                     week = "week11")

# week 12 matchups
w12_df <- data_frame(home = c("Brad", "Matt", "Toby", "Milf", "Drew", "Shep"),
                     away = c("Tony", "James", "Keith", "Billy", "Kevin", "Ross"),
                     week = "week12")

# week 13 matchups
w13_df <- data_frame(home = c("Tony", "Toby", "Keith", "Kevin", "Billy", "Ross"),
                     away = c("James", "Brad", "Matt", "Ryan", "Drew", "Milf"),
                     week = "week13")

# putting everything together
sched_df <- bind_rows(list(w11_df, w12_df, w13_df))

# Define functions --------------------------------------------------------



# helper function to tally wins for each team from simulated outcomes
count_wins <- function(sim_df) {
  sim_df %>% 
    group_by(outcome) %>% 
    tally()
}

# helper function to tally ties for each team from simulated outcomes
count_ties <- function(sim_df) {
  sim_df %>% 
    filter(outcome == "tie") %>% 
    melt(measure.vars = c("home", "away"), 
         value.name = "team") %>% 
    group_by(team) %>% 
    tally()
}

# function to add wins and ties to current records (and subtract from the total 
# number of weeks to get losses) to get final records for current simulation
update_records <- function(record_df, sim_df) {
  # first add wins
  final_record_df <- count_wins(sim_df) %>% 
    left_join(record_df, ., 
              by = c("team" = "outcome")) %>% 
    mutate(wins = ifelse(is.na(n), wins, wins + n)) %>% 
    select(-n)
  
  return(final_record_df)
}

# function to translate final records to official standings, based on divisions
# and (random) tie-breakers
get_ranking <- function(final_record_df) {
  # rank all teams by number of wins; break ties randomly (this step may not
  # be necessary..)
#   final_record_df <- final_record_df %>% 
#     mutate(rank = rank(desc(wins)))
  
  # select the top teams from each division and rank by number of wins
  final_record_df %>% 
    group_by(division) %>% 
    top_n(1, wins) %>% 
    arrange(desc(wins)) #%>% 
    
    # recombine with the remaining teams
#     bind_rows(anti_join(final_record_df, .) %>% 
#                 arrange(rank)) %>% 
#     select(-rank) %>% 
#     add_rownames("rank") %>% 
#     select(rank, division, team)
}


a_ply(outcomes[1:2, ], 1, function(x) { 
  y <<- w11_df %>% mutate(outcome = x)
  print(y) 
  })


# work out rankings.. -----------------------------------------------------

is_tied <- function(x) {
  duplicated(x) | duplicated(x, fromLast = TRUE)
}

contenders <- y %>% 
  mutate(outcome = ifelse(outcome == 0, home, away)) %>% 
  count_wins() %>% 
  update_records(record_df, .) %>% 
  mutate(rank = rank(desc(wins))) %>% 
  arrange(rank) %>% 
  filter(rank <= 4) %>% 
  select(-rank)

contenders

                      
  
#   mutate(tie = duplicated(wins) | duplicated(wins, fromLast = TRUE))

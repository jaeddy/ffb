
# Load packages -----------------------------------------------------------

library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)


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
weeks_completed <- 11
record_df <- data_frame(team = teams, 
                        division = divisions,
                        wins = c(5, 8, 9, 3, 5, 2, 7, 4, 3, 5, 7, 8),
                        ties = 0) %>% 
  mutate(losses = weeks_completed - wins - ties)

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
sched_df <- bind_rows(list(w12_df, w13_df))


# Define functions --------------------------------------------------------

# function to randomly assign an outcome of 'a', 'b', or 'tie'; uniform
# distibution gives equal probability of either outcome, so basically a coin
# flip (but unlike binomial, allows for the tie)
sim_outcome <- Vectorize(function(a, b) {
  rand_num <- runif(1, 0, 1)
  if (rand_num > 0.5) {
    return(a)
  } else if (rand_num < 0.5) {
    return(b)
  } else {
    return("tie")
  }
})

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
  
  # second add ties and calculate losses
  final_record_df <- count_ties(sim_df) %>% 
    left_join(final_record_df, ., by = "team") %>% 
    mutate(ties = ifelse(is.na(n), ties, ties + n),
           losses = 13 - wins - ties) %>% 
    select(-n)
  return(final_record_df)
}

# function to translate final records to official standings, based on divisions
# and (random) tie-breakers
get_ranking <- function(final_record_df) {
  # rank all teams by number of wins; break ties randomly (this step may not
  # be necessary..)
  final_record_df <- final_record_df %>% 
    mutate(rank = rank(desc(wins), ties.method = "random"))
  
  # select the top teams from each division and rank by number of wins
  final_record_df %>% 
    group_by(division) %>% 
    top_n(1, desc(rank)) %>% 
    ungroup() %>% 
    arrange(desc(wins)) %>% 

    # recombine with the remaining teams
    bind_rows(anti_join(final_record_df, .) %>% 
                arrange(rank)) %>% 
    select(-rank) %>% 
    add_rownames("rank") %>% 
    select(rank, team)
}

# function to count occurences of each final rank for each player over all
# simulations
update_rank_freqs <- function(rankings) {
  a_ply(rankings, 1, function(x) {
    y <<- ranks_df # wonky scope stuff...
    row_idx <- which(y$rank == x$rank)
    col_idx <- which(names(y) == x$team)
    ranks_df[row_idx, col_idx] <<- ranks_df[row_idx, col_idx] + 1
  })
}


# Run simulations ---------------------------------------------------------

# initialize rank frequency dataframe
possible_ranks <- seq(1, 12, 1)
ranks_df <- as.data.frame(matrix(0, nrow = length(possible_ranks), 
                                ncol = length(teams)))
names(ranks_df) <- teams
ranks_df <- ranks_df %>% 
  bind_cols(data_frame(rank = possible_ranks), .)

# do the work... (100,000 random simulations)
for (i in 1:100000) {
  message(i)
  sim_result <- sched_df %>% 
    mutate(outcome = sim_outcome(home, away))
  
  ranking <- update_records(record_df, sim_result) %>% 
    get_ranking()

  update_rank_freqs(ranking)
}

# Check results -------------------------------------------------------

# make sure rank occurrences add up to 100,000 for each team
ranks_df %>% 
  summarise_each_(funs(sum), teams)


# Format results ----------------------------------------------------------

# normalize counts to get probabilities for each rank
probs_df <- ranks_df %>% 
  mutate_each_(funs(./100000), teams)

# use the cumulative sum to get CDFs (and convert to percentage)
cumsum_df <- probs_df %>% 
  mutate_each_(funs(cumsum), teams) %>% 
  mutate_each_(funs(.*100), teams)

# Plot results ------------------------------------------------------------

p <- probs_df %>% 
  # ggplot likes 'tidy' data, so combine teams into a single column
  melt(id.vars = "rank", 
       variable.name = "team", value.name = "prob") %>% 
  
  # leave out zeros to make rendering a bit easier
  filter(prob != 0) %>% 
  
  # build and format plot layers
  ggplot(aes(x = rank, y = prob)) +
  geom_bar(aes(fill = prob), stat = "identity", alpha = 0.6) +
  facet_wrap(~ team, nrow = 3) +
  scale_x_continuous(breaks = 1:12) +
  scale_fill_gradient(low = my_cb_pal[6], high = my_cb_pal[1], 
                      guide = FALSE) +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        panel.grid.minor = element_blank()) +
  ylab("Probability") +
  xlab("Regular season finish")
p

# save to png file
ggsave("/Users/jaeddy/Desktop/sim_standings_w11.png", p,
       width = 8, height = 6)


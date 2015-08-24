library(readr)
library(dplyr)
library(reshape2)

teams <- c("Kevin", "Matt", "Brad", "Tony", "Drew", "James", "Shep", "Milf",
           "Toby", "Keith", "Ross", "Billy")
slots <- c("QB", "RB1", "RB2", "WR1", "WR2", "FLEX", "TE", "DST", "K",
           paste0("BENCH", c(1:7)))

rosterSize <- length(slots)
budget <- 200

# Load ticker
ticker <- read.csv("draftLog.csv", stringsAsFactors = FALSE) 
ticker <- ticker$x

# Load rosters
rosterTable <- read_csv("rosters.csv") %>% 
  select(-1)

# Read and format projections ---------------------------------------------

projections <- read_csv("FFA-Projections-ESPN.csv")

# Format projection data
projections <- projections %>% 
  dplyr::select(name = playername, position, 
                projectedPoints = points, 
                pointsLo = lower, pointsHi = upper, risk, 
                projectedCost = as.numeric(auctionValue)) %>% 
  filter(pointsHi > 0) %>% 
  mutate(projectedCost = ifelse(projectedCost < 1, 1, projectedCost)) %>% 
  # Modify projected costs to better reflect league tendencies
  mutate(projectedCost = ifelse(position == "QB" & projectedCost > 1, 
                                projectedCost + 10, projectedCost)) %>% 
  mutate(projectedCost = ifelse(position == "RB" & projectedCost > 1, 
                                projectedCost + 7, projectedCost)) %>% 
  mutate(projectedCost = ifelse(position == "TE" & projectedCost > 5, 
                                projectedCost + 5, projectedCost)) %>% 
  mutate(projectedCost = ifelse(position == "QB" & projectedCost < 20 & projectedCost > 3, 
                                projectedCost - 3, projectedCost)) %>% 
  na.omit() %>% 
  arrange(desc(projectedPoints))

projections <- projections %>%
  filter(!(name %in% rosterTable$player))

# Set up initial auction budget -------------------------------------------

budgets <-read_csv("budgets.csv")
budgets <- budgets %>% 
  melt(id.vars = "slot", variable.name = "budget", value.name = "amount")

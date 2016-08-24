library(readr)
library(tibble)
library(tidyr)
library(dplyr)
library(purrr)

# Set global variables ----------------------------------------------------

teams <- list("Kevin", "Matt", "Brad", "Tony", "Drew", "James", "Shep", "Milf",
              "Toby", "Keith", "Ross", "Billy")
slots <- c("QB", "RB1", "RB2", "WR1", "WR2", "FLEX", "TE", "DST", "K",
           paste0("BENCH", c(1:7)))

rosterSize <- length(slots)
budget <- 200
maxBid <- budget - length(slots) + 1
ticker <- "no players drafted yet"

# Set up master roster table ----------------------------------------------

init_roster_table <- function(teams, slots) {
  map(teams, function (x) { 
    tibble(team = x,
           slot = slots,
           player = "",
           position = "",
           playerTeam = "",
           projPts = 0,
           bidNum = 0,
           paid = 0) }) %>% 
    bind_rows()
}

rosterTable <- init_roster_table(teams, slots)

# Read and format projections ---------------------------------------------

projection_file <- "FFA-Projections-New.csv"

load_projections <- function(projection_file) {
  projections <- read_csv(projection_file)

  # Format projection data
  projections %>% 
    dplyr::select(name = playername, position, playerTeam = playerteam,
                  projectedPoints = points, 
                  pointsLo = lower, pointsHi = upper, 
                  ecr = overallECR, risk, 
                  projectedCost = as.numeric(auctionValue)) %>% 
    filter(pointsHi > 0) %>% 
    # Modify projected costs to better reflect league tendencies
    mutate(projectedCost = ifelse(projectedCost < 1, 1, projectedCost),
           projectedCost = ifelse(position == "QB" & projectedCost > 1, 
                                  projectedCost + 10, projectedCost),
           projectedCost = ifelse(position == "RB" & projectedCost > 1, 
                                  projectedCost + 7, projectedCost),
           projectedCost = ifelse(position == "TE" & projectedCost > 5, 
                                  projectedCost + 5, projectedCost),
           projectedCost = ifelse(position == "QB" & projectedCost < 20 
                                  & projectedCost > 3, 
                                  projectedCost - 3, projectedCost),
           draftedBy =) %>% 
    arrange(as.numeric(ecr))
}

projections <- load_projections(projection_file)

# Set up initial auction budget -------------------------------------------

budgets_file <- "budgets.csv"

init_budgets <- function(budgets_file, budget) {
  budgets <-read_csv("budgets.csv") %>% 
    gather(budget, amount, -slot)
  
  budgetVal <- budget
  
  budgets %>% 
    group_by(budget) %>% 
    mutate(total = sum(amount),
           amount = ifelse(slot == "BENCH", 
                           budgetVal - total - 5, 
                           amount)) %>% 
    select(-total) %>% 
    ungroup()
}

get_current_budget <- function(budgets, budgetOpt = NULL) {
  if (is.null(budgetOpt)) {
    budgetOpt <- budgets$budget[1]
    budgets %>%
      filter(budget == budgetOpt)
  }
}

budgets <- init_budgets(budgets_file)
curBudget <- get_current_budget(budgets)




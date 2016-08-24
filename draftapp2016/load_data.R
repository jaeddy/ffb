library(readr)
library(dplyr)

source("initialize_data.R")

# Load ticker
ticker <- read.csv("draftLog.csv", stringsAsFactors = FALSE) 
ticker <- ticker$x

# Load rosters
rosterTable <- read_csv("rosters.csv") %>% 
  select(-1)

# Read and format projections ---------------------------------------------

update_projections <- function(projections, rosterTable) {
  projections %>% 
    select(-draftedBy) %>% 
    left_join(rosterTable %>% 
                select(name = player, 
                       playerTeam = position, 
                       draftedBy = team))
}

projections <- load_projections(projection_file) %>% 
  update_projections(rosterTable)

# Set up initial auction budget -------------------------------------------

budgets <- init_budgets(budgets_file)
curBudget <- get_current_budget(budgets)


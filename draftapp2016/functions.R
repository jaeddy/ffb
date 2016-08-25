library(readr)
library(dplyr)
library(reshape2)
library(rCharts)
library(stringr)

add_player_to_roster <- function(rosterTable, playerPacket) {
  openSlots <- rosterTable %>% 
    filter(team == playerPacket$team, player == "")
  openRow <- openSlots %>% 
    filter(str_detect(slot, playerPacket$position)) %>% 
    slice(1)
  playerRow <- full_join(openRow, playerPacket) %>% 
    fill(slot) %>% 
    filter(player != "")
  rosterTable %>% 
    filter(team != playerRow$team | slot != playerRow$slot) %>% 
    bind_rows(playerRow) %>% arrange(team)
}


get_table_idx <- function(rosterTable, team_idx, pos) {
  slot_idx <- team_idx & rosterTable$player == "" &
    grepl(pos, rosterTable$slot)
  
  flex_idx <- team_idx & rosterTable$player == "" &
    grepl("FLEX", rosterTable$slot)
  
  if (!any(slot_idx)) {
    if (any(flex_idx) & pos %in% c("RB", "WR", "TE")) {
      slot_idx <- flex_idx
    } else {
      slot_idx <- team_idx & rosterTable$player == "" &
        grepl("BENCH", rosterTable$slot)
    }
  }
  
  table_idx <- seq(along = slot_idx)[slot_idx]
  return(table_idx)
}

get_money_chart <- function(rosterTable, budget) {
  chartTable <- rosterTable %>% 
    group_by(team) %>% 
    summarise(remainingBudget = budget - sum(paid),
              remainingSlots = sum(player == ""),
              maxBid = remainingBudget - remainingSlots + 1) %>%
    select(team, remainingBudget, maxBid) %>%
    melt(id.vars = "team", variable.name = "budget", 
         value.name = "amount")
  
  p <- nPlot(amount ~ team, group = "budget", data = chartTable,
              type = "multiBarChart", 
              width = 300, height = 250)
  p$chart(color = c("#1f77b4", "#ff7f0e"),
           reduceXTicks = FALSE,
           showControls = FALSE)
  p$xAxis(staggerLabels = FALSE,
           rotateLabels = 45)
  p$addParams(dom = "moneyChart")
  return(p)
}

get_percent_chart <- function(rosterTable, rosterSize) {
  chartTable <- rosterTable %>% 
    group_by(team) %>% 
    summarise(slotsFilled = sum(player != ""),
              slotsRemaining = rosterSize - slotsFilled) %>%
    select(team, slotsFilled, slotsRemaining) %>%
    melt(id.vars = "team", variable.name = "slots", value.name = "amount")
  
  p <- nPlot(amount ~ team, group = "slots", data = chartTable,
              type = "multiBarHorizontalChart",
              width = 300, height = 250)
  p$chart(color = c("#1f77b4", "#ff7f0e"),
           showControls = FALSE, stacked = TRUE)
  p$addParams(dom = "percentChart")
  return(p)
}

get_current_budget <- function(budgets, budgetOpt) {
  budgets %>% 
    filter(budget == budgetOpt)
}

format_budget <- function(budgets, budgetOpt) {
  curBudget <- get_current_budget(budgets, budgetOpt) %>% 
    mutate(amount = as.character(amount))
  row.names(curBudget) <- curBudget$slot
  curBudget %>% 
    select(-budget, -slot) %>% 
    t() %>% 
    as.data.frame()
}

get_my_table <- function(budgets, projections, budgetOpt, maxBid,
                         displaySlots, numPicks, wiggle) {
  
  curBudget <- get_current_budget(budgets, budgetOpt) %>% 
    mutate(amount = ifelse(slot == "BENCH", maxBid, amount))
  
  pickList <- lapply(as.list(c(1:nrow(curBudget))), function (x) {
    budgetSlot <- curBudget %>% slice(x)
    slot <- budgetSlot$slot
    
    if (slot == "FLEX") {
      posList <- c("RB", "WR", "TE")
    } else if (slot == "BENCH") {
      posList <- unique(projections$position)
    } else {
      posList <- str_extract(slot, "[A-Z]+")
    }
    
    projections %>%
      filter(position %in% posList ,
             projectedCost <= budgetSlot$amount + wiggle) %>% 
      slice(1:numPicks) %>%
      mutate(position = slot) %>% 
      rename(team = playerTeam)
  })
  
  bind_rows(pickList) %>% 
    filter(position %in% displaySlots)
}

get_team_table <- function(rosterTable, viewTeam) {
  rosterTable %>% 
    filter(team == viewTeam)
}

get_current_roster <- function(rosterTable, viewTeam) {
  get_team_table(rosterTable, viewTeam) %>% 
    select(slot, player, position, team = playerTeam, projPts, paid) %>%
    mutate(projPts = as.character(projPts),
           paid = as.character(paid))
}

get_spent_budget <- function(rosterTable, viewTeam) {
  teamTable <- get_team_table(rosterTable, viewTeam)
  sum(as.numeric(teamTable$paid))
}

get_remaining_budget <- function(rosterTable, viewTeam, budget) {
  budget - get_spent_budget(rosterTable, viewTeam)
}

get_filled_slots <- function(rosterTable, viewTeam) {
  teamTable <- get_team_table(rosterTable, viewTeam)
  sum(teamTable$player != "")
}

get_remaining_slots <- function(rosterTable, viewTeam, rosterSize) {
  rosterSize - get_filled_slots(rosterTable, viewTeam)
}

get_max_bid <- function(rosterTable, viewTeam, budget, rosterSize) {
  get_remaining_budget(rosterTable, viewTeam, budget) -
    get_remaining_slots(rosterTable, viewTeam, rosterSize) + 1
}

write_file <- function() {
  
}


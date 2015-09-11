# Select the best player for each roster slot
fill_optimum_slot <- function(currentSlot, roster) {
  numStarters = list(QB = 1, RB = 2, WR = 2, TE = 1, DST = 1, K = 1)
  if (currentSlot == "FLEX") {
    optSlot <- roster %>% 
      ungroup() %>% 
      filter(position %in% c("RB", "WR", "TE")) %>% 
      group_by(position) %>% 
      filter(rank(-points) > numStarters[position]) %>% 
      ungroup() %>% 
      slice(1)
  } else if (str_detect(currentSlot, "[1-2]")) {
    slotPos <- str_replace(currentSlot, "[1-2]", "")
    slotNum <- str_extract(currentSlot, "[1-2]") %>% as.numeric()
    optSlot <- roster %>%
      ungroup() %>% 
      filter(position %in% slotPos) %>% 
      arrange(desc(points)) %>% 
      slice(slotNum)
  } else {
    optSlot <- roster %>% 
      ungroup() %>% 
      filter(position %in% currentSlot) %>% 
      top_n(1)
  }
  optSlot <- mutate(optSlot, slot = currentSlot)
  return(optSlot)
}

# Select optimum starting roster for current team
fill_optimum_roster <- function(roster) {
  startSlots <- c("QB", "RB1", "RB2", "WR1", "WR2", "FLEX", "TE",
                  "DST", "K")
  for (startSlot in startSlots) {
    if (startSlot == "QB") {
      optRoster <- fill_optimum_slot(startSlot, roster)
    } else {
      optRoster <- optRoster %>% 
        bind_rows(fill_optimum_slot(startSlot, roster))
    }
  }
  return(optRoster)
}

optimize_rosters <- function(rosters, pointSource) {
  for (curOwner in unique(rosters$owner)) {
    roster <- rosters %>%
      filter(owner == curOwner) %>% 
      rename_(points = pointSource) %>% 
      select(owner, slot, position, player, points)
      
    
    if (curOwner == unique(rosters$owner)[1]) {
      optRosters <- fill_optimum_roster(roster)
    } else {
      optRosters <- optRosters %>% 
        bind_rows(fill_optimum_roster(roster))
    }
  }
  return(optRosters)
}

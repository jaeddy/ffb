library(testthat)

source("initialize_data.R")
source("functions.R")

team <- "James"
bidNum <- 1
paid <- 100

playerPacket <- projections %>% 
  slice(1) %>% 
  select(player = name, position, playerTeam, projPts = projectedPoints) %>% 
  mutate(team = team, bidNum = bidNum, paid = paid)

context("Roster updates")
test_that("add_player_to_roster correctly inserts information", {
  updatedTable <- add_player_to_roster(rosterTable, playerPacket)
  expect_equal(updatedTable %>% 
                 select(one_of(names(playerPacket))) %>% 
                 intersect(playerPacket), 
               playerPacket)
})
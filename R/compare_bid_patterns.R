
# Load environment --------------------------------------------------------

# Library
library(Rglpk)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(readr)

# Format inputs -----------------------------------------------------------

projections <- read_csv(paste0(getwd(),"/data/FFA-Projections-ESPN.csv"))

# Select input data for roster optimization
inputData <- projections %>% 
    mutate(name = toupper(playername) %>% str_replace_all(" |\\.", "")) %>% 
    dplyr::select(name, player = playername, pos = position, 
                  pointsLo = lower, pointsHi = upper, points, risk, 
                  avgCost = as.numeric(auctionValue)) %>% 
    filter(pointsHi > 0) %>% 
    mutate(pos = as.factor(pos)) %>% 
    mutate(avgCost = ifelse(avgCost < 1, 1, avgCost)) %>% 
    mutate(avgCost = ifelse(pos == "QB" & avgCost > 1, 
                            avgCost + 10, avgCost)) %>% 
    mutate(avgCost = ifelse(pos == "RB" & avgCost > 1, 
                            avgCost + 7, avgCost)) %>% 
    mutate(avgCost = ifelse(pos == "TE" & avgCost > 5, 
                            avgCost + 5, avgCost)) %>% 
    mutate(avgCost = ifelse(pos == "QB" & avgCost < 20 & avgCost > 3, 
                            avgCost - 3, avgCost)) %>% 
    na.omit() %>% 
    arrange(desc(points))

# Compare cost distributions ----------------------------------------------

ffaBids <- inputData %>% 
    arrange(desc(avgCost)) %>% 
    slice(1:200)

auction2014 <- read_csv("draftapp/results/rosters.csv")
bidDat <- auction2014 %>% 
    filter(projPts > 0) %>% 
    arrange(desc(paid)) %>% 
    slice(1:200)

compareDat <- bidDat %>% 
    select(pos = position, paid) %>% 
    mutate(costSource = "jeffCup") %>% 
    group_by(pos) %>% 
    mutate(posRank = rank(-paid)) %>% 
    bind_rows(ffaBids %>% 
                  select(pos, paid = avgCost) %>% 
                  mutate(costSource = "avgCost") %>% 
                  group_by(pos) %>% 
                  mutate(posRank = rank(-paid)))

compareDat %>% 
    ggplot(aes(x = posRank, y = paid)) +
    geom_line(aes(colour = costSource)) +
    facet_wrap(~ pos, scales = "free")

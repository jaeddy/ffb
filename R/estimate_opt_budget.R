
### FROM `Optimum Roster.R`

# dev.off()
# Load environment --------------------------------------------------------

# Library
library(Rglpk)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(readr)

# Functions
source(paste(getwd(),"/R/ffa_functions.R", sep=""))
source(paste(getwd(),"/R/ffa_settings.R", sep=""))

# Customize auction parameters
maxCost <- 185

# Functions ---------------------------------------------------------------

# Randomly generate simulated point values
simulate_points <- Vectorize(function(pointsHi, pointsLo, points, risk) {
    range <- pointsHi - pointsLo
    
    alpha <- 3
    beta <- 3 # risk
    
    skew = points - (pointsLo + range / 2)
    skew <- ifelse(skew >= 0, skew, 0)
    X <- rbeta(1, alpha, beta, ncp = skew / 3) # risk
    simPoints <- X * range + pointsLo
    return(simPoints)
})

# Parse `optimizeTeam` roster to tally costs per position
get_pos_costs <- function(optTeam, pos_i) {
    posDat <- optTeam$playerInfo %>% 
        filter(pos == pos_i)
    
    posSlots <- slots[str_detect(slots, pos_i)]
    
    slotMatches <- c()
    for (row in rownames(posDat)) {
        cost <- posDat[row, "cost"]
        
        slot <- paste0(pos_i, row)
        vals[[slot]] <<- append(vals[[slot]], cost)
        slotMatches <- append(slotMatches, slot)
    }
    posExtra <- setdiff(posSlots, slotMatches)
    if (length(posExtra)) {
        vals[[posExtra]] <<- append(vals[[posExtra]], 0)
    }
}


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

# Run simulations ---------------------------------------------------------

# Initialize objects for storing simulation results
slots <- c("QB1", "RB1", "RB2", "RB3", "WR1", "WR2", "WR3", "TE1", 
           "TE2", "K1", "DST1")
posList <- c("QB", "RB", "WR", "TE", "K", "DST")

vals <- rep(list(0), length(slots))
names(vals) <- slots

# Specify number of simulations
numSims <- 10000

system.time(for (sim in 1:numSims) {
    message(sprintf("Simulation %s of %s", sim, numSims))
    
    # Add simulated (noisy) points to input data
    optimizeData <- inputData %>% 
        mutate(simPoints = simulate_points(pointsHi, pointsLo, points, risk)) %>% 
        dplyr::select(-pointsLo, -pointsHi, -points) %>% 
        rename(points = simPoints)
    
    # Calculate Optimum Roster
    optTeam <- optimizeTeam(maxRisk = 9, playerCost = optimizeData$avgCost)
    
    # Tally team costs
    for (pos_i in posList) {
        get_pos_costs(optTeam, pos_i)
    }
})

valDat <- as_data_frame(vals) %>% 
    slice(2:n()) %>% 
    rename(K = K1, DST = DST1) %>% 
    melt(variable.name = "slot", value.name = "cost") %>% 
    mutate(cost = round(cost))

# Plot results ------------------------------------------------------------

valDat %>% 
    ggplot(aes(x = cost)) +
    geom_density(aes(fill = slot), alpha = 0.5) +
    facet_wrap(~ slot, scales = "free")

# Get optimum budget ------------------------------------------------------

slotCosts <- valDat %>% group_by(slot, cost) %>% 
    tally() %>% 
    ungroup() %>% 
    filter(!(slot %in% c("K", "DST") & duplicated(slot, fromLast = TRUE))) %>% 
    group_by(slot) %>%
    top_n(2) 

predRoster <- slotCosts %>% 
    group_by(slot) %>% 
    filter(n == max(n)) %>% 
    ungroup() %>% 
    mutate(total = cumsum(cost))
predRoster

altRoster <- predRoster %>% 
    select(-total) %>% 
    anti_join(slotCosts, .) %>% 
    ungroup() %>% 
    arrange(desc(n))
altRoster



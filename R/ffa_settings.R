###########################
# File: League Settings.R
# Description: User sets league settings
# Date: 6/1/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Season
season <- 2015
weekNo <- 0   # Set weekNo = 0 for seasonal projections

#Roster
numQBstarters <- 1
numRBstarters <- 2
numWRstarters <- 2
numTEstarters <- 1
numKstarters <- 1
numDSTstarters <- 1
numTotalStarters <- 9
numTotalPlayers <- 16

#League settings
defaultCap <- 200           #what the typical cap is for your service (ESPN, Yahoo, etc.) -- used for placing "avg cost" in context
leagueCap <- 200            #your league's cap
maxCost <- leagueCap - (numTotalPlayers - numTotalStarters)

#Variable names
prefix <- c("name","pos","sourceName")
sourceSpecific <- c("name","team")
scoreCategories <- c("passAtt","passComp","passIncomp","passYds","passTds","passInt",
                     "rushAtt","rushYds","rushTds",
                     "rec","recTgt","recYds","recTds",
                     "returnTds","twoPts","fumbles",
                     "idpSolo","idpAst","idpSack","idpFumlRec","idpFumlForce","idpInt","idpPD",
                     "dstPtsAllowed","dstYdsAllowed","dstSack","dstSafety","dstInt","dstFumlRec","dstFumlForce","dstBlk","dstTd",
                     "fg","fgAtt","fg019","fg2029","fg3039","fg4049","fg50","xp")
calculatedVars <- c("positionRank","overallRank","points","pointsLo","pointsHi","vor","pick","risk","sdPts","sdPick")
varNames <- c(calculatedVars, scoreCategories)
finalVarNames <- c("name","pos","team","sourceName","player","playerID","season", "playerId", "analystId", varNames)

#Scoring
passAttMultiplier <- 0      #0 pts per passing attempt
passCompMultiplier <- 0     #0 pts per passing completion
passIncompMultiplier <- 0   #0 pts per passing incompletion
passYdsMultiplier <- (1/25) #1 pt per 25 passing yds
passTdsMultiplier <- 6      #6 pts per passing td
passIntMultiplier <- -2     #-3 pts per passing interception
rushAttMultiplier <- 0      #0 pts per rushing attempt
rushYdsMultiplier <- (1/10) #1 pt per 10 rushing yds
rushTdsMultiplier <- 6      #6 pts per rushing touchdown
recMultiplier <- 0          #0 pts per reception
recYdsMultiplier <- (1/10)   #1 pt per 8 receiving yds
recTdsMultiplier <- 6       #6 pts per receiving touchdown
returnTdsMultiplier <- 6    #6 pts per return touchdown
twoPtsMultiplier <- 2       #2 pts per 2-point conversion
fumlMultiplier <- -2        #-3 pts per fumble lost

scoringRules <- list(
    QB = data.frame(dataCol = c("passYds", "passTds", "passInt", "rushYds", "rushTds", "twoPts", "fumbles"),
                    multiplier = c(1/25, 6, -2, 1/10, 6, 2, -2 )),
    RB = data.frame(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                    multiplier = c(1/10, 6, 0, 1/10, 6, 6, 2, -2)), 
    WR = data.frame(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                    multiplier = c(1/10, 6, 0, 1/10, 6, 6, 2, -2)),
    TE = data.frame(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                    multiplier = c(1/10, 6, 0, 1/10, 6, 6, 2, -2)),
    K = data.frame(dataCol = c("xp", "fg", "fg0019", "fg2029", "fg3039", "fg4049", "fg50"),
                   multiplier = c(1, 3, 3, 3, 3, 3, 3)),
    DST = data.frame(dataCol = c("dstFumlRec", "dstInt", "dstSafety", "dstSack", "dstTd", "dstBlk"),
                     multiplier = c(2, 2, 2, 1, 6, 1.5)),
    ptsBracket = data.frame(threshold = c(0, 6, 13, 17, 22, 28, 35, 46),
                             points = c(10, 7, 4, 1, -1, -4, -7, -10))
  )


#Projections
# c("CBS", "ESPN", "Yahoo") #
sourcesOfProjections <- c("Accuscore", "CBS1", "CBS2", "EDSfootball", "ESPN", "FantasyFootballNerd", "FantasyPros", "FantasySharks", "FFtoday", "Footballguys1", "Footballguys2", "Footballguys3", "Footballguys4", "FOX", "NFL", "numberFire", "WalterFootball", "Yahoo")

# sourcesOfProjections <- c("Jamey Eisenberg", "Dave Richard", "Yahoo Sports" , "ESPN", "NFL", "FOX Sports", "FFtoday", 
                          # "NumberFire", "FantasyPros") #, "Dodds-Norton", "Dodds", "Tremblay", "Herman", "Henry", "Wood", "Bloom") 

# sourcesOfProjectionsAbbreviation <- c("cbs", "espn", "yahoo") #

sourcesOfProjectionsAbbreviation <- c("accu", "cbs1", "cbs2", "eds", "espn", "ffn", "fp", "fs", "fftoday", "fbg1", "fbg2", "fbg3", "fbg4", "fox", "nfl", "nf", "wf", "yahoo")

#Weights applied to each source in calculation of weighted average of projections
weight_accu <- 1    #Accuscore
weight_cbs1 <- 1    #Jamey Eisenberg
weight_cbs2 <- 1    #Dave Richard"
weight_eds <- 1     #EDS Football
weight_espn <- 1    #ESPN
weight_ffn <- 1     #Fantasy Football Nerd
weight_fbg1 <- 1    #Footballguys: David Dodds
weight_fbg2 <- 1    #Footballguys: Bob Henry
weight_fbg3 <- 1    #Footballguys: Maurile Tremblay
weight_fbg4 <- 1    #Footballguys: Jason Wood
weight_fox <- 1    #FOX
weight_fp <- 1      #FantasyPros
weight_fs <- 1      #FantasySharks
weight_fftoday <- 1 #FFtoday
weight_nfl <- 1     #NFL.com
weight_nf <- 1      #numberFire
weight_wf <- 1      #WalterFootball
weight_yahoo <- 1   #Yahoo 

#Number of players at each position drafted in Top 100 (adjust for your league)
# qbReplacements <- 15
# rbReplacements <- 37
# wrReplacements <- 36
# teReplacements <- 11

#Alternative way of calculating the number of players at each position drafted in Top 100 based on league settings
numTeams <- 12  #number of teams in league
numQB <- 1      #number of avg QBs in starting lineup
numRB <- 2.33    #number of avg RBs in starting lineup
numWR <- 2.33    #number of avg WRs in starting lineup
numTE <- 1.33      #number of avg TEs in starting lineup

qbReplacements <- print(ceiling(numQB*numTeams*1.7))
rbReplacements <- print(ceiling(numRB*numTeams*1.4))
wrReplacements <- print(ceiling(numWR*numTeams*1.4))
teReplacements <- print(ceiling(numTE*numTeams*1.3))
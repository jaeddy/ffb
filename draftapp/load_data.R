library(dplyr)

teams <- c("Kevin", "Matt", "Brad", "Tony", "Drew", "James", "Shep", "Milf",
           "Toby", "Keith", "Ross", "Billy")
slots <- c("QB", "RB1", "RB2", "WR1", "WR2", "FLEX", "TE", "DST", "K",
           paste0("BENCH", c(1:7)))

rosterSize <- length(slots)
budget <- 200
ticker <- read.csv("results/draftLog.csv", stringsAsFactors = FALSE)
ticker <- ticker[, -1]

rosterTable <- read.csv("results/rosters.csv")
rosterTable$player <- as.character(rosterTable$player)
rosterTable$position <- as.character(rosterTable$position)

bTate <- rosterTable[74, 4:7]
dMcfadden <- rosterTable[174, 4:7]
empty <- rosterTable[106, 4:7]

rosterTable[74, 4:7] <- empty
rosterTable[106, 4:7] <- bTate
rosterTable[174, 4:7] <- empty
rosterTable[127, 4:7] <- dMcfadden

projections <- read.csv("projections.csv")
projections <- projections[, c(2:4, 6:13)]

k <- read.csv("kickers.csv")
dst <- read.csv("defenses.csv")
projections <- rbind(projections, k, dst)

projections$name <- as.character(projections$name)
projections <- projections %>% filter(!(name %in% unique(rosterTable$player)))

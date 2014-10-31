library(xlsx)
library(dplyr)


# load relevant data from the draft
draftRosters <- read.csv("draftapp/results/rosters.csv", stringsAsFactors = FALSE)
mattsLog <- read.xlsx("Fantasy Draft.xlsx", sheetName = "Drafted",
                      startRow = 3, endRow = 195, colIndex = c(2:5))

# load & format additional player database
projections <- read.csv("draftapp/projections.csv")
projections <- projections[, c(2:4, 6:13)]

k <- read.csv("draftapp/kickers.csv")
dst <- read.csv("draftapp/defenses.csv")
projections <- rbind(projections, k, dst)

projections$name <- as.character(projections$name)

dR <- draftRosters %>% 
    mutate(player = ifelse(player == "Ben Tate", "", player),
           position = ifelse(player == "", "", position),
           projPts = ifelse(player == "", 0, projPts),
           paid = ifelse(player == "", 0, paid))

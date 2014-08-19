
teams <- c("Kevin", "Matt", "Brad", "Tony", "Drew", "James", "Shep", "Milf",
           "Toby", "Keith", "Ross", "Billy")
slots <- c("QB", "RB1", "RB2", "WR1", "WR2", "FLEX", "TE", "DST", "K",
           paste0("BENCH", c(1:7)))

rosterSize <- length(slots)
budget <- 200
ticker <- "no players drafted yet"

rosterTable <- data.frame(matrix(ncol = 4, nrow = 0))
for (teamName in teams) {
    rosterTable <- rbind(rosterTable, data.frame(team = teamName,
                                                 slot = slots, 
                                                 player = "",
                                                 position = "",
                                                 projPts = 0,
                                                 paid = 0))
}
rosterTable$player <- as.character(rosterTable$player)
rosterTable$position <- as.character(rosterTable$position)

projections <- read.csv("projections.csv")
projections <- projections[, c(2:4, 6:13)]
projections$name <- as.character(projections$name)
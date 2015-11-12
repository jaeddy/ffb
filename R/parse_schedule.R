library(httr)
library(XML)
library(dplyr)

schedule_html_path <- "data/jeff_cup_schedule.html"
schedule_html <- htmlParse(schedule_html_path)
root = xmlRoot(schedule_html)
body <- xmlChildren(root)[[2]]
xmlChildren(body)[2]

season <- 2015
teams <- 1:12
rosterTable <- data.frame()
team <- 1
for (team in teams[1]) {
  teamURL <- paste0(leagueURL, "&teamId=", team, "&seasonId=", season)
  teamTables <- readHTMLTable(teamURL, stringsAsFactors = FALSE)
  #posProj <- posTables[[4]]
  #posProj <- posProj[-(1:15), c(2, ncol(posProj))]
  #posProj[, 1] <- gsub("Â[[:space:]]*", "", posProj[, 1])
  
  #posTable <- rbind(posTable, posProj)
}
#posName <- posName[pos %in% positions]
colnames(posTable) <- c(posName, "fftoday")


}
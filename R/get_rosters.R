library(httr)
library(XML)
library(dplyr)

leagueURL <- "http://games.espn.go.com/ffl/clubhouse?leagueId=52048"
season <- 2014
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


#<input type="hidden" name="failedAttempts" value="2">
#<input type="hidden" name="SUBMIT" value="1">
#<input type="hidden" name="failedLocation" value="http://games.espn.go.com/ffl/signin?redir=http%3A%2F%2Fgames.espn.go.com%2Fffl%2Fclubhouse%3FteamId%3D5%26leagueId%3D52048%26seasonId%3D2014&e=1">
#<input type="hidden" name="aff_code" value="espn_fantgames">
#<input type="hidden" name="appRedirect" value="http://games.espn.go.com/ffl/clubhouse?teamId=5&leagueId=52048&seasonId=2014">
#<input type="hidden" name="cookieDomain" value=".go.com">
#<input type="hidden" name="multipleDomains" value="true">
    
handle <- handle("http://games.espn.go.com/ffl/")
path <- "signin"
url <- "http://games.espn.go.com/ffl/signin"

path <- "espn/memberservices/pc/login"
handle <- handle("https://r.espn.go.com/")

submit = "Sign In",
failedAttempts = "2",
SUBMIT = "1",
failedLocation = "http://games.espn.go.com/ffl/signin&e=1",
aff_code = "espn_fantgames",
cookieDomain = ".go.com",
multipleDomains = "true",


login <- list(
    username = "elstupido85",
    password = "victory",
    appRedirect = "http://games.espn.go.com/ffl/frontpage")
#     failedLocation = paste0("http://games.espn.go.com/ffl/signin?redir=",
#                             "http%3A%2F%2Fgames.espn.go.com%2Fffl",
#                             "%2Fclubhouse%3FteamId%3D5%26",
#                             "leagueId%3D52048%26seasonId%3D2014&e=1"),
#    appRedirect = paste0("http://games.espn.go.com/ffl/clubhouse?",
#                         "leagueId=52048&teamId=5&seasonId=2014"))

    
response <- POST(url = url, body = login, encode = "form")
response$url
summary(response)

teamTables <- readHTMLTable(content(response))
    
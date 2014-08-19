library(XML)
library(dplyr)

fftodayURL <- "http://www.fftoday.com/rankings/playerproj.php?"
season <- 2014
positions <- c(10, 20, 30, 40, 80, 99)
posNames <- c("qb", "rb", "wr", "te", "k", "dst")
positions <- c(10)
projTable <- data.frame()
for (pos in positions) {
    positionURL <- paste0(fftodayURL, "Season=", season, "&PosID=", pos,
                          "&order_by=FFPts&sort_order=DESC")
    posTable = data.frame()
    for (page in seq(0, 1)) {
        tableURL <- paste0(positionURL, "&cur_page=", page)
        
        posTables <- readHTMLTable(tableURL, stringsAsFactors = FALSE)
        posProj <- posTables[[4]]
        posProj <- posProj[-(1:15), c(2, ncol(posProj))]
        posProj[, 1] <- gsub("Â[[:space:]]*", "", posProj[, 1])
        
        posTable <- rbind(posTable, posProj)
    }
    posName <- posName[pos %in% positions]
    colnames(posTable) <- c(posName, "fftoday")
    
    
}


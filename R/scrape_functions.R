

init_remote_driver <- function() {
  remote_driver <- remoteDriver(port = 4445L)
  remote_driver$open()
  remote_driver$setImplicitWaitTimeout(milliseconds = 1000)
  return(remote_driver)
}

start_connection <- function(remote_driver, url) {
  remote_driver$navigate(season_url)
  return(remote_driver)
}

espn_login <- function(remote_driver) {
  login_elem <- remote_driver$findElement(using = "id", 
                                          value = "disneyid-iframe")
  remote_driver$switchToFrame(login_elem)
  input_elems <- remote_driver$findElements(using = "xpath", 
                                            value = "//input")
  
  input_elems[[1]]$sendKeysToElement(list("james.a.eddy@gmail.com"))
  input_elems[[2]]$sendKeysToElement(list("**PASSWORD**"))
  
  button_elem <- remote_driver$findElements(using = "class name", 
                                            value = "btn")
  button_elem[[1]]$clickElement()
  return(remote_driver)
}

scrape_seasons <- function(league_id, year_start, year_end) {
  espn_base_url <- "http://games.espn.com/ffl/"
  schedule_url <- sprintf("%s/schedule?leagueId=%s", espn_base_url, league_id)
  start_url <- sprintf("%s&seasonId=%s", schedule_url, year_end)
}
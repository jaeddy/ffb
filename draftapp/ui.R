library(shiny)
library(rCharts)

source("initialize_data.R")

shinyUI(fluidPage(
    
    titlePanel("Draft App"),
    
    fluidRow(
        column(4,
               h3("Auction Control"),
               wellPanel(
                   fluidRow(
                       column(8,
                              selectInput("player",
                                          "Player nominated:",
                                          projections$name,
                                          multiple = TRUE,
                                          selectize = TRUE)
                       ),
                       column(4,
                              textInput("amount",
                                        "Winning bid:")
                       )
                   ),
                   fluidRow(
                       selectInput("draftTeam",
                                   "Highest bidder:",
                                   c(Choose = "", teams),
                                   selectize = TRUE)
                   ),
                   fluidRow(
                       p("Finalize auction:"),
                       actionButton("update",
                                    "Sold!")
                   )
               ),
               h3("Draft Log"),
               uiOutput("ticker")
        ),
        
        column(8,
               h3("League Information"),
               tabsetPanel(
                   tabPanel("Money",
                          showOutput("moneyChart", "nvd3")
                          ),
                   tabPanel("Draft Completion",
                          showOutput("percentChart", "nvd3")
                          )
               ),
               
               h3("Team Information"),
               selectInput("viewTeam", label = NULL, 
                           choices = teams, selected = "Select team..."),
               
               wellPanel(
                   fluidRow(
                       column(6,
                              h4(textOutput("currdentTeam")),
                              tableOutput("currentTable")
                       ),
                       column(6,
                              h4("Summary"),
                              fluidRow(
                                  column(6,
                                         strong("Slots filled:"), 
                                         textOutput("filledSlots"),
                                         br(),
                                         strong("Slots remaining:"), 
                                         textOutput("remainingSlots"),
                                         br(),
                                         strong("Maximum bid:"),
                                         textOutput("maxBid")
                                  ),
                                  column(6,
                                         strong("Total spent:"),
                                         textOutput("spentBudget"),
                                         br(),
                                         strong("Budget remaining:"),
                                         textOutput("remainingBudget")
                                  )
                              )
                       )
                   )
               )
               
        )
    )
))
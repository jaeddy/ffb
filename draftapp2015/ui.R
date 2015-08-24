library(shiny)
library(rCharts)

source("initialize_data.R")

shinyUI(fluidPage(
  
  titlePanel("Draft App"),
  hr(),
  fluidRow(
    # Left panel
    column(4,
           h3("Auction Control"),
           wellPanel(
             fluidRow(
               column(5,
                      h4("Current bid:")),
               column(7,
                      h4(textOutput("bidNum")))),
             fluidRow(
               column(8,
                      selectInput("player", "Player nominated:",
                                  projections$name,
                                  selected = projections$name[1],
                                  multiple = TRUE,
                                  selectize = TRUE)),
               column(4,
                      textInput("amount", "Winning bid:", value = 1))),
             fluidRow(
               column(12,
                      selectInput("draftTeam", "Highest bidder:",
                                  c(Choose = "", teams),
                                  selectize = TRUE))),
             fluidRow(
               column(5,
                      p("Finalize auction:"),
                      actionButton("update", "Sold!")),
               column(7,
                      p("Already drafted?"),
                      actionButton("load", "Load previous data")))),
           hr(),
           h3("Draft Tracking"),
           tabsetPanel(
             tabPanel("Money",
                      br(),
                      showOutput("moneyChart", "nvd3")),
             tabPanel("Draft Completion",
                      br(),
                      showOutput("percentChart", "nvd3"))
           ),
           hr(),
           h3("Draft Log"),
           uiOutput("ticker")),
    
    # Right panel
    column(8,
           h3("Suggested Picks"),
           fluidRow(
             column(3,
                    radioButtons("budgetOpt", label = strong("Budget"),
                                 choices = as.list(unique(as.character(budgets$budget))), 
                                 selected = unique(as.character(budgets$budget))[1],
                                 inline = TRUE)),
             column(9,
                    div(
                      tableOutput("budgetTable"),
                      style = "font-size:80%"))),
           fluidRow(
             column(12,
                    checkboxGroupInput("displaySlots", label = strong("Slots to Display"), 
                                       choices = as.list(unique(as.character(budgets$slot))),
                                       selected = unique(as.character(budgets$slot)),
                                       inline = TRUE))),
           fluidRow(
             column(3,
                    textInput("wiggle", label = "Bid wiggle:", value = 10)),
             column(9,
                    textInput("numPicks", label = "Number of picks per slot:",
                              value = 5))),
           fluidRow(
             div(
               dataTableOutput("myTable"),
               style = "font-size:80%")),
           hr(),
           h3("League Information"),
           tabsetPanel(
             tabPanel("Remaining Players",
                      fluidRow(
                        br(),
                        div(
                          dataTableOutput("remainTable"),
                          style = "font-size:80%"))),
             tabPanel("Rosters",
                      br(),
                      selectInput("viewTeam", label = NULL, 
                                  choices = teams, selected = "Select team..."),
                      
                      wellPanel(
                        fluidRow(
                          column(7,
                                 h4(textOutput("currentTeam")),
                                 div(
                                   tableOutput("currentTable"),
                                   style = "font-size:80%")),
                          column(5,
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
                                          textOutput("maxBid")),
                                   column(6,
                                          strong("Total spent:"),
                                          textOutput("spentBudget"),
                                          br(),
                                          strong("Budget remaining:"),
                                          textOutput("remainingBudget")))))))))
  )
))
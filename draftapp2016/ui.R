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
             hr(),
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
                      p("Clear last auction result"),
                      actionButton("undo", "Undo"))),
             hr(),
             div(
               fluidRow(
                 column(2,
                        p("Already drafted?")),
                 column(10,
                        actionButton("load", "Load previous data"))),
               style = "font-size:90%")),
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
           h3("My Picks"),
           fluidRow(
             column(12,
                    radioButtons("budgetOpt", label = strong("Budget strategy"),
                                 choices = as.list(unique(as.character(budgets$budget))), 
                                 selected = unique(as.character(budgets$budget))[1],
                                 inline = TRUE))),
           tabsetPanel(
             tabPanel("Suggested Picks",
                      br(),
                      fluidRow(
                        column(3,
                               p(strong("Current budget:"))),
                        column(9,
                               div(
                                 tableOutput("budgetSummary"),
                                 style = "font-size:80%"))),
                      fluidRow(
                        column(12,
                               checkboxGroupInput("displaySlots", label = strong("Slots to display"), 
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
                        column(12,
                               strong("Best players within budget"),
                               div(
                                 br(),
                                 dataTableOutput("myTable"),
                                 style = "font-size:80%")))),
             tabPanel("Edit Budget",
                      fluidRow(
                        column(5,
                               br(),
                               div(
                               textInput("qbVal", label = "QB",
                                         value = (curBudget %>% filter(slot == "QB"))$amount),
                               tags$style(type='text/css', 
                                          "#qbVal { font-size:90%; height:25px; width:50px; display:inline-block; }"),
                               textInput("rb1Val", label = "RB1",
                                         value = (curBudget %>% filter(slot == "RB1"))$amount),
                               tags$style(type='text/css', 
                                          "#rb1Val { font-size:90%; height:25px; width:50px; display:inline-block; }"),
                               textInput("rb2Val", label = "RB2",
                                         (curBudget %>% filter(slot == "RB2"))$amount),
                               tags$style(type='text/css', 
                                          "#rb2Val { font-size:90%; height:25px; width:50px; display:inline-block; }"),
                               textInput("wr1Val", label = "WR1",
                                         (curBudget %>% filter(slot == "WR1"))$amount),
                               tags$style(type='text/css', 
                                          "#wr1Val { font-size:90%; height:25px; width:50px; display:inline-block; }"),
                               textInput("wr2Val", label = "WR2",
                                         (curBudget %>% filter(slot == "WR2"))$amount),
                               tags$style(type='text/css', 
                                          "#wr2Val { font-size:90%; height:25px; width:50px; display:inline-block; }"),
                               style = "font-size:80%; text-align:right")),
                        column(3,
                               br(),
                               div(
                                 textInput("flexVal", label = "FLEX",
                                           (curBudget %>% filter(slot == "FLEX"))$amount),
                                 tags$style(type='text/css', 
                                            "#flexVal { font-size:90%; height:25px; width:50px; display:inline-block; }"),
                                 textInput("teVal", label = "TE",
                                           (curBudget %>% filter(slot == "TE"))$amount),
                                 tags$style(type='text/css', 
                                            "#teVal { font-size:90%; height:25px; width:50px; display:inline-block; }"),
                                 textInput("dstVal", label = "DST",
                                           (curBudget %>% filter(slot == "DST"))$amount),
                                 tags$style(type='text/css', 
                                            "#dstVal { font-size:90%; height:25px; width:50px; display:inline-block; }"),
                                 textInput("kVal", label = "K",
                                           (curBudget %>% filter(slot == "K"))$amount),
                                 tags$style(type='text/css', 
                                            "#kVal { font-size:90%; height:25px; width:50px; display:inline-block; }"),
                                 textInput("benchVal", label = "BENCH",
                                           (curBudget %>% filter(slot == "BENCH"))$amount),
                                 tags$style(type='text/css', 
                                            "#benchVal { font-size:90%; height:25px; width:50px; display:inline-block; }"),
                                 style = "font-size:80%; text-align:right"))),
                        fluidRow(
                          column(8,
                                 div(
                                 actionButton("edit", label = "Submit")),
                                 style = "text-align:right")))),
           hr(),
           h3("League Information"),
           tabsetPanel(
             tabPanel("Remaining Players",
                      fluidRow(
                        column(12,
                               br(),
                               div(
                                 dataTableOutput("remainTable"),
                                 style = "font-size:80%")))),
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
library(shiny)
library(dplyr)
library(reshape2)
library(rCharts)

source("initialize_data.R")
source("functions.R")

shinyServer(function(input, output, session) {
  
  # Collect input data for current auction
  values <- reactiveValues()
  values$bid <- 0
  values$table <- rosterTable
  values$ticker <- ticker
  values$projections <- projections
  
  loadData <- observe({
    if (input$load == 1) {
      source("load_data.R")
      
      isolate(values$table <- rosterTable)
      isolate(values$ticker <- ticker)
      isolate(values$projections <- projections)
    }
  })
  
  # Update table and ticker data
  updateData <- observe({
    if (input$update > values$bid) {
      team_idx <- values$table$team == input$draftTeam
      
      playerInfo <- values$projections %>%
        filter(name == input$player) %>%
        select(position, projectedPoints)
      pos <- as.character(playerInfo[[1]])
      pts <- round(playerInfo[[2]], 1)
      
      table_idx <- get_table_idx(values$table, team_idx, pos)
      
      # update roster table
      isolate(values$table[table_idx[1], 3:4] <- 
                c(input$player, pos))
      isolate(values$table[table_idx[1], 5:7] <- 
                c(pts, values$bid + 1, as.numeric(input$amount)))
      write.csv(values$table, "rosters.csv")
      
      # udpate ticker
      isolate(values$ticker <- append(paste(paste0(values$bid + 1, ":"),
                                            input$draftTeam, "selected",
                                            input$player, "for",
                                            input$amount),
                                      values$ticker)
      )
      write.csv(values$ticker, "draftLog.csv")
      
      # update projections
      isolate(values$projections <- values$projections %>%
                filter(name != input$player))
      
      # update bid number
      isolate(values$bid <- values$bid + 1)
      
      updateSelectInput(session, "player",
                        "Player nominated:",
                        values$projections$name,
                        selected = values$projections$name[1])
      updateTextInput(session, "amount",
                      "Winning bid:",
                      1)
      updateSelectInput(session, "draftTeam",
                        "Highest bidder:",
                        c(Choose = "", teams))
    } 
  })
  
  # Output bid number
  output$bidNum <- renderText({
    as.character(values$bid + 1)
  })
  
  # Output updated log
  output$ticker <- renderUI({
    HTML(paste(values$ticker, collapse = "<br/>"))
  })
  
  # Download data
  output$downloadTicker <- downloadHandler(
    filename = "draftLog.csv",
    content = function(file) {
      write.csv(saveTicker(), file)
    }
  )
  
  output$downloadTable <- downloadHandler(
    filename = "rosterTable.csv",
    content = function(file) {
      write.csv(saveTable(), file)
    }
  )
  
  # Create league-tracker chart
  output$moneyChart <- renderChart({
    get_money_chart(values$table, budget)
  })
  
  output$percentChart <- renderChart({
    get_percent_chart(values$table, rosterSize)
  })
  
  # Organize data for viewing team info
  output$currentTeam <- renderText({
    paste0(input$viewTeam, "'s Team")
  })
  
  # -- Suggested picks
  output$myTable <- renderDataTable({
    maxBid <- get_max_bid(values$table, "James", budget, rosterSize)
    
    budgetOpt <- input$budgetOpt
    displaySlots <- input$displaySlots
    numPicks <- as.numeric(input$numPicks)
    wiggle <- as.numeric(input$wiggle)
    
    get_my_table(budgets, values$projections, budgetOpt, maxBid,
                 displaySlots, numPicks, wiggle)
  }, options = list(pageLength = 5, searching = FALSE)
  )
  
  # -- Current roster table
  output$currentTable <- renderTable({
    get_current_roster(values$table, input$viewTeam)
  })
  
  output$filledSlots <- renderText({
    get_filled_slots(values$table, input$viewTeam)
  })
  
  output$remainingSlots <- renderText({
    get_remaining_slots(values$table, input$viewTeam, rosterSize)
  })
  
  output$budgetTable <- renderTable({
    format_budget(budgets, input$budgetOpt)
  })
  
  # -- Remaining players
  output$remainTable <- renderDataTable({
    values$projections
  }, options = list(pageLength = 10, 
                    columnDefs = list(list(className = "dt-center",
                                           targets = c(3:7) - 1, 
                                           searchable = FALSE)))
  )
  
  output$spentBudget <- renderText({
    get_spent_budget(values$table, input$viewTeam)
  })
  
  output$remainingBudget <- renderText({
    get_remaining_budget(values$table, input$viewTeam, budget)
  })
  
  output$maxBid <- renderText({
    get_max_bid(values$table, input$viewTeam, budget, rosterSize)
  })
})
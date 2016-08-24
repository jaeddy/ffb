library(shiny)
library(dplyr)

source("initialize_data.R")
source("functions.R")

shinyServer(function(input, output, session) {
  
  # Collect input data for current auction
  values <- reactiveValues()
  values$bid <- 1
  values$updateCount <- 0
  values$resetCount <- 0
  values$editCount <- 0
  values$table <- rosterTable
  values$ticker <- ticker
  values$projections <- projections
  values$budgets <- budgets
  
  loadData <- observe({
    if (input$load == 1) {
      source("load_data.R")
      
      isolate(values$table <- rosterTable)
      isolate(values$ticker <- ticker)
      isolate(values$projections <- projections)

      # update projections
      isolate(values$projections <- values$projections %>%
                filter(name != input$player))
      
      # update bid number
      isolate(values$bid <- sum(values$table$player != "") + 1)
      
      updateSelectInput(session, "player",
                        "Player nominated:",
                        values$projections$name,
                        selected = values$projections$name[1])
      updateTextInput(session, "amount",
                      "Winning bid:", 1)
      updateSelectInput(session, "draftTeam",
                        "Highest bidder:",
                        c(Choose = "", teams))
    }
  })
  
  # Update table and ticker data
  updateData <- observe({
    if (input$update > values$updateCount) {
      team_idx <- values$table$team == input$draftTeam
      
      playerInfo <- values$projections %>%
        filter(name == input$player) %>%
        select(position, playerTeam, projectedPoints)
      pos <- as.character(playerInfo$position)
      pteam <- as.character(playerInfo$playerTeam)
      pts <- round(playerInfo$projectedPoints, 1)
      
      table_idx <- get_table_idx(values$table, team_idx, pos)
      
      # update roster table
      isolate(values$table[table_idx[1], 3:5] <- 
                c(input$player, pteam, pos))
      isolate(values$table[table_idx[1], 6:8] <- 
                c(pts, values$bid, as.numeric(input$amount)))
      write.csv(values$table, "rosters.csv")
      
      # udpate ticker
      isolate(values$ticker <- append(paste(paste0(values$bid, ":"),
                                            input$draftTeam, "selected",
                                            input$player, "for",
                                            input$amount),
                                      values$ticker))
      write.csv(values$ticker, "draftLog.csv")
      
      # update projections
      isolate(values$projections <- values$projections %>%
                filter(name != input$player))
      
      # update bid number
      isolate(values$bid <- values$bid + 1)
      
      # update count
      isolate(values$updateCount <- values$updateCount + 1)
      
      updateSelectInput(session, "player",
                        "Player nominated:",
                        values$projections$name,
                        selected = values$projections$name[1])
      updateTextInput(session, "amount",
                      "Winning bid:", 1)
      updateSelectInput(session, "draftTeam",
                        "Highest bidder:",
                        c(Choose = "", teams))
    } 
  })
  
  # Reset table and ticker data
  resetData <- observe({
    if (input$undo > values$resetCount & values$bid > 1) {
      table_idx <- which(values$table$bidNum == values$bid - 1 )
      # print(table_idx)
      player <- values$table$player[table_idx]
      
      # reset roster table
      isolate(values$table[table_idx, 3:5] <- 
                c("", "", ""))
      isolate(values$table[table_idx, 6:8] <- 
                c(0, 0, 0))
      write.csv(values$table, "rosters.csv")
      
      # reset ticker
      isolate(values$ticker <- values$ticker[-1])
      write.csv(values$ticker, "draftLog.csv")
      
      # reset projections
      isolate(values$projections <- projections %>% 
                filter(name == player) %>% 
                bind_rows(values$projections))
      
      # reset bid number
      isolate(values$bid <- values$bid - 1)
      
      # update reset count
      isolate(values$resetCount <- values$resetCount + 1)
      
      updateSelectInput(session, "player",
                        "Player nominated:",
                        values$projections$name,
                        selected = values$projections$name[1])
    }
  })
  
  editBudget <- observe({
    if (input$edit > values$editCount) {
      budgetVal <- budget
      isolate(values$budgets <- values$budgets %>% 
                mutate(amount = ifelse(budget == input$budgetOpt & slot == "QB",
                                       as.numeric(input$qbVal), amount)))
      isolate(values$budgets <- values$budgets %>% 
                mutate(amount = ifelse(budget == input$budgetOpt & slot == "RB1",
                                       as.numeric(input$rb1Val), amount)))
      isolate(values$budgets <- values$budgets %>% 
                mutate(amount = ifelse(budget == input$budgetOpt & slot == "RB2",
                                       as.numeric(input$rb2Val), amount)))
      isolate(values$budgets <- values$budgets %>% 
                mutate(amount = ifelse(budget == input$budgetOpt & slot == "WR1",
                                       as.numeric(input$wr1Val), amount)))
      isolate(values$budgets <- values$budgets %>% 
                mutate(amount = ifelse(budget == input$budgetOpt & slot == "WR2",
                                       as.numeric(input$wr2Val), amount)))
      isolate(values$budgets <- values$budgets %>% 
                mutate(amount = ifelse(budget == input$budgetOpt & slot == "FLEX",
                                       as.numeric(input$flexVal), amount)))
      isolate(values$budgets <- values$budgets %>% 
                mutate(amount = ifelse(budget == input$budgetOpt & slot == "TE",
                                       as.numeric(input$teVal), amount)))
      isolate(values$budgets <- values$budgets %>% 
                mutate(amount = ifelse(budget == input$budgetOpt & slot == "DST",
                                       as.numeric(input$dstVal), amount)))
      isolate(values$budgets <- values$budgets %>% 
                mutate(amount = ifelse(budget == input$budgetOpt & slot == "K",
                                       as.numeric(input$kVal), amount)))
      isolate(values$budgets <- values$budgets %>% 
                group_by(budget) %>% 
                mutate(total = sum(amount)) %>% 
                mutate(amount = ifelse(budget == budgetOpt & slot == "BENCH", 
                                       budgetVal - total - 5, amount)) %>% 
                select(-total) %>% 
                ungroup())
      isolate(values$editCount <- values$editCount + 1)
      
      updateTextInput(session, "benchVal", "BENCH", 
                      (values$budgets %>% 
                         filter(budget == budgetOpt & slot == "BENCH"))$amount)
    }
  })
  
  # Output bid number
  output$bidNum <- renderText({
    as.character(values$bid)
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
  
  output$budgetSummary <- renderTable({
    format_budget(values$budgets, input$budgetOpt)
  }, include.rownames = FALSE)
  
  output$budgetTable <- renderTable({
    get_current_budget(budgets, input$budgetOpt) %>% 
      select(-budget)
  }, include.rownames = FALSE)
  
  # -- Suggested picks
  output$myTable <- renderDataTable({
    maxBid <- get_max_bid(values$table, "James", budget, rosterSize)
    
    budgetOpt <- input$budgetOpt
    displaySlots <- input$displaySlots
    numPicks <- as.numeric(input$numPicks)
    wiggle <- as.numeric(input$wiggle)
    
    get_my_table(values$budgets, values$projections, budgetOpt, maxBid,
                 displaySlots, numPicks, wiggle)
  }, options = list(pageLength = 10,
                    columnDefs = list(list(className = "dt-center",
                                           targets = c(3:9) - 1, 
                                           searchable = FALSE)))
  )
  
  # Organize data for viewing team info
  output$currentTeam <- renderText({
    paste0(input$viewTeam, "'s Team")
  })
  
  output$freeBudget <- renderText({
    curBudget <- values$budgets %>% 
      filter(budget == input$budgetOpt)
    budget - sum(curBudget$amount)
  })
  
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
  
  # -- Remaining players
  output$remainTable <- renderDataTable({
    values$projections %>% 
      rename(team = playerTeam)
  }, options = list(pageLength = 10, 
                    columnDefs = list(list(className = "dt-center",
                                           targets = c(3:9) - 1, 
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
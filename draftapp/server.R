library(shiny)
library(dplyr)
library(reshape2)
library(rCharts)

source("initialize_data.R")


shinyServer(function(input, output, session) {
    
    # Collect input data for current auction
    values <- reactiveValues()
    values$count <- 0
    values$table <- rosterTable
    values$ticker <- ticker
    values$projections <- projections
    
    # Update table and ticker data
    updateData <- observe({
        if (input$update > values$count) {
            team_idx <- values$table$team == input$draftTeam
            
            playerInfo <- values$projections %>%
                filter(name == input$player) %>%
                select(pos, projectedPts)
            pos <- as.character(playerInfo[[1]])
            pts <- round(playerInfo[[2]], 1)
            
            slot_idx <- team_idx & values$table$player == "" &
                grepl(pos, values$table$slot)
            
            if (!any(slot_idx)) {
                slot_idx <- team_idx & values$table$player == "" &
                    grepl("BENCH", values$table$slot)
            }
            
            table_idx <- seq(along = slot_idx)[slot_idx]
            
            # update roster table
            isolate(values$table[table_idx[1], 3:4] <- 
                        c(input$player, pos))
            isolate(values$table[table_idx[1], 5:6] <- 
                        c(pts, as.numeric(input$amount)))
            
            # udpate ticker
            isolate(values$ticker[length(values$ticker) + 1] <- 
                        paste(input$draftTeam, "selected",
                              input$player, "for",
                              input$amount))
            
            # update projections
            isolate(values$projections <- values$projections %>%
                        filter(name != input$player))
            
            # update count
            isolate(values$count <- values$count + 1)
            
            updateSelectInput(session, "player",
                              "Player nominated:",
                              values$projections$name)
            updateTextInput(session, "amount",
                            "Winning bid:",
                            "")
            updateSelectInput(session, "draftTeam",
                              "Highest bidder:",
                              c(Choose = "", teams))
        } 
    })
    
    # Output updated log
    output$ticker <- renderUI({
        HTML(paste(values$ticker, collapse = "<br/>"))
    })
    
    # Create league-tracker chart
    output$moneyChart <- renderChart({
        data <- values$table
        table <- data %>% 
            group_by(team) %>% 
            summarise(remainingBudget = budget - sum(paid),
                      remainingSlots = sum(player == ""),
                      maxBid = remainingBudget - remainingSlots + 1) %>%
            select(team, remainingBudget, maxBid) %>%
            melt(id.vars = "team", variable.name = "budget", 
                 value.name = "amount")
        
        p1 <- nPlot(amount ~ team, group = "budget", data = table,
                    type = "multiBarChart", 
                    width = 600, height = 250)
        p1$chart(color = c("#1f77b4", "#ff7f0e"),
                 reduceXTicks = FALSE,
                 showControls = FALSE)
        p1$xAxis(staggerLabels = FALSE,
                 rotateLabels = 45)
        p1$addParams(dom = "moneyChart")
        return(p1)
    })
    
    output$percentChart <- renderChart({
        data <- values$table
        table <- data %>% 
            group_by(team) %>% 
            summarise(slotsFilled = sum(player != ""),
                      slotsRemaining = rosterSize - slotsFilled) %>%
            select(team, slotsFilled, slotsRemaining) %>%
            melt(id.vars = "team", variable.name = "slots", 
                 value.name = "amount")
        
        p2 <- nPlot(amount ~ team, group = "slots", data = table,
                    type = "multiBarHorizontalChart",
                    width = 600, height = 250)
        p2$chart(color = c("#1f77b4", "#ff7f0e"),
                 showControls = FALSE,
                 stacked = TRUE)
        p2$addParams(dom = "percentChart")
        return(p2)
    })
    
    # Organize data for viewing team info
    output$currentTeam <- renderText({
        paste0(input$viewTeam, "'s Team")
    })
    
    output$currentTable <- renderTable({
        data <- values$table
        data %>% filter(team == input$viewTeam) %>%
            select(slot, player, position, projPts, paid) %>%
            mutate(projPts = as.character(projPts),
                   paid = as.character(paid))
    })
    
    output$filledSlots <- renderText({
        data <- values$table %>%
            filter(team == input$viewTeam)
        sum(data$player != "")
    })
    
    output$remainingSlots <- renderText({
        data <- values$table %>%
            filter(team == input$viewTeam)
        sum(data$player == "")
    })
    
    output$spentBudget <- renderText({
        data <- values$table %>%
            filter(team == input$viewTeam)
        sum(as.numeric(data$paid))
    })
    
    output$remainingBudget <- renderText({
        data <- values$table %>%
            filter(team == input$viewTeam)
        budget - sum(as.numeric(data$paid))
    })
    
    output$maxBid <- renderText({
        data <- values$table %>%
            filter(team == input$viewTeam) %>%
            summarise(remainingBudget = budget - sum(paid),
                      remainingSlots = sum(player == ""),
                      maxBid = remainingBudget - remainingSlots + 1)
        data$maxBid
            
    })
})
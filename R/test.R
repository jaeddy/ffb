library(shiny)

runApp(list(
    ui=pageWithSidebar(headerPanel("Adding entries to table"),
                       sidebarPanel(textInput("text1", "Column 1"),
                                    textInput("text2", "Column 2"),
                                    actionButton("update", "Update Table")),
                       mainPanel(tableOutput("table1"))),
    server=function(input, output, session) {
        values <- reactiveValues()
        values$df <- data.frame(Column1 = numeric(0), Column2 = numeric(0))
        newEntry <- observe({
            if(input$update > 0) {
                isolate(values$df[nrow(values$df) + 1,] <- 
                            c(input$text1, input$text2))
            }
        })
        output$table1 <- renderTable({values$df})
    }))
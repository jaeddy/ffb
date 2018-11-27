function(input, output, session) {

  withProgress(message = 'Loading data...',
               {source("load_data.R")})
  
  output$atlantic_wins_box <- renderInfoBox({
    atlantic_wins <- standings %>% 
      filter(division == "A") %>% 
      summarize(wins = sum(wins)) %>% 
      pluck("wins") %>% 
      as.integer()
    infoBox(
      "Atlantic Wins", atlantic_wins, icon = icon("trophy"),
      color = "light-blue", fill = TRUE
    )
  })
  
  output$atlantic_points_box <- renderInfoBox({
    atlantic_points <- standings %>% 
      filter(division == "A") %>% 
      summarize(points = sum(points)) %>% 
      pluck("points")
    infoBox(
      "Atlantic Points", atlantic_points, icon = icon("chart-bar"),
      color = "light-blue", fill = TRUE
    )
  })
  
  output$coastal_wins_box <- renderInfoBox({
    coastal_wins <- standings %>% 
      filter(division == "B") %>% 
      summarize(wins = sum(wins)) %>% 
      pluck("wins") %>% 
      as.integer()
    infoBox(
      "Coastal Wins", coastal_wins, icon = icon("trophy"),
      color = "orange", fill = TRUE
    )
  })
  
  output$coastal_points_box <- renderInfoBox({
    coastal_points <- standings %>% 
      filter(division == "B") %>% 
      summarize(points = sum(points)) %>% 
      pluck("points")
    infoBox(
      "Coastal Points", coastal_points, icon = icon("chart-bar"),
      color = "orange", fill = TRUE
    )
  })
  
  output$overall_chances <- DT::renderDT({
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Team'),
          th(rowspan = 2, 'Irrelevant'),
          th(colspan = 3, 'Division'),
          th(colspan = 3, 'Wilcard'),
          th(colspan = 3, 'Last')
        ),
        tr(
          lapply(rep(c('Clinch', 'Tie', 'Total'), 3), th)
        )
      )
    ))
    finish_summary(checks) %>% 
      arrange(desc(2*division_clinch + wildcard_clinch - last_clinch)) %>% 
      datatable(
        container = sketch,
        options = list(
          pageLength = 12,
          dom = "t"
        ),
        rownames = FALSE
      ) %>% 
      formatPercentage(2:11, 1)
  }, server = FALSE)
  
  current_outcomes <- reactive({
    list(
      match1 = input$match1,
      match2 = input$match2,
      match3 = input$match3,
      match4 = input$match4,
      match5 = input$match5,
      match6 = input$match6
    )
  })
  
  output$team_paths <- renderSankeyNetwork({
    selected_outcomes <- current_outcomes()
    outcomes_df <- as_tibble(selected_outcomes) %>%
      gather(source, result) %>%
      left_join(matchup_key, by = c("source" = "matchup")) %>%
      mutate(source = label,
             result = if_else(result == 0, "home", "away")) %>%
      select(-label)

    week_paths <- build_network(checks, input$team, current_week,
                                selected_outcomes = outcomes_df)
    week_sankey(week_paths)
  })
  
  
  selected_checks <- reactive({
    get_selected_checks(checks, 12,
                        selected_outcomes = current_outcomes()) %>% 
      get_team_checks(input$team) %>% 
      team_finish_summary()
  })

  output$team_div_clinch <- renderGauge({
    div_clinch_frac <- selected_checks() %>%
      filter(target == "division") %>% 
      pluck("frac")
    if (is.null(div_clinch_frac)) {
      div_clinch_frac <- 0
    }
    gauge(div_clinch_frac*100, 0, 100, symbol = "%",
          gaugeSectors(
            success = c(80, 100), warning = c(40, 79), danger = c(0, 39),
            colors = rev(brewer.pal(3, "Greens"))
          ),
          label = "Clinch")
  })
  
  output$team_div_tie <- renderGauge({
    div_tie_frac <- selected_checks() %>%
      filter(target == "division (tiebreak)") %>% 
      pluck("frac")
    if (is.null(div_tie_frac)) {
      div_tie_frac <- 0
    }
    gauge(div_tie_frac*100, 0, 100, symbol = "%",
          gaugeSectors(
            success = c(80, 100), warning = c(40, 79), danger = c(0, 39),
            colors = rev(brewer.pal(3, "Greens"))
          ),
          label = "Tie")
  })
    
  output$team_wc_clinch <- renderGauge({
    wc_clinch_frac <- selected_checks() %>%
      filter(target == "wildcard") %>% 
      pluck("frac")
    if (is.null(wc_clinch_frac)) {
      wc_clinch_frac <- 0
    }
    gauge(wc_clinch_frac*100, 0, 100, symbol = "%",
          gaugeSectors(
            success = c(80, 100), warning = c(40, 79), danger = c(0, 39),
            colors = rev(brewer.pal(3, "Blues"))
          ),
          label = "Clinch")
  })
  
  output$team_wc_tie <- renderGauge({
    wc_tie_frac <- selected_checks() %>%
      filter(target == "wildcard (tiebreak)") %>% 
      pluck("frac")
    if (is.null(wc_tie_frac)) {
      wc_tie_frac <- 0
    }
    gauge(wc_tie_frac*100, 0, 100, symbol = "%",
          gaugeSectors(
            success = c(80, 100), warning = c(40, 79), danger = c(0, 39),
            colors = rev(brewer.pal(3, "Blues"))
          ),
          label = "Tie")
  })
  
  output$team_last_clinch <- renderGauge({
    last_clinch_frac <- selected_checks() %>%
      filter(target == "last") %>% 
      pluck("frac")
    if (is.null(last_clinch_frac)) {
      last_clinch_frac <- 0
    }
    gauge(last_clinch_frac*100, 0, 100, symbol = "%",
          gaugeSectors(
            success = c(80, 100), warning = c(40, 79), danger = c(0, 39),
            colors = rev(brewer.pal(3, "Reds"))
          ),
          label = "Clinch")
  })
  
  output$team_last_tie <- renderGauge({
    last_tie_frac <- selected_checks() %>%
      filter(target == "last (tiebreak)") %>% 
      pluck("frac")
    if (is.null(last_tie_frac)) {
      last_tie_frac <- 0
    }
    gauge(last_tie_frac*100, 0, 100, symbol = "%",
          gaugeSectors(
            success = c(80, 100), warning = c(40, 79), danger = c(0, 39),
            colors = rev(brewer.pal(3, "Reds"))
          ),
          label = "Tie")
  })
  
  output$team_middle <- renderGauge({
    mid_frac <- selected_checks() %>%
      filter(target == "") %>% 
      pluck("frac")
    if (is.null(mid_frac)) {
      mid_frac <- 0
    }
    gauge(mid_frac*100, 0, 100, symbol = "%",
          gaugeSectors(
            success = c(80, 100), warning = c(40, 79), danger = c(0, 39),
            colors = rev(brewer.pal(3, "Greys"))
          ))
  })
}
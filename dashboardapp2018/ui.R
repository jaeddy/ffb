dashboardPage(
  dashboardHeader(
    title = "JeffCup 2018 Dashboard",
    titleWidth = 350
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("League Overview", tabName = "scenarios", 
               icon = icon("dashboard"))
    ),
    collapsed = TRUE
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(
        tabName = "scenarios",
        fluidRow(
          infoBoxOutput("atlantic_wins_box", width = 6) %>% 
            shinycssloaders::withSpinner(proxy.height = "125px"),
          infoBoxOutput("coastal_wins_box", width = 6)
        ),
        fluidRow(
          infoBoxOutput("atlantic_points_box", width = 6),
          infoBoxOutput("coastal_points_box", width = 6)
        ),
        fluidRow(
          box(
            title = "Overall Chances", width = 12,
            status = "primary",
            p(str_glue("The table below shows chances of how each team will ",
                       "finish the regular season, based on all (4,096) ",
                       "possible outcomes of the remaining 12 matchups.")),
            div(style = 'overflow-x: scroll', 
                DT::DTOutput('overall_chances') %>% 
                  shinycssloaders::withSpinner()
            )
          )
        ),
        fluidRow(
          box(
            title = "Week 12 Results",
            width = 12,
            p(str_glue("Pick teams to win each of the 6 matchups for Week 12, ",
                       "then select your team below. There are 64 (2^6) ",
                       "combinations you can try.")),
            wellPanel(
              fluidRow(
                column(
                  width = 2,
                  radioButtons("match1", "Match 1", 
                               c("Milf" = 0, "Keith" = 1))
                ),
                column(
                  width = 2,
                  radioButtons("match2", "Match 2", 
                               c("Billy" = 0, "Ross" = 1))
                ),
                column(
                  width = 2,
                  radioButtons("match3", "Match 3", 
                               c("Kevin" = 0, "Matt" = 1))
                ),
                column(
                  width = 2,
                  radioButtons("match4", "Match 4", 
                               c("Tony" = 0, "Shep" = 1))
                ),
                column(
                  width = 2,
                  radioButtons("match5", "Match 5", 
                               c("Toby" = 0, "Drew" = 1))
                ),
                column(
                  width = 2,
                  radioButtons("match6", "Match 6", 
                               c("James" = 0, "Brad" = 1)
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "End-of-Season Paths",
            width = 12, status = "primary",
            selectInput("team", 
                        label = "Select team",
                        choices = standings$team),
            fluidRow(
              column(
                width = 7,
                p(str_glue("This diagram shows the relative influence each ",
                           "matchup has on how you'll end the season, ",
                           "highlighting only the outcomes above. Mouse over ",
                           "individual paths to see more details. ")),
                p(strong("Note: "),
                  "all paths that include each matchup result are ",
                  "shown here; however, the overall Week 12 results ",
                  "represent just a subset of possible scenarios. ",
                  "The percentages to the right show your actual chances, ",
                  "given the 6 matchup winners above."),
                networkD3::sankeyNetworkOutput("team_paths")
              ),
              column(
                width = 5,
                p(str_glue("For the selected Week 12 outcomes, here are the ",
                           "chances of how you'll finish the season:")),
                fluidRow(
                  div(align="center", 
                      style="margin-top:15px; margin-bottom:-20px",
                      p("Division winner")
                  ),
                  column(
                    width = 6,
                    gaugeOutput('team_div_clinch', height = "90px") %>% 
                      shinycssloaders::withSpinner()
                  ),
                  column(
                    width = 6,
                    gaugeOutput('team_div_tie', height = "90px") %>% 
                      shinycssloaders::withSpinner()
                  )
                ),
                fluidRow(
                  div(align="center",
                      style="margin-top:50px; margin-bottom:-20px",
                      p("Wildcard (at large playoff spot)")
                  ),
                  column(
                    width = 6,
                    gaugeOutput('team_wc_clinch', height = "90px") %>% 
                      shinycssloaders::withSpinner()
                  ),
                  column(
                    width = 6,
                    gaugeOutput('team_wc_tie', height = "90px") %>% 
                      shinycssloaders::withSpinner()
                  )
                ),
                fluidRow(
                  div(align="center", 
                      style="margin-top:50px; margin-bottom:-20px",
                      p("Last place (mayo)")),
                  column(
                    width = 6,
                    gaugeOutput('team_last_clinch', height = "90px") %>% 
                      shinycssloaders::withSpinner()
                  ),
                  column(
                    width = 6,
                    gaugeOutput('team_last_tie', height = "90px") %>% 
                      shinycssloaders::withSpinner()
                  )
                ),
                fluidRow(
                  div(align="center", 
                      style="margin-top:50px; margin-bottom:-30px",
                      p("Irrelevant (not last, but no playoffs)")),
                  column(
                    width = 4, offset = 4,
                    gaugeOutput('team_middle', height = "60px") %>% 
                      shinycssloaders::withSpinner()
                  )
                )
              )
            )
          )
        )
      )
      
    )
  )
)

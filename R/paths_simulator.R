library(tibble)

# Define functions --------------------------------------------------------


# helper function to tally wins for each team from simulated outcomes
count_wins <- function(sim_df) {
  sim_df %>% 
    group_by(outcome) %>% 
    tally()
}

# function to add wins and ties to current records (and subtract from the total 
# number of weeks to get losses) to get final records for current simulation
update_standings <- function(standings, outcomes) {
  count_wins(outcomes) %>% 
    left_join(standings, ., by = c("team" = "outcome")) %>% 
    replace_na(list(n = 0)) %>% 
    mutate(wins = wins + n) %>% 
    select(-n)
}

format_tiebreak <- function(teams, points) {
  tibble(teams, points) %>% 
    arrange(points) %>% 
    mutate(points = abs(points - max(points))) %>% 
    arrange(points) %>% 
    str_glue_data("{teams}: +{prettyNum(points, '%0.1f')}") %>% 
    str_c(collapse = " | ")
}

# function to get division winners from final records
get_division_winners <- function(final_standings) {
  final_standings %>% 
    group_by(division, wins) %>% 
    summarize(clinch = format_tiebreak(team, points)) %>% 
    group_by(division) %>% 
    top_n(1, wins) %>% 
    ungroup() %>%
    arrange(desc(wins))
}

# function to translate final records to official standings, based on divisions
# and tie-breakers
get_ranking <- function(final_standings) {
  div_winners <- get_division_winners(final_standings)
  div_winners %>% 
    mutate(team = str_match_all(clinch, "[[:alpha:]]+")) %>% 
    unnest() %>% 
    anti_join(final_standings, ., by = "team") %>% 
    group_by(wins) %>% 
    summarize(clinch = format_tiebreak(team, points)) %>% 
    arrange(desc(wins)) %>% 
    
    # # recombine with the remaining teams
    bind_rows(div_winners, .) %>% 
    mutate(finish = case_when(
      row_number() <= 2 ~ "division",
      between(row_number(), 3, 4) ~ "wildcard",
      row_number() == nrow(.) ~ "last",
      TRUE ~ ""
    )) %>% 
    select(-division)
}

check_outcomes <- function(standings, schedule) {
  outcomes <- bincombinations(nrow(schedule))
  array_branch(outcomes, 1) %>% 
    map_df(function(x) {
      matchup_outcomes <- schedule %>% 
        mutate(outcome = x) %>% 
        unite("matchup", -outcome) %>% 
        spread(matchup, outcome)
      
      schedule %>%
        mutate(outcome = x,
               outcome = if_else(outcome > 0, away, home)) %>%
        update_standings(standings, .) %>%
        get_ranking() %>%
        nest() %>%
        bind_cols(matchup_outcomes) %>%
        unnest()
    }, .id = "outcome") %>% 
    mutate(clinch = if_else(str_detect(clinch, "\\|"),
                            clinch,
                            str_match(clinch, "\\w+")))
}
checks <- check_outcomes(standings, schedule)


# summary functions -------------------------------------------------------

finish_summary <- function(checks, scope = NULL) {
  summary <- checks %>% 
    gather(matchup, result, matches("[0-9]+_")) %>% 
    mutate(finish = if_else(str_detect(clinch, "\\|") & finish != "", 
                            str_c(finish, "_tie"), 
                            str_c(finish, "_clinch"))) %>% 
    mutate(team = str_match_all(clinch, "[[:alpha:]]+")) %>% 
    unnest() %>% 
    group_by(team, finish) %>% 
    summarise(frac = n_distinct(outcome) / 4096) %>% 
    ungroup() %>% 
    spread(finish, frac, fill = 0) %>% 
    mutate(division_total = division_clinch + division_tie,
           wildcard_total = wildcard_clinch + wildcard_tie,
           last_total = last_clinch + last_tie) %>% 
    select(irrelevant = `_clinch`,
           division_clinch, division_tie, division_total,
           wildcard_clinch, wildcard_tie, wildcard_total,
           last_clinch, last_tie, last_total)
  if (!is.null(scope)) {
    if (scope == "playoffs") {
      scope <- "(division|wildcard)*"
    }
    summary <- select(summary, matches(scope))
  }
  summary
}

get_team_checks <- function(checks, team) {
  filter(checks, str_detect(clinch, team))
}

# plot functions ----------------------------------------------------------




get_nodes <- function(links) {
  links %>% 
    distinct(source) %>% 
    rename(name = source) %>% 
    bind_rows(links %>% 
                distinct(target) %>% 
                rename(name = target)) %>% 
    mutate(node = as.integer(row_number() - 1),
           stage = case_when(
             str_detect(name, "12") ~ "Week12",
             str_detect(name, "13") ~ "Week13",
             TRUE ~ name
           )) %>% 
    select(one_of(c("node", "name", "stage")))
}

format_source <- function(checks) {
  checks %>% 
    separate(source, "_", into = c("week", "home", "away"), remove = FALSE) %>% 
    mutate(source = str_glue("Week {week}\n\n{home} (home) vs. {away} (away)")) %>% 
    select(-week, -home, -away)
}

get_links <- function(team_checks) {
  team_checks %>% 
    gather(source, result, matches("[0-9]+_")) %>% 
    format_source() %>% 
    mutate(target = if_else(str_detect(clinch, "\\|") & finish != "", 
                            str_c(finish, " (tiebreak)"), 
                            finish),
           result = if_else(result > 0, "away", "home")) %>% 
    group_by(target, source, result) %>% 
    summarise(value = n_distinct(outcome)) %>% 
    ungroup()
}


build_network <- function(team_checks) {
  links <- get_links(team_checks)
  nodes <- get_nodes(links)
  list(
    nodes = nodes,
    links = links %>% 
      left_join(select(nodes, -stage), by = c("source" = "name")) %>%
      mutate(source = node) %>%
      select(-node) %>%
      left_join(select(nodes, -stage), by = c("target" = "name")) %>%
      mutate(target = node) %>%
      select(one_of(c("source", "target", "value", "result")))
  )
}
build_network(team_checks)
 
# Set up data -------------------------------------------------------------

standings <- tribble(
  ~team, ~division, ~wins, ~points,
  "James", "A", 6, 1033.4,
  "Tony", "A", 7, 1097.5,
  "Brad", "A", 5, 992.2,
  "Drew", "A", 5, 1071.5,
  "Toby", "A", 5, 906.7,
  "Shep", "A", 9, 1119.1,
  "Kevin", "B", 4, 857.1,
  "Keith", "B", 5, 1020.4,
  "Matt", "B", 4, 789.2,
  "Billy", "B", 4, 963.7,
  "Milf", "B", 6, 1152.5,
  "Ross", "B", 6, 1087.7
)

# REMAINING SCHEDULE

schedule <- tribble(
  ~week, ~home, ~away,
  "12", "Milf", "Keith",
  "12", "Billy", "Ross",
  "12", "Kevin", "Matt",
  "12", "Tony", "Shep",
  "12", "Toby", "Drew",
  "12", "James", "Brad",
  "13", "Ross", "Milf",
  "13", "Matt", "Billy",
  "13", "Kevin", "Keith",
  "13", "Toby", "Shep",
  "13", "Brad", "Drew",
  "13", "Tony", "James"
)


# plot data ---------------------------------------------------------------

team_network <- build_network(team_checks)
sn <- sankeyNetwork(
  Links = team_network$links,
  Nodes = team_network$nodes, 
  Source = "source",
  Target = "target", 
  Value = "value",
  NodeID = "name",
  NodeGroup = "stage",
  LinkGroup = "result",
  fontSize = 8, 
  fontFamily = "Roboto",
  nodeWidth = 8
  )
sn

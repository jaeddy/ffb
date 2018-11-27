
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
    select(team,
           irrelevant = `_clinch`,
           division_clinch, division_tie, division_total,
           wildcard_clinch, wildcard_tie, wildcard_total,
           last_clinch, last_tie, last_total)
  if (!is.null(scope)) {
    if (scope == "playoffs") {
      scope <- "(team|division*|wildcard*)"
    }
    summary <- select(summary, matches(scope))
  }
  summary
}

get_selected_checks <- function(checks, week_num, selected_outcomes = NULL) {
  week_num <- as.character(week_num)
  if (is.null(selected_outcomes)) {
    selected_outcomes <- list(
      match1 = 0,
      match2 = 0,
      match3 = 0,
      match4 = 0,
      match5 = 0,
      match6 = 0
    )
  }
  checks %>% 
    select(outcome, wins, clinch, finish, starts_with(week_num)) %>% 
    filter(`12_Milf_Keith` == selected_outcomes$match1,
           `12_Billy_Ross` == selected_outcomes$match2,
           `12_Kevin_Matt` == selected_outcomes$match3,
           `12_Tony_Shep` == selected_outcomes$match4,
           `12_Toby_Drew` == selected_outcomes$match5,
           `12_James_Brad` == selected_outcomes$match6)
}

get_team_checks <- function(checks, team) {
  filter(checks, str_detect(clinch, team))
}

team_finish_summary <- function(team_checks) {
  team_checks %>% 
    gather(source, result, matches("[0-9]+_")) %>% 
    format_source() %>% 
    mutate(target = if_else(str_detect(clinch, "\\|") & finish != "", 
                            str_c(finish, " (tiebreak)"), 
                            finish),
           result = if_else(result > 0, "away", "home")) %>% 
    group_by(target) %>% 
    summarize(frac = n_distinct(outcome) / 64)
}

format_source <- function(checks) {
  checks %>% 
    separate(
      source, "_", into = c("week", "home", "away"), remove = FALSE
    ) %>% 
    mutate(
      source = str_glue("{week}: {away} vs. {home}")
    ) %>% 
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
    group_by(source, result, target) %>% 
    summarise(value = n_distinct(outcome) / 2048) %>% 
    ungroup()
}

get_nodes <- function(links) {
  links %>% 
    distinct(source) %>% 
    rename(name = source) %>% 
    bind_rows(links %>% 
                distinct(target) %>% 
                rename(name = target)) %>% 
    mutate(node = as.integer(row_number() - 1),
           stage = case_when(
             str_detect(name, "wins") ~ "matchup",
             TRUE ~ name
           )) %>% 
    select(one_of(c("node", "name", "stage")))
}

get_winner <- Vectorize(function(matchup, result) {
  idx <- if_else(result == "home", 4, 2)
  str_split(matchup, " ")[[1]][idx]
})

build_network <- function(checks, team, 
                          week_num = NULL, 
                          selected_outcomes = NULL) {
  team_checks <- get_team_checks(checks, team)
  links <- get_links(team_checks)
  if (!is.null(week_num)) {
    links <- links %>% 
      filter(str_detect(source, str_glue("{week}:", week = week_num)))
  }
  if (is.null(selected_outcomes)) {
    selected_outcomes <- links %>% 
      filter(result == "home") %>% 
      select(source, result)
  }
  links <- left_join(selected_outcomes, links, 
                     by = c("source", "result")) %>% 
    distinct() %>% 
    mutate(winner = get_winner(source, result),
           source = str_glue("{winner} wins"),
           target = if_else(target == "", "irrelevant", target)) %>%
    select(-winner) 
  nodes <- get_nodes(links)
  list(
    nodes = nodes,
    links = links %>%
      left_join(select(nodes, -stage), by = c("source" = "name")) %>%
      mutate(source = node) %>%
      select(-node) %>%
      left_join(select(nodes, -stage), by = c("target" = "name")) %>%
      mutate(result = target, target = node) %>%
      select(one_of(c("source", "target", "value", "result")))
  )
}

week_sankey <- function(week_paths) {
  sn <- sankeyNetwork(
    Links = week_paths$links,
    Nodes = week_paths$nodes, 
    Source = "source",
    Target = "target", 
    Value = "value",
    NodeID = "name",
    NodeGroup = "stage",
    LinkGroup = "result",
    fontSize = 10, 
    fontFamily = "Roboto",
    nodeWidth = 8
  )
  htmlwidgets::onRender(sn,
    '
    function(el, x) {
    d3.selectAll(".link").select("title foreignObject body pre")
    .text(function(d) { 
      return "In " + d3.format(".0%")(d.value) + " of scenarios when " + 
        d.source.name + ",\\nyou finish:\\n" + d.target.name; });
    }
    '
  )
}

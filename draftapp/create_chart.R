
table <- rosterTable %>% 
    group_by(team) %>% 
    summarise(balance = 200 - sum(paid),
           slots_remaining = sum(player == ""),
           max_bid = balance - slots_remaining + 1) %>%
    melt(id.vars = "team", variable.name = "budget", 
              value.name = "amount")

table %>% ggvis(x = ~team, y = ~amount, fill = ~budget) %>%
    layer_bars()

library(rCharts)
p1 <- nPlot(amount ~ team, group = "budget", data = table,
                 type = "multiBarChart", width = 800)
p1

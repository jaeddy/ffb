
# matchups
brad_drew <- 1.0
brad_james <- 0.4
brad_tony <- 0.3
james_tony <- 1.0
drew_tony <- 0.5
drew_james <- 0.2

matchups <- c("brad_drew",
              "brad_james",
              "brad_tony",
              "james_tony",
              "drew_tony",
              "drew_james")

hate_vec <- c(brad_drew,
              brad_james,
              brad_tony,
              james_tony,
              drew_tony,
              drew_james)

obj <- c(rep(0, 13), 1)
obj

var_types <- rep("C", 13)

# matrix
A <- rbind(cbind(rep(0, 6), diag(6), -1*diag(6), 0),
      c(1, rep(-1, 6), rep(0, 6), 0),
      c(rep(0, 7), hate_vec, -1))
A

dir <- c(rep("==", 7), ">=")
dir

b <- c(8,
       rep(2, 12))
b <- rep(0, 8)
b

bounds <- list(lower = list(ind = c(1L:13L), val = c(8, rep(1, 12))),
               upper = list(ind = c(1L:13L), val = c(8, rep(2, 12))))
bounds

Rglpk_solve_LP(obj = obj, mat = A, dir = dir, rhs = b, bounds = bounds, types = var_types, max = TRUE)


A2 <- rbind(c(1, rep(-1, 6), 0),
      c(0, hate_vec, -1))
b2 <- c(0, 0)
dir2 <- c("==", ">=")
vt2 <- c("C", "C")
bounds2 <- list(lower = list(ind = c(1L:7L), val = c(8, rep(1, 6))),
                upper = list(ind = c(1L:7L), val = c(8, rep(2, 6))))
obj2 <- c(rep(0, 7), 1)
sol <- Rglpk_solve_LP(obj = obj2, mat = A2, dir = dir2, rhs = b2, bounds = bounds2, types = vt2, max = TRUE)

opt_matchups <- sol$solution[2:7] %>% 
  setNames(matchups)
opt_matchups


rival_dist <- rival_mat[1:4, 1:4]
combn(names(rival_dist), 2)
matchups <- combn(dimnames(rival_dist)[[1]], 2, simplify = FALSE) %>% 
  purrr::map(function(x) {str_c(x, collapse = "_")}) %>% 
  flatten_chr()
rival_dist[upper.tri(rival_dist)]

matchup_df <- data.frame(rival_dist) %>% 
  rownames_to_column("team1") %>% 
  gather(team2, rival_score, -team1) %>% 
  filter(team1 < team2) %>% 
  rowwise() %>% 
  mutate(matchup = str_c(team1, team2, sep = "_"))

num_weeks <- 4
num_teams <- n_distinct(dimnames(rival_dist)[[1]])
num_games <- (num_teams / 2) * num_weeks

num_matchups <- length(matchup_df$matchup)
m_names <- c("games", matchup_df$matchup, "rivalness")

A <- rbind(c(1, rep(-1, num_matchups), 0),
           c(0, matchup_df$rival_score, -1))
A
b <- c(0, 0)
dir <- c("==", ">=")
var_types <- c("C", "C")
bounds2 <- list(lower = list(ind = c(1L:7L), val = c(8, rep(1, 6))),
                upper = list(ind = c(1L:7L), val = c(8, rep(2, 6))))
obj2 <- c(rep(0, 7), 1)

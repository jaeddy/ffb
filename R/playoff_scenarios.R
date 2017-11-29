library(networkD3)



# plot --------------------------------------------------------------------

nodes = data.frame(
  
)

sankeyNetwork(
  Links = tribble(
    ~source, ~target, ~value, ~owner,
    0, 5, 1/3, "James",
    1, 5, 1/3, "James",
    3, 5, 1/3, "James",
    0, 6, 1/3, "James",
    2, 6, 1/3, "James",
    4, 6, 1/3, "James",
    1, 8, 1/3, "Drew",
    0, 8, 1/3, "Drew",
    7, 8, 1/3, "Drew",
    2, 10, 1/3, "Shep",
    0, 10, 1/3, "Shep",
    9, 10, 1/3, "Shep"
  
  ),
  Nodes = tribble(
    ~node, ~name, ~foo,
    0, "James Wins", "James",
    1, "Drew Wins", "Drew",
    2, "Shep Wins", "Shep",
    3, "Drew does NOT score 54.4 more than James", "James/Drew",
    4, "Shep does NOT score 78.1 more than James", "James/Shep",
    5, "J1", "James",
    6, "J2", "James",
    7, "Drew scores 54.4 points more than James", "James/Drew",
    8, "D1", "Drew",
    9, "Shep scores 78.1 more than James", "James/Shep",
    10, "S1", "Shep"
  ), 
  Source = "source",
  Target = "target", 
  Value = "value", 
  NodeID = "name",
  NodeGroup = "foo",
  LinkGroup = "owner",
  fontSize = 10, 
  fontFamily = "OpenSans",
  nodeWidth = 20
)
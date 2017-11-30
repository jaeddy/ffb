library(networkD3)
library(htmlwidgets)
library(dplyr)




# data --------------------------------------------------------------------

links = tribble(
  ~source, ~target, ~value, ~owner, ~scenario,
  0, 24, 0.2, "James", "J1",
  11, 24, 0.2, "James", "J1",
  0, 25, 0.2, "James", "J2",
  13, 25, 0.2, "James", "J2",
  0, 26, 0.2, "James", "J3",
  3, 26, 0.2, "James", "J3",
  0, 27, 0.2, "James", "J4",
  5, 27, 0.2, "James", "J4",
  1, 28, 0.2, "James", "J5",
  3, 28, 0.2, "James", "J5",
  11, 28, 0.2, "James", "J5",
  5, 28, 0.2, "James", "J5",
  13, 28, 0.2, "James", "J5",
  1, 29, 0.2, "James", "J6",
  7, 29, 0.2, "James", "J6",
  3, 29, 0.2, "James", "J6",
  11, 29, 0.2, "James", "J6",
  1, 30, 0.2, "James", "J7",
  7, 30, 0.2, "James", "J7",
  5, 30, 0.2, "James", "J7",
  13, 30, 0.2, "James", "J7",
  2, 31, 0.2, "Drew", "D1",
  10, 31, 0.2, "Drew", "D1",
  2, 32, 0.2, "Drew", "D2",
  15, 32, 0.2, "Drew", "D2",
  2, 33, 0.2, "Drew", "D3",
  1, 33, 0.2, "Drew", "D3",
  2, 34, 0.2, "Drew", "D4",
  5, 34, 0.2, "Drew", "D4",
  3, 35, 0.2, "Drew", "D5",
  1, 35, 0.2, "Drew", "D5",
  10, 35, 0.2, "Drew", "D5",
  5, 35, 0.2, "Drew", "D5",
  15, 35, 0.2, "Drew", "D5",
  19, 35, 0.2, "Drew", "D5",
  3, 36, 0.2, "Drew", "D6",
  7, 36, 0.2, "Drew", "D6",
  1, 36, 0.2, "Drew", "D6",
  10, 36, 0.2, "Drew", "D6",
  19, 36, 0.2, "Drew", "D6",
  3, 37, 0.2, "Drew", "D7",
  7, 37, 0.2, "Drew", "D7",
  5, 37, 0.2, "Drew", "D7",
  15, 37, 0.2, "Drew", "D7",
  19, 37, 0.2, "Drew", "D7",
  23, 37, 0.2, "Drew", "D7",
  4, 38, 0.2, "Shep", "S1",
  12, 38, 0.2, "Shep", "S1",
  4, 39, 0.2, "Shep", "S2",
  14, 39, 0.2, "Shep", "S2",
  4, 40, 0.2, "Shep", "S3",
  1, 40, 0.2, "Shep", "S3",
  4, 41, 0.2, "Shep", "S4",
  3, 41, 0.2, "Shep", "S4",
  5, 42, 0.2, "Shep", "S5",
  7, 42, 0.2, "Shep", "S5",
  3, 42, 0.2, "Shep", "S5",
  14, 42, 0.2, "Shep", "S5",
  17, 42, 0.2, "Shep", "S5",
  21, 42, 0.2, "Shep", "S5",
  6, 43, 0.2, "Matt", "M1",
  1, 43, 0.2, "Matt", "M1",
  3, 43, 0.2, "Matt", "M1",
  6, 44, 0.2, "Matt", "M2",
  1, 44, 0.2, "Matt", "M2",
  5, 44, 0.2, "Matt", "M2",
  6, 45, 0.2, "Matt", "M3",
  3, 45, 0.2, "Matt", "M3",
  5, 45, 0.2, "Matt", "M3",
  8, 46, 0.2, "Milf", "W1",
  7, 46, 0.2, "Milf", "W1",
  3, 46, 0.2, "Milf", "W1",
  18, 46, 0.2, "Milf", "W1",
  5, 46, 0.2, "Milf", "W1",
  20, 46, 0.2, "Milf", "W1",
  48, 46, 0.2, "Milf", "W1",
  5, 49, 0.2, "Toby", "T1",
  16, 49, 0.2, "Toby", "T1",
  7, 49, 0.2, "Toby", "T1",
  3, 49, 0.2, "Toby", "T1",
  22, 49, 0.2, "Toby", "T1",
  47, 49, 0.2, "Toby", "T1",
  5, 50, 0.2, "Toby", "T2",
  16, 50, 0.2, "Toby", "T2",
  7, 50, 0.2, "Toby", "T2",
  3, 50, 0.2, "Toby", "T2",
  22, 50, 0.2, "Toby", "T2",
  9, 50, 0.2, "Toby", "T2",
  24, 51, 0.4, "James", "",
  25, 51, 0.4, "James", "",    
  26, 51, 0.4, "James", "",
  27, 51, 0.4, "James", "",
  28, 51, 1, "James", "",
  29, 51, 0.8, "James", "",
  30, 51, 0.8, "James", "",
  31, 52, 0.4, "Drew", "",
  32, 52, 0.4, "Drew", "",    
  33, 52, 0.4, "Drew", "",
  34, 52, 0.4, "Drew", "",
  35, 52, 1.2, "Drew", "",
  36, 52, 1, "Drew", "",
  37, 52, 1.2, "Drew", "",
  38, 53, 0.4, "Shep", "",
  39, 53, 0.4, "Shep", "",    
  40, 53, 0.4, "Shep", "",
  41, 53, 0.4, "Shep", "",
  42, 53, 1.2, "Shep", "",
  43, 54, 0.6, "Matt", "",
  44, 54, 0.6, "Matt", "",
  45, 54, 0.6, "Matt", "",
  46, 55, 1.4, "Milf", "",    
  49, 56, 1.2, "Toby", "",
  50, 56, 1.2, "Toby", ""
)

nodes = tribble(
  ~node, ~name, ~foo,
  0, "James beats Tony", "R",
  1, "Tony beats James", "R",
  2, "Drew beats Brad", "R",
  3, "Brad beats Drew", "R",
  4, "Shep beats Toby", "R",
  5, "Toby beats Shep", "R",
  6, "Matt beats Billy", "R",
  7, "Billy beats Matt", "R",
  8, "Milf beats Ross", "R",
  9, "Ross beats Milf", "R",
  10, "Drew scores > James + 54.4", "Points",
  11, "Drew scores < James + 54.4", "Points",
  12, "Shep scores > James + 78.1", "Points",
  13, "Shep scores < James + 78.1", "Points",
  14, "Shep scores > Drew + 23.7", "Points",
  15, "Shep scores < Drew + 23.7", "Points",
  16, "Toby scores > Shep + 75.1", "Points",
  17, "Toby scores < Shep + 75.1", "Points",
  18, "Milf scores > Drew + 64.8", "Points",
  19, "Milf scores < Drew + 64.8", "Points",
  20, "Milf scores > Shep + 41.1", "Points",
  21, "Milf scores < Shep + 41.1", "Points",
  22, "Toby scores > Drew + 98.8", "Points",
  23, "Toby scores < Drew + 98.8", "Points",
  24, "J1", "James",
  25, "J2", "James",
  26, "J3", "James",
  27, "J4", "James",
  28, "J5", "James",
  29, "J6", "James",
  30, "J7", "James",
  31, "D1", "Drew",
  32, "D2", "Drew",
  33, "D3", "Drew",
  34, "D4", "Drew",
  35, "D5", "Drew",
  36, "D6", "Drew",
  37, "D7", "Drew",
  38, "S1", "Shep",
  39, "S2", "Shep",
  40, "S3", "Shep",
  41, "S4", "Shep",
  42, "S5", "Shep",
  43, "M1", "Matt",
  44, "M2", "Matt",
  45, "M3", "Matt",
  46, "W1", "Milf",
  47, "Toby scores > Milf + 34", "Points",
  48, "Toby scores < Milf + 34", "Points",
  49, "T1", "Toby",
  50, "T2", "Toby",
  51, "James PLAYOFFS", "James",
  52, "Drew PLAYOFFS", "Drew",
  53, "Shep PLAYOFFS", "Shep",
  54, "Matt PLAYOFFS", "Matt",
  55, "Milf PLAYOFFS", "Milf",
  56, "Toby PLAYOFFS", "Toby"
)

# table -------------------------------------------------------------------

scenarios_df <- links %>% 
  left_join(nodes, by = c("source" = "node")) %>% 
  select(-source, -target, -value, -foo) %>% 
  filter(stringr::str_length(scenario) > 0) %>%
  group_by(scenario) %>% 
  summarise(outcomes = stringr::str_c(unique(name), collapse = "</br>"))


# plot --------------------------------------------------------------------


sn <- sankeyNetwork(
  Links = links,
  Nodes = nodes, 
  Source = "source",
  Target = "target", 
  Value = "value", 
  NodeID = "name",
  NodeGroup = "foo",
  LinkGroup = "owner",
  colourScale = JS("d3.scaleOrdinal()
                   .domain(['James', 'Drew', 'Shep', 'Matt', 'Milf', 'Toby', 'R', 'Points'])
                   .range(['#bcbd22', '#17becf', '#ffb300', '#7f7f7f', '#d62728','#393b79', '#000', '#777']);"),
  fontSize = 11, 
  fontFamily = "Roboto",
  nodeWidth = 15,
  margin = list(right = 150)
)

# sn$x$nodes$foo <- nodes$foo
# sn <- onRender(
#   sn,
#   '
#   d3.selection.prototype.moveToFront = function() {  
#     return this.each(function(){
#       this.parentNode.appendChild(this);
#     });
#   };
#   function(el, x) {
#     svg.selectAll(".link")
#     .on("mouseover", function(d) {
#       d3.select(this).moveToFront();
#     })
#   }
#   '
# )

sn <- onRender(
  sn,
  '
  function(el,x){
  var node_text = d3.select(el)
  .selectAll(".node text")
  .filter(function(d) { return (d.name.search("beats|scores") > -1); })
  .attr("x", x.options.nodeWidth - 20)
  .attr("text-anchor", "end")
  .style("font-weight", "bold");
  }
  '
)

# function(el,x){
#   var node_text = d3.select(el)
#   .selectAll(".node text")
#   .filter(function(d) { return (d.name.search("beats|scores") > -1); })
#   .attr("x", x.options.nodeWidth - 20)
#   .attr("text-anchor", "end");
# };
# 
# var titleText = d.name + " - " +
#   format(d.value) + " total" + "\n" + "\n";
# var sourcesText = "";  
# d.targetLinks.forEach(function(dstLnk){
#   sourcesText += "from " + dstLnk.source.name + " - " +
#     format(dstLnk.value) + "\n";
# });
# return titleText + sourcesText;
# 
# 
# var node_text = d3.selectAll(".node")
# .select("title foreignObject body pre")
# .text(function (d) {
#   return d.foo;

sn
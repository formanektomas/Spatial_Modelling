library(igraph)
library(ggraph)
library(ggplot2)
library(tidygraph)
library(sfnetworks)
#
#
#-------------------------------------------------------------------------------
#
# make a simple undirected graph using igraph::make_graph()

g1 <- make_graph(~ A--B,
                A--C,
                B--C,
                B--D,
                C--E,
                D--E)

g1
summary(g1)
set.seed(1)
plot(g1)
set.seed(2)
plot(g1)

#
# choose and stabilize graph layout (Fruchterman-Reingold layout algorithm)

set.seed(123456)
FRL <- layout_with_fr(g1)
plot(g1, layout = FRL)

# add one vertice and edge

g_extended <- add_vertices(g1, nv = 1, name = "F") # nv = no. of vertices to add
g_extended <- add_edges(g_extended,c("E", "F"))
# plot and keep the original layout
set.seed(12345)
plot(g_extended) 

#-------------------------------------------------------------------------------
# supervised exercise: make and plot a tree graph:
# 
#
#          A
#        /   \
#       B     C
#      / \
#     D   E
#
# 5 vertices, 4 edges

g2 <- make_graph(~ A--B,
                 A--C,
                 B--D,
                 B--E)

g2
plot(g2)
tree_layout <- layout_as_tree(
  g2,
  root = which(V(g2)$name == "A"),
  mode = "all",
  flip.y = TRUE
)
plot(g2,layout = tree_layout)
#-------------------------------------------------------------------------------
#
# make a simple directed graph using edges()
g3 <- make_graph(edges=c(1,2,
                         1,3,
                         2,4,
                         3,1,
                         3,5,
                         5,1,
                         4,3))

g3
plot(g3)

# undirected graph 
g4 <- make_graph(edges=c(1,2,
                         1,3,
                         2,4,
                         3,1,
                         3,5,
                         5,1,
                         4,3),directed = FALSE)

g4
plot(g4)
#-------------------------------------------------------------------------------
#
# adding graph attributes
# Another Edge-List dataset with names instead of numbers:
people <- make_graph(edges=c("Anand","Beth",
                             "Anand","Chris",
                             "Beth","Dieter",
                             "Chris","Sun",
                             "Chris","Ali",
                             "Beth","Ali",
                             "Ali","Sun",
                             "Sun","Beth",
                             "Fatima","Chris",
                             "Fatima","Sun"),
                     directed=FALSE)

people
plot(people)

# adjacency matrix - a standard representation of a graph
as_adjacency_matrix(people) # 7 x 7 sparse Matrix of class "dgCMatrix"

# edges:
E(people)
# verticies:
V(people)

# pull a vector of variables from a graph: use $name of vertices
V(people)$name

# let's include more data to the graph
V(people)$gender <- c("M","F","M","M","F","M","F")
V(people)$gender

# alternative way to include attributes to graph elements
people <- set_vertex_attr(people, "age", value = c(20,26,19,34,22,30,21))
V(people)$age


# Also, we assign some weights to the edges: note the E(.)$ syntax
E(people)$weight <- rep(1:5,2)
E(people)$weight


# plot
V(people)$plot_color <- ifelse(V(people)$gender == "F","#E78AC3","#8DA0CB")

plot(people,
     vertex.color = V(people)$plot_color,
     edge.label = E(people)$weight,
     edge.label.color = "black",
     edge.label.cex = 0.9,
     edge.color = "grey60",
     edge.width = 1.5 * E(people)$weight)


#-------------------------------------------------------------------------------
#
# graph from a dataframe (E and V dataframes)

# vertex data frame
V_df <- data.frame(
  name = c("Anand","Beth","Chris","Dieter","Sun","Ali","Fatima"),
  gender = c("M", "F", "M", "M", "F", "M", "F"),
  age = c(20, 26, 19, 34, 22, 30, 21))

V_df

# edge data frame

E_df <- data.frame(
  from = c("Anand","Anand","Beth","Chris","Chris","Beth","Ali","Sun","Fatima","Fatima"),
  to = c("Beth","Chris","Dieter","Sun","Ali","Ali","Sun","Beth","Chris","Sun"),
  weight = c(1, 2, 1, 2, 1,2, 1, 2, 1, 2))

E_df

# undirected igraph object

people2 <- graph_from_data_frame(
  d = E_df, # edge list
  directed = FALSE,
  vertices = V_df # vertex metadata
)

people2
plot(people2)

# retrieve the original tables from the graph
as_data_frame(people2,what = "vertices")

as_data_frame(people2,what = "edges")

#-------------------------------------------------------------------------------
#
# other graphs

g_star <- igraph::make_star(n = 8,mode = "undirected",center = 1)
plot(g_star)

g_full <- igraph::make_full_graph(4, directed = FALSE, loops = FALSE)
plot(g_full)

g_wheel <- make_wheel(10, mode = "undirected")
plot(g_wheel)

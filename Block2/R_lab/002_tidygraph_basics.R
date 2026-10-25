#-------------------------------------------------------------------------------
#
# tidygraph: integrates graphs (igraph objects) into the tidyverse environment
#            simplifies many types of graph-related operations
#
# ggraph:    extends ggplot2 for graph analysis
#
# tidy data
# Relational data cannot in any meaningful way be encoded as a single tidy 
# data frame. On the other hand, both node and edge data fit very well within 
# the tidy concept as each node and edge is, in a sense, a single observation. 
# Thus, a close approximation of tidyness for relational data is two 
# tidy data frames, one describing node data and one describing edge data.
#
#
library(igraph)
library(tidyverse)
library(tidygraph)
library(ggraph)
rm(list=ls())
#
#-------------------------------------------------------------------------------
#
# create a simple graph of a given type

ring10 <- create_ring(10)
# Give nodes simple labels
V(ring10)$name <- LETTERS[1:10] # LETTERS is a {base} function
ring10
plot(ring10)

# Assign random strength to each edge
set.seed(123456)
E(ring10)$strength <- sample(1:10,size = ecount(ring10),replace = TRUE)

# extract tables for nodes and edges:
# 
ring10 %>% activate(nodes) %>% as_tibble()
ring10 %>% activate(edges) %>% as_tibble()

# alternatively:
ring10_tbl <- as_tbl_graph(ring10)
ring10_tbl
ring10_tbl %>% activate(nodes) %>% as_tibble()
ring10_tbl %>% activate(edges) %>% as_tibble()

# use the activate() function within a dplyr/tidyverse pipeline
ring10 <- ring10 %>%
  # Work with the edge table
  activate(edges) %>%
  mutate(
    strength_class = case_when(
      strength <= 3 ~ "Weak",
      strength <= 7 ~ "Moderate",
      strength >  7 ~ "Strong")) %>% 
  mutate(strength_class = as.factor(strength_class))

# update tbl_graph object or perform the above on the tbl_graph object
ring10_tbl <- as_tbl_graph(ring10)


# plot the graph
plot(ring10)
plot(ring10, edge.label = E(ring10)$strength)

# use ggraph
ggraph(ring10_tbl, layout = "circle") +
  geom_edge_link(
    aes(width = strength,label = strength,colour = strength_class),
    label_colour = "black",label_dodge = unit(3, "mm"),
    angle_calc = "along",
    show.legend = FALSE) +
  geom_node_point(size = 8,colour = "steelblue") +
  geom_node_text(aes(label = name),colour = "white",
    fontface = "bold") +
  scale_edge_width(range = c(0.5, 2)) +
  theme_graph() +
  labs(title = "Ring graph with random edge strengths")

#
#-------------------------------------------------------------------------------
#
# advanced graph / tidyverse integration
#
# .N() accesses the node table while the edge table is active
# .E() provides access to the edge table while nodes are active

# Mark node as important, if at least one adjacent edge is classified as Strong
ring10 <- ring10 %>%
  activate(nodes) %>% # we are going to assign new value to nodes
  mutate(
    Important = if_else(
      row_number() %in% c(
        .E()$from[.E()$strength_class == "Strong"], # .. based on 'from' edge information
        .E()$to[.E()$strength_class == "Strong"]),  # .. based on 'to' edge information
      "Yes","No")) # the “at least one match” logic is built into %in%., no need to use any()

# Display the resulting node table
ring10 %>%
  activate(nodes) %>%
  as_tibble()

#
#-------------------------------------------------------------------------------
#
# create and plot a tree graph
?create_tree
create_tree(20, 3) %>% 
  mutate(leaf = node_is_leaf(), root = node_is_root()) %>% 
  ggraph(layout = 'tree') +
  geom_edge_diagonal() +
  geom_node_point(aes(filter = leaf), colour = 'forestgreen', size = 10) +
  geom_node_point(aes(filter = root), colour = 'firebrick', size = 10) +
  theme_graph()
#
#-------------------------------------------------------------------------------
#
# evaluate centrality on a sample graph
?play_smallworld
# play_smallworld(n_dim,dim_size,order,p_rewire,loops = FALSE,multiple = FALSE)

set.seed(123456789)
SW <- tidygraph::play_smallworld(
  n_dim = 1,       # one-dimensional starting lattice
  dim_size = 100,  # 100 nodes
  order = 3,       # connect to 3 nearest neighbours on each side
  p_rewire = 0.1)  # rewire edges with probability 0.1
SW

# betwenness centrality 
SW %>% 
  activate(nodes) %>% 
  mutate(betweenness = centrality_betweenness()) %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point(aes(size = betweenness, colour = betweenness)) + 
  scale_color_continuous(guide = 'legend') + 
  theme_graph()

# closeness centrality 
SW %>% 
  activate(nodes) %>% 
  mutate(closeness = centrality_closeness()) %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point(aes(size = closeness, colour = closeness)) + 
  scale_color_continuous(guide = 'legend') + 
  theme_graph()

# we can save betweenness to the SW object:

SW <- SW %>%
  activate(nodes) %>%
  mutate(betweenness = centrality_betweenness())

SW %>% activate(nodes) %>% as_tibble()
#
#-------------------------------------------------------------------------------
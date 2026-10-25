---
title: "Graphs, space syntax analysis"
author: ""
date: ""
output: html_document
---

```{r setup, include=FALSE}
library(sf)
library(sfnetworks)
library(tidygraph)
library(igraph)
library(dplyr)
library(ggplot2)
library(patchwork)
library(viridis)
```



# Space syntax

## Depth

```{r}



#---------------------------------------
# Example graph
#---------------------------------------

g <- make_graph(
  ~ A--B,
    A--C,
    B--C,
    B--D,
    C--E,
    D--E
)

plot(g)

#---------------------------------------
# Depth matrix
#---------------------------------------

D <- distances(g)

cat("\nDepth matrix:\n")
print(D)

#---------------------------------------
# Total Depth (TD)
#---------------------------------------

TD <- rowSums(D)

cat("\nTotal depth:\n")
print(TD)

#---------------------------------------
# Mean Depth (MD)
#---------------------------------------

n <- gorder(g)
n
MD <- TD / (n - 1)

cat("\nMean depth:\n")
print(round(MD, 3))

#---------------------------------------
# Integration (simplified)
#---------------------------------------

Integration <- 1 / MD

cat("\nIntegration (1/MD):\n")
print(round(Integration, 3))

#---------------------------------------
# Closeness Centrality
#---------------------------------------

Closeness <- closeness(
  g,
  normalized = TRUE
)

cat("\nCloseness centrality:\n")
print(round(Closeness, 3))

#---------------------------------------
# Choice (= Betweenness)
#---------------------------------------

Choice <- betweenness(
  g,
  normalized = TRUE
)

cat("\nChoice (betweenness):\n")
print(round(Choice, 3))

#---------------------------------------
# Summary table
#---------------------------------------

results <- data.frame(
  Node = V(g)$name,
  TotalDepth = TD,
  MeanDepth = round(MD, 3),
  Integration = round(Integration, 3),
  Closeness = round(Closeness, 3),
  Choice = round(Choice, 3)
)

cat("\nSummary:\n")
print(results)

```


--- 

## Depth, integration, closeness Roxel 

```{r}


# SPACE SYNTAX ON THE ROXEL NETWORK
# Mean Depth, Integration, and Closeness Centrality




data(roxel, package = "sfnetworks")
net <- as_sfnetwork(roxel)
g <- as_tbl_graph(net)
cat("Directed:", is_directed(g), "\n")
cat("Vertices:", gorder(g), "\n")
cat("Edges:", gsize(g), "\n")

cmp <- components(g)
cmp$no
sort(cmp$csize, decreasing = TRUE)
largest_component <- which.max(cmp$csize)
cmp$csize[largest_component]

net <- net %>%
  activate(nodes) %>%
  mutate(component = cmp$membership) %>%
  filter(component == largest_component) 


g_R <- as_tbl_graph(net)
# Compute shortest-path distance matrix
D <- distances(g_R)
# Mean Depth
#
# MD_i = sum_j(d_ij)/(n-1)
#
n <- gorder(g_R)
mean_depth <- rowSums(D) / (n - 1)


RA <- 2 * (mean_depth - 1) / (n - 2)
Dn <- 2 *(((log2((n + 2) / 3) - 1) * n + 1)) /((n - 1) * (n - 2))
RRA <- RA / Dn

integration <- 1/RRA
closeness_c <- 1 / mean_depth
BC <- betweenness(g_R)
BCN <- betweenness(g_R, normalized = TRUE)
#

# Store results in network node table
net_metrics <- net %>%
  activate(nodes) %>%
  mutate(
    MeanDepth  = mean_depth,
    Integration = integration,
    RelativeAsymmetry = RA,
    Closeness  = closeness_c,
    Betweenness = BC,
    Betweenness_N = BCN)


# Convert nodes and edges to sf
nodes_sf <- st_as_sf(net_metrics, "nodes")
edges_sf <- st_as_sf(net_metrics, "edges")


# Mean Depth map
#
# Low values = central
# High values = deep

p_depth <- ggplot() +

  geom_sf(
    data = edges_sf,
    colour = "grey80",
    linewidth = 0.20
  ) +

  geom_sf(
    data = nodes_sf,
    aes(colour = MeanDepth),
    size = 1
  ) +

  scale_colour_viridis_c(
    option = "plasma",
    direction = -1
  ) +

  labs(
    title = "Mean depth") +

  theme_void() +

  theme(
    plot.title = element_text(face = "bold",hjust = 0.5,),
    legend.position = "right"
  )


#---------------------------------------------------------
# RA map

p_RA <- ggplot() +

  geom_sf(
    data = edges_sf,
    colour = "grey80",
    linewidth = 0.20
  ) +

  geom_sf(
    data = nodes_sf,
    aes(colour = RelativeAsymmetry),
    size = 1
  ) +

  scale_colour_viridis_c(
    option = "viridis",
    direction = -1,
    name = "Relative \nAsymmetry"
  ) +

  labs(
    title = "Relative Asymmetry") +

  theme_void() +

  theme(
    plot.title = element_text(face = "bold",hjust = 0.5,),
    legend.position = "right"
  )

#---------------------------------------------------------
# Integration map


p_integration <- ggplot() +

  geom_sf(
    data = edges_sf,
    colour = "grey80",
    linewidth = 0.20
  ) +

  geom_sf(
    data = nodes_sf,
    aes(colour = Integration),
    size = 1
  ) +

  scale_colour_viridis_c(
    option = "viridis"
  ) +

  labs(
    title = "Integration"
  ) +

  theme_void() +

  theme(
    plot.title = element_text(face = "bold",hjust = 0.5,),
    legend.position = "right"
  )

#---------------------------------------------------------
# Closeness map


p_closeness <- ggplot() +

  geom_sf(
    data = edges_sf,
    colour = "grey80",
    linewidth = 0.20
  ) +

  geom_sf(
    data = nodes_sf,
    aes(colour = Closeness),
    size = 1
  ) +

  scale_colour_viridis_c(
    option = "magma",
    direction = 1
  ) +

  labs(
    title = "Closeness",
  ) +

  theme_void() +

  theme(
    plot.title = element_text(face = "bold",hjust = 0.5,),
    legend.position = "right"
  )


# Combine maps


((p_depth | p_RA) /
 (p_integration | p_closeness)) +
  plot_annotation(
    title = "Space Syntax Measures on the Roxel Street Network",
    subtitle =
      "Mean Depth, Relative Asymmetry, Integration, and Closeness Centrality",
    theme = theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 16
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 12
      )
    )
  )

# Node ranking table


results <- nodes_sf %>%
  st_drop_geometry() %>%
  select(
    MeanDepth,
    Integration,
    Closeness
  ) %>%
  arrange(MeanDepth)

head(results, 20)


# Summary statistics


summary(mean_depth)
summary(integration)
summary(closeness_c)


#---------------------------------------------------------
# betweenness

p_BC <- ggplot() +

  geom_sf(
    data = edges_sf,
    colour = "grey80",
    linewidth = 0.20
  ) +

  geom_sf(
    data = nodes_sf,
    aes(colour = Betweenness),
    size = 1
  ) +

  scale_colour_viridis_c(
    option = "magma",
    direction = 1
  ) +

  labs(
    title = "Betwenness",
  ) +

  theme_void() +

  theme(
    plot.title = element_text(face = "bold",hjust = 0.5,),
    legend.position = "right"
  )

p_BCN <- ggplot() +

  geom_sf(
    data = edges_sf,
    colour = "grey80",
    linewidth = 0.20
  ) +

  geom_sf(
    data = nodes_sf,
    aes(colour = Betweenness_N),
    size = 1
  ) +

  scale_colour_viridis_c(
    option = "magma",
    direction = 1
  ) +

  labs(
    title = "Betwenness (normalized)",
  ) +

  theme_void() +

  theme(
    plot.title = element_text(face = "bold",hjust = 0.5,),
    legend.position = "right"
  )

(p_BC | p_BCN) +
  plot_annotation(
    title = "Space Syntax Measures on the Roxel Street Network",
    subtitle =
      "Choice/Betweenness,     Choice/Betweenness (normalized)",
    theme = theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 16
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 12
      )
    )
  )
```


## Simple comparison: Integration, Choice

```{r}
#=========================================================
# Two illustrative networks:
# 1. Dense central structure
# 2. Two neighborhoods connected through a bridge node X
#=========================================================


#---------------------------------------------------------
# GRAPH 1: Dense central structure
#
# A ----- B
# | \   / |
# |   C   |
# | /   \ |
# D ----- E
#---------------------------------------------------------

edges_1 <- c(
  "A", "B",
  "A", "D",
  "B", "E",
  "D", "E",
  "A", "C",
  "B", "C",
  "C", "D",
  "C", "E"
)

g1 <- graph(
  edges = edges_1,
  directed = FALSE
)

# Fixed node coordinates
layout_1 <- matrix(
  c(
    0, 2,   # A
    2, 2,   # B
    1, 1,   # C
    0, 0,   # D
    2, 0    # E
  ),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(
    c("A", "B", "C", "D", "E"),
    c("x", "y")
  )
)

# Ensure coordinate order corresponds to vertex order
layout_1 <- layout_1[V(g1)$name, ]


#---------------------------------------------------------
# GRAPH 2: Two neighborhoods connected by bridge node X
#
# A --- B             F --- G
# |   /               |   /
# C --- D ----------- X --- H --- I
#---------------------------------------------------------

edges_2 <- c(
  # Left neighborhood
  "A", "B",
  "A", "C",
  "B", "C",
  "B", "D",
  "C", "D",

  # Bridge
  "D", "X",

  # Right neighborhood
  "X", "F",
  "F", "G",
  "G", "H",
  "X", "H",
  "H", "I"
)

g2 <- graph(
  edges = edges_2,
  directed = FALSE
)

# Fixed node coordinates
layout_2 <- matrix(
  c(
    0, 1,     # A
    1.5, 1,   # B
    0, 0,     # C
    1.5, 0,   # D
    4.0, 0,   # X
    4.0, 1,   # F
    5.5, 1,   # G
    5.5, 0,   # H
    7.0, 0    # I
  ),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(
    c("A", "B", "C", "D", "X", "F", "G", "H", "I"),
    c("x", "y")
  )
)

# Ensure coordinate order corresponds to vertex order
layout_2 <- layout_2[V(g2)$name, ]


#---------------------------------------------------------
# Calculate centrality measures
#---------------------------------------------------------

V(g1)$closeness <- closeness(
  g1,
  normalized = TRUE
)

V(g1)$choice <- betweenness(
  g1,
  directed = FALSE,
  normalized = TRUE
)

V(g2)$closeness <- closeness(
  g2,
  normalized = TRUE
)

V(g2)$choice <- betweenness(
  g2,
  directed = FALSE,
  normalized = TRUE
)


#---------------------------------------------------------
# Highlight important connector node X in graph 2
#---------------------------------------------------------

colors_1 <- rep("#56B4E9", vcount(g1))

colors_2 <- ifelse(
  V(g2)$name == "X",
  "#D55E00",
  "#56B4E9"
)


#---------------------------------------------------------
# Plot both graphs side by side
#---------------------------------------------------------

old_par <- par(
  mfrow = c(2,1),
  mar = c(1, 1, 4, 1)
)

plot(
  g1,
  layout = layout_1,
  vertex.size = 30,
  vertex.color = colors_1,
  vertex.frame.color = "grey25",
  vertex.label.color = "black",
  vertex.label.cex = 1,
  vertex.label.font = 2,
  edge.color = "grey35",
  edge.width = 2,
  edge.curved = 0,
  main = "Dense Central Structure"
)


plot(
  g2,
  layout = layout_2,
  vertex.size = 30,
  vertex.color = colors_2,
  vertex.frame.color = "grey25",
  vertex.label.color = "black",
  vertex.label.cex = 1,
  vertex.label.font = 2,
  edge.color = "grey35",
  edge.width = 2,
  edge.curved = 0,
  main = "Bridge Between Two Areas"
)


par(old_par)


#---------------------------------------------------------
# Display numerical results
#---------------------------------------------------------

results_g1 <- data.frame(
  node = V(g1)$name,
  closeness = round(V(g1)$closeness, 3),
  choice = round(V(g1)$choice, 3)
)

results_g2 <- data.frame(
  node = V(g2)$name,
  closeness = round(V(g2)$closeness, 3),
  choice = round(V(g2)$choice, 3)
)

cat("\nGraph 1 centrality measures:\n")
print(
  results_g1[order(-results_g1$choice), ],
  row.names = FALSE
)

cat("\nGraph 2 centrality measures:\n")
print(
  results_g2[order(-results_g2$choice), ],
  row.names = FALSE
)

```
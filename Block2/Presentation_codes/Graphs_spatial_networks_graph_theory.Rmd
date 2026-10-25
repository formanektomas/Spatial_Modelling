---
title: "Graphs, spatial networks, graph theory"
author: ""
date: ""
output: html_document
---

```{r setup, include=FALSE}
library(igraph)
library(ggraph)
library(ggplot2)
library(patchwork)
library(grid)

```

## Simple graph

```{r simple}

g_no_loop <- make_graph(
  ~ A--B,
  A--C,
  B--C,
  B--D,
  C--E,
  D--E
)

# ------------------------------------------------------------
# Calculate the layout once from the graph without the loop (for later use)


set.seed(123)

xy <- layout_with_fr(g_no_loop)

# Plot the graph

p_no_loop <- ggraph(
  g_no_loop,
  layout = "manual",
  x = xy[, 1],
  y = xy[, 2]
) +
  geom_edge_link(
    colour = "grey65",
    linewidth = 0.8
  ) +
  geom_node_point(
    size = 9,
    shape = 21,
    fill = "#DCEAF6",
    colour = "#2C6E9F",
    stroke = 1.1
  ) +
  geom_node_text(
    aes(label = name),
    size = 4
  ) +
  coord_equal(
    clip = "off"
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.30)
  ) +
  scale_y_continuous(
    expand = expansion(mult = 0.30)
  ) +
  theme_void()

p_no_loop
```


# Graphs continued 


```{r }
library(igraph)
library(ggraph)
library(ggplot2)

# ------------------------------------------------------------
# 1. Create the original undirected simple graph
# ------------------------------------------------------------

g_no_loop <- make_graph(
  ~ A--B,
    A--C,
    B--C,
    B--D,
    C--E,
    D--E
)

# ------------------------------------------------------------
# 2. Calculate the original layout for vertices A to E
# ------------------------------------------------------------

set.seed(123)

xy <- layout_with_fr(g_no_loop)

# ------------------------------------------------------------
# 3. Add vertices F and G
#
# F is a pendant vertex connected only to E.
# G is an isolated vertex.
# ------------------------------------------------------------

g_extended <- add_vertices(
  g_no_loop,
  nv = 2,
  name = c("F", "G")
)

g_extended <- add_edges(
  g_extended,
  c("E", "F")
)

# ------------------------------------------------------------
# 4. Preserve the original positions of A to E
#
# F is placed close to E.
# G is placed halfway between C and F.
# ------------------------------------------------------------

E_row <- which(V(g_no_loop)$name == "E")
C_row <- which(V(g_no_loop)$name == "C")

F_xy <- xy[E_row, ] + c(0.8, -0.4)

G_xy <- (xy[C_row, ] + F_xy) / 2

xy_extended <- rbind(
  xy,
  F_xy,
  G_xy
)

# ------------------------------------------------------------
# 5. Plot the extended graph without a loop
# ------------------------------------------------------------

p_extended <- ggraph(
  g_extended,
  layout = "manual",
  x = xy_extended[, 1],
  y = xy_extended[, 2]
) +
  geom_edge_link(
    colour = "grey65",
    linewidth = 0.8
  ) +
  geom_node_point(
    size = 9,
    shape = 21,
    fill = "#DCEAF6",
    colour = "#2C6E9F",
    stroke = 1.1
  ) +
  geom_node_text(
    aes(label = name),
    size = 4
  ) +
  coord_equal(
    clip = "off"
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.30)
  ) +
  scale_y_continuous(
    expand = expansion(mult = 0.30)
  ) +
  theme_void()

p_extended

# ------------------------------------------------------------
# 6. Copy the extended graph and add a self-loop to B
# ------------------------------------------------------------

g_with_loop <- g_extended

B_id <- as.integer(
  V(g_with_loop)["B"]
)

g_with_loop <- add_edges(
  g_with_loop,
  c(B_id, B_id)
)

# ------------------------------------------------------------
# 7. Plot the graph with the loop using the same layout
# ------------------------------------------------------------

p_with_loop <- ggraph(
  g_with_loop,
  layout = "manual",
  x = xy_extended[, 1],
  y = xy_extended[, 2]
) +
  
  # Draw the ordinary edges
  geom_edge_link(
    colour = "grey65",
    linewidth = 0.8
  ) +
  
  # Draw the self-loop separately
  geom_edge_loop(
    aes(
      direction = 140,
      span = 100,
      strength = 1.5
    ),
    colour = "#D95F02",
    linewidth = 1.2
  ) +
  
  # Draw vertices over the edges
  geom_node_point(
    size = 9,
    shape = 21,
    fill = "#DCEAF6",
    colour = "#2C6E9F",
    stroke = 1.1
  ) +
  
  # Vertex labels
  geom_node_text(
    aes(label = name),
    size = 4
  ) +
  
  coord_equal(
    clip = "off"
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.30)
  ) +
  scale_y_continuous(
    expand = expansion(mult = 0.30)
  ) +
  theme_void()

p_with_loop

# ------------------------------------------------------------
# 8. Verify the graph structure
# ------------------------------------------------------------

as_edgelist(
  g_with_loop,
  names = TRUE
)

which_loop(g_with_loop)

degree(g_with_loop)

# ----------
```


## Directed and mixed graphs

```{r}
# graph is created as directed, direction plotted selectively to make the example...
# Install patchwork first if necessary:
# install.packages("patchwork")

# ------------------------------------------------------------
# 1. Define vertices
# ------------------------------------------------------------

vertices <- data.frame(
  name = c("A", "B", "C", "D", "E")
)

# ------------------------------------------------------------
# 2. Create the original undirected simple graph
# ------------------------------------------------------------

undirected_edges <- data.frame(
  from = c("A", "A", "B", "B", "C", "D"),
  to   = c("B", "C", "C", "D", "E", "E")
)

g_undirected <- graph_from_data_frame(
  d = undirected_edges,
  directed = FALSE,
  vertices = vertices
)

# ------------------------------------------------------------
# 3. Calculate one fixed layout
#
# This layout is reused in all three panels.
# ------------------------------------------------------------

set.seed(123)

xy <- layout_with_fr(g_undirected)

# Store coordinates together with vertex names
layout_fixed <- data.frame(
  name = V(g_undirected)$name,
  x = xy[, 1],
  y = xy[, 2]
)

# Ensure coordinates follow the same vertex order in each graph
get_coordinates <- function(graph, layout_data) {
  
  matched_rows <- match(
    V(graph)$name,
    layout_data$name
  )
  
  as.matrix(
    layout_data[matched_rows, c("x", "y")]
  )
}

# ------------------------------------------------------------
# 4. Create the directed graph
#
# Directions:
# A -> B
# C -> A
# B -> C
# D -> B
# C -> E
# E -> D
# ------------------------------------------------------------

directed_edges <- data.frame(
  from = c("A", "C", "B", "D", "C", "E"),
  to   = c("B", "A", "C", "B", "E", "D")
)

g_directed <- graph_from_data_frame(
  d = directed_edges,
  directed = TRUE,
  vertices = vertices
)

# ------------------------------------------------------------
# 5. Define the mixed graph
#
# Undirected edges:
# A -- C
# B -- C
# C -- E
#
# Directed edges:
# A -> B
# B -> D
# E -> D
#
# The two edge types are plotted using separate graph objects.
# This avoids treating the entire mixed graph as either
# uniformly directed or uniformly undirected.
# ------------------------------------------------------------

mixed_undirected_edges <- data.frame(
  from = c("A", "B", "C"),
  to   = c("C", "C", "E")
)

mixed_directed_edges <- data.frame(
  from = c("A", "B", "E"),
  to   = c("B", "D", "D")
)

g_mixed_undirected <- graph_from_data_frame(
  d = mixed_undirected_edges,
  directed = FALSE,
  vertices = vertices
)

g_mixed_directed <- graph_from_data_frame(
  d = mixed_directed_edges,
  directed = TRUE,
  vertices = vertices
)

# ------------------------------------------------------------
# 6. Extract coordinates in the correct order for each graph
# ------------------------------------------------------------

xy_undirected <- get_coordinates(
  g_undirected,
  layout_fixed
)

xy_directed <- get_coordinates(
  g_directed,
  layout_fixed
)

xy_mixed_undirected <- get_coordinates(
  g_mixed_undirected,
  layout_fixed
)

xy_mixed_directed <- get_coordinates(
  g_mixed_directed,
  layout_fixed
)

# ------------------------------------------------------------
# 7. Define common coordinate limits
#
# Identical limits ensure equal scaling in all three panels.
# ------------------------------------------------------------

x_limits <- range(layout_fixed$x)
y_limits <- range(layout_fixed$y)

# ------------------------------------------------------------
# 8. Plot 1: undirected graph
# ------------------------------------------------------------

p_undirected <- ggraph(
  g_undirected,
  layout = "manual",
  x = xy_undirected[, 1],
  y = xy_undirected[, 2]
) +
  geom_edge_link(
    colour = "grey65",
    linewidth = 0.8
  ) +
  geom_node_point(
    size = 9,
    shape = 21,
    fill = "#DCEAF6",
    colour = "#2C6E9F",
    stroke = 1.1
  ) +
  geom_node_text(
    aes(label = name),
    size = 4
  ) +
  coord_equal(
    xlim = x_limits,
    ylim = y_limits,
    clip = "off"
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.30)
  ) +
  scale_y_continuous(
    expand = expansion(mult = 0.30)
  ) +
  labs(
    title = "Undirected graph"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 13,
      face = "bold",
      hjust = 0.5
    ),
    plot.margin = margin(5, 5, 5, 5)
  )

# ------------------------------------------------------------
# 9. Plot 2: directed graph
# ------------------------------------------------------------

p_directed <- ggraph(
  g_directed,
  layout = "manual",
  x = xy_directed[, 1],
  y = xy_directed[, 2]
) +
  geom_edge_link(
    colour = "grey65",
    linewidth = 0.8,
    arrow = arrow(
      type = "closed",
      length = unit(3.5, "mm")
    ),
    start_cap = circle(
      5.5,
      "mm"
    ),
    end_cap = circle(
      5.5,
      "mm"
    )
  ) +
  geom_node_point(
    size = 9,
    shape = 21,
    fill = "#DCEAF6",
    colour = "#2C6E9F",
    stroke = 1.1
  ) +
  geom_node_text(
    aes(label = name),
    size = 4
  ) +
  coord_equal(
    xlim = x_limits,
    ylim = y_limits,
    clip = "off"
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.30)
  ) +
  scale_y_continuous(
    expand = expansion(mult = 0.30)
  ) +
  labs(
    title = "Directed graph"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 13,
      face = "bold",
      hjust = 0.5
    ),
    plot.margin = margin(5, 5, 5, 5)
  )

# ------------------------------------------------------------
# 10. Plot 3: mixed graph
#
# First construct an empty plot with the fixed node layout.
# Then add the undirected and directed edges as separate layers.
# ------------------------------------------------------------

mixed_undirected_edge_data <- get_edges()(
  create_layout(
    g_mixed_undirected,
    layout = "manual",
    x = xy_mixed_undirected[, 1],
    y = xy_mixed_undirected[, 2]
  )
)

mixed_directed_edge_data <- get_edges()(
  create_layout(
    g_mixed_directed,
    layout = "manual",
    x = xy_mixed_directed[, 1],
    y = xy_mixed_directed[, 2]
  )
)

p_mixed <- ggraph(
  g_undirected,
  layout = "manual",
  x = xy_undirected[, 1],
  y = xy_undirected[, 2]
) +
  
  # Undirected edges
  geom_edge_link(
    data = mixed_undirected_edge_data,
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      group = edge.id
    ),
    colour = "grey65",
    linewidth = 0.8,
    inherit.aes = FALSE
  ) +
  
  # Directed edges
  geom_edge_link(
    data = mixed_directed_edge_data,
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      group = edge.id
    ),
    colour = "#D95F02",
    linewidth = 0.9,
    arrow = arrow(
      type = "closed",
      length = unit(3.5, "mm")
    ),
    start_cap = circle(
      5.5,
      "mm"
    ),
    end_cap = circle(
      5.5,
      "mm"
    ),
    inherit.aes = FALSE
  ) +
  
  geom_node_point(
    size = 9,
    shape = 21,
    fill = "#DCEAF6",
    colour = "#2C6E9F",
    stroke = 1.1
  ) +
  geom_node_text(
    aes(label = name),
    size = 4
  ) +
  coord_equal(
    xlim = x_limits,
    ylim = y_limits,
    clip = "off"
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.30)
  ) +
  scale_y_continuous(
    expand = expansion(mult = 0.30)
  ) +
  labs(
    title = "Mixed graph"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 13,
      face = "bold",
      hjust = 0.5
    ),
    plot.margin = margin(5, 5, 5, 5)
  )

# ------------------------------------------------------------
# 11. Combine the three graphs side by side
# ------------------------------------------------------------

p_combined <- p_undirected +
  p_directed +
  p_mixed +
  plot_layout(
    ncol = 3
  )

p_combined

p_combined2 <- p_undirected +
  p_directed +
  plot_layout(
    ncol = 1
  )

p_combined2

```


## shortest path


```{r}


# ------------------------------------------------------------
# 1. Define the common vertex set
# ------------------------------------------------------------

vertices <- data.frame(
  name = c("A", "B", "C", "D", "E")
)

# ------------------------------------------------------------
# 2. Create the original undirected simple graph
# ------------------------------------------------------------

undirected_edges <- data.frame(
  from = c("A", "A", "B", "B", "C", "D"),
  to   = c("B", "C", "C", "D", "E", "E")
)

g_undirected <- graph_from_data_frame(
  d = undirected_edges,
  directed = FALSE,
  vertices = vertices
)

# ------------------------------------------------------------
# 3. Calculate one fixed layout
#
# The same coordinates are used for both graphs.
# ------------------------------------------------------------

set.seed(123)

xy <- layout_with_fr(g_undirected)

# ------------------------------------------------------------
# 4. Find the shortest undirected path from A to D
#
# The expected path is:
# A -- B -- D
# ------------------------------------------------------------

shortest_undirected <- shortest_paths(
  g_undirected,
  from = "A",
  to = "D",
  output = "both"
)

undirected_path_vertices <- shortest_undirected$vpath[[1]]
undirected_path_edges <- shortest_undirected$epath[[1]]

# Mark edges belonging to the shortest path
E(g_undirected)$on_shortest_path <- FALSE

E(g_undirected)$on_shortest_path[
  as.integer(undirected_path_edges)
] <- TRUE

# Display the path in the R console
V(g_undirected)$name[
  as.integer(undirected_path_vertices)
]

# ------------------------------------------------------------
# 5. Create the directed graph
#
# Directions:
# A -> B
# C -> A
# B -> C
# D -> B
# C -> E
# E -> D
#
# The directed path from A to D is:
# A -> B -> C -> E -> D
# ------------------------------------------------------------

directed_edges <- data.frame(
  from = c("A", "C", "B", "D", "C", "E"),
  to   = c("B", "A", "C", "B", "E", "D")
)

g_directed <- graph_from_data_frame(
  d = directed_edges,
  directed = TRUE,
  vertices = vertices
)

# ------------------------------------------------------------
# 6. Find the shortest directed path from A to D
# ------------------------------------------------------------

shortest_directed <- shortest_paths(
  g_directed,
  from = "A",
  to = "D",
  mode = "out",
  output = "both"
)

directed_path_vertices <- shortest_directed$vpath[[1]]
directed_path_edges <- shortest_directed$epath[[1]]

# Mark arcs belonging to the shortest directed path
E(g_directed)$on_shortest_path <- FALSE

E(g_directed)$on_shortest_path[
  as.integer(directed_path_edges)
] <- TRUE

# Display the directed path in the R console
V(g_directed)$name[
  as.integer(directed_path_vertices)
]

# ------------------------------------------------------------
# 7. Define common coordinate ranges
# ------------------------------------------------------------

x_limits <- range(xy[, 1])
y_limits <- range(xy[, 2])

# ------------------------------------------------------------
# 8. Plot the undirected graph
#
# Grey edges are not part of the shortest path.
# Orange edges form the shortest path A -- B -- D.
# ------------------------------------------------------------

p_undirected_path <- ggraph(
  g_undirected,
  layout = "manual",
  x = xy[, 1],
  y = xy[, 2]
) +
  
  # All edges in grey
  geom_edge_link(
    colour = "grey75",
    linewidth = 0.8
  ) +
  
  # Shortest-path edges in orange
  geom_edge_link(
    aes(
      filter = on_shortest_path
    ),
    colour = "#D95F02",
    linewidth = 1.8
  ) +
  
  # Vertices
  geom_node_point(
    size = 9,
    shape = 21,
    fill = "#DCEAF6",
    colour = "#2C6E9F",
    stroke = 1.1
  ) +
  
  # Vertex labels
  geom_node_text(
    aes(label = name),
    size = 4
  ) +
  
  coord_equal(
    xlim = x_limits,
    ylim = y_limits,
    clip = "off"
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.30)
  ) +
  scale_y_continuous(
    expand = expansion(mult = 0.30)
  ) +
  labs(
    title = "Undirected graph",
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 13,
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 11,
      colour = "#D95F02",
      hjust = 0.5
    ),
    plot.margin = margin(5, 5, 5, 5)
  )

# ------------------------------------------------------------
# 9. Plot the directed graph
#
# All arcs retain arrowheads.
# Orange arcs form the directed shortest path:
# A -> B -> C -> E -> D
# ------------------------------------------------------------

p_directed_path <- ggraph(
  g_directed,
  layout = "manual",
  x = xy[, 1],
  y = xy[, 2]
) +
  
  # All directed edges in grey
  geom_edge_link(
    colour = "grey75",
    linewidth = 0.8,
    arrow = arrow(
      type = "closed",
      length = unit(3.5, "mm")
    ),
    start_cap = circle(
      5.5,
      "mm"
    ),
    end_cap = circle(
      5.5,
      "mm"
    )
  ) +
  
  # Directed shortest-path edges in orange
  geom_edge_link(
    aes(
      filter = on_shortest_path
    ),
    colour = "#D95F02",
    linewidth = 1.8,
    arrow = arrow(
      type = "closed",
      length = unit(3.5, "mm")
    ),
    start_cap = circle(
      5.5,
      "mm"
    ),
    end_cap = circle(
      5.5,
      "mm"
    )
  ) +
  
  # Vertices
  geom_node_point(
    size = 9,
    shape = 21,
    fill = "#DCEAF6",
    colour = "#2C6E9F",
    stroke = 1.1
  ) +
  
  # Vertex labels
  geom_node_text(
    aes(label = name),
    size = 4
  ) +
  
  coord_equal(
    xlim = x_limits,
    ylim = y_limits,
    clip = "off"
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.30)
  ) +
  scale_y_continuous(
    expand = expansion(mult = 0.30)
  ) +
  labs(
    title = "Directed graph",
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 13,
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 11,
      colour = "#D95F02",
      hjust = 0.5
    ),
    plot.margin = margin(5, 5, 5, 5)
  )

# ------------------------------------------------------------
# 10. Combine the plots side by side
# ------------------------------------------------------------

p_shortest_paths <- p_undirected_path +
  p_directed_path +
  plot_layout(
    ncol = 1
  )

p_shortest_paths
```


## Connectivity-based taxonomy

```{r}

# ------------------------------------------------------------
# 1. Define the common vertex set
# ------------------------------------------------------------

vertices <- data.frame(
  name = c("A", "B", "C", "D")
)

# ------------------------------------------------------------
# 2. Define one fixed layout
#
# The same vertex positions are used in all four graphs.
#
#     A       B
#
#     C       D
# ------------------------------------------------------------

xy <- matrix(
  c(
    0, 1,  # A
    1, 1,  # B
    0, 0,  # C
    1, 0   # D
  ),
  ncol = 2,
  byrow = TRUE
)

# ------------------------------------------------------------
# 3. Create an empty graph
#
# Four vertices and no edges.
# All vertices are isolated.
# ------------------------------------------------------------

g_empty <- make_empty_graph(
  n = 4,
  directed = FALSE
)

V(g_empty)$name <- vertices$name

# ------------------------------------------------------------
# 4. Create a disconnected graph
#
# Two connected components:
#
# A -- B
#
# C -- D
# ------------------------------------------------------------

g_disconnected <- graph_from_data_frame(
  d = data.frame(
    from = c("A", "C"),
    to   = c("B", "D")
  ),
  directed = FALSE,
  vertices = vertices
)

# ------------------------------------------------------------
# 5. Create a connected graph
#
# A path joining all four vertices:
#
# A -- B -- D -- C
# ------------------------------------------------------------

g_connected <- graph_from_data_frame(
  d = data.frame(
    from = c("A", "B", "D"),
    to   = c("B", "D", "C")
  ),
  directed = FALSE,
  vertices = vertices
)

# ------------------------------------------------------------
# 6. Create a complete graph K4
#
# Every pair of distinct vertices is connected.
# ------------------------------------------------------------

g_complete <- make_full_graph(
  n = 4,
  directed = FALSE,
  loops = FALSE
)

V(g_complete)$name <- vertices$name

# ------------------------------------------------------------
# 7. Define a function for consistent plotting
# ------------------------------------------------------------

plot_graph <- function(graph, graph_title, graph_subtitle) {

  # Start with the graph and fixed manual layout
  p <- ggraph(
    graph,
    layout = "manual",
    x = xy[, 1],
    y = xy[, 2]
  )

  # Add edges only when the graph contains at least one edge.
  # This avoids problems for the empty graph.
  if (ecount(graph) > 0) {
    p <- p +
      geom_edge_link(
        colour = "grey65",
        linewidth = 1.1
      )
  }

  # Add vertices, labels, and common formatting
  p +
    geom_node_point(
      size = 9,
      shape = 21,
      fill = "#DCEAF6",
      colour = "#2C6E9F",
      stroke = 1.1
    ) +
    geom_node_text(
      aes(label = name),
      size = 4
    ) +
    coord_equal(
      xlim = c(-0.20, 1.20),
      ylim = c(-0.20, 1.20),
      clip = "off"
    ) +
    labs(
      title = graph_title
    ) +
    theme_void() +
    theme(
      plot.title = element_text(
        size = 13,
        face = "bold",
        hjust = 0.5,
        margin = margin(
          b = 3
        )
      ),
      plot.subtitle = element_text(
        size = 10,
        colour = "grey35",
        hjust = 0.5,
        margin = margin(
          b = 5
        )
      ),
      plot.margin = margin(
        t = 5,
        r = 5,
        b = 5,
        l = 5
      )
    )
}

# ------------------------------------------------------------
# 8. Create the four plots
# ------------------------------------------------------------

p_empty <- plot_graph(
  graph = g_empty,
  graph_title = "Empty graph"
)

p_disconnected <- plot_graph(
  graph = g_disconnected,
  graph_title = "Disconnected graph"
)

p_connected <- plot_graph(
  graph = g_connected,
  graph_title = "Connected graph"
)

p_complete <- plot_graph(
  graph = g_complete,
  graph_title = expression(paste("Complete graph  ", K[4]))
)

# ------------------------------------------------------------
# 9. Combine all four graphs horizontally
# ------------------------------------------------------------

p_taxonomy <- (
  p_empty |
  p_disconnected |
  p_connected |
  p_complete
) +
  plot_annotation(
    title = "Connectivity-based taxonomy",
    theme = theme(
      plot.title = element_text(
        size = 16,
        face = "bold",
        hjust = 0.5,
        margin = margin(
          b = 10
        )
      )
    )
  )

p_taxonomy

# ------------------------------------------------------------
# 10. Verify graph characteristics
# ------------------------------------------------------------

graph_summary <- data.frame(
  graph = c(
    "Empty",
    "Disconnected",
    "Connected",
    "Complete"
  ),
  vertices = c(
    vcount(g_empty),
    vcount(g_disconnected),
    vcount(g_connected),
    vcount(g_complete)
  ),
  edges = c(
    ecount(g_empty),
    ecount(g_disconnected),
    ecount(g_connected),
    ecount(g_complete)
  ),
  components = c(
    components(g_empty)$no,
    components(g_disconnected)$no,
    components(g_connected)$no,
    components(g_complete)$no
  ),
  connected = c(
    is_connected(g_empty),
    is_connected(g_disconnected),
    is_connected(g_connected),
    is_connected(g_complete)
  )
)

graph_summary

```


## Star graph


```{r}


# ------------------------------------------------------------
# Function for creating and plotting a star graph K_{1,n}
#
# n_leaves = number of pendant vertices
# Total number of vertices = n_leaves + 1
# ------------------------------------------------------------

plot_star_graph <- function(n_leaves) {
  
  # Create the star graph:
  # vertex 1 is the central vertex
  # all remaining vertices are pendant vertices
  g_star <- make_star(
    n = n_leaves + 1,
    mode = "undirected",
    center = 1
  )
  
  # ----------------------------------------------------------
  # Construct a circular layout
  #
  # The central vertex is at (0, 0).
  # The pendant vertices are distributed evenly around it.
  # The first pendant vertex is placed at the top.
  # ----------------------------------------------------------
  
  angles <- seq(
    from = pi / 2,
    to = pi / 2 + 2 * pi,
    length.out = n_leaves + 1
  )[1:n_leaves]
  
  xy <- rbind(
    
    # Central vertex
    c(0, 0),
    
    # Pendant vertices
    cbind(
      cos(angles),
      sin(angles)
    )
  )
  
  # Mathematical label below the graph
  graph_label <- bquote(
    K[1 * "," * .(n_leaves)]
  )
  
  # ----------------------------------------------------------
  # Plot the star graph
  # ----------------------------------------------------------
  
  ggraph(
    g_star,
    layout = "manual",
    x = xy[, 1],
    y = xy[, 2]
  ) +
    geom_edge_link(
      colour = "black",
      linewidth = 1.1
    ) +
    geom_node_point(
      size = 5.5,
      shape = 21,
      fill = "black",
      colour = "black",
      stroke = 0
    ) +
    coord_equal(
      xlim = c(-1.20, 1.20),
      ylim = c(-1.20, 1.20),
      clip = "off"
    ) +
    labs(
      caption = graph_label
    ) +
    theme_void() +
    theme(
      plot.caption = element_text(
        size = 18,
        colour = "black",
        hjust = 0.5,
        margin = margin(
          t = 5
        )
      ),
      plot.margin = margin(
        t = 10,
        r = 8,
        b = 5,
        l = 8
      )
    )
}

# ------------------------------------------------------------
# Create the sequence of star graphs
# ------------------------------------------------------------

p_star_3 <- plot_star_graph(
  n_leaves = 3
)

p_star_4 <- plot_star_graph(
  n_leaves = 4
)

p_star_5 <- plot_star_graph(
  n_leaves = 5
)

p_star_6 <- plot_star_graph(
  n_leaves = 6
)

# ------------------------------------------------------------
# Combine the four graphs horizontally
# ------------------------------------------------------------

p_star_sequence <- (
  p_star_3 |
  p_star_4 |
  p_star_5 |
  p_star_6
) +
  plot_layout(
    ncol = 4,
    widths = c(1, 1, 1, 1)
  )

p_star_sequence


```


## Trees

```{r}


# Helper function for creating a tree plot
# ------------------------------------------------------------

plot_tree <- function(edges, vertices, coordinates, label) {
  
  # Create the undirected tree
  g <- graph_from_data_frame(
    d = edges,
    directed = FALSE,
    vertices = data.frame(name = vertices)
  )
  
  # Match coordinates to the internal vertex order
  coordinates <- coordinates[
    match(V(g)$name, coordinates$name),
  ]
  
  # Verify that the graph is a tree
  stopifnot(is_tree(g))
  
  # Plot
  ggraph(
    g,
    layout = "manual",
    x = coordinates$x,
    y = coordinates$y
  ) +
    geom_edge_link(
      colour = "black",
      linewidth = 1.1
    ) +
    geom_node_point(
      size = 5.5,
      shape = 21,
      fill = "black",
      colour = "black",
      stroke = 0
    ) +
    coord_equal(
      xlim = c(-1.8, 1.8),
      ylim = c(-0.30, 2.30),
      clip = "off"
    ) +
    labs(
      caption = label
    ) +
    theme_void() +
    theme(
      plot.caption = element_text(
        size = 18,
        colour = "black",
        hjust = 0.5,
        margin = margin(t = 7)
      ),
      plot.margin = margin(
        t = 10,
        r = 8,
        b = 5,
        l = 8
      )
    )
}

# ============================================================
# TREE 1: T4
#
#        A
#       / \
#      B   C
#          |
#          D
#
# 4 vertices, 3 edges
# ============================================================

edges_T4 <- data.frame(
  from = c("A", "A", "C"),
  to   = c("B", "C", "D")
)

vertices_T4 <- c(
  "A", "B", "C", "D"
)

coordinates_T4 <- data.frame(
  name = vertices_T4,
  x = c(
     0.0,   # A
    -0.9,   # B
     0.9,   # C
     0.9    # D
  ),
  y = c(
    2.0,    # A
    1.0,    # B
    1.0,    # C
    0.0     # D
  )
)

p_T4 <- plot_tree(
  edges = edges_T4,
  vertices = vertices_T4,
  coordinates = coordinates_T4,
  label = expression(T[4])
)

# ============================================================
# TREE 2: T5
#
#          A
#        /   \
#       B     C
#      / \
#     D   E
#
# 5 vertices, 4 edges
# ============================================================

edges_T5 <- data.frame(
  from = c("A", "A", "B", "B"),
  to   = c("B", "C", "D", "E")
)

vertices_T5 <- c(
  "A", "B", "C", "D", "E"
)

coordinates_T5 <- data.frame(
  name = vertices_T5,
  x = c(
     0.0,   # A
    -0.8,   # B
     0.9,   # C
    -1.4,   # D
    -0.3    # E
  ),
  y = c(
    2.0,    # A
    1.0,    # B
    1.0,    # C
    0.0,    # D
    0.0     # E
  )
)

p_T5 <- plot_tree(
  edges = edges_T5,
  vertices = vertices_T5,
  coordinates = coordinates_T5,
  label = expression(T[5])
)

# ============================================================
# TREE 3: T7
#
#             A
#           /   \
#          B     C
#         / \   / \
#        D   E F   G
#
# 7 vertices, 6 edges
# ============================================================

edges_T7 <- data.frame(
  from = c("A", "A", "B", "B", "C", "C"),
  to   = c("B", "C", "D", "E", "F", "G")
)

vertices_T7 <- c(
  "A", "B", "C", "D", "E", "F", "G"
)

coordinates_T7 <- data.frame(
  name = vertices_T7,
  x = c(
     0.0,   # A
    -0.9,   # B
     0.9,   # C
    -1.5,   # D
    -0.5,   # E
     0.5,   # F
     1.5    # G
  ),
  y = c(
    2.0,    # A
    1.0,    # B
    1.0,    # C
    0.0,    # D
    0.0,    # E
    0.0,    # F
    0.0     # G
  )
)

p_T7 <- plot_tree(
  edges = edges_T7,
  vertices = vertices_T7,
  coordinates = coordinates_T7,
  label = expression(T[7])
)

# ============================================================
# TREE 4: T10
#
#                  A
#              /       \
#             B         C
#           /   \     /   \
#          D     E   F     G
#         / \       /       \
#        H   I     J
#
# 10 vertices, 9 edges
# ============================================================

edges_T10 <- data.frame(
  from = c(
    "A", "A",
    "B", "B",
    "C", "C",
    "D", "D",
    "F"
  ),
  to = c(
    "B", "C",
    "D", "E",
    "F", "G",
    "H", "I",
    "J"
  )
)

vertices_T10 <- c(
  "A", "B", "C", "D", "E",
  "F", "G", "H", "I", "J"
)

coordinates_T10 <- data.frame(
  name = vertices_T10,
  x = c(
     0.0,   # A
    -0.9,   # B
     0.9,   # C
    -1.4,   # D
    -0.4,   # E
     0.4,   # F
     1.4,   # G
    -1.7,   # H
    -1.1,   # I
     0.4    # J
  ),
  y = c(
    2.2,    # A
    1.5,    # B
    1.5,    # C
    0.8,    # D
    0.8,    # E
    0.8,    # F
    0.8,    # G
    0.0,    # H
    0.0,    # I
    0.0     # J
  )
)

p_T10 <- plot_tree(
  edges = edges_T10,
  vertices = vertices_T10,
  coordinates = coordinates_T10,
  label = expression(T[10])
)

# ------------------------------------------------------------
# Combine all four trees horizontally
# ------------------------------------------------------------

p_tree_sequence <- (
  p_T4 |
  p_T5 |
  p_T7 |
  p_T10
) +
  plot_layout(
    ncol = 4,
    widths = c(1, 1, 1, 1)
  )

p_tree_sequence

# ------------------------------------------------------------
# Verify the number of vertices and edges
# ------------------------------------------------------------

tree_summary <- data.frame(
  Tree = c(
    "T4",
    "T5",
    "T7",
    "T10"
  ),
  Vertices = c(
    length(vertices_T4),
    length(vertices_T5),
    length(vertices_T7),
    length(vertices_T10)
  ),
  Edges = c(
    nrow(edges_T4),
    nrow(edges_T5),
    nrow(edges_T7),
    nrow(edges_T10)
  )
)

tree_summary

```


## planar graph

```{r}


# ------------------------------------------------------------
# 1. Define the graph
#
# This is the cycle graph C5, drawn first as a five-pointed
# star and then as a crossing-free pentagon.
#
# Edge sequence:
#
# A -- C -- E -- B -- D -- A
#
# IMPORTANT:
# Intersections between star edges are not vertices.
# ------------------------------------------------------------

vertices <- data.frame(
  name = c("A", "B", "C", "D", "E")
)

edges <- data.frame(
  from = c("A", "C", "E", "B", "D"),
  to   = c("C", "E", "B", "D", "A")
)

g_star <- graph_from_data_frame(
  d = edges,
  directed = FALSE,
  vertices = vertices
)

# ------------------------------------------------------------
# 2. Create coordinates for five positions on a circle
#
# Position 1 is at the top.
# The other positions proceed clockwise.
# ------------------------------------------------------------

angles <- pi / 2 - 2 * pi * (0:4) / 5

circle_positions <- data.frame(
  position = 1:5,
  x = cos(angles),
  y = sin(angles)
)

# ------------------------------------------------------------
# 3. Layout 1: five-pointed star drawing
#
# Vertices are assigned around the circle in alphabetical
# order:
#
# Position 1: A
# Position 2: B
# Position 3: C
# Position 4: D
# Position 5: E
#
# Because the edges connect every second position, the
# resulting drawing is a five-pointed star.
# ------------------------------------------------------------

layout_star <- data.frame(
  name = c("A", "B", "C", "D", "E"),
  x = circle_positions$x,
  y = circle_positions$y
)

# Match coordinates to the internal igraph vertex order
layout_star <- layout_star[
  match(
    V(g_star)$name,
    layout_star$name
  ),
]

# ------------------------------------------------------------
# 4. Layout 2: crossing-free planar drawing
#
# The vertices are placed around the circle in edge order:
#
# A, C, E, B, D
#
# The graph is unchanged. Only the vertex positions change.
# The same edges now form a pentagon without crossings.
# ------------------------------------------------------------

planar_order <- c("A", "C", "E", "B", "D")

layout_planar <- data.frame(
  name = planar_order,
  x = circle_positions$x,
  y = circle_positions$y
)

# Match coordinates to the internal igraph vertex order
layout_planar <- layout_planar[
  match(
    V(g_star)$name,
    layout_planar$name
  ),
]

# ------------------------------------------------------------
# 5. Plot 1: star-shaped drawing with crossings
# ------------------------------------------------------------

p_star <- ggraph(
  g_star,
  layout = "manual",
  x = layout_star$x,
  y = layout_star$y
) +
  geom_edge_link(
    colour = "black",
    linewidth = 1.2
  ) +
  geom_node_point(
    size = 9,
    shape = 21,
    fill = "#DCEAF6",
    colour = "#2C6E9F",
    stroke = 1.2
  ) +
  geom_node_text(
    aes(label = name),
    size = 4.2,
    colour = "black"
  ) +
  coord_equal(
    xlim = c(-1.25, 1.25),
    ylim = c(-1.20, 1.25),
    clip = "off"
  ) +
  labs(
    title = "Star-shaped drawing",
    subtitle = "Edges cross, but crossings are not vertices",
    caption = expression(G %cong% C[5])
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 14,
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 10.5,
      colour = "grey35",
      hjust = 0.5,
      margin = margin(b = 5)
    ),
    plot.caption = element_text(
      size = 17,
      colour = "black",
      hjust = 0.5,
      margin = margin(t = 6)
    ),
    plot.margin = margin(
      t = 8,
      r = 8,
      b = 5,
      l = 8
    )
  )

# ------------------------------------------------------------
# 6. Plot 2: crossing-free planar drawing
# ------------------------------------------------------------

p_planar <- ggraph(
  g_star,
  layout = "manual",
  x = layout_planar$x,
  y = layout_planar$y
) +
  geom_edge_link(
    colour = "black",
    linewidth = 1.2
  ) +
  geom_node_point(
    size = 9,
    shape = 21,
    fill = "#DCEAF6",
    colour = "#2C6E9F",
    stroke = 1.2
  ) +
  geom_node_text(
    aes(label = name),
    size = 4.2,
    colour = "black"
  ) +
  coord_equal(
    xlim = c(-1.25, 1.25),
    ylim = c(-1.20, 1.25),
    clip = "off"
  ) +
  labs(
    title = "Crossing-free drawing",
    subtitle = "A planar embedding of the same graph",
    caption = expression(C[5])
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 14,
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 10.5,
      colour = "grey35",
      hjust = 0.5,
      margin = margin(b = 5)
    ),
    plot.caption = element_text(
      size = 17,
      colour = "black",
      hjust = 0.5,
      margin = margin(t = 6)
    ),
    plot.margin = margin(
      t = 8,
      r = 8,
      b = 5,
      l = 8
    )
  )

# ------------------------------------------------------------
# 7. Combine both drawings horizontally
# ------------------------------------------------------------

p_star_planarity <- (
  p_star |
  p_planar
) +
  plot_layout(
    ncol = 2,
    widths = c(1, 1)
  ) +
  plot_annotation(
    title = "Two drawings of the same planar graph",
    subtitle = "E = {{A,C}, {C,E}, {E,B}, {B,D}, {D,A}}",
    theme = theme(
      plot.title = element_text(
        size = 16,
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 11,
        hjust = 0.5,
        margin = margin(b = 8)
      )
    )
  )

p_star_planarity

# ------------------------------------------------------------
# 8. Verify that the graph is C5 and is planar
# ------------------------------------------------------------

as_edgelist(
  g_star,
  names = TRUE
)

degree(g_star)
# Every vertex has degree 2

is_connected(g_star)
# TRUE




```


## Bridges of Königsberg problem


```{r}


# ------------------------------------------------------------
# 1. Define the four land masses
#
#              C
#       Northern riverbank
#
#              A                 D
#       Kneiphof island      Lomse island
#
#              B
#       Southern riverbank
# ------------------------------------------------------------

vertices <- data.frame(
  name = c("A", "B", "C", "D"),
  land_mass = c(
    "Kneiphof island",
    "Southern bank",
    "Northern bank",
    "Lomse island"
  )
)

# ------------------------------------------------------------
# 2. Define the seven bridges
#
# There are:
#   2 bridges between A and C
#   2 bridges between A and B
#   1 bridge  between A and D
#   1 bridge  between C and D
#   1 bridge  between B and D
#
# Therefore:
#   d(A) = 5
#   d(B) = 3
#   d(C) = 3
#   d(D) = 3
# ------------------------------------------------------------

bridges <- data.frame(
  from = c(
    "A", "A",   # two bridges A--C
    "A", "A",   # two bridges A--B
    "A",        # one bridge A--D
    "C",        # one bridge C--D
    "B"         # one bridge B--D
  ),
  to = c(
    "C", "C",
    "B", "B",
    "D",
    "D",
    "D"
  ),
  bridge = paste0("e", 1:7)
)

# ------------------------------------------------------------
# 3. Create the undirected multigraph
#
# Do not use simplify(), because it would remove the
# parallel bridges.
# ------------------------------------------------------------

g_konigsberg <- graph_from_data_frame(
  d = bridges,
  directed = FALSE,
  vertices = vertices
)

# ------------------------------------------------------------
# 4. Calculate and store vertex degrees
# ------------------------------------------------------------

V(g_konigsberg)$vertex_degree <- degree(
  g_konigsberg,
  loops = TRUE
)

V(g_konigsberg)$plot_label <- paste0(
  V(g_konigsberg)$name,
  "\n",
  "d = ",
  V(g_konigsberg)$vertex_degree
)

# ------------------------------------------------------------
# 5. Define the geographic-style layout
#
# C = northern bank
# A = Kneiphof island
# B = southern bank
#
# C, A, and B are vertically aligned.
# D is placed to the right of A.
# ------------------------------------------------------------

layout_coordinates <- data.frame(
  name = c("A", "B", "C", "D"),
  x = c(
    0.0,   # A: Kneiphof island
    0.0,   # B: southern bank
    0.0,   # C: northern bank
    2.6    # D: Lomse island
  ),
  y = c(
     0.0,   # A: central island
    -1.4,   # B: southern bank
     1.4,   # C: northern bank
     0.0    # D: eastern island
  )
)

# Match coordinates to the internal igraph vertex order
layout_coordinates <- layout_coordinates[
  match(
    V(g_konigsberg)$name,
    layout_coordinates$name
  ),
]

# ------------------------------------------------------------
# 6. Create manually positioned bridge labels
#
# The labels for each pair of parallel bridges are shifted
# to opposite sides of the corresponding edges.
# ------------------------------------------------------------

bridge_labels <- data.frame(
  bridge = paste0("e", 1:7),

  x = c(
    -0.12,  0.12,  # e1, e2: A--C
    -0.12,  0.12,  # e3, e4: A--B
     0.80,         # e5: A--D
     0.86,         # e6: C--D
     0.86          # e7: B--D
  ),

  y = c(
     0.70,  0.70,  # e1, e2: A--C
    -0.70, -0.70,  # e3, e4: A--B
     0.10,         # e5: A--D
     0.82,         # e6: C--D
    -0.72          # e7: B--D
  ),

  angle = c(
    90, 90,        # vertical A--C labels
    90, 90,        # vertical A--B labels
     0,            # horizontal A--D label
   -41,            # diagonal C--D label
    41             # diagonal B--D label
  )
)

# ------------------------------------------------------------
# 7. Plot the Königsberg bridge multigraph
# ------------------------------------------------------------

p_konigsberg <- ggraph(
  g_konigsberg,
  layout = "manual",
  x = layout_coordinates$x,
  y = layout_coordinates$y
) +

  # Draw all seven bridges.
  #
  # strength > 1 produces wider separation between parallel
  # bridges A--C and A--B.
  geom_edge_fan(
    colour = "grey45",
    linewidth = 1.2,
    strength = 1.4
  ) +

  # Add bridge labels separately.
  #
  # fill = "white" gives each label a small white background,
  # which keeps the text readable over the edges.
  geom_label(
    data = bridge_labels,
    aes(
      x = x,
      y = y,
      label = bridge,
      angle = angle
    ),
    inherit.aes = FALSE,
    size = 3.8,
    label.size = 0,
    label.padding = unit(0.10, "lines"),
    fill = "white",
    colour = "black"
  ) +

  # Draw the four land-mass vertices
  geom_node_point(
    size = 17,
    shape = 21,
    fill = "#DCEAF6",
    colour = "#2C6E9F",
    stroke = 1.4
  ) +

  # Show vertex names and degrees
  geom_node_text(
    aes(
      label = plot_label
    ),
    size = 4,
    lineheight = 0.9
  ) +

  coord_equal(
    xlim = c(-0.9, 2.4),
    ylim = c(-1.9, 1.9),
    clip = "off"
  ) +

  labs(
    title = "The Seven Bridges of Königsberg",
    subtitle = paste(
      "A = Kneiphof island;",
      "B = southern bank;",
      "C = northern bank;",
      "D = Lomse island"
    ),
    caption = "Undirected multigraph"
  ) +

  theme_void() +

  theme(
    plot.title = element_text(
      size = 17,
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 10.5,
      colour = "grey30",
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.caption = element_text(
      size = 11,
      face = "italic",
      colour = "grey35",
      hjust = 0.5,
      margin = margin(t = 8)
    ),
    plot.margin = margin(
      t = 10,
      r = 15,
      b = 10,
      l = 15
    )
  )

p_konigsberg
# ------------------------------------------------------------
# 8. Verify the vertex degrees
# ------------------------------------------------------------

vertex_degrees <- degree(
  g_konigsberg
)

vertex_degrees

# Expected:
#
# A B C D
# 5 3 3 3

# ------------------------------------------------------------
# 9. Identify odd-degree vertices
# ------------------------------------------------------------

odd_vertices <- names(
  vertex_degrees[
    vertex_degrees %% 2 == 1
  ]
)

number_of_odd_vertices <- length(
  odd_vertices
)

odd_vertices
number_of_odd_vertices

# All four vertices have odd degree.

# ------------------------------------------------------------
# 10. Check Euler conditions
# ------------------------------------------------------------

has_euler_circuit <- (
  is_connected(g_konigsberg) &&
  number_of_odd_vertices == 0
)

has_open_euler_trail <- (
  is_connected(g_konigsberg) &&
  number_of_odd_vertices == 2
)

has_euler_circuit
# FALSE

has_open_euler_trail
# FALSE

```


## igraph basics

```{r}
g1 <- igraph::make_graph(
     ~ A--B,
     A--C,
     B--C,
     B--D,
     C--E,
     D--E
)
g1
plot(g1)
```


## NUTS2 regions: conectivity-based graphs

```{r}
library(dplyr)
library(eurostat)
library(giscoR)
library(sf)
library(spdep)
library(mapview)


map1 <- giscoR::gisco_get_nuts()
## Contiguity rule (common border) 
## Connectivity and Spatial weights matrices 
## 
#
#
Poland.sf <- map1 %>%   
  dplyr::filter(LEVL_CODE == 2 & CNTR_CODE %in% "PL") %>% 
  dplyr::select(NUTS_ID) 
plot(Poland.sf, key.pos=1)
#
#
# Centroids (only necessary to plot the neighborhood structure, NOT to calucate it)
PL_centroids <- Poland.sf %>% 
  st_geometry() %>% 
  st_centroid()
PL_centroids
#
# manually shift "centroid" of "Mazowiecki regionalny" from Warszaw
# .. for visualization only, will not affect contiguity evaluation
PL_centroids[17] <- st_point(c(21.08923, 53))
#
#
# ?poly2nb # transforms polygons to nb, based on common borders
PL_contig <- poly2nb(Poland.sf, snap = 0.03) # note the snap & queen arguments
summary(PL_contig)
PL_contig[[1]]
#
# dev.off()
plot(Poland.sf, border = "black", reset = F)
plot(PL_contig, PL_centroids, col = "black", add = T,cex=2,pch=16)


#
# Connectivity and weight matrices
#
C <- nb2mat(PL_contig, style="B") # nb to binary matrix of connectivity
colnames(C) <- Poland.sf$NUTS_ID
rownames(C) <- Poland.sf$NUTS_ID
C
#
W <- nb2mat(PL_contig, style="W") # nb to row-standardized matrix of connectivity
colnames(W) <- Poland.sf$NUTS_ID
rownames(W) <- Poland.sf$NUTS_ID
round(W,2)

```



#####  Euler and Hamilton 


```{r}
# install.packages("igraph")
library(igraph)

# ============================================================
# Helper function
# ============================================================

plot_traversal <- function(g, layout, traversal_edges,
                           main,
                           subtitle,
                           start_vertex = NULL,
                           end_vertex = NULL,
                           vertex_order = NULL) {
  
  # Default appearance
  V(g)$color <- "#D9EAF7"
  V(g)$frame.color <- "#253494"
  V(g)$label.color <- "black"
  V(g)$size <- 33
  
  E(g)$color <- "grey70"
  E(g)$width <- 2
  E(g)$label <- NA
  E(g)$label.color <- "black"
  E(g)$label.cex <- 1.4
  
  # Highlight the relevant traversal
  E(g)[traversal_edges]$color <- "#D7301F"
  E(g)[traversal_edges]$width <- 5
  
  # Number edges by traversal order
  E(g)$label[traversal_edges] <- seq_along(traversal_edges)
  
  # Optional vertex visit order
  if (!is.null(vertex_order)) {
    V(g)$label <- paste0(
      V(g)$name,
      "\n(",
      match(V(g)$name, vertex_order),
      ")"
    )
  }
  
  # Highlight start and end
  if (!is.null(start_vertex)) {
    V(g)[start_vertex]$color <- "#66C2A5"
  }
  
  if (!is.null(end_vertex)) {
    V(g)[end_vertex]$color <- "#FDAE61"
  }
  
  # If beginning and end are identical
  if (!is.null(start_vertex) &&
      !is.null(end_vertex) &&
      start_vertex == end_vertex) {
    
    V(g)[start_vertex]$color <- "#FFD92F"
  }
  
  plot(
    g,
    layout = layout,
    main = main,
    sub = subtitle,
    vertex.label.cex = 1.05,
    vertex.label.family = "sans",
    vertex.label.font = 2,
    edge.label.family = "sans",
    edge.label.font = 2,
    edge.label.bg = "white",
    edge.curved = 0,
    asp = 1,
    margin = 0.12
  )
}


# ============================================================
# 1. EULER TRAIL, BUT NO EULER CIRCUIT
#
# Exactly two vertices, A and B, have odd degree.
# The path traverses every EDGE exactly once.
# ============================================================

g_euler_trail <- graph_from_literal(
  A--B, B--C, C--D, D--A,
  A--C,
  B--E, E--C
)

layout_euler_trail <- matrix(
  c(
    0, 1,    # A
    2, 1,    # B
    2, 0,    # C
    0, 0,    # D
    3, 0.5   # E
  ),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(c("A", "B", "C", "D", "E"), NULL)
)

# Order coordinates according to the igraph vertex order
layout_euler_trail <- layout_euler_trail[
  V(g_euler_trail)$name, ,
  drop = FALSE
]

stopifnot(has_eulerian_path(g_euler_trail))
stopifnot(!has_eulerian_cycle(g_euler_trail))

et <- eulerian_path(g_euler_trail)

et_edges <- as.integer(et$epath)
et_vertices <- V(g_euler_trail)$name[as.integer(et$vpath)]

et_vertices
degree(g_euler_trail)


# ============================================================
# 2. EULER CIRCUIT
#
# Two triangles share vertex A.
# Every vertex has even degree.
# ============================================================

g_euler_circuit <- graph_from_literal(
  A--B, B--C, C--A,
  A--D, D--E, E--A
)

layout_euler_circuit <- matrix(
  c(
    0,  0,     # A
    -2,  1,     # B
    -2, -1,     # C
    2,  1,     # D
    2, -1      # E
  ),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(c("A", "B", "C", "D", "E"), NULL)
)

layout_euler_circuit <- layout_euler_circuit[
  V(g_euler_circuit)$name, ,
  drop = FALSE
]

stopifnot(has_eulerian_path(g_euler_circuit))
stopifnot(has_eulerian_cycle(g_euler_circuit))

ec <- eulerian_cycle(g_euler_circuit)

ec_edges <- as.integer(ec$epath)
ec_vertices <- V(g_euler_circuit)$name[as.integer(ec$vpath)]

ec_vertices
degree(g_euler_circuit)


# ============================================================
# 3. HAMILTON PATH, BUT NO HAMILTON CYCLE
#
# A and F have degree 1, so neither can lie on a cycle.
# The highlighted path visits every VERTEX exactly once.
# ============================================================

g_hamilton_path <- graph_from_literal(
  A--B, B--C, C--D, D--E, E--F,
  B--D, C--E
)

layout_hamilton_path <- matrix(
  c(
    0, 0,     # A
    1, 1,     # B
    2, 0,     # C
    3, 1,     # D
    4, 0,     # E
    5, 1      # F
  ),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(c("A", "B", "C", "D", "E", "F"), NULL)
)

layout_hamilton_path <- layout_hamilton_path[
  V(g_hamilton_path)$name, ,
  drop = FALSE
]

hp_vertices <- c("A", "B", "C", "D", "E", "F")

hp_edges <- as.integer(
  E(g_hamilton_path, path = hp_vertices)
)

# Check that all vertices occur exactly once
stopifnot(
  length(hp_vertices) == vcount(g_hamilton_path),
  length(unique(hp_vertices)) == vcount(g_hamilton_path),
  all(degree(g_hamilton_path)[c("A", "F")] == 1)
)


# ============================================================
# 4. HAMILTON CYCLE
#
# The outer hexagon is a Hamilton cycle.
# The three internal chords are deliberately not used.
# ============================================================

g_hamilton_cycle <- graph_from_literal(
  A--B, B--C, C--D, D--E, E--F, F--A,
  A--D, B--E, C--F
)

angle <- seq(
  from = pi / 2,
  by = -2 * pi / 6,
  length.out = 6
)

layout_hamilton_cycle <- cbind(
  cos(angle),
  sin(angle)
)

rownames(layout_hamilton_cycle) <- LETTERS[1:6]

layout_hamilton_cycle <- layout_hamilton_cycle[
  V(g_hamilton_cycle)$name, ,
  drop = FALSE
]

hc_vertices <- c("A", "B", "C", "D", "E", "F", "A")

hc_edges <- as.integer(
  E(g_hamilton_cycle, path = hc_vertices)
)

# Check: all vertices occur once before returning to A
stopifnot(
  length(unique(hc_vertices[-length(hc_vertices)])) ==
    vcount(g_hamilton_cycle),
  
  hc_vertices[1] ==
    hc_vertices[length(hc_vertices)]
)


# ============================================================
# PLOT ALL FOUR GRAPHS
# ============================================================



plot_traversal(
  g = g_euler_trail,
  layout = layout_euler_trail,
  traversal_edges = et_edges,
  main = "Euler trail",
  subtitle = "Every edge once; start and end differ",
  start_vertex = et_vertices[1],
  end_vertex = et_vertices[length(et_vertices)]
)

plot_traversal(
  g = g_euler_circuit,
  layout = layout_euler_circuit,
  traversal_edges = ec_edges,
  main = "Euler circuit",
  subtitle = "Every edge once; returns to the start",
  start_vertex = ec_vertices[1],
  end_vertex = ec_vertices[length(ec_vertices)]
)

plot_traversal(
  g = g_hamilton_path,
  layout = layout_hamilton_path,
  traversal_edges = hp_edges,
  main = "Hamilton path",
  subtitle = "Every vertex once; start and end differ",
  start_vertex = hp_vertices[1],
  end_vertex = hp_vertices[length(hp_vertices)],
  vertex_order = hp_vertices
)

plot_traversal(
  g = g_hamilton_cycle,
  layout = layout_hamilton_cycle,
  traversal_edges = hc_edges,
  main = "Hamilton cycle",
  subtitle = "Every vertex once; returns to the start",
  start_vertex = hc_vertices[1],
  end_vertex = hc_vertices[length(hc_vertices)],
  vertex_order = hc_vertices[-length(hc_vertices)]
)

mtext(
  "Red edges show the selected traversal; numbers indicate traversal order",
  outer = TRUE,
  side = 3,
  line = 0,
  font = 2,
  cex = 1.15
)

```





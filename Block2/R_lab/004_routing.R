#-------------------------------------------------------------------------------
#
# Routing-related examples based on sfnetworks functionality
#
# adapted from 
# https://luukvdmeer.github.io/sfnetworks/articles/sfn04_routing.html
#
#
#
#-------------------------------------------------------------------------------
# load libraries and Roxel example data
#
library(sf)
library(sfnetworks)
library(tidygraph)
library(dplyr)
library(purrr)
library(ggplot2)
rm(list=ls())

data("roxel", package = "sfnetworks")


#-------------------------------------------------------------------------------

# convert Roxel streets to sfnetwork
net <- roxel |>
  as_sfnetwork(directed = FALSE) |>
  st_transform(3035) |>
  activate("edges") |>
  mutate(weight = edge_length())

# shortest path identification
# The function st_network_paths() is a wrapper around the 
# igraph function igraph::shortest_paths() 

paths <- st_network_paths(
  net,
  from = 495,
  to = c(458, 121),
  weights = "weight")
# the output of st_network_paths() is a tibble, with one row per returned path

#-------------------------------------------------------------------------------

# visualization (prepare data and use ggplot)

nodes <- net |>
  activate("nodes") |>
  st_as_sf() |>
  mutate(node_id = row_number())

edges <- net |>
  activate("edges") |>
  st_as_sf() |>
  mutate(edge_id = row_number())

?map2_dfr

path_edges <- map2_dfr(
  paths$edge_paths,
  seq_along(paths$edge_paths),
  \(edge_ids, path_id) {
    edges[edge_ids, ] |>
      mutate(path = factor(path_id))
  }
)

plot(path_edges[,"path"])


selected_nodes <- nodes |>
  filter(node_id %in% c(495, 458, 121)) |>
  mutate(
    role = case_when(
      node_id == 495 ~ "Origin",
      TRUE ~ "Destination"
    )
  )

ggplot() +
  geom_sf(
    data = edges,
    color = "grey75",
    linewidth = 0.4
  ) +
  geom_sf(
    data = path_edges,
    aes(color = path),
    linewidth = 1.8
  ) +
  geom_sf(
    data = selected_nodes,
    aes(shape = role, fill = role),
    color = "black",
    size = 4
  ) +
  scale_color_manual(
    values = c(
      "1" = "#E66101",
      "2" = "#5E3C99"
    ),
    name = "Route"
  ) +
  scale_shape_manual(
    values = c(
      "Origin" = 21,
      "Destination" = 22
    )
  ) +
  scale_fill_manual(
    values = c(
      "Origin" = "#31A354",
      "Destination" = "#DE2D26"
    )
  ) +
  coord_sf(datum = NA) +
  theme_void() +
  theme(
    legend.position = "right"
  )




#-------------------------------------------------------------------------------

# Closest facility analysis

# The purpose of closest facility analysis is, given a set of destination 
# locations (also referred to as the facilities) and origin locations 
# (also referred to as the sites), to find the closest n facilities to each 
# site. For example, you might want to find the nearest transit hub for each 
# address in a city, or the nearest hospital to high-risk road intersections.


# Retain the largest connected component for the analysis
net <- net |>
  activate("nodes") |>
  filter(group_components() == 1)

# Create the sampling area
hull <- net |>
  activate("nodes") |>
  st_as_sf() |>
  st_geometry() |>
  st_combine() |>
  st_convex_hull()

# Generate sites (origin locations) and facilities (destinations)
set.seed(128)

sites <- st_sample(
  hull,
  size = 20,
  type = "random"
)

set.seed(123)
facilities <- st_sample(
  hull,
  size = 4,
  type = "random"
)

# Blend sites and facilities into the network
net_blended <- net |>
  st_network_blend(
    c(sites, facilities)
  )

# Calculate site-to-facility network distances
cost_matrix <- st_network_cost(
  net_blended,
  from = sites,
  to = facilities,
  weights = "weight"
)

# Identify the closest facility to every site
closest_id <- apply(
  cost_matrix,
  MARGIN = 1,
  FUN = which.min
)

closest <- facilities[closest_id]

# Create straight lines for visualizing assignments
# .. note that shortest paths are calculated
connections <- mapply(
  FUN = function(site, facility) {
    st_cast(
      c(site, facility),
      "LINESTRING"
    )
  },
  site = sites,
  facility = closest,
  SIMPLIFY = FALSE
) |>
  st_sfc(crs = st_crs(net))

# Plot
plot(
  net_blended,
  col = "grey75",
  lwd = 1,
  main = "Closest facility by network distance"
)

plot(
  connections,
  add = TRUE,
  col = "#3182BD",
  lwd = 2
)

plot(
  sites,
  add = TRUE,
  pch = 21,
  bg = "#3182BD",
  col = "white",
  cex = 1.5
)

plot(
  facilities,
  add = TRUE,
  pch = 23,
  bg = "#E34A33",
  col = "black",
  cex = 2,
  lwd = 1.5
)

legend(
  "topright",
  legend = c(
    "Site",
    "Facility",
    "Closest-facility assignment"
  ),
  pch = c(21, 23, NA),
  pt.bg = c("#3182BD", "#E34A33", NA),
  col = c("white", "black", "#3182BD"),
  lty = c(NA, NA, 1),
  lwd = c(NA, NA, 2),
  pt.cex = c(1.5, 2, NA),
  bty = "n"
)


#-------------------------------------------------------------------------------

# Isochrones and isodistances

# With respect to a given point p and a given travel time t, an isochrone is 
# the line for which it holds that the travel time from any point on the line 
# to or from p is equal to t. 

# When using distances instead of time, it is called an isodistance.

# In sfnetworks there are no dedicated, optimized functions for calculating 
# isochrones or isodistances. However, we can roughly approximate them by using 
# a combination of sf and tidygraph functions.


# Prepare network and retain the largest connected component
net <- roxel |>
  as_sfnetwork(directed = FALSE) |>
  st_transform(3035) |>
  activate("nodes") |>
  filter(group_components() == 1) |>
  activate("edges") |>
  mutate(
    length_m = edge_length(),
    speed_m_s = units::set_units(5, "km/h") |>
      units::set_units("m/s"),
    time_s = length_m / speed_m_s
  )

# Select the network node nearest to the network centre
nodes <- net |>
  activate("nodes") |>
  st_as_sf() |>
  mutate(node_id = row_number())

centre <- nodes |>
  st_geometry() |>
  st_combine() |>
  st_centroid()

origin_id <- st_nearest_feature(centre, nodes)

origin <- nodes[origin_id, ]

# Calculate travel time from the origin to every node
reachable_nodes <- net |>
  activate("nodes") |>
  mutate(
    travel_time_s = node_distance_from(
      origin_id,
      weights = time_s
    )
  ) |>
  st_as_sf() |>
  filter(travel_time_s <= 600)

# Approximate the 10-minute isochrone
isochrone <- reachable_nodes |>
  st_geometry() |>
  st_combine() |>
  st_convex_hull()

# Extract edges for plotting
edges <- net |>
  activate("edges") |>
  st_as_sf()

# Plot
plot(
  st_geometry(edges),
  col = "grey75",
  lwd = 1,
  main = "Approximate 10-minute walking isochrone"
)

plot(
  isochrone,
  add = TRUE,
  col = adjustcolor("#3182BD", alpha.f = 0.25),
  border = "#08519C",
  lwd = 3
)

plot(
  st_geometry(reachable_nodes),
  add = TRUE,
  pch = 16,
  col = "#3182BD",
  cex = 0.7
)

plot(
  st_geometry(origin),
  add = TRUE,
  pch = 8,
  col = "#CB181D",
  cex = 2,
  lwd = 2
)

legend(
  "topright",
  legend = c(
    "Street network",
    "10-minute isochrone",
    "Reachable nodes",
    "Origin"
  ),
  col = c(
    "grey75",
    "#08519C",
    "#3182BD",
    "#CB181D"
  ),
  lty = c(1, 1, NA, NA),
  lwd = c(1, 3, NA, NA),
  pch = c(NA, NA, 16, 8),
  bty = "n"
)


#-------------------------------------------------------------------------------
# Add color coding to the plot based on walking distances

net <- roxel |>
  as_sfnetwork(directed = FALSE) |>
  st_transform(3035) |>
  activate("nodes") |>
  filter(group_components() == 1) |>
  activate("edges") |>
  mutate(
    length_m = as.numeric(edge_length()),
    walking_time_min = length_m / (5 * 1000 / 60)
  )

nodes <- net |>
  activate("nodes") |>
  st_as_sf() |>
  mutate(node_id = row_number())

edges <- net |>
  activate("edges") |>
  st_as_sf()

network_centre <- nodes |>
  st_geometry() |>
  st_combine() |>
  st_centroid()

origin_id <- st_nearest_feature(
  network_centre,
  nodes
)

origin <- nodes[origin_id, ]

nodes_accessibility <- net |>
  activate("nodes") |>
  mutate(
    walking_time = node_distance_from(
      origin_id,
      weights = walking_time_min
    )
  ) |>
  st_as_sf() |>
  filter(is.finite(walking_time))

ggplot() +
  geom_sf(
    data = edges,
    color = "grey80",
    linewidth = 0.35
  ) +
  geom_sf(
    data = nodes_accessibility,
    aes(color = walking_time),
    size = 1.7
  ) +
  geom_sf(
    data = origin,
    shape = 8,
    color = "red",
    size = 4,
    stroke = 1.2
  ) +
  scale_color_viridis_c(
    option = "viridis",
    direction = -1,
    name = "Walking time\n(minutes)"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Walking time from the origin",
    subtitle = "Network distances converted using a walking speed of 5 km/h"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


#-------------------------------------------------------------------------------
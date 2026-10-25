#
# sfnetworks basics
#
# sfnetworks connects functionalities of the tidygraph package for 
# network analysis and the sf package for spatial data science.
#
# Geospatial networks are graphs embedded in geographical space. Both the nodes 
# and edges in the graph can be represented as geographic features: the nodes 
# most commonly as points, and the edges as linestrings. They play an important 
# role in many different domains, ranging from transportation planning and 
# logistics to ecology and epidemiology. 
#
# script adapted from: https://luukvdmeer.github.io/sfnetworks/

#-------------------------------------------------------------------------------
library(dplyr)
library(sfnetworks)
library(sf)
library(tidygraph)
library(igraph)
library(ggplot2)
rm(list=ls())
#-------------------------------------------------------------------------------

?roxel
roxel # sf object
net <- as_sfnetwork(roxel)
net # sf object, directed multigraph
plot(net)


# use dplyr/tidyverse operations on the spatial graph

net <- net %>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>%
  activate("nodes") %>%
  mutate(bc = centrality_betweenness(weights = weight, directed = FALSE))

# extract graph tables
net %>% activate("edges") %>% as_tbl_graph() 
net %>% activate("nodes") %>% as_tbl_graph() 

# extract sf tables
net %>% activate("edges") %>% st_as_sf() 
net %>% activate("nodes") %>% st_as_sf() 


# plot betweenness centrality using ggplot2

ggplot() +
  geom_sf(data = st_as_sf(net, "edges"), col = "grey50") +
  geom_sf(data = st_as_sf(net, "nodes"), aes(col = bc, size = bc)) +
  ggtitle("Betweenness centrality in Münster Roxel")

#-------------------------------------------------------------------------------

# spatial filtering - polygon based


# create a polygon for the filtering (we use a simple rectangle)
# - a rectangle within the borders of Roxel
# - LAEA Europe - EPSG:3035
p1 <- st_point(c(4151358, 3208045))
p2 <- st_point(c(4151340, 3207120))
p3 <- st_point(c(4151856, 3207106))
p4 <- st_point(c(4151874, 3208031))
poly <- st_multipoint(c(p1, p2, p3, p4)) %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = 3035)

# the poly and net geometries need to match for any sf-based operation
net <- as_sfnetwork(roxel) %>%
  st_transform(3035)

filtered_net <- st_filter(net, poly, .pred = st_intersects)

plot(net, col = "grey")
plot(poly, border = "red", lty = 4, lwd = 4, add = TRUE)
plot(filtered_net, col = "black", add = TRUE)

# or use just the filtered part
plot(filtered_net)

# clean-up the results by removing isolated nodes (vertices)
cleaned_net <- filtered_net %>%
  activate("nodes") %>%
  filter(!node_is_isolated())

plot(cleaned_net) 

# note that the output is a multigraph with isolated sub-graphs
# often, we only keep the largest element:

# Identify connected components
comp <- igraph::components(cleaned_net, mode = "weak")
comp

# ID of the largest connected component
largest_comp <- which.max(comp$csize)

# Keep only nodes belonging to the largest component
cleaned_net2 <- cleaned_net %>%
  activate(nodes) %>%
  mutate(component = comp$membership) %>%
  filter(component == largest_comp)

# Plot the largest connected component
plot(cleaned_net2)

#-------------------------------------------------------------------------------

# spatial filtering - distance based

# identify the center of roxel graph
net # make sure CRS is  EPSG:3035 
center_point <- st_centroid(st_combine(net))

# filter nodes withing 500 meters 
filtered <- net %>%
  activate("nodes") %>%
  st_filter(center_point, .predicate = st_is_within_distance, dist = 500)

# plot the output
plot(net, col = "grey")
plot(center_point, col = "red", cex = 3, pch = 20, add = TRUE)
plot(filtered, col = "black", add = TRUE)

# plot the filtered graph only
#
plot(filtered)

# .. clean the filtered graph is necessary for further analysis

#-------------------------------------------------------------------------------

# allocate graph elements to a separately given grid/sector information
# - here, we create imaginary postal code areas for the Roxel dataset

net # make sure CRS is  EPSG:3035 (projected, distances in meters)
codes <- net %>%
  st_make_grid(n = c(2, 2)) %>%
  st_as_sf() %>%
  mutate(post_code = as.character(seq(1000, 1000 + n() * 10 - 10, 10)))

joined <- st_join(net, codes, join = st_intersects)
joined
joined %>% activate(nodes) %>% as_tibble()

plot(net, col = "grey")
plot(codes, col = NA, border = "red", lty = 4, lwd = 4, add = TRUE)
text(st_coordinates(st_centroid(st_geometry(codes))), codes$post_code, cex = 2)
plot(st_as_sf(joined, "nodes"), pch = 20, add = TRUE) # plot.sf() automatically maps attribute values to colors

# plot just the selected section (post code)
net_1020 <- joined %>%
  activate(nodes) %>%
  filter(post_code == "1020")

plot(net_1020)


#-------------------------------------------------------------------------------
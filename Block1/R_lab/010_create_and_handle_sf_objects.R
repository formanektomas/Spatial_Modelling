################################################################################
# sf geometries
# - creating sf geometries
# - basic operations of sf geometries
# - this script is somewhat technical, as it covers selected basic sf topics
#
#
library(sf)
library(tidyverse)
library(mapview)
# rm(list=ls())
################################################################################
#
# Section 1: Creating sf geometries - points
#
#
# one point
p <- st_point(c(0,1)) 
p
class(p)
plot(p, pch = 16)
title("point")
box(col = 'grey')
# note that
p <- st_sf(p, crs = 4326)
# We cannot assign CRS directly to sfg object. Instead, wrap it into an sfc:
p |> st_sfc() |> st_sf(crs = 4326)
# 
# 
#
# one point with CRS (VSE-based)
p2 <- sf::st_as_sfc("POINT(14.442408 50.083300)", crs = 4326)
p2
p2[[1]]
class(p2[[1]])
p2
class(p2)
mapview::mapview(p2)
#
#
# multipoint - can be constructed using rbind()
mp <- st_multipoint(rbind(c(1,2), c(2, 1), c(4, 1), c(3, 3.5), c(2,4)))
mp
class(mp)
plot(mp, pch = 16)
title("multipoint")
box(col = 'grey')
# again CRS can be added to sfc, not sfg
#
# We can separate a multipoint sfg into an sfc collection of points:
# - wrap the sfg object in an sfc first, then cast
mp_sfc <- st_sfc(mp)
mp_sfc
points <- st_cast(mp_sfc, "POINT")
points
plot(points, pch = 16)
title("multiple points")
box(col = 'grey')
#
#
# Calculate Euclidean distances among points
st_distance(x= points, y=points, by_element = FALSE)
#
#
# Plot the distances on a spatial graph
# (supplementary visualization only)
#   Put points into an sf object with IDs
nodes <- st_sf(id = seq_along(points),geometry = points)
#   Coordinates of points
coords <- st_coordinates(nodes)
#   All pairwise combinations of points
edge_index <- t(combn(nodes$id, 2))
# - from edge_index I can manually remove edges (rows of matrix), if appropriate
#   Create edges as LINESTRING geometries
edges <- lapply(seq_len(nrow(edge_index)), function(i) {
  from_id <- edge_index[i, 1]
  to_id   <- edge_index[i, 2]
  st_linestring(rbind(coords[from_id, ],coords[to_id, ]))})
edges <- st_sf(from = edge_index[, 1],to = edge_index[, 2],geometry = st_sfc(edges))
#   Calculate Euclidean edge lengths
edges <- edges |>
  mutate(distance = as.numeric(st_length(geometry)),
    distance_lab = round(distance, 2))
#   Midpoints for distance labels
edge_labels <- edges |> mutate(geometry = st_centroid(geometry))
#   Plot graph
ggplot() +
  geom_sf(data = edges,color = "grey50",linewidth = 0.6 ) +
  geom_sf(data = nodes,color = "red", size = 3) +
  geom_sf_text(data = nodes,aes(label = id),nudge_y = 0.12, size = 4) +
  geom_sf_text(data = edge_labels,aes(label = distance_lab),color = "blue", size = 3) +
  coord_sf(expand = TRUE) +
  labs(title = "Spatial graph with Euclidean distances among all points") +
  theme_minimal()
#
################################################################################
#
# Section 2: Creating sf geometries - lines
#
#
# one line
my_ls <- st_linestring(rbind(c(1,1), c(5,3), c(5, 6), c(4, 6), c(3, 4), c(2, 3)))
plot(my_ls, lwd = 2)
title("linestring")
box(col = 'grey')
my_ls
my_ls <- st_sfc(my_ls)
my_ls
class(my_ls)
#
#
# one geocoded linestring 
# "VSE" >> Bukowski's Bar" >> "Palac Akropolis" >> "U Sadu"
route <- st_as_sfc(
  "LINESTRING(
    14.442408 50.083300, 
    14.448200 50.084950,
    14.450750 50.083550,
    14.451700 50.082200
  )", crs = 4326)
route
st_is_valid(route)
class(route)
mapview(p2, col.regions = "red", cex = 6) +
mapview(route, color = "blue", lwd = 4, layer.name = "Route")
#
#
# Again, we can use st_cast() to get points along the line/route
points_along_the_route <- st_cast(route, "POINT")
points_along_the_route
mapview(points_along_the_route, col.regions = "red", cex = 6) +
  mapview(route, color = "blue", lwd = 4, layer.name = "Route")
#
#
# multilinestring
mls <- st_multilinestring(list(
  rbind(c(1,1), c(5,3), c(5, 6), c(4, 6), c(3, 4), c(2, 3)),
  rbind(c(3,0), c(4,1), c(2,1))))
plot(mls, lwd = 2)
title("multilinestring")
box(col = 'grey')
mls
st_is_valid(mls)
#
#
# split multilinstring into indivual linestrings
st_cast(st_sfc(mls), "LINESTRING")
#
################################################################################
#
# Section 3: Creating sf geometries - polygons
#
#
# single polygon, no holes
# - polygon rings are closed (the last point equals the first)
# - polygon is created as a list object
my_pol <- st_polygon(list(rbind(c(14.442408, 50.083300), 
                                c(14.448200, 50.084950),
                                c(14.450750, 50.083550),
                                c(14.451700, 50.082200),
                                c(14.442408, 50.083300)
                                )))
my_pol <- st_sfc(my_pol, crs = 4326)
my_pol
class(my_pol)
# by convention outer ring of a polygon is winded counter-clockwise, 
# while the holes are winded clockwise, but polygons for which 
# this is not the case are still considered valid
# - like in this example.
st_is_valid(my_pol)
mapview(points_along_the_route, col.regions = "red", cex = 6, layer.name = "Start") +
  mapview(my_pol, color = "blue", lwd = 4, layer.name = "Full route")
#
#
# Polygon-based operations - area
st_area(my_pol) # WGS 84
my_pol |> st_transform(5514) |> st_area() # Czech planar projection (Krovak)
my_pol |> st_transform(3035) |> st_area() # European equal-area projection
# small differences arise because each method uses a different geometric model
#
#
# Extract elements
st_cast(my_pol, "POINT")
st_cast(my_pol, "LINESTRING")
# - returns a single linestring (valid). To get separate linestrings/edges
#   we need to extract the edges first (see spatial graph above for guidance)
#
#
# Simple polygon with a hole
po <- st_polygon(list(rbind(c(2,1), c(3,1), c(5,2), c(6,3), c(5,3), c(4,4), c(3,4), c(1,3), c(2,1)),
                      rbind(c(2,2), c(3,3), c(4,3), c(4,2), c(2,2))))
plot(po, border = 'black', col = '#ff8888', lwd = 2)
title("polygon")
box(col = 'grey')
po <- st_sfc(po)
po
# Polygons are created as list - elements can be accessed by [[]] operator.
po[[1]] # both "rings
po[[1]][[1]] # outer ring
po[[1]][[2]] # inner ring
st_area(po) # the hole is properly subtracted as area is calculated:
po[[1]][[1]] |> list() |> st_polygon() |> st_sfc() |> st_area()
po[[1]][[2]] |> list() |> st_polygon() |> st_sfc() |> st_area()
#
################################################################################
#
# Section 4: Intersections
#
#
plot(po, border = 'black', col = '#ff8888', lwd = 2,
     xlim = c(0.5, 6.5),ylim = c(0.5, 6.5),reset=FALSE)
plot(my_ls, lwd = 2, , col="blue",add=TRUE)
title("polygon - line intersections")
box(col = 'grey')
#
#
# The following functions return TRUE/FALSE 
# (intersection measurements discussed further below)
#
# st_intersects() asks whether the two geometries share any space.
st_intersects(my_ls, po)
st_intersects(my_ls, po, sparse = FALSE)
# st_crosses() asks whether the line passes through the polygon.
st_crosses(my_ls, po, sparse = FALSE)
# st_within() asks whether the whole line is inside the polygon.
st_within(my_ls, po, sparse = FALSE)
# st_disjoint() asks whether they have no spatial contact.
st_disjoint(my_ls, po, sparse = FALSE)
# st_touches() TRUE if intersects = TRUE & crosses = FALSE
st_touches(my_ls, po, sparse = FALSE)
#
#
# Line length calculations, based on intersections
# total length
total_length <- st_length(my_ls)
total_length
# Intersection: line inside polygon
line_inside <- st_intersection(my_ls, po)
line_inside
st_length(line_inside)
# Intersection: line outside polygon
line_outside <- st_difference(my_ls, po)
line_outside
st_length(line_outside)
# Visualize the intersections
ggplot() +
  geom_sf(data = st_sf(geometry = po), fill = "#ff8888", color = "black", alpha = 0.5) +
  geom_sf(data = st_sf(part = "outside", geometry = line_outside),
          color = "green", linewidth = 1.2) +
  geom_sf(data = st_sf(part = "inside", geometry = line_inside),
          color = "blue", linewidth = 1.4) +
  coord_sf(expand = TRUE) +
  labs(
    title = "Parts of the linestring inside and outside the polygon"
  ) +
  theme_minimal()
#
################################################################################
#
# Section 5: Multipolygons and Geometry collections
#
#
# Multipolygon
mpo <- st_multipolygon(list(
  list(rbind(c(2,1), c(3,1), c(5,2), c(6,3), c(5,3), c(4,4), c(3,4), c(1,3), c(2,1)),
       rbind(c(2,2), c(3,3), c(4,3), c(4,2), c(2,2))),
  list(rbind(c(3,7), c(4,7), c(5,8), c(3,9), c(2,8), c(3,7)))))
plot(mpo, border = 'black', col = '#ff8888', lwd = 2)
title("multipolygon")
box(col = 'grey')
#
#
# Geometry collections (requires a list of sfg geometries as inputs)
gc <- st_geometrycollection(list(p2[[1]],route[[1]],my_pol[[1]]))
gc 
class(gc)
# compare to 
gc2 <- rbind(p2,route,my_pol)
gc2 <- st_sfc(gc2)
gc2
class(gc2)
#
################################################################################
#
# Section 6: sf dataframes
#
# Make sf dataframe from scf + data
# Individual edges of the above "route" (polygon version)
edges <- st_sf(
  geometry = st_sfc(
    st_linestring(rbind(
      c(14.442408, 50.083300),
      c(14.448200, 50.084950)
    )),
    st_linestring(rbind(
      c(14.448200, 50.084950),
      c(14.450750, 50.083550)
    )),
    st_linestring(rbind(
      c(14.450750, 50.083550),
      c(14.451700, 50.082200)
    )),
    st_linestring(rbind(
      c(14.451700, 50.082200),
      c(14.442408, 50.083300)
    )),
    crs = 4326
  )
)
# Calculate edge length in metres, use projected CRS 
edges <- edges |>
  mutate(edge_length_m = as.numeric(st_length(st_transform(geometry, 5514)))) |>
  mutate(edge_length_m = round(edge_length_m, 1))
edges
class(edges)
# plot the sf object
mapview(points_along_the_route, col.regions = "red", cex = 6) +
mapview(edges, zcol = "edge_length_m", lwd = 5, layer.name = "Distances along the route")
#
#
# Make sf dataframe from a dataframe
cities <- data.frame(
  Name = c("Tokyo", "Delhi", "Shanghai", "Dhaka", "São Paulo"),
  lon  = c(139.6917, 77.1025, 121.4737, 90.4125, -46.6333),
  lat  = c(35.6895, 28.7041, 31.2304, 23.8103, -23.5505)
)
cities
# make an sf object
cities_sf <- cities |>
  st_as_sf(coords = c("lon", "lat"),crs = 4326)
cities_sf
# to keep the longitude and lattitude data as columns in the sf, use remove=FALSE
cities_sf <- cities |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326,remove = FALSE)
cities_sf


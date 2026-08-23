#-------------------------------------------------------------------------------
#
#
# Rasterization
#
# Goal of this script: 
#
# Show conversion of vector objects into their representations in rasters
# using dataset on cycle hire points in London (from the spData package)
#
# 
#-------------------------------------------------------------------------------
# packages
library(sf)
library(spData)
library(ggplot2)
library(grid)
library(ggplot2)
library(dplyr)
library(tibble)
library(terra)
rm(list=ls())
#
#-------------------------------------------------------------------------------
#
#
# Read and visualize the data
cycleHire <- spData::cycle_hire_osm
cycleHire
mapview::mapview(cycleHire)

# Transform data from WGS84 to planar projection EPSG:27700 - British National Grid (BNG) CRS
cycleHire_projected <- cycleHire |>
  st_transform("EPSG:27700")
#
# Create a raster template with extension covering the observed bike hire points
# - use 250m x 250m resolution
raster_template <- rast(ext(cycleHire_projected), resolution = 250,
                        crs = crs(cycleHire_projected)) |> 
  extend(c(2,2)) # add buffer for smoother analysis

# Create a raster showing (binary) presence/absence of cycle hire points
ch_raster1 <- rasterize(cycleHire_projected, raster_template)
plot(ch_raster1)
# plot raster on map of London
ch_raster1a <- project(ch_raster1, "epsg:4326")
mapview::mapview(ch_raster1a,legend=FALSE)



# Different cycle hire locations have different numbers of bicycles 
# described by the capacity variable
# - we calculate total capacity in each grid cell 
ch_raster2 <- rasterize(cycleHire_projected, raster_template, 
                        field = "capacity", fun = sum, na.rm = TRUE)
# plot bike hire capacity on the map of London
ch_raster2a <- project(ch_raster2, "epsg:4326")
mapview::mapview(ch_raster2a,col.regions = viridisLite::viridis)



# Distance to the nearest cycle-hire point
# - straight-line distance from the centre of every raster cell
# - to the nearest original cycle-hire point.
#
# Convert the projected sf points to a terra SpatVector
cycleHire_vect <- terra::vect(cycleHire_projected)
ch_distance <- terra::distance(
  x = raster_template,
  y = cycleHire_vect,
  unit = "m",
  rasterize = FALSE)
names(ch_distance) <- "distance_m"
ch_distance
# Plot the distances
ch_distance_wgs84 <- project(ch_distance,"EPSG:4326")
mapview::mapview(
  ch_distance_wgs84,
  layer.name = "Distance to cycle hire (m)",
  col.regions = hcl.colors(100, "YlOrRd", rev = TRUE),
  alpha.regions = 0.3) +
  mapview::mapview(
    cycleHire,
    layer.name = "Cycle-hire points",
    col.regions = "blue")

#-------------------------------------------------------------------------------
#
library(plotly)
library(mapview)
library(h3jsr)
library(osmdata)
library(sf)
library(RCzechia)
library(terra) 
library(dplyr)
library(exactextractr)
library(ggplot2)
rm(list = ls())
#
#-------------------------------------------------------------------------------
#
# Goal of this script: Raster data manipulation, subsetting, vector interactions
# 
# - visualize raster data
# - crop and mask 
# - subset to points, lines, polygons
#   move raster data into {sf} objects
# 

#
#-------------------------------------------------------------------------------

# Get elevation raster data (Copenicus-based, approx. 70 MB)
CZProfile <- RCzechia::vyskopis(format="actual",cropped = F)
CZProfile
names(CZProfile) <- "elevation"
plot(CZProfile)

# get Czechia state boundaries
CZbound <- RCzechia::republika(resolution = "low")
#
# plot raster
plot(CZProfile)
plot(CZbound,
     border = "red",lwd=2,
     col = NA,
     add = TRUE)

# Crop raster image (to a bounding box of CZ borders)
CZcropped <- terra::crop(CZProfile, CZbound) # crop raster to the bbox of polygon

# plot cropped raster
plot(CZcropped)
plot(CZbound,
     border = "red",lwd=2,
     col = NA,
     add = TRUE)
#
# Mask raster to a polygon boundary
CZmasked <- terra::mask(CZcropped, CZbound)

# Plot masked raster
plot(CZmasked)
#
# Note: masking can be done without cropping, yet cropping adjusts map "range"
# to match the bounding box for the object of interest
# Compare to:
CZProfile %>% 
  terra::mask(CZbound) %>% 
  plot()
# here, the raster is larger, many NA values can be removed w/o effect on analysis
#
#-------------------------------------------------------------------------------

# Subset points - extract elevation at given raster points (coordinates)

CZmasked[1500,1500] # choose some point on raster (in CZ): returns the elevation
# to plot the point on the raster, we need cell ID first
cell_id <- cellFromRowCol(CZmasked, row = 1500, col = 1500)
# next step is to get spatial coordinates from cell ID
point_xy <- xyFromCell(CZmasked, cell_id)
point_xy
class(point_xy)
plot(CZmasked)
points(point_xy, pch = 21,bg = "red",col = "red",cex = 1.5)

# Now, we can produce an sf object with elevation and geometry (POINT)
# - vectorize the raster element, note the type argument
selected_point <- vect(point_xy,type = "points",crs = crs(CZmasked))
# - convert to sf
selected_point_sf <- st_as_sf(selected_point)
selected_point_sf
# 
selected_point_sf <- selected_point_sf |>
  # terra::extract() returns a data frame, while mutate() needs the extracted 
  # column as a vector. Select the elevation column from the extraction result:
  mutate(elevation = terra::extract(CZmasked,point_xy)[["elevation"]]) |>
  select(elevation, geometry)
selected_point_sf

# Now, repeat the same for three specific places:
addresses <- c(
  "nám. W. Churchilla, Prague, Czechia",
  "Václavské náměstí, Prague, Czechia",
  "Náměstí Míru, Prague, Czechia",
  "nám. Svobody, Brno, Czechia")
locs <- tidygeocoder::geo(address = addresses) |> 
  as.data.frame() |> 
  dplyr::filter(!is.na(long), !is.na(lat)) |> 
  # st_as_sf does not work with NAs
  sf::st_as_sf(coords = c("long", "lat"),crs = 4326, remove = FALSE)
locs
# locs <- st_read("data/locs.gpkg") # if the geocoding API request fails..


locs <- locs |>
  # terra::extract() returns a data frame, while mutate() needs the extracted 
  # column as a vector. Select the elevation column from the extraction result:
  mutate(elevation = terra::extract(CZmasked,locs)[["elevation"]])
locs
#
#-------------------------------------------------------------------------------

# Subset lines - extract elevation along a straight line

# Create a straight line going through Czechia
Prague_to_Brno_line <- locs[c(2, 4), ] |> # select two rows from the sf dataframe
  st_transform(crs(CZmasked)) |> # not quite necessary, as both object in WGS84
  st_combine() |> # make a MULTIPOINT object (1 row) from the two POINT objects.
  st_cast("LINESTRING") |> # cast to LINESTRING.
  # in  R pipe, _ is a placeholder representing the object passed from 
  # the preceding step. Here, it is not strictly necessary, as previous
  # command produces an sfc element
  st_sf(geometry = _) # makes the output an sf object (general safety step, not necessary here)
plot(CZmasked)
lines(Prague_to_Brno_line, pch = 21,bg = "red",col = "red",lwd=2)

# Elevation data can be collected (a) with some desired granularity, 
#                                 (b) at raster resolution

# (a) subsetting at desired granularity
# we use the st_segmentize() function
# - dfMaxLength = 250 is reliably interpreted as 250 map units only in a projected metric CRS
# - so project to Krovak first (units are meters)
Prague_to_Brno <- st_transform(Prague_to_Brno_line, 5514)
Prague_to_Brno <- st_segmentize(Prague_to_Brno, dfMaxLength = 250) 
as.character(Prague_to_Brno$geometry)
# This is still a LINESTRING, just "denser" (more points) 
# >> covert to points for elevation extraction:
Prague_to_Brno = st_cast(Prague_to_Brno, "POINT") # points along the line
Prague_to_Brno$id <- 1:nrow(Prague_to_Brno)
Prague_to_Brno
# For plotting and meaningful analysis, we typically want to measure 
# distances from the start (along the original line from Prague to Brno)

Prague_to_Brno$dist <- NA # prepare variable for distance storage
first_point <- st_geometry(Prague_to_Brno)[1]
for (i in seq_len(nrow(Prague_to_Brno))) {
  Prague_to_Brno$dist[i] <- as.numeric(
    st_distance(st_geometry(Prague_to_Brno)[i],first_point))
}

Prague_to_Brno$dist_km <- Prague_to_Brno$dist / 1000
Prague_to_Brno <- Prague_to_Brno %>%
  st_transform(4326) %>%  # back to WGS84 - to match the elevation raster
  # terra::extract() returns a data frame, while mutate() needs the extracted 
  # column as a vector. Select the elevation column from the extraction result:
  mutate(elevation = terra::extract(CZmasked, . )[["elevation"]]) 
  # "." is a placeholder if %>% (magrittr pipe) is used, doesn't work with |> (base pipe)
Prague_to_Brno

# Plot the results
Prague_to_Brno |>
  arrange(dist_km) |>
  ggplot(aes(x = dist_km, y = elevation)) +
  geom_line(linewidth = 0.8, colour = "steelblue") +
  labs(
    x = "Distance along the line from Prague (left) to Brno (right), in km",
    y = "Elevation",
    title = "Elevation profile along the straight line (Prague to Brno)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# (b) subsetting can be performed "directly" - at the raster resolution as well:

# Extract all Prague2Brno raster cells touched by the line
P2B_cells <- terra::extract(
  CZmasked,
  terra::vect(Prague_to_Brno_line), # the LINESTRING with a WGS84 CRS
  cells = TRUE,
  xy = TRUE,
  touches = TRUE,
  ID = FALSE
)

head(P2B_cells) 
class(P2B_cells)
# interactive 3D plot
plot_ly(
  data = P2B_cells,
  x = ~x,
  y = ~y,
  z = ~elevation,
  color = ~elevation,
  colors = viridisLite::viridis(100),
  type = "scatter3d",
  mode = "lines+markers",
  marker = list(size = 2),
  line = list(width = 4)
) |>
  layout(
    title = "Three-dimensional elevation profile along D5",
    scene = list(
      xaxis = list(title = "X coordinate"),
      yaxis = list(title = "Y coordinate"),
      zaxis = list(title = "Elevation")
    )
  )

#
#-------------------------------------------------------------------------------

# Subset lines - extract raster data - elevation - along a general line (D5 higway)
# 
# - most of the steps are identical to the previous example
# - main difference: how to calculate distance along the line (highway),
#   rather then aerial distances from the origin

# Query OSM for D5 motorway
# You can also use getbb("Czech Republic"), but a smaller bbox is faster.
# - i.e. there is higher probability that opq() will return the data and not time-out
bbox_d5 <- c(xmin = 12.3,ymin = 49.4,xmax = 15.0,ymax = 50.3)
osm_d5 <- opq(bbox = bbox_d5) |>
  add_osm_feature(key = "highway", value = "motorway") |>
  add_osm_feature(key = "ref", value = "D5") |>
  osmdata_sf()
D5_osm <- osm_d5$osm_lines
#    If osm database is not responding (heavy traffic is common)
#    you can read the D5_osm next 
#    (the OSM dowload has 4MB+, so just the LINESTRING objects are stored)
# D5_osm <- st_read("data/D5_osm.gpkg")
D5_osm # this is a typical result of a "road" query... many small segments are returned
st_geometry(D5_osm) <- "geometry" # as the geometry column from OSM is "geom"


# Merge connected segments and keep the main D5 line
D5 <- D5_osm |>
  st_transform(5514) |>                    # metric Czech CRS
  st_make_valid() |>                       # precautionary, not necessary here
  summarise(geometry = st_union(geometry)) |> # all into one geometry, typically a MULTILINESTRING
  st_line_merge() |> # Joins connected line segments into longer continuous lines. Does NOT close gaps...
  st_cast("LINESTRING") # Separates the MULTILINESTRING into individual LINESTRING features.
# Often, a main line and multiple smaller segments (eg. carriageways) are created.
# - additional filtering and steps would be necessary
# - this is not the case here, there are two LINESTRING elements (D5 Prague>>Border, D5 Border>>Prague)
D5
D5 <- D5[2,] # direction: Border >> Prague
D5wgs84 <- D5 |> # create object for plotting the highway, analysis stays in EPSG 5514
  st_transform(4326)

# Plot the D5 highway
plot(CZmasked,main = "CZ elevation")
plot(st_geometry(D5wgs84),col = "red",lwd = 2,add = TRUE)

# Sample D5 highway, use 250 m segments 
# (plus endpoint adjustment if length not divisible by 250)
sample_distance_m <- 250
# Total D5 length in metres
D5_length_m <- as.numeric(st_length(D5))
# Distances measured along D5
dist_m <- seq(from = 0,to = D5_length_m,by = sample_distance_m)

# Include the final endpoint if the total length is not divisible by 250
if (tail(dist_m, 1) < D5_length_m) {
  dist_m <- c(dist_m, D5_length_m)
}
tail(dist_m) # the endpoint adjustment is visible here

# Convert distances into relative positions from 0 to 1
sample_positions <- dist_m / D5_length_m

# Sample points along D5 and convert MULTIPOINT to POINT features
D5_points <- st_line_sample(st_geometry(D5),sample = sample_positions) |> # dense LINESTRING
  st_cast("POINT") |>
  st_sf(id = seq_along(dist_m),
        dist_m = dist_m, # include the dist_m object as a column to output sf object
        dist_km = dist_m / 1000,
        geometry = _) # represents the object created in previous step (POINT), i.e. adds geometry
# Inspect result
D5_points

# Extract elevation from raster - same logic as for a straight line from now on.
D5_points <- D5_points |>
  st_transform(crs = 4326) |> # harmonize CRS with CZmasked
  # terra::extract() returns a data frame, while mutate() needs the extracted 
  # column as a vector. Select the elevation column from the extraction result:
  mutate(elevation = terra::extract(CZmasked,D5_points)[["elevation"]])
D5_points

# Plot the results
D5_points |>
  arrange(dist_km) |>
  ggplot(aes(x = dist_km, y = elevation)) +
  geom_line(linewidth = 0.8, colour = "steelblue") +
  labs(
    x = "Distance along D5 higway from the border (left) to Prague (right), in km",
    y = "Elevation",
    title = "Elevation profile along the D5 higway"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#
#-------------------------------------------------------------------------------

# Subset to polygons - extract raster data - elevation - to polygons
# 
# a) Average profile (meters) of historical (1930) judicial counties
CZcounties <- RCzechia::historie("okresy_1930")
#
# CZcounties is an {sf} object, create a new variable with profile height value (m)
# for raster-to-polygon extraction, exactextractr::exact_extract() is faster...
CZcounties$elevation <- exactextractr::exact_extract(
  x = CZmasked, # source, masked raster with CZ profile
  y = CZcounties, # target, sf polygons 
  fun = "mean", # calculation of average profile height 
  weights = "area",
  coverage_area = T
) 
#
# plot processed data
ggplot(data = CZcounties, aes(fill = elevation, label = round(elevation))) +
  geom_sf(lwd = 1/3) +
  scale_fill_viridis_c(name = "Terrain \nprofile \n(elevation)",option = "D") +
  theme(axis.title = element_blank(),
        plot.caption = element_text(face = "italic"))


#
# b) calculate average profile on hexagonal grid
# elevation to H3 (Uber) hexagons, level 6
# prepare H3 hexagonss
CZ_buff <- CZbound %>% 
  st_transform(crs = 3587) %>% # planar projection (units are meters)
  st_buffer(dist = 800) %>% # buffer to create hexagons along the border
  st_transform(crs = 4326)

hex_ids <- h3jsr::polygon_to_cells(CZ_buff, res = 6) 
# returns all the H3 cell indexes within the supplied polygon geometry
hex_sf <- h3jsr::cell_to_polygon(hex_ids, simple = F)
# takes an H3 cell index and returns its bounding shape (usually a hexagon) in WGS84
hex_sf <- st_transform(hex_sf, st_crs(CZbound))
hex_sf <- hex_sf[st_intersects(hex_sf, CZbound, sparse = F), ]
hex_sf <- st_intersection(hex_sf, CZbound)
hex_sf
hex_sf <- hex_sf[st_is(hex_sf,c("POLYGON", "MULTIPOLYGON")),] # filter (multi)polygons
# only polygons and multipolygons are allowed in exact_extract()
# (lines and poins can be generated by the intersection() function)
plot(hex_sf[,1])
#
hex_sf$elevation <- exactextractr::exact_extract( # faster than terra::extract()
  x = CZmasked, # source, masked raster with CZ profile
  y = hex_sf, # target, sf polygons 
  fun = "mean", # calculation of average built-up - can be used for air pollution, etc.
  weights = "area",
  coverage_area = TRUE,
  progress = FALSE # hide console progress visualization
) 

# elevation to H3 grid
ggplot(data = hex_sf, aes(fill = elevation, label = round(elevation))) +
  geom_sf(lwd = 1/3) +
  scale_fill_viridis_c(
    name = "Terrain \nprofile \nH3 lev.6 \n(2321 h.)"
  ) +
  theme(
    axis.title = element_blank(),
    plot.caption = element_text(face = "italic"))






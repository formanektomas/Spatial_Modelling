#-------------------------------------------------------------------------------
#
# Raster algebra - local operations example
#
# Goal of this script:
#
# - Remote sensing & RGB layers, NIR bandwith
# - NDVI: Normalized Difference Vegetation Index 
# - NDWI: Normalized Difference Water Index
# 
#-------------------------------------------------------------------------------
# packages
if (!requireNamespace("spDataLarge", quietly = TRUE)) {
  install.packages("spDataLarge",repos = "https://geocompx.r-universe.dev")
}
library(sf)
library(ggplot2)
library(grid)
library(ggplot2)
library(dplyr)
library(tibble)
library(patchwork)
library(terra)
library(spDataLarge)
#
#-------------------------------------------------------------------------------
#
#
# Landsat 8 data (for NDVI)
#
#
# Normalized Difference Vegetation Index (NDVI = (nir - red)/(nir + red)) 
# Landsat 8 data - Red band: 640-670 nm, NIR band: 850-880 nm
#
#    NDVI is used to measure the presence, density, and approximate condition of
#    green vegetation from satellite or aerial imagery.
#
#    - healthy vegetation absorbs red light strongly because of chlorophyll
#    - healthy vegetation reflects near-infrared light strongly (leaf structure)
#
#    Water, clouds, snow, shadows    |    negative or near 0
#    Bare soil, built-up land        |    around 0
#    Sparse or stressed vegetation   |    low positive, 0.1–0.3
#    Moderate vegetation             |    around 0.3–0.5
#    Dense, healthy vegetation       |    high values, 0.5–0.8 or more
#
# NDVI usage: to assess vegetation state (stress level)
# - For example, droughts would lower NDVI as vegetation becomes less green 
#   and less dense. Note: NDVI measures plant response (not drought directly)
#
#-------------------------------------------------------------------------------
#
# Read and plot the data: Landsat 8 / Zion National Park, Utah (USA)
#
multi_raster_file <- system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast <- rast(multi_raster_file)
multi_rast <- project(multi_rast, "epsg:4326")
names(multi_rast)[c(1:4)] <- c("Blue", "Green","Red","NIR")
# https://www.usgs.gov/faqs/how-do-i-use-a-scale-factor-landsat-level-2-science-products
# https://www.usgs.gov/faqs/what-are-band-designations-landsat-satellites 
# Landsat level-2 products are stored as integers to save disk space, 
# we need to convert them to floating-point numbers before doing any calculations. 
# We need to apply a scaling factor (0.0000275) and add an offset (-0.2) 
# to the original values.
multi_rast <- (multi_rast * 0.0000275) - 0.2
multi_rast
plot(multi_rast)

summary(values(multi_rast))
# Values should be in a range between 0 and 1. 
# This is not the case here, probably due to the presence of clouds, snow cover, etc. 

# Fix negative values for RGB pictures (photos)
photoRast <- multi_rast
photoRast[photoRast < 0] = 0

# Color image can be assembled from the RGB layers
plotRGB(
  photoRast,
  r = 3, # layer 3 as red
  g = 2, # layer 2 as green
  b = 1, # layer 1 as blue
  stretch = "lin", #  stretch the values to increase contrast: "lin" (linear) or "hist" (histogram)
  mar = c(2, 2, 3, 2),
  axes = TRUE,
  main = "Landsat RGB composite")
#
#-------------------------------------------------------------------------------
#
# NDVI calculation and visualization
#
# Declare a function for NDVI calculation using the Red and NIR layers
ndviFN <- function(nir, red){
  (nir - red) / (nir + red)
}
# create a raster (single layer) with NDVI values
ndvi_rast = lapp(multi_rast[[c(4, 3)]], fun = ndviFN)
names(ndvi_rast) <- "NDVI"
summary(values(ndvi_rast))
# Keep NDVI values within theoretical NDVI range
ndvi_rast <- clamp(ndvi_rast,lower = -1,upper = 1,values = TRUE)
# Optional: keep values within theoretical NDVI range
ndvi_rast <- clamp(
  ndvi_rast,
  lower = -1,
  upper = 1,
  values = TRUE
)
# Red-white-green divergent palette
ndvi_pal <- colorRampPalette(
  c("red", "white", "darkgreen")
)(100)

plot(ndvi_rast,col = ndvi_pal,main = "NDVI",axes = TRUE)
#
#-------------------------------------------------------------------------------
#
# Use NDVI for further analysis (vegetation status classification)
#
# Prepare NDVI-based classification and plot
#
# NDVI <= 0        Water, clouds, snow, shadows
# 0 < NDVI <= 0.1  Bare soil, built-up land
# 0.1-0.3          Sparse/stressed vegetation
# 0.3-0.5          Moderate vegetation
# NDVI > 0.5       Dense/healthy vegetation

# terra::classify() is used for classification
# - the rcl matrix is 5x3, used for the classify() function
#   5 rows (5 classes)
#   3 columns (min value in class, max value in class, class ID)


classify_ndvi <- function(ndvi) {
  # Classification matrix:
  # from, to, class_id
  rcl <- matrix(
    c(-Inf, 0.0, 1,
      0.0, 0.1, 2,
      0.1, 0.3, 3,
      0.3, 0.5, 4,
      0.5, Inf, 5),
    ncol = 3,
    byrow = TRUE)
  ndvi_class <- classify(
    ndvi,
    rcl = rcl,
    include.lowest = TRUE, # lowest boundary of the entire classification is included in the first interval
    right = TRUE)  # ( ] i.e. open on the left, closed on the right. 0.1 -> class 2
  ndvi_class <- as.factor(ndvi_class)
  levels(ndvi_class) <- data.frame(
    value = 1:5,
    NDVI_class = c(
      "Water, clouds, snow, shadows",
      "Bare soil, built-up land",
      "Sparse or stressed vegetation",
      "Moderate vegetation",
      "Dense, healthy vegetation"))
  names(ndvi_class) <- "NDVI_class"
  ndvi_class
}

ndvi_cols <- c(
  "Water, clouds, snow, shadows" = "red",
  "Bare soil, built-up land" = "grey80",
  "Sparse or stressed vegetation" = "khaki",
  "Moderate vegetation" = "yellowgreen",
  "Dense, healthy vegetation" = "darkgreen"
)

ndvi_class <- classify_ndvi(ndvi_rast)
ndvi_class

plot(ndvi_class,col = ndvi_cols,main = "NDVI classification")
#
#-------------------------------------------------------------------------------
#
# Convert NDVI classifiction to sf-based polygons
# - typically a computationally expensive operation

ndvi_multi_sf <- ndvi_class |>
  terra::as.polygons(
    aggregate = TRUE,
    values = TRUE,
    na.rm = TRUE
  ) |>
  st_as_sf() |>
  group_by(NDVI_class) |>
  summarise(
    geometry = st_union(geometry),
    .groups = "drop"
  ) |>
  st_cast("MULTIPOLYGON")
# plot the sf object 
ggplot(ndvi_multi_sf) +
  geom_sf(
    aes(fill = NDVI_class),
    colour = NA
  ) +
  scale_fill_manual(
    values = ndvi_cols,
    name = "NDVI class",
    drop = FALSE
  ) +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

#
#-------------------------------------------------------------------------------
#
#
# Landsat 8 data (for NDWI)
#
# Normalized Difference Water Index (NDWI = (green - nir)/(green + nir)) 
#
#  NDWI for open water detection, McFeeters NDWI
#  - This is the common version for mapping surface water bodies, such as rivers,
#    lakes, reservoirs, wetlands, and floods. 
#    It uses the green and near-infrared, NIR bands:
  
#  NDWI = (Green−NIR)/Green+NIR) 
#
#  NDWI values usually range from-1 to +1.

#  Typical wavelengths are approximately:
#  Green: about 510-580 nm
#  NIR: about 770-900 nm
#
#
# NDWI value                Typical interpretation
# 
# strongly negative	        dry land, built-up areas, vegetation, shadows
# around 0	                mixed pixels, wet soil, uncertain boundary areas
# positive values	          likely water or wet surfaces
# high positive values	    open water, often clearer/deeper water
#
#
# In terms of NDWI values:
#
# NDWI < 0                  usually non-water
# NDWI 0-0.3                possible moisture, wet soil, shallow/flooded areas
# NDWI > 0.3                likely open water
# NDWI > 0.5                strong water signal


# Thresholds are not universal (this holds for NDVI, NDWI, etc.). 
# They depend on the sensor, atmospheric correction, season, turbidity, shadows, 
# built-up surfaces, and whether the image contains shallow water or mixed pixels. 
#

# Important note:
# NDWI may confuse open water and snow cover (if raster contains snow cover)
# Shortwave Infrared 1 (SWIR1) can be used for better snow identification
# Normalized difference snow index
# NDSI = (Green - SWIR1)/(Green+SWIR1); SWIR1 1570–1650 nm (approx.)
#
#-------------------------------------------------------------------------------
#
#
# NDWI calculation
#
# In multi_rast:
# Blue  = layer 1
# Green = layer 2
# Red   = layer 3
# NIR   = layer 4
#
# NDWI = (Green - NIR) / (Green + NIR)

# Declare a function for NDWI calculation
ndwiFN <- function(green, nir) {
  (green - nir) / (green + nir)
}

# Select Green first and NIR second, matching the function arguments
ndwi_rast <- lapp(multi_rast[[c(2, 4)]],fun = ndwiFN)
names(ndwi_rast) <- "NDWI"
summary(values(ndwi_rast))

# Keep values within the theoretical NDWI range
ndwi_rast <- clamp(ndwi_rast,lower = -1,upper = 1,values = TRUE)

# Plot the output using a brown-white-blue divergent palette
ndwi_pal <- colorRampPalette(c("sienna4", "white", "deepskyblue", "darkblue"))(100)
plot(ndwi_rast,col = ndwi_pal,main = "NDWI",axes = TRUE)

# Identify and plot water / snow (max. elevation on the map is  2892m)
water = ndwi_rast > 0.3
names(water) <- "water or snow presence"
plot(c(ndwi_rast,water))

















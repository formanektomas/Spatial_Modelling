#-------------------------------------------------------------------------------
#
library(giscoR)
library(mapview)
library(h3jsr)
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
# Goal of this script: Empirical case studies - requires large raster downloads
# 
#
# - Study 1: Built-up analysis in Prague (summer surface heat vs builtup) 
# - Study 2: Air pollution over Europe (NO2 pollution as a function of builtup)
#
#
#-------------------------------------------------------------------------------
#
# Case study 1: Built-up analysis in Prague 
#
#
# Step 1: Download a pre-processed image from Copernicus for the whole Czechia
# - combine rasters using mosaic(), use crop(), mask(), visualize
#
# Website with remote imaging data
# https://human-settlement.emergency.copernicus.eu/download.php
#
# Download and unzip the two GeoTIFF images (save in R working directory)
# https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_BUILT_S_GLOBE_R2023A/GHS_BUILT_S_E2020_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_BUILT_S_E2020_GLOBE_R2023A_54009_100_V1_0_R4_C19.zip
# https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_BUILT_S_GLOBE_R2023A/GHS_BUILT_S_E2020_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_BUILT_S_E2020_GLOBE_R2023A_54009_100_V1_0_R4_C20.zip

# read the data

CE_east <- terra::rast("GHS_BUILT_S_E2020_GLOBE_R2023A_54009_100_V1_0_R4_C20.tif")
CE_west <- terra::rast("GHS_BUILT_S_E2020_GLOBE_R2023A_54009_100_V1_0_R4_C19.tif")
CE <- mosaic(CE_east, CE_west, fun="mean")
CE # EPSG 6326, ESRI:54009
plot(CE)

# scale information
# spatial resolution is 100m x 100m - i.e. 10.000 sq.m. per pixel
# builtup area range: 0 - 10.000 (sq. meters covered by buildings in a given cell/pixel)

# WGS84-based bounding box 
st_as_sf(as.polygons(ext(CE), crs = crs(CE))) |> st_transform(4326) |> mapview()


# Download Czechia and Prague polygons
CZbound <- RCzechia::republika(resolution = "low")
CZcounties <- RCzechia::okresy(resolution = "low")
PrDistricts <- RCzechia::casti() |> 
  filter(NAZ_OBEC == "Praha")
# for simpler calculation (100x100 meter grid), transform CZbound and CZcounties
# to Mollweide 
plot(CZbound)
CZbound <- st_transform(CZbound, crs = "ESRI:54009")
plot(CZbound)
CZcounties <- st_transform(CZcounties, crs = "ESRI:54009")
PrDistricts <- st_transform(PrDistricts, crs = "ESRI:54009")


# Visualization for the whole country
cropped <- terra::crop(CE, CZbound) # crop
CZrast <- terra::mask(cropped, CZbound) # mask to a defined polygon boundary
terra::plot(CZrast, main = "Built-up raster data: Czechia")
plot(CZcounties$geometry, add = T, border=0)

# Given spatial resolution of the raster (100m by 100m), we can focus on a single county/city
Praha <- CZcounties |> 
  filter(KOD_CZNUTS3 == "CZ010")
PrDistricts <- PrDistricts[Praha, op = st_intersects] # keep only districts fully inside CZ010
PrCropped <- terra::crop(CE, Praha) # crop
PrMasked <- terra::mask(PrCropped, Praha) # mask to a defined polygon boundary
terra::plot(PrMasked, main = "Built-up raster data: Czechia")
plot(PrDistricts$geometry, add = T, border=0)

# calculate average buitup area (proportion) at city-district level
PrDistricts <- PrDistricts |>
  mutate(
    builtup_area_m2 = exactextractr::exact_extract(
      x = PrMasked,
      y = PrDistricts,
      fun = function(values, coverage_fraction) {
        sum(values * coverage_fraction, na.rm = TRUE)
      },
      progress = FALSE
    ),
    district_area_m2 = as.numeric(st_area(geometry)),
    builtup_prop = builtup_area_m2 / district_area_m2,
    builtup_pct = 100 * builtup_prop
  )


ggplot(data = PrDistricts, aes(fill = builtup_pct, label = round(builtup_pct))) +
  geom_sf(lwd = 1/3) +
  scale_fill_viridis_c(name = "Build-up area\n(as % of total)\n ") +
  labs(title = "Relative Build-up Area as of 2020",
       caption = " (c) Copernicus") +
  theme(axis.title = element_blank(),
        plot.caption = element_text(face = "italic"))
#
#
# Cast on H3 hexagons (level 7)
PrahaWGS84 <- st_transform(Praha, 4326)
hex_ids <- h3jsr::polygon_to_cells(PrahaWGS84, res = 8)
hex_sf <- h3jsr::cell_to_polygon(hex_ids, simple = F)
hex_sf <- st_transform(hex_sf, st_crs(Praha))
hex_sf <- hex_sf[st_intersects(hex_sf, Praha, sparse = F), ]
hex_sf <- st_intersection(hex_sf, Praha)
plot(hex_sf[,1])
#
hex_sf <- hex_sf |>
  mutate(
    builtup_area_m2 = exactextractr::exact_extract(
      PrMasked,
      hex_sf,
      fun = function(values, coverage_fraction) {
        sum(values * coverage_fraction, na.rm = TRUE)
      },
      progress = FALSE
    ),
    hex_area_m2 = as.numeric(st_area(geometry)),
    builtup_prop = builtup_area_m2 / hex_area_m2,
    builtup_pct = 100 * builtup_prop
  )

ggplot(data = hex_sf, aes(fill = builtup_pct, label = round(builtup_pct))) +
  geom_sf(lwd = 1/3) +
  scale_fill_viridis_c(name = "Build-up area\n(as % of total)\n ") +
  geom_sf(data=PrDistricts,fill=NA,color="white")+
  labs(title = "Relative Build-up Area as of 2020",
       caption = " (c) Copernicus") +
  theme(axis.title = element_blank(),
        plot.caption = element_text(face = "italic"))
#
#
# Note on the exactextractr::exact_extract() function used
# - different from previous script (elevation)
# - elevation and built-up area represent different types of quantities

# Elevation is an intensive variable, measured in metres at each raster cell. 
# Averaging cell values is meaningful.

# Built-up area is an extensive variable, measured in m^2 within each raster cell. 
# Values in hexagon cells should be allocated proportionally across partially covered cells and 
# then summed before dividing by polygon area... to get the relative coverage.
# - using the mean function (see previous script) doesn't necessarily lead to
#   large errors (after proper scaling), but proper methodology is preferable.

#-------------------------------------------------------------------------------
#
# Evaluate the influence of builtup area on surface temperatures
#
# https://landsurfacetemperature.com/ 
#
# 2025 data (summer 2025, average Celsius temp. across several observations)
# - 30m x 30m original resolution resampled to match builtup
# - we are using free data, so we ignore the errors due to time sampling differences
#   2020 data for builtup (slow changes anyway), 2025 data for the surface heat
#
#
surfaceHeat <- rast("data/Prague2025SummerHeat.tif")
surfaceHeat
plot(surfaceHeat)
myRast <- c(PrMasked,surfaceHeat)
names(myRast) <- c("builtup","surfaceHeat")
plot(myRast)

# average surface temperature in a given hexagon
hex_sf$surfaceHeat<- exactextractr::exact_extract(
  x = surfaceHeat, # source
  y = hex_sf, # target
  fun = "mean", # calculation of average air pollution in the area
  weights = "area"
) 
hex_sf
linearModel <- lm(surfaceHeat ~ poly(builtup_pct,2,raw = TRUE), data=hex_sf)
summary(linearModel)
#
# simple visualization
ggplot(hex_sf, aes(x = builtup_pct, y = surfaceHeat)) +
  geom_point(alpha = 0.4) +
  stat_smooth(
    method = "lm",
    formula = y ~ poly(x, 2, raw = TRUE),
    colour = "red",
    linewidth = 1
  ) +
  theme_minimal()
# discussion: variability of observations at x=0, s.e. at x_max
#
#
#
#
#-------------------------------------------------------------------------------
#
# Case study 2: Air pollution over Central Europe

library(ncdf4) # package for netcdf manipulation
rm(list = ls())
#
# website: https://maps.s5p-pal.com/no2/
# download NO2 pollution image data using the icon on lower-right of the map
# change name of the NetCDF to some simple name, eg. "NO2data.nc"
# Note: File has approx. 1GB
#
# example follows from the tutorial here: https://rpubs.com/boyerag/297592
# and https://rpubs.com/GeospatialEcologist/SpatialRaster
#
#-----------------
#
r <- terra::rast("NO2data.nc")
r # way too large to plot on most PCs

# The maps show the concentration of nitrogen dioxide (NO2) in the lowest kilometers 
# of the atmosphere. Nitrogen oxides are mainly produced by human activity and the 
# combustion of (fossil) fuels, such as road traffic, ships, power plants and other 
# industrial facilities. Burning activities and wildfires also contribute significantly 
# to the NO2 concentrations observed. NO2 can have a significant impact on human health, 
# both directly and indirectly through the formation of ozone and small particles.


# crop to Europe general area for plotting 
# (we will plot NO2 pollution for AT,CZ,DE,PL,SK)
EuropeBBOX <- sf::st_bbox(c(xmin = -9, xmax = 32, ymax = 72, ymin = 35), crs = sf::st_crs(4326))
rEurope <- crop(r[[1]], EuropeBBOX)
plot(rEurope)
st_as_sf(as.polygons(ext(rEurope), crs = crs(rEurope))) |> st_transform(4326) |> mapview()
#
# To save the raster for later use, we can keep the netCDF format
writeCDF(rEurope, "NO2Europe.nc", overwrite=TRUE, varname="NO2",
         longname="troposhperic_NO2_density_umol/m2")
# or save as GeoTIFF using the terra::writeRaster() function
# 
#
# Download NUTS0 and NUTS3 polygons for the countries of interest 
NUTS3 <- giscoR::gisco_get_nuts(country=c("Germany","Austria","Czechia","Poland","Slovakia"),nuts_level = 3)
NUTS0 <- giscoR::gisco_get_nuts(country=c("Germany","Austria","Czechia","Poland","Slovakia"),nuts_level = 0)
NUTS0 <- select(NUTS0, NUTS_ID)

# Subset raster data to polygons
NUTS3$NO2 <- exactextractr::exact_extract(
  x = rEurope, # source
  y = NUTS3, # target
  fun = "mean", # calculation of average air pollution in the area
  weights = "area"
) 
# 
summary(NUTS3$NO2)
# 
# Plot the results
ggplot(data = NUTS3) +
  geom_sf(aes(fill = NO2), lwd = 1/3) +
  scale_fill_viridis_c(name = "NO2 air\npollution") +
  labs(title = "NO2 air pollution infomap",
       caption = " (c) Copernicus Service Information") +
  geom_sf(data=NUTS0, color="white", fill=NA, linewidth=1)+
  theme_dark()

# Supervised work
# Gather builtup area data for the countries just plotted
# - download builtup data, use the mosaic() function /you can try builtup volume instead of builtup area/
# - estimate the relationship between NO2 air pollution and builtup (NUTS3, or hexagonal grid)

#-------------------------------------------------------------------------------
#
# Final note:
#
# Data analysis at the raster-level resolution (without aggregation to sf object)
# is possible, this is shown in a separate R script (resampling may be necessary)
#
#-------------------------------------------------------------------------------


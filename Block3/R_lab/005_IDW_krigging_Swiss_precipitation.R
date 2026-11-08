library(dplyr)
library(giscoR)
library(gstat)
library(sf)
library(ggplot2)
library(mapview)
mapview::mapviewOptions(basemaps = "OpenStreetMap")
rm(list = ls())

#-------------------------------------------------------------------------------

#import swiss precipitation data
dat_swiss <- sf::st_read("data/swiss_precipitation.gpkg")

swiss_border <- giscoR::gisco_get_countries(
  country = "CH",
  year = "2020",
  resolution = "01",
  epsg = "4326") |>
  st_transform(st_crs(dat_swiss))
# swiss_border <- st_read("data/swiss_border.gpkg")

# Plot precipitation observations and Swiss boundary
ggplot() +
  geom_sf(data = swiss_border,fill = "grey95",colour = "grey40") +
  geom_sf(data = dat_swiss,aes(colour = Z_1_10MM),size = 2) +
  scale_colour_viridis_c(name = "Mean yearly\nprecipitation [cm]") +
  theme_minimal() +
  theme(axis.title = element_blank())
#
#-------------------------------------------------------------------------------
#
# Estimate the empirical semivariogram for use in krigging
empirical_variogram <- gstat::variogram(Z_1_10MM~1, data = dat_swiss, width = 5000)
estimated_variogram <- fit.variogram(empirical_variogram, vgm('Exp'))
estimated_variogram
plot(empirical_variogram, estimated_variogram)

#-------------------------------------------------------------------------------
#
# Create a 5 x 5 km prediction grid

prediction_grid <- swiss_border %>%
  st_make_grid(cellsize = 5000,what = "polygons",square = TRUE) %>%
  st_as_sf() %>%
  st_filter(swiss_border) %>% # default version of the: st_filter(swiss_border, .predicate = st_intersects)
  mutate(grid_id = row_number())
plot(prediction_grid)

# Clip cells along the national border
prediction_grid <- st_intersection(prediction_grid,st_geometry(swiss_border))
plot(prediction_grid)

# In R, we can predict directly to prediction_grid. For sf polygon geometries,
# gstat::idw() and gstat::krige() use a representative point for each polygon, 
# effectively its centroid.
#-------------------------------------------------------------------------------
#
# Task 1
# Complete the IDW interpolation code
# 

idw_prediction <- gstat::idw()

#-------------------------------------------------------------------------------

plot(idw_prediction["var1.pred"],main = "IDW precipitation prediction")

# Add predictions to the polygon grid
prediction_grid$idw_prediction <- idw_prediction$var1.pred

#
# Plot the IDW prediction

ggplot() +
  geom_sf(data = prediction_grid,aes(fill = idw_prediction),colour = NA) +
  geom_sf(data = swiss_border,fill = NA,colour = "grey20",linewidth = 0.4) +
  geom_sf(data = dat_swiss,shape = 21,fill = "white",colour = "black",size = 1) +
  scale_fill_viridis_c(
    name = "Predicted\nprecipitation [cm]") +
  labs(title = "IDW prediction of mean yearly precipitation") +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank())

#
#-------------------------------------------------------------------------------
#
# Task 2: Ordinary Kriging
# Complete the krige() function

kriging_prediction <- gstat::krige()

#
#-------------------------------------------------------------------------------

plot(kriging_prediction["var1.pred"],main = "Kriging precipitation prediction")
plot(kriging_prediction["var1.var"],main = "Kriging prediction variance")


# Transfer predictions and kriging variances to the polygon grid
prediction_grid$kriging_prediction <- kriging_prediction$var1.pred
prediction_grid$kriging_variance <- kriging_prediction$var1.var

#
# Plot the Kriging prediction
ggplot() +
  geom_sf(data = prediction_grid,aes(fill = kriging_prediction),colour = NA) +
  geom_sf(data = swiss_border,fill = NA,colour = "grey20",linewidth = 0.4) +
  geom_sf(data = dat_swiss,shape = 21,fill = "white",colour = "black",size = 1) +
  scale_fill_viridis_c(name = "Predicted\nprecipitation [cm]") +
  labs(title = "Ordinary Kriging prediction of mean yearly precipitation") +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank())

#
# Plot the Kriging prediction variance
ggplot() +
  geom_sf(data = prediction_grid,aes(fill = kriging_variance),colour = NA) +
  geom_sf(data = swiss_border,fill = NA,colour = "grey20",linewidth = 0.4) +
  geom_sf(data = dat_swiss,shape = 21,fill = "white",colour = "black",size = 1) +
  scale_fill_gradient(low = "white", high="red") +
  labs(title = "Ordinary Kriging prediction variance") +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank())


#-------------------------------------------------------------------------------
#
# Anisotropic ordinary Kriging - an example
#
# anis = c(angle, ratio)
#
# angle = principal direction, clockwise from north
# ratio = minor-range / major-range
#
# Here, 45 degrees and 0.6 are illustrative values.
anisotropic_model <- gstat::vgm(
  psill = estimated_variogram$psill[2],
  model = estimated_variogram$model[2],
  range = estimated_variogram$range[2],
  nugget = estimated_variogram$psill[1],
  anis = c(45, 0.6)
)


anisotropic_prediction <- gstat::krige(
  Z_1_10MM ~ 1,
  locations = dat_swiss,
  newdata = prediction_grid,
  model = anisotropic_model)

prediction_grid$anisotropic_prediction <- anisotropic_prediction$var1.pred

ggplot() +
  geom_sf(data = prediction_grid,aes(fill = anisotropic_prediction),colour = NA) +
  geom_sf(data = swiss_border,fill = NA,colour = "grey20",linewidth = 0.4) +
  geom_sf(data = dat_swiss,shape = 21,fill = "white",colour = "black",size = 1) +
  scale_fill_viridis_c(name = "Predicted\nprecipitation [cm]") +
  labs(title = "Anisotropic ordinary Kriging") +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank())

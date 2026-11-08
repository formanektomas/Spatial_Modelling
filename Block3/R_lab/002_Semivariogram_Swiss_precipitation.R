library(dplyr)
library(gstat)
library(sf)
library(ggplot2)
library(mapview)
mapview::mapviewOptions(basemaps = "OpenStreetMap")
rm(list = ls())

#-------------------------------------------------------------------------------

#import swiss precipitation data
dat_swiss <- sf::st_read("data/swiss_precipitation.gpkg")

#plot precipitation data
ggplot2::ggplot(data = dat_swiss ) +
  ggplot2::geom_sf(aes(color = Z_1_10MM)) +
  scale_color_continuous(name = "Mean Yearly\nprecipitation [cm]")

dat_swiss %>% 
  st_transform(4326) %>% 
  mapview(zcol = "Z_1_10MM",cex = 6,layer.name = "Precipitation")



###Task 1
# Calculate an empirical semivariogram from precipitation data (Z_1_10MM). 
# Use "width = 5000" for variogram calculation. Explain the meaning of the argument.
# Interpret the following 
#
# width (lag size) 
# range
# sill
# spatial autocorrelation 





###Task 2
# Calculate a theoretical semivariogram using the "Exp" functional form
# What is the difference between theoretical and empirical semivariogram?
# Explain parameter values in function vgm in your own words. 




###Task 3
# Use another semivariogram model of your choice to fit the sample semivariogram 
# Can you achieve better fit?





###Task 4
# Directional sample semivariograms (see parameter 'alpha') consider anisotropy 
# in spatial autocorrelation.
# Why does alpha 0 and 180, 270 and 90, 315 and 135, 225 and 45 yield 
#  the same results, respectively?
empirical_variogram.dir <- gstat::variogram(Z_1_10MM~1, data = dat_swiss, 
                                            width = 10000, 
                                            alpha=c(0,45,90,135,180,225,270,315))
plot(empirical_variogram.dir) 
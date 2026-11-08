### A simple example of Inverse Distance Weighting (IDW)
#
# amended from https://rpubs.com/hungle510/202761
library(dplyr)
library(sf)
library(gstat)

rm(list = ls())

#
# Known sampled data with X, Y coordinates
# and corresponding Z values

X <- c(61, 63, 64, 68, 71, 73, 75)
Y <- c(139, 140, 129, 128, 140, 141, 128)
Z <- c(477, 696, 227, 646, 606, 791, 783)

#
# POI: X and Y coordinates are known, but Z is unknown

X0 <- 65
Y0 <- 137

#-------------------------------------------------------------------------------
# Create an sf object for the known sampled data
# No CRS is assigned because these are artificial local coordinates.

knowndt <- data.frame(X, Y, Z) |>
  st_as_sf(coords = c("X", "Y"),crs = NA)

plot(knowndt["Z"],pch = 20,main = "Observed Z values")
knowndt
#
# Create an sf object for the point of interest

POI <- data.frame(X0, Y0) |>
  st_as_sf(coords = c("X0", "Y0"), crs = NA)

#-------------------------------------------------------------------------------
# IDW
# Assume no trend in Z by Z ~ 1
# Set maximum distance to infinity
# idp = inverse-distance weighting power

idwmodel <- gstat::idw(Z ~ 1,locations = knowndt,
                       newdata = POI,maxdist = Inf,idp = 1)
idwmodel

#
# Extract the predicted Z value at the point of interest

Z0 <- idwmodel$var1.pred

Z0

#-------------------------------------------------------------------------------
# IDW can also be calculated over a regular grid

grid <- knowndt |>
  st_bbox() |>
  st_as_sfc() |>
  st_buffer(dist = 2) |> # add some grid space around the observed points
  st_make_grid(n = c(100, 100),what = "centers") |>
  st_as_sf()

plot(st_geometry(grid), pch = 20, cex = 0.15)
plot(st_geometry(knowndt), pch = 21, bg = "red", add = TRUE)
#
# Apply IDW to all grid points

idw.p <- gstat::idw(Z ~ 1,locations = knowndt,
                    newdata = grid,maxdist = Inf,idp = 1)

#
# Plot predicted values

plot(idw.p["var1.pred"],pch = 15,main = "IDW prediction, p = 1")

#
# High distance-decay power generates a Voronoi-like result

idw.Voronoi <- gstat::idw(Z ~ 1,locations = knowndt,
                          newdata = grid,maxdist = Inf,idp = 20)

plot(idw.Voronoi["var1.pred"],pch = 15,main = "IDW prediction, p = 20")

# Calculate Z0 for p = 5

idwmodel_p5 <- gstat::idw(Z ~ 1,locations = knowndt,
                          newdata = POI,maxdist = Inf,idp = 5)

Z0_p5 <- idwmodel_p5$var1.pred
Z0_p5

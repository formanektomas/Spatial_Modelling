#### Impacts - direct and indirect effects (spillovers) 
#### using
#### Spatial lag model
#### Spatial Durbin model 
#
#
#  
# Data, data description & plots
library(sf)
library(ggplot2) 
library(spdep) 
library(spatialreg)
rm(list=ls())
CE_data <- read.csv("data/NUTS2_data.csv")
#
# Data description:
# 
# U_pc_2012              Dependent variable, the general rate of unemployment 
#                        for a NUTS2 region i at time t (2012)
# EUR_HAB_EU_2011        region?s GDP per capita (current EUR prices of 2011) 
#                        expressed as percentage of EU average
# EUR_HAB_EU_2010 
# TechEmp_2012           percentage of employees working in the ?high-tech industry? 
#                        (NACE r.2 code HTC) in a given region and t = 2012
# NUTS_ID                NUTS2 region-identifier (NUTS.2010)
# long, lat              coordinates of regions' centroids
#
#
#      Unemployment model to be estimated:
#
#      U_pc_2012 <- I(EUR_HAB_EU_2011-EUR_HAB_EU_2010) + TechEmp_2012
#
#
#     Distance based neighbors - maximum neighbor distance threshold: 250 km
#
#
#
# Step 1 Prepare spatial objects for subsequent analysis:
#
# (a) coordinates and IDs
CE_sf <- CE_data |>
  st_as_sf(coords = c("long", "lat"),crs = 4326,remove = FALSE)

# .. note that CE_sf contains coordinates: reference POINT elements 
# .. (not POLYGON objects for regions), so we can skip the st_coordinates() step
# 
nb250km <- dnearneigh(CE_sf, d1=0, d2=250, row.names = CE_sf$NUTS_ID) 
summary(nb250km)

# (b) calculate the spatial "weights matrix" (a list object)
W.matrix <- nb2listw(nb250km)
summary(W.matrix)
#
#
#
# Step 2 Spatial lag model estimation, impacts
#
spatial.lag <- lagsarlm(U_pc_2012 ~ I(EUR_HAB_EU_2011-EUR_HAB_EU_2010) + TechEmp_2012, 
                        data=CE_data, W.matrix)
summary(spatial.lag)
# Impacts
?spatialreg::impacts
impacts(spatial.lag, listw= W.matrix)
#
impacts.obj <- impacts(spatial.lag, listw= W.matrix, R=500)
summary(impacts.obj, zstats=T, short=T)
# 
# plot(impacts.obj)
#
#
#
# Step 3 Spatial Durbin model estimation: coefficients & impacts
#
spatial.Durbin <- lagsarlm(U_pc_2012 ~ I(EUR_HAB_EU_2011-EUR_HAB_EU_2010) + TechEmp_2012, 
                        data=CE_data, W.matrix, Durbin = T)
#
summary(spatial.Durbin) # Note the sign on spatial lag of TechEmp
#
# Impacts
impacts(spatial.Durbin, listw= W.matrix)
#
impacts.obj2 <- impacts(spatial.Durbin, listw= W.matrix, R=500)
summary(impacts.obj2, zstats=T, short=T)
# plot(impacts.obj2)
#
#
#
#
#
## Quick Exercise:
## Evaluate stability of the results with respect to
## changing spatial structure
## use kNN method to set-up neighbors, as follows:
?knearneigh # use k = 4
?knn2nb     # use symmetric transformation: sym = T
W.matrix <- nb2listw()   # use nb2listw with the output from knn2bn
#
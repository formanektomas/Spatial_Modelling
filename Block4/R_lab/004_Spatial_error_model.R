#### Spatial error model & KNN-based neighbors
#  
#
#
#  
#   Example of a spatial error model for NUTS2 regions of
#   Austria, Czech Republic, Germany, Hungary, Poland, Slovakia
#
#
# 
# Data, data description & plots

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
#
#
library(spdep)
library(spatialreg)
rm(list=ls())
#
CE_data <- read.csv("data/NUTS2_data.csv")
# (a) coordinates and IDs
CE_sf <- CE_data |>
  st_as_sf(coords = c("long", "lat"),crs = 4326,remove = FALSE)

# .. note that CE_sf contains coordinates: reference POINT elements 
# .. (not POLYGON objects for regions), so we can skip the st_coordinates() step

#
# Step 1 Prepare spatial objects for subsequent analysis:
#
# (a) identify neighbors given tau distance threshold
knns <- knearneigh(CE_sf, k=12) # returns 12 nearest neighbors
nb.knn <- knn2nb(knns, sym = T) # converts object into neighbours list
W.matrix <- nb2listw(nb.knn) # calculates spatial weights
summary(W.matrix)
#
#
#
# # Basic OLS model for subsequent model selection & tests
OLS.1 <- lm(U_pc_2012 ~ I(EUR_HAB_EU_2011-EUR_HAB_EU_2010) + TechEmp_2012, data=CE_data)
summary(OLS.1)
AIC(OLS.1)
BIC(OLS.1)
#
# Specification test (spatial dependence) for KNN-based neigbors, k = 12
# .. k = 12 is not the best setup, but it can be used to demonstrate 
# .. SEM estimation
spdep::lm.RStests(OLS.1, W.matrix, test=c("LMlag", "LMerr", "RLMlag", "RLMerr"))
#
#
#
#
# Spatial error model for KNN, k = 12
?spatialreg::errorsarlm
spatial.err <- errorsarlm(U_pc_2012 ~ I(EUR_HAB_EU_2011-EUR_HAB_EU_2010) + TechEmp_2012, 
                            data=CE_data, W.matrix)
summary(spatial.err)
AIC(spatial.err)
BIC(spatial.err)
#
moran.test(spatial.err$residuals, W.matrix, 
           randomisation=FALSE, alternative="two.sided")
#
# Breusch-Pagan test for heteroskedasticity, generalized for spatial models,
# .. taking rho coefficient into account
bptest.Sarlm(spatial.err)
#
## Assignment 1
## Replicate the Actual vs. fitted values plot as in 
## Spatial lag model (R10_Spatial_lag_model.R)
##
#
#
#
#
## Assignment 
## Use a spatial error model "spatial.err" (line 58) and
## replicate the coefficient stability plots as shown 
## in previous example. Show 
## LogLik, AIC, all beta-coefficients and the spatial error AR term
## Use KNN-based neighbors, for k = 2 to k = 12 
## .. use increments of 1 (add one nearest neighbor at a time)
#
#
#
#
#
#
#
#
#-------------------------------------------------------------------------------
#
#
# Raster algebra - local operations examples
#
# Goal of this script: 
# 
# Rraster-based OLS and GLS modelling, 
# - using the NDVI dataset, and elevation data for the same area (Zion park)
#
# 
#-------------------------------------------------------------------------------
# packages
if (!requireNamespace("spDataLarge", quietly = TRUE)) {
  install.packages("spDataLarge",repos = "https://geocompx.r-universe.dev")
}
library(lmtest)
library(ggplot2)
library(grid)
library(ggplot2)
library(dplyr)
library(tibble)
library(patchwork)
library(terra)
library(spDataLarge)
rm(list=ls())
#
#-------------------------------------------------------------------------------
#
#
# Data source 1: NDVI (this codes is mostly repeated from previous script)
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
plot(multi_rast)
summary(values(multi_rast))
# declare a function for NDVI calculation using the R and NIR layers
ndviFN <- function(nir, red){
  (nir - red) / (nir + red)
}
# Calculate NDVI (local operation)
ndvi_rast <- lapp(multi_rast[[c(4, 3)]], fun = ndviFN)
names(ndvi_rast) <- "NDVI"
# Keep values within theoretical NDVI range
ndvi_rast <- terra::clamp(ndvi_rast,lower = -1,upper = 1,values = TRUE)
# Red-white-green divergent palette
ndvi_pal <- colorRampPalette(
  c("red", "white", "darkgreen")
)(100)
ndvi_breaks <- seq(-1, 1, length.out = 21)
plot(ndvi_rast,col = ndvi_pal,breaks = ndvi_breaks,main = "NDVI",axes = TRUE)
#
#
# Data source 2: elevation data
# Read in the elevation data, harmonize the two datasets (extent, resolution..)
srtm <- rast(system.file("raster/srtm.tif", package = "spDataLarge"))
names(srtm) <- "elevation"
plot(srtm)
# compare the rasters
ndvi_rast
srtm
crs(ndvi_rast) == crs(srtm)
ext(ndvi_rast)
ext(srtm)
res(ndvi_rast)
res(srtm)
res(ndvi_rast) == res(srtm)
nlyr(ndvi_rast)
nlyr(srtm)
#
#
# srtm has lower resolution, so let's harmonize to srtm for cell-to-cell operations.
ndvi_crop <- crop(ndvi_rast, srtm)
ndvi_on_srtm_grid <- resample(ndvi_crop,srtm,method = "bilinear")
plot(ndvi_on_srtm_grid)
ext(ndvi_on_srtm_grid) == ext(srtm)
compareGeom(ndvi_on_srtm_grid, srtm, stopOnError = FALSE)
#
# For the analysis, we combine the two rasters into a two-layer raster
# (other ways to proceed are possible, this one is rather convenient)
model_rast <- c(ndvi_on_srtm_grid, srtm)
model_rast
plot(model_rast)
#
#-------------------------------------------------------------------------------
#
# OLS estimation based on raster data:
# - convert to data.frame before doing OLS (easier output handling)
# - keep data in raster-friendly shape (for plotting fitted/residuals)
#
#
# Option 1: convert to dataframe and estimate by OLS
df_from_raster <- as.data.frame(model_rast,xy = TRUE,na.rm = FALSE)
dim(df_from_raster)
summary(df_from_raster)
ols_ndvi_elev <- lm(NDVI ~ poly(elevation,degree = 2,raw=TRUE), data = df_from_raster)
summary(ols_ndvi_elev)
# here, lots of points are plotted, may take time to render
ggplot(df_from_raster, aes(x = elevation, y = NDVI)) +
  geom_point(alpha = 0.2, size = 0.5) +
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, degree = 2, raw = TRUE),
    color = "red",
    linewidth = 1
  ) +
  labs(
    x = "Elevation",
    y = "NDVI",
    title = "Quadratic OLS fit: NDVI as a function of elevation"
  ) +
  theme_minimal()
#
#
# Option 2: keep the data in raster-friendly shape (for plotting fitted/residuals)
vals <- values(model_rast, mat = TRUE) # values are returned as a matrix
dim(vals)
head(vals)
# estimate the model
ols_ndvi_elev <- lm(vals[, "NDVI"] ~ poly(vals[, "elevation"],degree=2,raw=TRUE))
summary(ols_ndvi_elev) #same OLS results as above...
#
# Calculated and plot fitted values
# Extract coefficients
b0 <- coef(ols_ndvi_elev)[1]
b1 <- coef(ols_ndvi_elev)[2]
b2 <- coef(ols_ndvi_elev)[3]
srtmSq <- srtm * srtm
# the terra::predict() is a cleaner version of this approach... however,
# for raster prediction, it is safer to avoid transformations inside the formula
# Predicted NDVI raster
ndvi_hat <- b0 + b1 * srtm + b2 * srtmSq
names(ndvi_hat) <- "Predicted_NDVI"
plot(ndvi_hat,col = ndvi_pal,breaks = ndvi_breaks,main = "NDVI predicted from elevation")
# compare to the observed raster (repeated from above)
plot(c(ndvi_on_srtm_grid,ndvi_hat),col = ndvi_pal,breaks = ndvi_breaks)
# main features preserved (R^2 = 0.55)
#
# Residual raster: observed minus fitted
ndvi_resid <- ndvi_on_srtm_grid - ndvi_hat
names(ndvi_resid) <- "NDVI_residual"
# plot residuals
ndvi_resid <- ndvi_on_srtm_grid - ndvi_hat
names(ndvi_resid) <- "NDVI-elevation model residuals"
resid_pal <- colorRampPalette(c("blue", "white", "red"))(100)
plot(ndvi_resid,col = resid_pal,
     main = "OLS residuals: observed NDVI - predicted NDVI")
#
#
#-------------------------------------------------------------------------------
#
# GLS: We estimate a logistic function, with dependent variable cast as:
# HealtyVegetaion = 1 if ndvi_on_srtm_grid > 0.5
#
# Create the new binary variable
model_rast[[1]]
HV <- model_rast[[1]] > 0.5
names(HV) <- "healthyVegetation"
HV 
model_rast2 <- c(HV,srtm)
plot(model_rast2)
#
# GLM estimation, keeping the raster friendly format
vals2 <- values(model_rast2, mat = TRUE)
dim(vals2)
summary(vals2)
model2 <- glm(vals2[,"healthyVegetation"] ~ vals2[,"elevation"] + I(vals2[,"elevation"]^2), family=binomial)
summary(model2)
lmtest::lrtest(model2)
# 
# Fitted values (on the scale of the dependent variable), 1/0 predictions
# - again, it is safer to calculate fitted values "manually"
# Extract coefficients
b <- coef(model2)
# Linear predictor on logit scale
eta_rast <- b[1] +
  b[2] * srtm +
  b[3] * srtm^2
# Convert fitted logit to probability
prob_rast <- 1 / (1 + exp(-eta_rast))
names(prob_rast) <- "p_healthyVegetation"
plot(prob_rast,main = "Fitted probability of healthy vegetation")
#
# predict healthy vegetation, based on the 0.5 probability threshold
pred_binary <- terra::ifel(prob_rast > 0.5,1,0) # ifelse() function for rasters
names(pred_binary) <- "Predicted HV, threshold = 0.5"
comb_rast <- c(HV,pred_binary)
# compare to original binary observations
plot(comb_rast)
#
# Evaluate the logit-based model and its fit to data 
#
# Generate a confusion matrix, calculate basic evaluation metrics
eval_vals <- values(c(HV, pred_binary),mat = TRUE)
eval_vals <- eval_vals[complete.cases(eval_vals), ]
conf_mat <- table(
  Observed = eval_vals[, 1],
  Predicted = eval_vals[, 2])
conf_mat
TN <- conf_mat[1,1]
FP <- conf_mat[1,2]
FN <- conf_mat[2,1]
TP <- conf_mat[2,2]
(accuracy <- (TP + TN) / sum(conf_mat))
(sensitivity <- TP / (TP + FN))  # true positive rate
(specificity <- TN / (TN + FP))  # true negative rate
#
# Plot correct and incorrect binary predictions
error_rast <- pred_binary - HV
names(error_rast) <- "prediction_error"
plot(error_rast,col = c("red", "grey85", "blue"),main = "Prediction errors")
#
#
#
# Save output as GeoTIFF
# - this is an example, choose any raster (multi-layer raster)
my_rasters <- c(HV,srtm,pred_binary,error_rast)
writeRaster(my_rasters,"myRasterExport.tif",overwrite = TRUE)
#
# To read the data back to R, use
my_rasters_read <- rast("myRasterExport.tif")
my_rasters_read
plot(my_rasters_read)









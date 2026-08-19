library(sf)
library(RCzechia)
library(terra) 
library(dplyr)
library(exactextractr)
library(ggplot2)
rm(list = ls())
#
#
#-------------------------------------------------------------------------------
#
# Create and combine rasters and raster layers
#
#-------------------------------------------------------------------------------
#
# Create three rasters
x <- rast(xmin=-110, xmax=-60, ymin=40, ymax=70, res=1, vals=1, crs = "local")
y <- rast(xmin=-95, xmax=-45, ymax=60, ymin=30, res=1, vals=2, crs = "local")
z <- rast(xmin=-80, xmax=-30, ymax=50, ymin=20, res=1, vals=3, crs = "local")
x
y
z
#
# basic raster operations: subsetting (retrieving values)
x[1] # [1] is cell ID, layer is not named
x[1,1] # row 1, column 1
y[1,1]
# 
# subset by coordinates
id <- cellFromXY(z, matrix(c(-50,25), ncol =2))
id
z[id]
# subset a whole row from x:
x[10,] # row 10, all columns (50 columns)
#
#
# basic raster operations: plotting
# rasters differ in origin and extent (same CRS, resolution, and size 30x50)
plot(x)
plot(y) # each plot has its own scale, colors not informative across figures
plot(z)
#
# combine the rasters
# sum and other simple operations do not work
x+y # produces an error: extents do not match
# cannot create a raster with two layers: x, y  (same reason)
c(x,y)
#
# we need to create new objects with an extent covering both x and y
# ext()   : Create, get or set a SpatExtent
e_xy <- ext(
  min(xmin(x), xmin(y)),
  max(xmax(x), xmax(y)),
  min(ymin(x), ymin(y)),
  max(ymax(x), ymax(y))
)
# Extend both rasters to the common extent
x_ext <- extend(x, e_xy)
y_ext <- extend(y, e_xy)
# Now addition works
xy_sum <- x_ext + y_ext
plot(xy_sum)
# Newly added cells (in the extended rasters x_ext, y_ext) are filled with NA. 
# Therefore:
# cells where both x and y exist become 1 + 2 = 3,
# cells where only one raster exists become NA,
# cells where neither original (x,y) cells exists are also NA.
# .. this follows the logic of R data handling and additions in R.
#
# Often, we want to treat new cells as zero values, for further analysis:
x_ext0 <- extend(x, e_xy, fill = 0) # fill relates to new (extended) cells
y_ext0 <- extend(y, e_xy, fill = 0)
xy_sum0 <- x_ext0 + y_ext0
plot(xy_sum0)
# 
# If convenient, we can do a final cleanup (e.g. before exporting data)
xy_sum0[xy_sum0 == 0] <- NA # Replace all zero values with NA
plot(xy_sum0)
# 
# Layers: with matching extent(CRS, etc.), multi-layer objects can be created:
multilayer <- c(x_ext0,y_ext0,xy_sum0)
multilayer
plot(multilayer) 
# note how each plot has a separate scale
# - this can be fixed by using comon scale
#
# Common discrete breaks and colours
common_breaks <- c(-0.5, 0.5, 1.5, 2.5, 3.5) # 5 breaks >> 4 sections
common_cols <- c(
  "honeydew2",     # 0
  "palegreen3",    # 1
  "seagreen3",     # 2
  "darkgreen")     # 3
plot(multilayer,breaks = common_breaks, col = common_cols,colNA = "orange")
#
#
#-------------------------------------------------------------------------------
#
# mosaic function
#
#-------------------------------------------------------------------------------
# mosaic()
# - combines the above tasks into one function
# - works on multiple rasters at once
m1 <- mosaic(x, y, z, fun="sum") # fun="mean" is the default setting
plot(m1)
#
#
m2 <- mosaic(x, y, z, fun="blend") # smoothed averages (not sums!)
plot(m2)
#
#
# 
#-------------------------------------------------------------------------------
#
# merge function
#
#-------------------------------------------------------------------------------
# combines rasters while keeping selected overlapping values, and dropping others
# - rasters should the same spatial origin and resolution, 
#   but automatic resampling can be done 
# - In areas where the SpatRasters overlap, values the first in the sequence 
#   of arguments will be retained (unless first=FALSE).
m3 <- merge(x, y, z)
plot(m3)
m4 <- merge(z, y, x)
plot(m4)
m5 <- merge(y, x, z)
plot(m5)

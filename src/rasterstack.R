# Loading necessary libraries
library(sf)
library(raster)
library(rgdal)
library(ggplot2)

# Creating output directory
out <- 'data/outputs_raster_ts'
dir.create(out, showWarnings = FALSE)

# read raster data and create a stack
ndvi_yrly <- Sys.glob('data/r_ndvi_*.tif')
ndvi_yrly

ndvi <- stack(ndvi_yrly)
names(ndvi) <- c(
  'Avg NDVI 2002',
  'Avg NDVI 2009'
  )

# sanity check
plot(ndvi)

# display metadata fo the 1st raster in the ndvi stack
raster(ndvi, 1)

# Set the CRS (coordinate reference system) using EPSG (european petrolieum survey
#   group) code 3338 for an Albers Equal Area projection of Alaska using the 
#   NAD38 datum
crs(ndvi) <- '+init=epsg:3338'

# display metadata for the ndvi stack
raster(ndvi, 0)

#* Whereas read.csv() would load the named file into memory, the raster library
#* handles files like a database where possible. The values can be accessed, to
#* make those plots for example, but are not held in memory. This is the key to
#* working with deep stacks of large rasters.


##############################################################################
########################### Wildfires in ALaska ##############################
#* Many and massive wildfires burned in Alaska and the Yukon between 2001 and 2009.
#* Three large fires that burned during this perios (their locations are in a 
#* shapefile) occurred withi boreal forest areas of central Alaska.

# read polygon shapefile containing the geometry of a wildfire in central Alaska.
scar <- st_read(
  'data/OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399_reclassed',
  crs = 3338
)
# -> Ignore error message

# Plot first raster layer in the ndvi stack and plot and overlay the shapefile
plot(ndvi[[1]])
plot(st_geometry(scar), add = TRUE)

# Define bounding box with st_bbox() to which the plot gets cropped afterwards
burn_bbox <- st_bbox(scar)
ndvi <- crop(ndvi, burn_bbox)

plot(ndvi[[1]], ext = burn_bbox)
plot(st_geometry(scar), add = TRUE)


##* Pixel Change
#* With element-wise substraction one can give a difference raster, where negative
#* values indicate a higher NDVI values for 2002 and 2009
diff_ndvi <- ndvi[[2]] - ndvi[[1]]
names(diff_ndvi) <- 'Difference'

plot(diff_ndvi)
plot(st_geometry(scar), add = TRUE)

#* The histogram shows clearly that change in NDVI within this corner of Alaska
#*   clusters around two modes
hist(diff_ndvi)

#* To 'classify' pixels as potentially affected by wildfire is to threshold the 
#*   difference. Pixels below -0.1 mostly belong to the smaller mode, and may
#*   represent impacts of wildfire.
plot(diff_ndvi < -0.1)
plot(st_geometry(scar), add = TRUE)

#* To center and scale the pixel values requires computation of their mean and 
#*    standard variation. The cellStats function efficiently applies a few common
#*    functions across large rasters, regardless of whether the values are in 
#*    memory on on disk.
diff_ndvi_mean <- cellStats(diff_ndvi, 'mean')
diff_ndvi_sd <- cellStats(diff_ndvi, 'sd')

# With these values one can standardize the NDVI difference
diff_ndvi_stdz <- (diff_ndvi - diff_ndvi_mean) / diff_ndvi_sd
names(diff_ndvi_stdz) <- 'Std. Diff.'

# histogramm of standardized NDVI differences
hist(diff_ndvi_stdz, breaks = 20)

# this does not change the overall result:
plot(diff_ndvi_stdz < -1)
plot(st_geometry(scar), add = TRUE)

##* Raster Time Series
#* Taking a closer look at NDVI using products covering 16-day periods in 2005. 
#*    These images are stored as separate files.
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

# ndvi_16day object contains all the .tif's files from 2005 (23 files)
ndvi_16day <- Sys.glob('data/NDVI_alaska_2005/*.tif')
ndvi <- stack(ndvi_16day)
crs(ndvi) <- '+init=epsg:3338'

# extract the date of each image from its filename
dates <- as.Date(
  sub(
    'alaska_NDVI_', '', names(ndvi)),
  '%Y_%m_%d'
  )

# sanitiy check
dates

names(ndvi) <- format(dates, '%b %d %Y')

# Sanity check
plot(subset(ndvi, 1:2))

###* Raster Bricks
#* A RasterBrick representation of tightly integrated raster layers, such as a 
#* time series of remote sensing data from sequential overflights, has advantages
#* for speed but limitations on flexibility.
#* 
#* A RasterStack is more felxible because it can mix values stored on disk with
#* those in memory. Adding a layer of inmemory values to a RasterBrick causes the
#* entire brick to be loaded into memory, which may not be possible given the 
#* avialable memory.
#* 
#* A RasterBrick can be created from a RasterStack.

ndvi <- crop(ndvi, burn_bbox,
             filename = file.path(out, 'crop_alaska_ndvi.grd'),
             overwrite = TRUE)
#* Crop creates a RasterBrick. In fact, we have been working with a RasterBrick
#* in memory since first using crop.
ndvi

#* The immediate challenge is trying to represent the data in ways we can explore
#* and interpret the characteristics of wildfire visible by remote sensing.
animate(ndvi, pause = 0.7, n = 1)

####* Pixel Time Series
#* Verify that something happend very abpuptly by plotting the time series at 
#* pixles corresponding to locations with dramatic NDVI variation in the layer from 
#* Aug 13, 2005:
idx <- match('Aug.13.2005', names(ndvi))
plot(ndvi[[idx]])

pixel <- click(ndvi[[idx]], cell = TRUE)
pixel # -> press esc to exit the pixel clicker

# Create a scar_pixel dataframe for the burn scar. 
pixel <- c(2813, 3720, 2823, 4195, 9910)
scar_pixel <- data.frame(
  Date = rep(dates, each = length(pixel)),
  cell = rep(pixel, length(dates)),
  Type = 'Burn scar?',
  NDVI = c(ndvi[pixel])
  )

# sanity check
scar_pixel

# Reapeating the selection with click for 'normal looking pixels.
pixel <- c(1710, 4736, 7374, 1957, 750)
normal_pixel <- data.frame(
  Date = rep(dates, each = length(pixel)),
  cell = rep(pixel, length(dates)),
  Type = 'normal',
  NDVI = c(ndvi[pixel])
  )

# Joining the samples together for comparison as time series
pixel <- rbind(normal_pixel, scar_pixel)
ggplot(pixel,
       aes(x = Date, y = NDVI,
           group = cell, col = Type)) +
  geom_line()

####* Zonal Averages
#* One cannot very well analyze the time series for every pixel, so one has
#* to reduce the dimensionality of the data. One way is to summarize it by 'zones'
#* defined by another spatial data source
#* 
#* Currently there is raster data (ndvi) and vector data (scar). In order to 
#* aggregate by polygon, one has to join these two datasets. There are two approaches:
#* 1. Treat the raster data as POINT geometries in a table and perform a spatial
#*    join to the table with POLYGON geometries.
#* 2. Turn the polygons into a raster and summarize the raster masked for each
#*    polygon. 
#*    
#*  In the follwing option 2 is pursued.
#*  
#*  Convert out scar shapefile to raster with the rasterize funtion
# Rasterize polygon shapefile.
scar_geom <- as(st_geometry(scar), 'Spatial')
scar_zone <- rasterize(scar_geom, ndvi,
                       background = 0,
                       filename = 'outputs_raster_ts/scar.grd',
                       overwrite = TRUE)

crs(scar_zone) <- '+init=epsg:3338'
scar_zone <- crop(scar_zone, ndvi)
scar_zone

#* Using as() coerces the scar shapefile as an object of the 'Spatial' class. Then
#* one uses rasterize() to create a Rasterlayer object for the shapefile. Then
#* crop the raster to the same extent as ndvi.
#* 
#* The zonal function calculates fun (here, mean) over each zone.
?zonal
scar_ndvi <- zonal(ndvi, scar_zone, 'mean')

# Rearranging the data for visualization as a time series
zone <- factor(scar_ndvi[, 1])
scar_ndvi <- scar_ndvi[, -1]
scar_zone <- data.frame(
  Date = rep(dates, each = nrow(scar_ndvi)),
  Zone = rep(zone, length(dates)),
  NDVI = c(scar_ndvi))
scar_zone

#* Here we convert the zone category labels to a factor variable, then manually 
#* reshape the data from wide form to long form, with one column for the date, 
#* one for the zone category, and one for the mean NDVI value for that category.
#* 
#* What appears to be the most pronounced signal in this view is an early loss of
#* greenness (green line in following plot) compared to the background NDVI (Zone 0).
ggplot(scar_zone,
       aes(x = Date, y = NDVI,
           col = Zone)) +
  geom_line() 

###* Eliminating time
#* Because changes to NDVI at each pixel follow a similar pattern over the course 
#* of a year, the slices are highly correlated. Consider representing the NDVI 
#* values as a simple matrix with
#*    - each time slice as a variable
#*    - each pixel as an observation
#*    
#* Principal component analysis (PCA), is an image classification technique. It 
#* is a technique for reducing dimensionality of a dataset based on correlation 
#* between variables. The method proceeds either by eigenvalue decomposition of a 
#* covariance matrix or singular-value decomposition of the entire dataset. 
#* Principal components are the distinctive or peculiar features of an image.
#* 
#* To perform PCA on raster data, it’s efficient to use specialized tools that 
#* calculate a covariance matrix without reading in that big data matrix.
ndvi_lS <- layerStats(
  ndvi, 'cov', na.rm = TRUE)
ndvi_mean <- ndvi_lS[['mean']]
ndvi_cov <- ndvi_lS[['covariance']]
ndvi_cor <- cov2cor(ndvi_cov)

#* Here, we use layerStats to calculate the covariance between each pair of layers 
#* (dates) in our NDVI brick, and the mean of each date. layerStats calculates summary 
#* statistics across multiple layers of a RasterStack or RasterBrick, analogous 
#* to how cellStats calculates summary statistics for each layer separately. 
#* After calculating the summary statistics we extract each one to a separate 
#* object and then scale the covariance matrix using cov2cor.
#* 
#* The layerStats function only evaluates standard statistical summaries. The calc 
#* function, however, can apply user-defined functions over or across raster layers.
ndvi_std <- sqrt(diag(ndvi_cov))
ndvi_stdz <- calc(ndvi,
                  function(x) (x - ndvi_mean) / ndvi_std,
                  filename = file.path(out, 'ndvi_stdz.grd'),
                  overwrite = TRUE)

#* Here we find the standard deviation of each NDVI time slice by pulling out the 
#* diagonal of the covariance matrix with diag(), which contains the variance of 
#* each time slice, then taking the square root of the variance. Next, inside 
#* calc(), we define a function inline to standardize a value by subtracting the 
#* mean then dividing by the standard deviation, then apply it to each pixel in 
#* each layer of ndvi. We use filename to write the output directly to disk.
#* 
#* Standardizing the data removes the large seasonal swing, but not the correlation 
#* between “variables,” i.e., between pixels in different time slices. Only the 
#* correlation matters for PCA.
animate(ndvi_stdz, pause = 0.5, n = 1)

#* Now, we use princomp to calculate the principal components of the NDVI correlations, 
#* which is just a 23-by-23 matrix of pairwise correlations between the 23 time 
#* slices. The plot method of the output shows the variance among pixels, not at 
#* each time slice, but on each principal component.
pca <- princomp(covmat = ndvi_cor)
plot(pca)

#* The bar plot of eigenvalues expressed in percentage plotted above gives us the 
#* information retained in each principal component (PC). Notice that the last PCs
#* eigenvalues are small and less significant, this is where dimensionality reduction 
#* comes into play. If we chose to keep the first four relevant components that 
#* retain most of the information then the final data can be reduced to 4 PCs 
#* without loosing much information.
#* 
#* Principal component “loadings” correspond to the weight each time slice 
#* contributes to each component.
npc <- 4
loading <- data.frame(
  Date = rep(dates, npc), 
  PC = factor(
    rep(1:npc, each = length(dates))
  ),
  Loading = c(pca$loadings[, 1:npc])
)

#* Here we manually reshape the output of princomp into a data frame for plotting.
#* 
#* The first principal component is a more-or-less equally weighted combination of 
#* all time slices, like an average.
#* 
#* In contrast, components 2 through 4 show trends over time. PC2 has a broad 
#* downward trend centered around July 2005, while PC3 has a sharp downward trend 
#* centered around April 2005.
ggplot(loading,
       aes(x = Date, y = Loading,
           col = PC)) +
  geom_line()

#* The principal component scores are projections of the NDVI values at each 
#* time point onto the components. Memory limitation may foil a straightforward 
#* attempt at this calculation, but the raster package predict wrapper carries 
#* the princomp predict method through to the time series for each pixel.
pca$center <- pca$scale * 0
ndvi_scores <- predict(
  ndvi_stdz, pca,
  index = 1:npc,
  filename = file.path(out, 'ndvi_scores.grd'),
  overwrite = TRUE)
plot(ndvi_scores)

#* A complication in here is that the pca object does not know how the original 
#* data were centered, because we didn’t give it the original data. The predict 
#* function will behave as if we performed PCA on ndvi_stdz[] if we set the 
#* centering vector to zeros. We specify index = 1:npc to restrict the prediction 
#* to the first four principal component axes. This returns a standardized value 
#* for each pixel on each of the four axes.
#* 
#* The first several principal components account for most of the variance in the 
#* data, so approximate the NDVI time series by “un-projecting” the scores.
#* 
#* The flexible overlay function allows you to pass a custom function for pixel-wise 
#* calculations on one or more of the main raster objects.
ndvi_dev <- overlay(
  ndvi_stdz, ndvi_scores,
  fun = function(x, y) {
    x - y %*% t(pca$loadings[, 1:npc])
  },
  filename = file.path(out, 'ndvi_dev.grd'),
  overwrite = TRUE)
names(ndvi_dev) <- names(ndvi)

#* Here we define a function fun that takes two arguments, x and y. Those are the 
#* first two arguments we pass to overlay. The overlay function then “overlays” 
#* raster brick x, the standardized NDVI time series with 23 layers, on raster 
#* brick y, the first 4 PCA scores for that time series, and applies fun to each 
#* pixel of those two raster bricks. fun performs matrix multiplication of one 
#* pixel of y with the first 4 PCA loadings and then subtracts the result from x, 
#* resulting in the difference between the observed standardized pixel values and 
#* the values predicted by the principal components.
#* 
#* Verify that the deviations just calculated are never very large, (that is, the 
#* PCA predicts the true values fairly well), then try the same approximation using 
#* even fewer principal components.
animate(ndvi_dev, pause = 0.5, n = 1)

#* Based on the time variation in the loadings for principal components 2 and 3, 
#* as we saw in the graph of loadings versus time we made earlier, we might guess 
#* that they correspond to one longer-term and one shorter-term departure from the 
#* seasonal NDVI variation within this extent.
plot(
  ndvi_scores[[2]] < -2 |
    ndvi_scores[[3]] < -2)
plot(st_geometry(scar), add = TRUE)

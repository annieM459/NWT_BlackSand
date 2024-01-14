#### Calculating topographic metrics using a digital elevation model(dem)
## Original code written by Meagan Oldfather
## Modified by Annika Rose-Person 2021-2022
library(raster)
library(sp)
library(spatial)
library(sf)
library(ggplot2)
library(exactextractr)
library(dplyr)

# Read in your Digital Elevation Model
# Annika is using Anderson, R. (2013). Lidar Data, Niwot Ridge Long-Term Ecological Research Site, Colorado. Distributed by OpenTopography. https://doi.org/10.5069/G9FJ2DQM.)
dem = raster("output_be 2.tif")
plot(dem, main="1m DEM for Niwot Ridge") #this works!

#Annie's code for bringing in plot shapefiles
# Read in the shapefile (replace 'your_shapefile.shp' with the path to your shapefile)
shapefile_sf <- st_read("gdf_subplots.shp")

# Transform the shapefile CRS to match the DEM
shapefile_sf_transformed <- st_transform(shapefile_sf, crs(dem))

# Plot to check overlap 
# Convert DEM to dataframe for ggplot
dem_df <- as.data.frame(rasterToPoints(dem), stringsAsFactors = FALSE)
names(dem_df) <- c("lon", "lat", "elevation")

# Basic DEM plot
ggplot() +
  geom_raster(data = dem_df, aes(x = lon, y = lat, fill = elevation)) +
  scale_fill_viridis_c() +
  geom_sf(data = shapefile_sf_transformed, color = "red", size = 0.5, fill = NA) +
  coord_sf() +
  labs(fill = "Elevation (m)") +
  theme_minimal()

#Bummer, only two sites, Audobon & East Knoll are captured in the 1 m DEM. Will 
#need to use the 2 m DEM for the rest
# Using base R
subset_shapefile_sf <- shapefile_sf_transformed[shapefile_sf_transformed$Site %in% c("A", "E"), ]

# Calculate topographic variables
slope = terrain(dem, opt = "slope")
aspect = terrain(dem, opt = "aspect", unit = "degrees")
tpi = terrain(dem, opt = "TPI")

####### function to calculate TPI at a different scale than 30x30m
#####ON TPI VALUES##########
#The variable W in the TPI calculation refers to the width and height of the 
#square window used in the focal operation. For example, if W = 31, it means that 
#the TPI is calculated using a 31x31 cell window. In this window, the central 
#cell's elevation is compared to the average elevation of the surrounding 30x30 
#cells (excluding the central one).
#our shapefile polygons (subplots) are located near the edges of the DEM, 
#larger W values are more likely to extend beyond the DEM boundaries when 
#extracting TPI values resuling in NAs
#Interpretation of TPI values:
#  A TPI value near zero suggests that the terrain at that cell is relatively flat or on a constant slope.
#Positive TPI values indicate that the cell is higher than its surroundings, such as on a hilltop or ridge.
#Negative TPI values suggest that the cell is lower than its surroundings, indicating valleys or depressions.
W<-11 # this determines the size of the focal operation (radius)
f <- matrix(1, nrow=W, ncol=W)
mid<-ceiling(W^2 / 2)
tpi11 <- focal(dem, w=f, fun=function(x, ...) x[mid] - mean(x[-mid]), pad=TRUE, padValue=NA)

W<-31 # this determines the size of the focal operation 
f <- matrix(1, nrow=W, ncol=W)
mid<-ceiling(W^2 / 2)
tpi31 <- focal(dem, w=f, fun=function(x, ...) x[mid] - mean(x[-mid]), pad=TRUE, padValue=NA)

W<-3 #W needs to be >1 and odd
f <- matrix(1, nrow=W, ncol=W)
mid<-ceiling(W^2 / 2)
tpi3 <- focal(dem, w=f, fun=function(x, ...) x[mid] - mean(x[-mid]), pad=TRUE, padValue=NA)

# Extract values for shapefile polygons
slope_vals <- exact_extract(slope, subset_shapefile_sf)
aspect_vals <- exact_extract(aspect, subset_shapefile_sf)
elevation_vals <- exact_extract(dem, subset_shapefile_sf)
tpi11_vals <- exact_extract(tpi11, subset_shapefile_sf)
tpi3_vals<-extract(tpi3, subset_shapefile_sf)
tpi31_vals<-extract(tpi31, subset_shapefile_sf)

# Calculate the average value for each metric
average_slope_vals <- lapply(slope_vals, function(x) mean(x$value, na.rm = TRUE))
average_aspect_vals <- lapply(aspect_vals, function(x) mean(x$value, na.rm = TRUE))
average_elevation_vals <- lapply(elevation_vals, function(x) mean(x$value, na.rm = TRUE))
average_tpi11_vals <- lapply(tpi11_vals, function(x) mean(x$value, na.rm = TRUE))
average_tpi31_vals <- lapply(tpi31_vals, function(x) {
  if (is.list(x) && "value" %in% names(x)) {
    mean(x$value, na.rm = TRUE)
  } else {
    mean(x, na.rm = TRUE)
  }
})
average_tpi3_vals <- lapply(tpi3_vals, function(x) {
  if (is.list(x) && "value" %in% names(x)) {
    mean(x$value, na.rm = TRUE)
  } else {
    mean(x, na.rm = TRUE)
  }
})

# lots of nas for 31 & 11 TPI will use the 2m DEM to calculate TPI for those plots.

# Attach values to the original shapefile data
subset_shapefile_sf$slope <- average_slope_vals
subset_shapefile_sf$aspect <- average_aspect_vals
subset_shapefile_sf$elevation <- average_elevation_vals
subset_shapefile_sf$tp11 <- average_tpi11_vals
subset_shapefile_sf$tp3 <- average_tpi3_vals
subset_shapefile_sf$tp31 <- average_tpi31_vals

# transform aspect
subset_shapefile_sf$aspect<-as.numeric(subset_shapefile_sf$aspect)
subset_shapefile_sf$cosaspect<-cos(subset_shapefile_sf$aspect)

#calculate "radiation" term cos(Apect)*sin(Slope)
subset_shapefile_sf$slope<-as.numeric(subset_shapefile_sf$slope)
subset_shapefile_sf$radiation<-subset_shapefile_sf$cosaspect*sin(subset_shapefile_sf$slope)

#######################################################################
####### Using 2-m resolution DEM for the rest of the plots#############
#######################################################################
# Read in your Digital Elevation Model
# Annika is using Anderson, R. (2013). Lidar Data, Niwot Ridge Long-Term Ecological Research Site, Colorado. Distributed by OpenTopography. https://doi.org/10.5069/G9FJ2DQM.)
dem2 = raster("cropped_dem2.tif")
plot(dem2, main="2 m DEM for Niwot Ridge") #this works!

# Plot to check overlap 
# Convert DEM to dataframe for ggplot
dem_df2 <- as.data.frame(rasterToPoints(dem2), stringsAsFactors = FALSE)
names(dem_df2) <- c("lon", "lat", "elevation")

# Basic DEM plot
ggplot() +
  geom_raster(data = dem_df2, aes(x = lon, y = lat, fill = elevation)) +
  scale_fill_viridis_c() +
  geom_sf(data = shapefile_sf_transformed, color = "red", size = 0.5, fill = NA) +
  coord_sf() +
  labs(fill = "Elevation (m)") +
  theme_minimal()

#complete overlap, great. will use this for the rest of the subplots. will also need
#it for TPI11 & TPI 30 for East Knoll and Audobon sites

# Using base R
subset_shapefile_sf2 <- shapefile_sf_transformed[shapefile_sf_transformed$Site %in% c("L", "T", "S"), ]

# Calculate topographic variables
slope2 = terrain(dem2, opt = "slope")
aspect2 = terrain(dem2, opt = "aspect", unit = "degrees")
tpi2 = terrain(dem2, opt = "TPI")

####### function to calculate TPI at a different scale than 30x30m
W<-11 # this determines the size of the focal operation (radius)
f <- matrix(1, nrow=W, ncol=W)
mid<-ceiling(W^2 / 2)
tpi11_2 <- focal(dem2, w=f, fun=function(x, ...) x[mid] - mean(x[-mid]), pad=TRUE, padValue=NA)

W<-31 # this determines the size of the focal operation 
f <- matrix(1, nrow=W, ncol=W)
mid<-ceiling(W^2 / 2)
tpi31_2 <- focal(dem2, w=f, fun=function(x, ...) x[mid] - mean(x[-mid]), pad=TRUE, padValue=NA)

W<-3 #W needs to be >1 and odd
f <- matrix(1, nrow=W, ncol=W)
mid<-ceiling(W^2 / 2)
tpi3_2 <- focal(dem2, w=f, fun=function(x, ...) x[mid] - mean(x[-mid]), pad=TRUE, padValue=NA)

# Extract values for shapefile polygons
slope_vals2 <- exact_extract(slope2, subset_shapefile_sf2)
aspect_vals2 <- exact_extract(aspect2, subset_shapefile_sf2)
elevation_vals2 <- exact_extract(dem2, subset_shapefile_sf2)
tpi11_vals2 <- exact_extract(tpi11_2, subset_shapefile_sf2)
tpi3_vals2<-extract(tpi3_2, subset_shapefile_sf2)
tpi31_vals2<-extract(tpi31_2, subset_shapefile_sf2)

# Calculate the average value for each metric
average_slope_vals2 <- lapply(slope_vals2, function(x) mean(x$value, na.rm = TRUE))
average_aspect_vals2 <- lapply(aspect_vals2, function(x) mean(x$value, na.rm = TRUE))
average_elevation_vals2 <- lapply(elevation_vals2, function(x) mean(x$value, na.rm = TRUE))
average_tpi11_vals2 <- lapply(tpi11_vals2, function(x) mean(x$value, na.rm = TRUE))
average_tpi31_vals2 <- lapply(tpi31_vals2, function(x) {
  if (is.list(x) && "value" %in% names(x)) {
    mean(x$value, na.rm = TRUE)
  } else {
    mean(x, na.rm = TRUE)
  }
})
average_tpi3_vals2 <- lapply(tpi3_vals2, function(x) {
  if (is.list(x) && "value" %in% names(x)) {
    mean(x$value, na.rm = TRUE)
  } else {
    mean(x, na.rm = TRUE)
  }
})

# no nas here

# Attach values to the original shapefile data
subset_shapefile_sf2$slope <- average_slope_vals2
subset_shapefile_sf2$aspect <- average_aspect_vals2
subset_shapefile_sf2$elevation <- average_elevation_vals2
subset_shapefile_sf2$tp11 <- average_tpi11_vals2
subset_shapefile_sf2$tp3 <- average_tpi3_vals2
subset_shapefile_sf2$tp31 <- average_tpi31_vals2

# transform aspect
subset_shapefile_sf2$aspect<-as.numeric(subset_shapefile_sf2$aspect)
subset_shapefile_sf2$cosaspect<-cos(subset_shapefile_sf2$aspect)

#calculate "radiation" term cos(Apect)*sin(Slope)
subset_shapefile_sf2$slope<-as.numeric(subset_shapefile_sf2$slope)
subset_shapefile_sf2$radiation<-subset_shapefile_sf2$cosaspect*sin(subset_shapefile_sf2$slope)

#######################################################################
####### Getting the missing TPI values ################################
#######################################################################
#TPI 11 is missing from AESS1,AESS5, & TPI31 is missing for ACOS1,ACOS5, AESS1:AESS8
# subset for these sites
subset_shapefile_sf3 <- shapefile_sf_transformed[shapefile_sf_transformed$Original_N 
                                                 %in% c("AESS1","AESS5", "TPI31","ACOS1",
                                                        "ACOS5","AESS2", "AESS3","AESS4",
                                                        "AESS6", "AESS7","AESS8"), ]


# Extract values for shapefile polygons
tpi11_vals3 <- exact_extract(tpi11_2, subset_shapefile_sf3)
tpi31_vals3<-extract(tpi31_2, subset_shapefile_sf3)

# Calculate the average value for each metric
average_tpi11_vals3 <- lapply(tpi11_vals3, function(x) mean(x$value, na.rm = TRUE))
average_tpi31_vals3 <- lapply(tpi31_vals3, function(x) {
  if (is.list(x) && "value" %in% names(x)) {
    mean(x$value, na.rm = TRUE)
  } else {
    mean(x, na.rm = TRUE)
  }
})

# Attach values to the original shapefile data
subset_shapefile_sf2$tp11 <- average_tpi11_vals3
subset_shapefile_sf3$tp31 <- average_tpi31_vals3

#######################################################################
####### now combining everything ######################################
#######################################################################
#first turn it all to dataframes
df1 <- st_drop_geometry(subset_shapefile_sf)
df2 <- st_drop_geometry(subset_shapefile_sf2)
filler <- st_drop_geometry(subset_shapefile_sf3)

# now, need to fill those empty TPI values 
# Perform a left join to merge the data
df_combined <- left_join(df1, filler, by = c("Site", "Plot", "Original_N"), suffix = c("", "_fill"))

# Replace missing values in df1 with values from filler
df_combined$tp11 <- ifelse(is.na(df_combined$tp11), df_combined$tp11_fill, df_combined$tp11)
df_combined$tp31 <- ifelse(is.na(df_combined$tp31), df_combined$tp31_fill, df_combined$tp31)

# Drop the filler columns
df_final <- select(df_combined, -ends_with("_fill"))

# View the final data frame
head(df_final)

# now combine with the other shapefile
final_topography_blacksands_subplots <- rbind(df_final,df2)
# Extracting the first element from each list in columns 6 to 9 to convert from list to numeric
final_topography_blacksands_subplots[, 6:9] <- sapply(final_topography_blacksands_subplots[, 6:9], function(x) sapply(x, `[`, 1))

write.csv(final_topography_blacksands_subplots, "final_topography_blacksands_subplots.csv")

dem_cropped <- crop(dem2, extent(shapefile_sf_transformed))
writeRaster(dem_cropped, "cropped_dem2.tif", format = "GTiff", overwrite = TRUE)


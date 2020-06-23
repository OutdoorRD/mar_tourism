## Exploring and preparing climate data from Columbia
## Updated 5/18/20

## Note 6/5/20: Once again having trouble getting rgdal installed on ubuntu
## In a desire not to destroy all my environments again, I've switched over to
## running this on my mac. Seems to work well there.

## Note 6/9/20: Seems to have resolved itself (I reinstalled raster and then rgdal,
##  full notes of this saga on the drive). Switching back to ubuntu preference

library(tidyverse)
library(sf)
library(raster)

setwd("~/Documents/MAR/GIS/Predictors/Climate/Climate from Columbia/")
# mac
#setwd("~/Documents/Work/Recreation/MAR/ClimateFromColumbia/")

## # maybe what makes more sense is to reproject the points, then create a raster. 
## Update 5/18/20. I tried this, and got it to work. Unfortunately it creates a raster which is even farther from
# the northern part of my AOI. I messed around with some workarounds below, but I don't think it's worth the trouble.
## I am going to go back to the method where I transform the raster object, and deal with the resampling that happens

## Ok, let's abstract the code below (in legacy) 
## to make a function for converting columbia's
##  csvs to a raster
#climatecsv <- precip1

# converting the points into a raster
# and Assigning them crs 32616 at the same time
#climatecsv <- precip
convertClimate <- function(climatecsv){
  wgs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
  longproj = "+proj=utm +zone=16 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  climate2 <- climatecsv %>%
    mutate(x = lon-360) %>%
    rename(y = lat) %>%
    dplyr::select(x, y, Mean, `25thPercentile`, `75thPercentile`)
  climate_rast <- rasterFromXYZ(climate2, crs = wgs)
  climate_32616 <- projectRaster(climate_rast, crs = longproj)
  climate_32616
}

## importing updated baseline precip data
precip <- read_csv("NEX_TOURISM_PRECIP_GRID_MesoAmericanReef_BASELINE_Annual_Corrected.csv")
precipcon <- convertClimate(precip)

# write it out
## NOTE: I didn't actually use this code to write these files out, I did them on my mac and then downloaded them
writeRaster(precipcon, "RastersSGW_WGS/PRECIP_BASELINE_corrected_2.tif", format = "GTiff")
writeRaster(precipcon, "../../Baseline_Inputs/ProjectedForInvest/PRECIP_BASELINE_corrected_2_32616.tif", format = "GTiff")

# mac
#writeRaster(precipcon, "PRECIP_BASELINE_corrected_2.tif", format = "GTiff")
#writeRaster(climate_rast, "comparisons/wgs_precip_test.tif", format = "GTiff")


####### Climate Scenarios ##########
### Using the RCP 85 projections for 2050, specifically the 25th and 75th percentiles. 

# precip
precip50 <- read_csv("NEX_TOURISM_PRECIP_GRID_MesoAmericanReef_RCP85_2050s_Annual_Corrected.csv")
precip50con <- convertClimate(precip50)
precip50con

# hot days
hotdays50 <- read_csv("NEX_TOURISM_MaxTempDaysAbove35C_GRID_MesoAmericanReef_RCP85_2050s_Annual.csv")
hd50con <- convertClimate(hotdays50)

# temp
temp50 <- read_csv("NEX_TOURISM_MEANTEMP_GRID_MesoAmericanReef_RCP85_2050s_Annual.csv")
temp50con <- convertClimate(temp50)

# Write them out
writeRaster(precip50con, "RastersSGW_WGS/Precip_RCP85_2050s_corrected.tif", format = "GTiff")
writeRaster(hd50con, "RastersSGW_WGS/MaxTempDaysAbove35C_RCP85_2050s.tif", format = "GTiff")
writeRaster(temp50con, "RastersSGW_WGS/MeanTemp_RCP85_2050s.tif", format = "GTiff")


#### OLD ##########
##############################################################################
############# Method for going csv -> shp -> 32616 shp -> raster #######
## TODO: I want to make the extent of my empty raster a bit bigger than the extent of the points them selves
#extent(climate_sf_32)
#test <- raster(nrows = 37, ncols = 29, xmn = -92.25, xmx = -85.25, ymn = 13, ymx = 22)
#empty_rast <- projectExtent(test, crs = longproj)
#test2 <- raster(nrows = 37, ncols = 29, xmn = -700)

convertClimate <- function(climatecsv){
  climate2 <- climatecsv %>% mutate(lon = lon - 360)
  climate_sf <- st_as_sf(climate2, coords = c("lon", "lat"), crs = 4326)
  # reproject
  climate_sf_32 <- st_transform(climate_sf, crs = 32616)
  # make an empty raster, and get the points into it
  empty_rast <- raster(climate_sf_32, nrows = 37, ncols = 29)
  climate_rast <- rasterize(as(climate_sf_32, "Spatial"), empty_rast, field = c("Mean", "X25thPercentile", "X75thPercentile"))
  climate_rast
}


# let's try it
precip <- read_csv("NEX_TOURISM_PRECIP_GRID_MesoAmericanReef_BASELINE_Annual_Corrected.csv")
precip_raster <- convertClimate(precip)
writeRaster(precip_raster, "RastersSGW_WGS/Precip_Baseline_corrected.tif", format = "GTiff")


############### Legacy ##################
# matthew agrees
# Let's give it a try, and see how it compares to stacie's tif. If still different, then 
# it's probably not worth me redoing my other layers
precip1 <- read_csv("NEX_TOURISM_PRECIP_GRID_MesoAmericanReef_BASELINE_Annual.csv")

# ok. First turn it into a shapefile
precip2 <- precip1 %>% mutate(lon = lon-360)
precip_sf <- st_as_sf(precip2, coords = c("lon", "lat"), crs = 4326)
#ggplot(precip_sf) + geom_sf()
# project 
precip_sf_32 <- st_transform(precip_sf, crs = 32616)
#ggplot(precip_sf_32) + geom_sf()

# ok. And now make a raster from this

empty_rast <- raster(precip_sf_32, nrows = 37, ncols = 29)
precip_rast <- rasterize(as(precip_sf_32, "Spatial"), empty_rast)
plot(precip_rast)

# excellent. let's write it out
writeRaster(precip_rast, "comparisons/precip_original_pts_trans_then_raster_tech_test.tif", format = "GTiff")
# ok. That does look better. Still not identical to stacie's, but closer.
# The question remains whether I should re-do the old layers or not. Maybe yes, since it's really
# just a couple layers to rerun

############### Legacy ###################


# ruh roh. Some differences in how these are looking spatially

################# Legacy pre-function above ##################

## importing baseline data
heat <- read_csv("NEX_TOURISM_MaxTempDaysAbove35C_GRID_MesoAmericanReef_BASELINE_Annual.csv")
temp <- read_csv("NEX_TOURISM_MEANTEMP_GRID_MesoAmericanReef_BASELINE_Annual.csv")
precip <- read_csv("NEX_TOURISM_PRECIP_GRID_MesoAmericanReef_BASELINE_Annual.csv")
daysrain <- read_csv("NEX_TOURISM_PrecipDaysAbove1mm_GRID_MesoAmericanReef_BASELINE_Annual.csv")

# importing aoi
aoi <- read_sf("../../../AOI/AOI_v3/Tourism_AOI_v3_4326.shp")
t_bounding <- read_sf("../../../AOI/AOI_v3/Tourism_v3_Bounding_Box.shp")

# looking at them
ggplot(heat) +
  geom_point(aes(x = lon2, y = lat, col = Mean), size = 6)
summary(heat)

ggplot() +
  geom_sf(data = t_bounding) +
  geom_sf(data = aoi) +
  geom_point(data = heat, aes(x = lon2, y = lat, col = Mean), size = 2)

# hmm. It doesn't really look like they used the aoi I provided

# converting the points into a raster
# and Assigning them crs 32616 at the same time
wgs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
longproj = "+proj=utm +zone=16 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

heat2 <- heat %>%
  mutate(x = lon-360) %>%
  rename(y = lat) %>%
  dplyr::select(x, y, Mean, `25thPercentile`, `75thPercentile`)
heat_rast <- rasterFromXYZ(heat2, crs = wgs)
heat_32616 <- projectRaster(heat_rast, crs = longproj)

## temperature
temp2 <- temp %>%
  mutate(x = lon-360) %>%
  rename(y = lat) %>%
  dplyr::select(x, y, Mean, `25thPercentile`, `75thPercentile`)
temp_rast <- rasterFromXYZ(temp2, crs = wgs)
temp_32616 <- projectRaster(temp_rast, crs = longproj)

## precip
precip2 <- precip %>%
  mutate(x = lon-360) %>%
  rename(y = lat) %>%
  dplyr::select(x, y, Mean, `25thPercentile`, `75thPercentile`)
precip_rast <- rasterFromXYZ(precip2, crs = wgs)
precip_32616 <- projectRaster(precip_rast, crs = longproj)

## daysrain
daysrain2 <- daysrain %>%
  mutate(x = lon-360) %>%
  rename(y = lat) %>%
  dplyr::select(x, y, Mean, `25thPercentile`, `75thPercentile`)
daysrain_rast <- rasterFromXYZ(daysrain2, crs = wgs)
daysrain_32616 <- projectRaster(daysrain_rast, crs = longproj)

# write it out
writeRaster(heat_rast, "RastersSGW_WGS/MaxTempDaysAbove35C_BASELINE.tif", format = "GTiff")
writeRaster(temp_rast, "RastersSGW_WGS/MEANTEMP_BASELINE.tif", format = "GTiff")
writeRaster(precip_rast, "RastersSGW_WGS/PRECIP_BASELINE.tif", format = "GTiff")
writeRaster(daysrain_rast, "RastersSGW_WGS/PrecipDaysAbove1mm_BASELINE.tif", format = "GTiff")

writeRaster(heat_32616, "../../Baseline_Inputs/ProjectedForInvest/MaxTempDaysAbove35C_BASELINE_32616.tif", format = "GTiff")
writeRaster(temp_32616, "../../Baseline_Inputs/ProjectedForInvest/MEANTEMP_BASELINE_32616.tif", format = "GTiff")
writeRaster(precip_32616, "../../Baseline_Inputs/ProjectedForInvest/PRECIP_BASELINE_32616.tif", format = "GTiff")
writeRaster(daysrain_32616, "../../Baseline_Inputs/ProjectedForInvest/PrecipDaysAbove1mm_BASELINE_32616.tif", format = "GTiff")

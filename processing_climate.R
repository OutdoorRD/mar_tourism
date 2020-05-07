## Exploring and preparing climate data from Columbia

library(tidyverse)
library(sf)
library(raster)

setwd("~/Documents/MAR/GIS/Predictors/Climate/Climate from Columbia/")

## Ok, let's abstract the code below (in legacy) to make a function for converting columbia's
##  csvs to a raster


# converting the points into a raster
# and Assigning them crs 32616 at the same time
climatecsv <- precip
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
writeRaster(precipcon, "RastersSGW_WGS/PRECIP_BASELINE_corrected.tif", format = "GTiff")
writeRaster(precipcon, "../../Baseline_Inputs/ProjectedForInvest/PRECIP_BASELINE_corrected_32616.tif", format = "GTiff")


writeRaster(climate_rast, "comparisons/wgs_precip_test.tif", format = "GTiff")

# ruh roh. Some differences in how these are looking spatially
# Can I get away without doing the projectraster step?
preciptest <- rasterFromXYZ(climate2, crs = longproj)
preciptest
# no. 

# maybe what makes more sense is to reproject the points, then create a raster
# matthew agrees
precip


############### Legacy ###################
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

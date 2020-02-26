## Exploring and preparing climate data from Columbia

library(tidyverse)
library(sf)
library(raster)

setwd("~/Documents/MAR/GIS/Predictors/Climate/Climate from Columbia/")

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

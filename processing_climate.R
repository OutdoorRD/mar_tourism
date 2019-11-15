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

heat2 <- heat %>%
  mutate(x = lon-360) %>%
  rename(y = lat) %>%
  dplyr::select(x, y, Mean, `25thPercentile`, `75thPercentile`)
heat_rast <- rasterFromXYZ(heat2)

## temperature
temp2 <- temp %>%
  mutate(x = lon-360) %>%
  rename(y = lat) %>%
  dplyr::select(x, y, Mean, `25thPercentile`, `75thPercentile`)
temp_rast <- rasterFromXYZ(temp2)

## precip
precip2 <- precip %>%
  mutate(x = lon-360) %>%
  rename(y = lat) %>%
  dplyr::select(x, y, Mean, `25thPercentile`, `75thPercentile`)
precip_rast <- rasterFromXYZ(precip2)

## daysrain
daysrain2 <- daysrain %>%
  mutate(x = lon-360) %>%
  rename(y = lat) %>%
  dplyr::select(x, y, Mean, `25thPercentile`, `75thPercentile`)
daysrain_rast <- rasterFromXYZ(daysrain2)

# write it out
writeRaster(heat_rast, "RastersSGW/MaxTempDaysAbove35C_BASELINE.tif", format = "GTiff")
writeRaster(temp_rast, "RastersSGW/MEANTEMP_BASELINE.tif", format = "GTiff")
writeRaster(precip_rast, "RastersSGW/PRECIP_BASELINE.tif", format = "GTiff")
writeRaster(daysrain_rast, "RastersSGW/PrecipDaysAbove1mm_BASELINE.tif", format = "GTiff")



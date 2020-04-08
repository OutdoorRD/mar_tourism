#### 
#### Creating a squared temperature raster for invest run,
####  And creating individual rasters for each country to fake dummy variables
####
#### 4/8/20

library(raster)
library(tidyverse)
library(sf)

setwd("~/Documents/MAR/GIS/Predictors/Baseline_Inputs/")

# read temp
temp <- raster("ProjectedForInvest/MEANTEMP_BASELINE_32616.tif")
crs(temp)

temp
temp2 <- temp^2
temp2

# write it out
#writeRaster(temp2, "ProjectedForInvest/MEANTEMP_squared_BASELINE_32616.tif")


### Also need to create 4 rasters for country variable (1s in country, 0s out)

# read in bounding box, and convert to 32616
bbox <- st_read("../../AOI/AOI_v3/Tourism_v3_Bounding_Box.shp")
bbox32 <- st_transform(bbox, crs = 32616)

# read in countries, and convert to 32616
countries <- st_read("../../Downscaling/intermediate/countriesplusoffshore.shp")
countries32 <- st_transform(countries, crs = 32616)

# create raster based on bbox32
empty <- raster(bbox32, resolution = 250)

## Let's start with Mexico
mexico <- countries32 %>% filter(CNTRY_NAME == "Mexico")

mex_raster <- rasterize(mexico, empty, background = 0)
plot(mex_raster)

belize <- countries32 %>% filter(CNTRY_NAME == "Belize")
bz_raster <- rasterize(belize, empty, background = 0)

guat <- countries32 %>% filter(CNTRY_NAME == "Guatemala")
gt_raster <- rasterize(guat, empty, background = 0)

hon <- countries32 %>% filter(CNTRY_NAME == "Honduras")
hn_raster <- rasterize(hon, empty, background = 0)

plot(bz_raster)
plot(gt_raster)
plot(hn_raster)

# write it out
writeRaster(mex_raster, "InVEST_inputs_20200408/mexico_raster.tif")
writeRaster(bz_raster, "InVEST_inputs_20200408/belize_raster.tif")
writeRaster(gt_raster, "InVEST_inputs_20200408/guatemala_raster.tif")
writeRaster(hn_raster, "InVEST_inputs_20200408/honduras_raster.tif")

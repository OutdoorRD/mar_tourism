####
### Preparing climate projections for use in the visitation model
### Equivalent to "preparing_predictors.R", but only for climate
###
### Requires: aoi, climate tifs (output of processing_climate.R)
### Outputs: Future_Climate_RCP85_2050s_and_Current.csv, which has 25th and 75th projections by pid,
###          and, as of 7/1, baseline climate as well
### 6/23/20
###
### Updated 7/1/20 to include current climate as well, in a single csv

library(tidyverse)
library(sf)
library(lwgeom)
library(raster)

# setwd
setwd("~/Documents/MAR")

# read in gridded aoi
aoi <- read_sf("GIS/AOI/AOI_v3/Intersected/T_AOI_intersected_pid_32616_no_slivers.shp")
all(st_is_valid(aoi))
crs(aoi)

# convert to point shapefile
aoi_centers <- st_centroid(aoi)

# Read in future climate data
# Note that choosing "band = 2" pulls in the 25th percentile data. Band = 3 corresponds to 75th percentile
precip25 <- raster("GIS/Predictors/Climate/Climate from Columbia/RastersSGW_WGS/Precip_RCP85_2050s_corrected.tif", 
                   band = 2)
precip75 <- raster("GIS/Predictors/Climate/Climate from Columbia/RastersSGW_WGS/Precip_RCP85_2050s_corrected.tif", 
                   band = 3)
hotdays25 <- raster("GIS/Predictors/Climate/Climate from Columbia/RastersSGW_WGS/MaxTempDaysAbove35C_RCP85_2050s.tif",
                    band = 2)
hotdays75 <- raster("GIS/Predictors/Climate/Climate from Columbia/RastersSGW_WGS/MaxTempDaysAbove35C_RCP85_2050s.tif",
                    band = 3)
temp25 <- raster("GIS/Predictors/Climate/Climate from Columbia/RastersSGW_WGS/MeanTemp_RCP85_2050s.tif",
                 band = 2)
temp75 <- raster("GIS/Predictors/Climate/Climate from Columbia/RastersSGW_WGS/MeanTemp_RCP85_2050s.tif",
                 band = 3)

# Read in current climate data
# Band 1 = mean
precip0 <- raster("GIS/Predictors/Climate/Climate from Columbia/RastersSGW_WGS/PRECIP_BASELINE_corrected_2.tif",
                  band = 1)
hotdays0 <- raster("GIS/Predictors/Climate/Climate from Columbia/RastersSGW_WGS/MaxTempDaysAbove35C_BASELINE.tif",
                   band = 1)
temp0 <- raster("GIS/Predictors/Climate/Climate from Columbia/RastersSGW_WGS/MEANTEMP_BASELINE.tif",
                band = 1)


climate_points <- aoi_centers %>%
  mutate(precip0 = raster::extract(precip0, aoi_centers),
         precip25 = raster::extract(precip25, aoi_centers),
         precip75 = raster::extract(precip75, aoi_centers),
         hotdays0 = raster::extract(hotdays0, aoi_centers),
         hotdays25 = raster::extract(hotdays25, aoi_centers),
         hotdays75 = raster::extract(hotdays75, aoi_centers),
         temp0 = raster::extract(temp0, aoi_centers),
         temp25 = raster::extract(temp25, aoi_centers),
         temp75 = raster::extract(temp75, aoi_centers)) %>%
  dplyr::select(pid, ends_with("5"), ends_with("0"))

#plot(climate_points)

climate_pids <- climate_points %>%
  st_set_geometry(NULL)

# write it out
write_csv(climate_pids, "mar_tourism/Data/Future_Climate_RCP85_2050s_and_Current.csv")

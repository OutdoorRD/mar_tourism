## Preparing predictors for MAR Visitation model ###

library(sf)
library(lwgeom)
library(tidyverse)
library(ggplot2)
library(raster)

setwd("~/Documents/MAR/GIS/")

# Import gridded AOI
aoi <- st_read("AOI/AOI_v2/5km/twitter/aoi_5km_wgs84_pid.shp")
#plot(aoi)
aoi_valid <- st_make_valid(aoi)

## Starting with habitat layers from coastal vulnerability

## Corals
# import corals
coral <- st_read("Predictors/CV_inputs_draft2_v2/Habitat_CV_36/Corals_1.shp")
coral_valid <- st_make_valid(coral)

# create an indicator for whether or not corals occur in each grid cell
# check the projection
st_crs(aoi_valid)
st_crs(coral_valid)

# reproject aoi to match coral (and hopefully other CV layers)
aoi_nad83 <- st_transform(aoi_valid, crs = st_crs(coral_valid))

# take intersection of grid and corals
corals_int <- st_intersection(aoi_nad83, coral_valid)

# extracting pids which contain corals
coral_pids <- corals_int$pid

predictors <- aoi_nad83 %>% 
  mutate(corals = if_else(pid %in% coral_pids, 1, 0))

ggplot(predictors) + geom_sf(aes(fill = corals))

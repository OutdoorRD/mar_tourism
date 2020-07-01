####
#### Ok, let's make a really clean preparing_predictors script that only deals with my final selected preds
#### The goal here is to be able to feed in new data, and then run all the intersections to output a csv
### that can be read into the model predict script (or, to refit the model, as necessary)

# Those predictors are:
# Beach - presence/absence (line)
# Coral - % coverage (poly)
# Mangrove - p/a (poly)
# Wildlife - p/a (poly)
# Forest - p/a (poly)
# Temp - raster centroid
# HotDays - raster centroid
# Precip - raster centroid
# Ruins - p/a (point)
# Ports/Air - distance (point)
# Roads - p/a (line)
# Development - p/a (poly)
# Country - categorical

library(tidyverse)
library(sf)
library(lwgeom)
library(raster)

# setwd
setwd("~/Documents/MAR")

# Start by reading in the AOI

# read in gridded aoi
aoi <- read_sf("GIS/AOI/AOI_v3/Intersected/T_AOI_intersected_pid_32616_no_slivers.shp")
all(st_is_valid(aoi))
crs(aoi)
# pull out country, and otherwise drop others
aoi <- aoi %>%
  dplyr::select(pid, country = Country, MPA = Name_short)

## Now. Can I create an abstracted function for calculation presence/absence?
# Let's try with beach
beach <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/beach_from_geomorph_MAR_v4_shift_BZ_MX_32616.shp")

# Create a function that will return presence/absence of a predictor in each pid grid cell
PresAbsFunc <- function(predictor, aoi){
  predName <- substitute(predictor)            # get the name of the predictor you entered as an argument
  pred_int <- st_intersection(aoi, predictor)
  pred_pid <- pred_int$pid
  aoi_preds <- aoi %>%
    mutate("{predName}" := if_else(pid %in% pred_pid, 1, 0))
}

beach_pid <- PresAbsFunc(beach, aoi)
beach_pid
ggplot(beach_pid) + geom_sf(aes(fill = beach))
# awesome. this works for lines at least!

# Let's see if it works for a non-line P/A
wildlife <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/wildlife3_32616.shp")

wildlife_pid <- PresAbsFunc(wildlife, aoi)
wildlife_pid
ggplot(wildlife_pid) + geom_sf(aes(fill = wildlife))
# fabulous. works for polys

# And for points?
ruins <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/archaeological_sites_combined_32616.shp")
ruins_pid <- PresAbsFunc(ruins, aoi)
ggplot(ruins_pid) + geom_sf(aes(fill = ruins))
# excellent

## Todo: loop this all and combine the outputs
## Todo: Also need to write functions for distance, percent coverage, and raster centroid

############## Function for percent coverage (for coral) ########
### NOTE: The workflow below is incredibly slow. 
### TODO: Test whether it would be quicker to develop a workflow based on a coral raster
###       And check whether I should create that raster from the AOI, or use one of Jade's outputs
# read in my current best coral footprint (note that this is likely to change, as of 6/30)
coral <- read_sf("GIS/Predictors/Coral/coral_reef_aoi_pol_native.shp")
crs(coral)
# convert to wgs 84 
coral_32 <- st_transform(coral, crs = 32616)
plot(coral_32)
coral_valid <- st_make_valid(coral_32)
# let's make the transfomration and make valid requirements to happen before this step.
# I think I already ahve a script set up to do it

# calculate the area of each cell in the aoi
aoi$area <- unclass(st_area(aoi))

# intersect
coral_int <- st_intersection(aoi, coral_valid) # slow (>30 min)
# check this out, and make sure it doesn't already include the "area" from above

# calculate the area of each intersected polygon (only includes coral)
coral_int$area <- unclass(st_area(coral_int))
coral_areas <- coral_int %>%
  st_set_geometry(NULL) %>%
  group_by(pid) %>%
  summarise(coral_area = sum(area)) ## 

aoi_preds_coral <- aoi %>%
  left_join(coral_areas, by = "pid") %>%
  mutate(prop_coral = if_else(is.na(coral_area), 0, coral_area/area))





##################
### before, for land:
##### % land in polygon
land <- read_sf("landmass_adjusted_clipped_shift_BZ_MX_32616.shp")

## I want to create a predictor which shows what proportion of the grid cell is land

aoi$area <- unclass(st_area(aoi))

crs(land)

land_int <- st_intersection(aoi, land)
#plot(land_int)

# calculate the area of each intersected polygon (only includes land)
land_int$area <- unclass(st_area(land_int))

land_areas <- land_int %>% 
  st_set_geometry(NULL) %>%
  group_by(pid) %>% 
  summarise(land_area = sum(area))

# bind on to all pids
aoi_land <- aoi %>%
  left_join(land_areas, by = "pid") %>%
  mutate(prop_land = if_else(is.na(land_area), 0, land_area/area))


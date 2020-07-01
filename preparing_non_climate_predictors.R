####
#### Ok, let's make a really clean preparing_predictors script that only deals with my final selected preds
#### The goal here is to be able to feed in new data, and then run all the intersections to output a csv
### that can be read into the model predict script (or, to refit the model, as necessary)
###
### Requires: gridded aoi, and shapefiles for all non-climate predictors (lined out below)
### Outputs: NonClimatePredictors_dddd.csv and .geojson
### 7/1/20

# Those predictors are:
# Beach - presence/absence (line)
# Coral - % coverage (poly)
# Mangrove - p/a (poly)
# Wildlife - p/a (poly)
# Forest - p/a (poly)
# Temp - raster centroid (handled in preparing_climate_predictors.R)
# HotDays - raster centroid (handled in preparing_climate_predictors.R)
# Precip - raster centroid (handled in preparing_climate_predictors.R)
# Ruins - p/a (point)
# Ports/Air - distance (point)
# Roads - p/a (line)
# Development - p/a (poly)
# Country - categorical

library(tidyverse)
library(sf)
library(lwgeom)
library(raster)

dddd <- gsub("-", "", Sys.Date())

# Create a function that will return presence/absence of a predictor in each pid grid cell
# Other types don't have custom functions, since I'm only running them for single predictors (min dist, % coverage)
PresAbsFunc <- function(predictor, aoi = aoi){
  predName <- substitute(predictor)            # get the name of the predictor you entered as an argument
  pred_int <- st_intersection(aoi, predictor)
  pred_pid <- pred_int$pid
  aoi_preds <- aoi %>%
    st_set_geometry(NULL) %>%
    dplyr::select(pid) %>%
    mutate("{predName}" := if_else(pid %in% pred_pid, 1, 0))
}


# setwd
setwd("~/Documents/MAR")

# Start by reading in the files
aoi <- read_sf("GIS/AOI/AOI_v3/Intersected/T_AOI_intersected_pid_32616_no_slivers.shp")

# Presence/absence variables
beach <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/beach_from_geomorph_MAR_v4_shift_BZ_MX_32616.shp")
mangrove <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/Mangroves_2_32616.shp")
wildlife <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/wildlife3_32616.shp")
forest <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/Forest_4_32616.shp")
ruins <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/archaeological_sites_combined_32616.shp")
roads <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/roads_MAR_clip_32616.shp")
develop <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/lulc_developed_national_baseline_32616.shp")

# Coral
coral <- read_sf("GIS/Predictors/Coral/coral_reef_aoi.shp")

# Ports / Air
ports_air <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/ports_air_32616.shp")

# Work with AOI
#all(st_is_valid(aoi))
#crs(aoi)
# pull out country, and otherwise drop others
aoi <- aoi %>%
  dplyr::select(pid, country = Country, MPA = Name_short)

# Run Presence/Absence
# note that I tried to use my PresAbsFunc in a loop, but it breaks the nice "{predName}" functionality

beach_pid <- PresAbsFunc(beach, aoi)
mangrove_pid <- PresAbsFunc(mangrove, aoi) # slow
wildlife_pid <- PresAbsFunc(wildlife, aoi)
forest_pid <- PresAbsFunc(forest, aoi) # slowish
ruins_pid <- PresAbsFunc(ruins, aoi)
roads_pid <- PresAbsFunc(roads, aoi)
develop_pid <- PresAbsFunc(develop, aoi)


# bind them all together
preds_pa <- aoi %>% 
  left_join(beach_pid) %>%
  left_join(mangrove_pid) %>%
  left_join(wildlife_pid) %>%
  left_join(forest_pid) %>%
  left_join(ruins_pid) %>%
  left_join(roads_pid) %>%
  left_join(develop_pid)



############## Function for percent coverage (for coral) ########
### NOTE: The workflow below is fairly slow (better with the simplified poly from jade) 
### TODO: Test whether it would be quicker to develop a workflow based on a coral raster
###       And check whether I should create that raster from the AOI, or use one of Jade's outputs
# read in my current best coral footprint (note that this is likely to change, as of 6/30)
crs(coral)
# convert to wgs 84 
coral_32 <- st_transform(coral, crs = 32616)
#plot(coral_32)
st_is_valid(coral_32)
coral_valid <- st_make_valid(coral_32)
# let's make the transfomration and make valid requirements to happen before this step.
# I think I already ahve a script set up to do it

# calculate the area of each cell in the aoi
preds_pa$cellarea <- unclass(st_area(preds_pa))

# intersect
coral_int <- st_intersection(aoi, coral_valid) # slow. Started 3:22, finished at 3:32

# calculate the area of each intersected polygon (only includes coral)
coral_int$area <- unclass(st_area(coral_int)) 
coral_areas <- coral_int %>%
  st_set_geometry(NULL) %>%
  group_by(pid) %>%
  summarise(coral_area = sum(area)) ## 

preds_coral <- preds_pa %>%
  left_join(coral_areas, by = "pid") %>%
  mutate(prop_coral = if_else(is.na(coral_area), 0, coral_area/cellarea)) %>%
  dplyr::select(-coral_area)

preds_coral
#ggplot(preds_coral) +
 # geom_sf(aes(fill = prop_coral))

########### Distance to nearest port/air ######

# calculate distance to nearest airport/port
pa_dists <- st_distance(preds_coral, ports_air)
pa_min_dist <- apply(pa_dists, 1, min)

preds_coral$pa_min_dist <- pa_min_dist
#ggplot(preds_coral) + geom_sf(aes(fill = pa_min_dist))
preds_coral
#####

## Write it out
write_sf(preds_coral, paste0("mar_tourism/Data/NonClimatePredictors_", dddd, ".geojson"))
write_csv(preds_coral %>% st_set_geometry(NULL), paste0("mar_tourism/Data/NonClimatePredictors_", dddd, ".csv"))

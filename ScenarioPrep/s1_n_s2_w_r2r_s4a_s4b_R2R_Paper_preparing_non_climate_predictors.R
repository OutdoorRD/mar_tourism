###
###
### Created 1/4/22 to create the nonClimatePredictors for the R2R_Paper scenarios
### Forked from s1_2_Restore_Protect_Forest_preparing_non_climate_predictors
### Needs to account for both coral and forest change
###
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

# specify s1 or s2, or s4a or s4b
anum <- "s4a"
aname <- "local_planning"

# Start by reading in the files
aoi <- read_sf("ModelRuns/baseline_20200715/T_AOI_v4_5k_32616_pid.shp")

# read in variables that have already been intersected in QGIS (forest & coral)
### THIS IS THE DIFFERENCE FOR THESE SCENARIOS
forest <- read_sf(paste0("R2R_Paper/Scenarios/Re_Analysis_2022_Aug/", anum, "_", aname, "/T_AOI_", anum, "_forest.geojson"))
coral <- read_sf(paste0("R2R_Paper/Scenarios/Re_Analysis_2022_Aug/", anum, "_", aname, "/T_AOI_", anum, "_coral.geojson"))

# Presence/absence variables
beach <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/beach_from_geomorph_MAR_v4_shift_BZ_MX_32616.shp")
wildlife <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/wildlife3_32616.shp")
ruins <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/archaeological_sites_combined_32616.shp")
roads <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/roads_MAR_clip_32616.shp")
develop <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/lulc_developed_national_baseline_32616.shp")

# Ports / Air
ports_air <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/ports_air_32616.shp")

# Mangrove (for % coverage)
mangrove <- read_sf("GIS/Predictors/Baseline_Inputs/ProjectedForInvestValid/MARwide_mangrove_baseline_healthy_footprint_Jan2021_32616.shp")

multiplier <- pull(read.csv("mar_tourism/Data/areaTo30mCellMultiplier.csv"))

# Work with AOI
#all(st_is_valid(aoi))
#crs(aoi)
# reordering and renaming columns in aoi
aoi <- aoi %>%
  dplyr::select(pid, country = CNTRY_NAME, name_2, cellarea = area)

# cleanup coral and forest
# note that baseline_mean is the mean of the binary coral raster w/in the hex. So, it ends up being proportion of the 
# hex covered by the footprint ## WRONG. This only works if the raster covers the entire extent of the AOI, which mine don't
## Especially wrong for coral
# Also grabbing MPA info from this layer

## Note: below was used to create the multiplier that is now read in above
# figuring out a conversion between area and number of raster cells, to calculate the true proportions
# Note that this is approximate, since the number of raster cells per hex changes as you go south due to 
# my projection. But, close enough
#mult_calcs <- forest %>%
 # filter(!is.na(baseline_c)) %>%
#  arrange(desc(baseline_c)) %>%
 # mutate(multiplier = baseline_c/area) %>%
  #filter(baseline_c >= 23990) # this looks like the smallest number of cells in a full hex
#summary(mult_calcs)
#mult_calcs
#multiplier <- median(mult_calcs$multiplier)
# write it out
#write.csv(multiplier, "mar_tourism/Data/areaTo30mCellMultiplier.csv", row.names = FALSE)

# ok. So area * 0.00111 should give me the number of raster cells that could fit in that hex
# And then the sum of cells that are forest, divided by the number of cells in the area, gives 
# me proportion coverage
forest_pid <- forest %>%
  st_set_geometry(NULL) %>%
  mutate(forest_prop = fors_sum / (area*multiplier),
         forest_prop = if_else(forest_prop > 1, 1, forest_prop)) %>%
  dplyr::select(pid, forest_prop)
# Note that there are a bunch of NAs here. I assume they're from MX... let's keep an eye on them, but
# generally hope that they get taken care of later (also, we don't need MX IPMs for these strategies)

#ggplot(forest_pid) + geom_sf(aes(fill = forest_prop))

# do the same for coral. Removed c1 and c2 since we don't have other climates in this paper
coral
coral_pid <- coral %>% 
  st_set_geometry(NULL) %>%
  mutate(coral_prop = if_else(is.na(c0_sum), 0, c0_sum / (area*multiplier))) %>%
  dplyr::select(pid, coral_prop)
#ggplot(coral_pid) + geom_sf(aes(fill = coral_prop))

# TODO: write this out as a geojson in the future and standardize naming

# Run Presence/Absence
# note that I tried to use my PresAbsFunc in a loop, but it breaks the nice "{predName}" functionality

beach_pid <- PresAbsFunc(beach, aoi)
wildlife_pid <- PresAbsFunc(wildlife, aoi)
ruins_pid <- PresAbsFunc(ruins, aoi)
roads_pid <- PresAbsFunc(roads, aoi)
develop_pid <- PresAbsFunc(develop, aoi)


# bind them all together
predictors <- aoi %>% 
  left_join(coral_pid) %>%
  left_join(forest_pid) %>%
  left_join(beach_pid) %>%
  left_join(wildlife_pid) %>%
  left_join(ruins_pid) %>%
  left_join(roads_pid) %>%
  left_join(develop_pid)



########### Distance to nearest port/air #####

# calculate distance to nearest airport/port
pa_dists <- st_distance(predictors, ports_air)
pa_min_dist <- apply(pa_dists, 1, min)

predictors$pa_min_dist <- pa_min_dist
#ggplot(preds_coral) + geom_sf(aes(fill = pa_min_dist))
predictors
#####


########### Proportion coverage of Mangrove #####

# intersect
mangrove_int <- st_intersection(aoi, mangrove) 

# calculate the area of each intersected polygon (only includes mangrove)
mangrove_int$area <- unclass(st_area(mangrove_int)) 
mangrove_areas <- mangrove_int %>%
  st_set_geometry(NULL) %>%
  group_by(pid) %>%
  summarise(mangrove_area = sum(area)) ## 

preds_mangrove <- predictors %>%
  left_join(mangrove_areas, by = "pid") %>%
  mutate(mangrove_prop = if_else(is.na(mangrove_area), 0, mangrove_area/cellarea)) %>%
  dplyr::select(-mangrove_area)

preds_mangrove


## Write it out
write_sf(preds_mangrove, paste0("mar_tourism/Data/Scenarios/", anum, "_", aname, "_NonClimatePredictors_", dddd, ".geojson"), delete_dsn = TRUE)
write_csv(preds_mangrove %>% st_set_geometry(NULL), paste0("mar_tourism/Data/Scenarios/", anum, "_", aname, "_NonClimatePredictors_", dddd, ".csv"))








#################################### OLD #############################
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

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
predictor <- beach

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
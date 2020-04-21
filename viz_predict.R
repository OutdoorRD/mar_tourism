#####
### Using the Viz Model to predict Viz under different scenarios
###
### First pass test using the corals layer that Jess created
### 4/20/20 SGW

### RUN viz_model_clean.R first ##################

library(tidyverse)
library(sf)
library(lwgeom)

## Need to: read in AOI, new data layer, run intersection to get model inputs,
##    scale to match data inputs (relevant for non-binary predictors),
##    create "newdata" df that holds all else equal,
##    predict tourism, write out shapefile

## Additionally, for the ROOT change in service rasters, need to run two scenarios 
##    and subtract them from each other

setwd("~/Documents/MAR/ROOT/Restore_coral_test/")

baselines <- read_csv("../../GIS/Predictors/Baseline_Inputs/CombinedPredictors_20200320.csv")

coral <- read_sf("corals_all_activity_mask_removed.shp")
aoi <- read_sf("../../GIS/AOI/AOI_v3/Intersected/T_AOI_intersected_pid_32616_no_slivers.shp")

#coral
## intersect corals with aoi
# reproject and make valid
#corals_32 <- st_transform(corals_full, crs = 32616)
#corals_valid <- st_make_valid(corals_32)

corals_int <- st_intersection(aoi, coral)

corals_pids <- corals_int$pid

# make a newdata frame
pred2 <- pred_scaled %>%
  mutate(corals_new = if_else(pid %in% corals_pids, 1, 0))

ggplot(pred2) +
  geom_point(aes(x = jitter(corals), y = jitter(corals_new)))

#### Create shapefile of fitted values (corals_full equivalent)
## Not rerunning the model, since I'm assuming that corals_full is what I built it with
modeled <- pred_scaled %>%
  select(pid, vis_log) 

modeled$fitted <- vis_model$fitted.values
modeled
# really?
test <- aoi %>%
  left_join(modeled, by = "pid")

ggplot(test) +
  geom_sf(aes(fill = vis_log))

ggplot(test) +
  geom_sf(aes(fill = fitted))
# ok

### Todo: predict at new values of coral (build newdata frame first)
### Do expm1 transformation to get them into visitors
### Subtract one from the other to get a change map
### Join both sets of predictions & change to the spatial aoi
### Write out three different shape files (possibly only for belize)

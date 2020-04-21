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
  select(pid, vis_log, est_vis) 

modeled$fitted <- vis_model$fitted.values
modeled$fitted_vis <- expm1(modeled$fitted)
modeled

# build newdata frame
newdata <- pred2 %>%
  select(Country, 
         corals = corals_new, 
         mangroves, 
         beach,
         forest, 
         temp, 
         dayshot, 
         precip, 
         wildlife,
         pa_min_dist, 
         ruins, 
         prop_dev, 
         roads_min_dist)

preds <- predict(vis_model, newdata = newdata)
modeled$preds <- preds
modeled$preds_vis <- expm1(preds)
modeled

# calculate difference
modeled <- modeled %>%
  mutate(diff_vis = round(fitted_vis - preds_vis, 2))

# join to spatial 
modeled_sp <- aoi %>%
  select(pid, NAME) %>%
  left_join(modeled, by = "pid")

# subset to belize
modeled_bz <- modeled_sp %>% filter(NAME == "Belize")

# check it out
ggplot(modeled_bz) +
  geom_sf(aes(fill = diff_vis))

# clean up for writing
modeled_tw <- modeled_bz %>%
  select(pid, visWcoral = fitted_vis, visWOcoral = preds_vis, vis_diff = diff_vis)

# write it out
st_write(modeled_tw, "coral_test_tourism.shp")

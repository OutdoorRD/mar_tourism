###
### Creating estimates of visitation under the climate scenarios
### w/o any adaptation strategies
### Forked from the top of climate_predictions_exploring_future_vis_multipliers.R on 5/7/21

library(tidyverse)
library(sf)
library(lwgeom)

## Need to: read in model object, existing predictors,
##    AOI, new data layer, run intersection to get model inputs,
##    scale to match data inputs (relevant for non-binary predictors),
##    create "newdata" df that holds all else equal,
##    predict tourism, write out shapefile

## Additionally, for the ROOT change in service rasters, need to run two scenarios 
##    and subtract them from each other

setwd("~/Documents/MAR/")

baselines <- read_csv("mar_tourism/Data/Predictors_Baseline.csv")
climate_vals <- read_csv("mar_tourism/Data/Future_Climate_RCP85_2050s_and_Current.csv")
viz_model_raw <- read_rds("mar_tourism/Models/viz_model_raw.rds")
aoi <- read_sf("ModelRuns/baseline_20200715/T_AOI_v4_5k_32616_pid.shp")

## Getting oriented in naming scheme
climate <- "clim2" #Baseline climate = clim0; 25th perc = clim1; 75th perc = clim2
ipm <- "climate" #"imp_06"
aname <- "noact" #"prot_corl"

#### Joining climate onto baselines
base_climate <- baselines %>%
  left_join(climate_vals, by = "pid")

clim_post <- case_when(climate == "clim0" ~ "0",
                       climate == "clim1" ~ "25",
                       climate == "clim2" ~ "75")

# create a special code for coral
clim_post_c <- case_when(climate == "clim0" ~ "",
                         climate == "clim1" ~ "25",
                         climate == "clim2" ~ "75")


#### Create shapefile of fitted values (current conditions)
## Not rerunning the model
modeled <- baselines %>%
  dplyr::select(pid, vis_log, est_vis) 

modeled$fitted <- viz_model_raw$fitted.values
modeled$fitted_vis_current <- exp(modeled$fitted)
modeled

# build newdata frame

newdata <- base_climate %>%
  dplyr::select(country, 
                coral_prop = paste0("coral_prop", clim_post_c), 
                mangrove_prop, 
                beach,
                forest_prop, 
                temp = paste0("temp", clim_post), 
                hotdays = paste0("hotdays", clim_post), 
                precip = paste0("precip", clim_post), 
                wildlife,
                pa_min_dist, 
                ruins, 
                develop, 
                roads,
                cellarea)

preds <- predict(viz_model_raw, newdata = newdata)
modeled$preds <- preds
modeled$preds_vis <- exp(preds)
modeled

# and then, what if future vis are *2.67?
modeled$preds_vis_future <- modeled$preds_vis*2.67
modeled

# calculate difference
modeled <- modeled %>%
  mutate(diff_vis = round(preds_vis_future - fitted_vis_current, 2), # need to be careful about this line and what it means for each scenario
          perc_change = 100*(preds_vis_future - fitted_vis_current) / fitted_vis_current) # need to be careful about this line and what it means for each scenario

modeled
summary(modeled)
# join to spatial 
modeled_sp <- aoi %>%
  dplyr::select(pid, CNTRY_NAME) %>%
  left_join(modeled, by = "pid")

modeled_sp

ggplot(modeled_sp)+# %>% filter(diff_vis != 0)) +
  geom_sf(aes(fill = diff_vis), size = .1)

ggplot(modeled_sp)+# %>% filter(diff_vis != 0)) +
  geom_sf(aes(fill = perc_change), size = .1)

# write this out (for the whole MAR region)
#st_write(modeled_sp, paste0("Scenarios/Climate/MARwide_", ipm, "_", aname, "_rec_", climate, ".geojson"), delete_dsn = TRUE)

# Make a simpler version to write out to share with partners
modeled_simple <- modeled_sp %>%
  select(pid, CNTRY_NAME, diff_vis, perc_change)

st_write(modeled_simple, paste0("Scenarios/Climate/MARwide_visitation_change_", climate, ".shp"), delete_layer = TRUE)

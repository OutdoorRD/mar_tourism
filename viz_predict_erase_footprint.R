#####
### Using the Viz Model to predict Viz under different scenarios
### This script is optimized for "protect" scenarios that simply remove the entire 
###  footprint of the habitat (mangroves, coral, others?).
### IS NOT APPROPRIATE for protect forest, which doesn't simply remove the entire footprint

## Code has been used to create the mangrove scnearios, and should be ready for protect coral

### Forked from viz_predict.R on 7/27/20
### Updated 12/22/20 to create Protect Coral and Protect Mangrove scenarios

### Updated 2/19/21 to recreate Protect Mangrove scenarios

library(tidyverse)
library(sf)
library(lwgeom)
library(raster)
library(fasterize)

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

anum <- "05" #05 = prot_mang, 06 = prot_corl
aname <- "prot_mang"
climate <- "clim0" #Baseline climate = clim0; 25th perc = clim1; 75th perc = clim2
climshort <- "c0"

ipm <- paste0("ipm_", anum)

#newNonClimate <- read_csv(paste0("ROOT/ProtectForest/NonClimatePredictors_", country, "_", climshort, ".csv"))

clim_post <- case_when(climate == "clim0" ~ "0",
                       climate == "clim1" ~ "25",
                       climate == "clim2" ~ "75")

# create a special code for coral
clim_post_c <- case_when(climate == "clim0" ~ "",
                       climate == "clim1" ~ "25",
                       climate == "clim2" ~ "75")

modeled <- baselines %>%
  dplyr::select(pid, vis_log, est_vis) %>%
  arrange(pid)

#### Joining climate onto baselines
base_climate_all <- baselines %>%
  left_join(climate_vals, by = "pid")

### Create tibble of baseline values in new climate
base_clim_data <- base_climate_all %>%
  dplyr::select(pid,
                country, 
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
                cellarea) %>%
    arrange(pid)

preds_base_clim <- predict(viz_model_raw, newdata = base_clim_data)
modeled$preds_base_clim <- preds_base_clim
modeled$preds_base_clim_vis <- exp(preds_base_clim) # Note: I'm doing exp and not expm1 to avoid negative visitors 
## TODO: examine the assumption above a bit more closely!
modeled

# Create tibble of SCENARIO data in new climate
## Don't need to read in any new data, since we can just overwrite all the mangrove
## pids with zero

#scen_climate_all <- newNonClimate %>%
 # left_join(climate_vals, by = "pid")
#scen_climate_all

if(aname == "prot_mang"){
  scen_clim_data <- base_clim_data %>%
    mutate(mangrove_prop = 0)
}else if (aname == "prot_corl"){
  scen_clim_data <- base_clim_data %>%
    mutate(coral_prop = 0)
} else {
  print("Error: Check that the protect scenario is correct and coded: scen_clim_data not created")
}
summary(scen_clim_data)

#scen_clim_data <- scen_climate_all %>%
 # dplyr::select(country, 
  #              coral_prop, #= paste0("coral_prop", clim_post_c), 
   #             mangrove, 
    #            beach,
     #           forest_prop, 
      #          temp = paste0("temp", clim_post), 
       #         hotdays = paste0("hotdays", clim_post), 
        #        precip = paste0("precip", clim_post), 
         #       wildlife,
          #      pa_min_dist, 
           #     ruins, 
            #    develop, 
             #   roads,
              #  cellarea)

preds_scen_clim <- predict(viz_model_raw, newdata = scen_clim_data)
modeled$preds_scen_clim <- preds_scen_clim
modeled$preds_scen_clim_vis <- exp(preds_scen_clim)
modeled

# Apply future vis multiplier of 2.67
modeled$preds_base_clim_vis_future <- modeled$preds_base_clim_vis * 2.67
modeled$preds_scen_clim_vis_future <- modeled$preds_scen_clim_vis * 2.67
modeled

# calculate difference
modeled <- modeled %>%
  mutate(diff_vis = round(preds_base_clim_vis_future - preds_scen_clim_vis_future, 2),
         perc_change = 100*(preds_base_clim_vis_future - preds_scen_clim_vis_future) / preds_scen_clim_vis_future) # need to be careful about this line and what it means for each scenario
modeled
summary(modeled)
# join to spatial 
modeled_sp <- aoi %>%
  dplyr::select(pid, CNTRY_NAME) %>%
  left_join(modeled, by = "pid")

ggplot(modeled_sp %>% filter(diff_vis != 0)) +
  geom_sf(aes(fill = diff_vis), size = .1)

ggplot(modeled_sp %>% filter(diff_vis != 0)) +
  geom_sf(aes(fill = perc_change), size = .1)

# write this out (for the whole MAR region)
st_write(modeled_sp, paste0("ROOT/", anum, "_", aname, "/IPMs/MARwide_", ipm, "_", aname, "_rec_", climate, ".geojson"), delete_dsn = TRUE)


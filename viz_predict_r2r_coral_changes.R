#####
### Forked from viz_predict_erase_footrpint on 2/23/21
### This script is for predicting changes in vis due to R2R coral changes!
### Updated 8/11/22 to recreate effect of s1, s2, and s3 coral changes on 
### tourism, after Jade discovered an updated and much improved coral baseline 
### layer. 
###
### NOTE: The healthy coral "baseline" is being modified in this 
### script to use the newly discovered layer. So, this script:
###    - Reads in the true baseline
###    - And Modifies the coral_prop values acccording to T_AOI_r2r_baseline_coral.geojson
###    - Uses that as the new "baseline" for these scenarios
###    - Compares it's predictions to those from the scenario baseline layer

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

anumcor <- "s1" # these are equivalent to the anums, but with "s" and just used for the r2r scenarios
climate <- "clim0" #Baseline climate = clim0; 25th perc = clim1; 75th perc = clim2
climshort <- "c0"
#
#ipm <- paste0("ipm_", anum)

## reading in alternative coral data (and doing minor processing on it to make it useable)
# This piece comes from preparing_non_climate_predictors code
newcoral <- read_sf("ROOT/R2R_Scenarios/Re_Analysis_2022_Aug/T_AOI_R2R_clim0.geojson")
multiplier <- pull(read.csv("mar_tourism/Data/areaTo30mCellMultiplier.csv"))


## Reading in alternative s0 coral values & processing
coral_alt_s0 <- read_sf("R2R_Paper/Scenarios/Re_Analysis_2022_Aug/BaselineCoral/T_AOI_r2r_baseline_coral.geojson")
coral_s0_pid <- coral_alt_s0 %>% 
  st_set_geometry(NULL) %>%
  mutate(coral_prop = if_else(is.na(c0_sum), 0, c0_sum / (area*multiplier))) %>%
  dplyr::select(pid, coral_prop)

newcoral
newcoral_pid <- newcoral %>% 
  st_set_geometry(NULL) %>%
  left_join(aoi %>% dplyr::select(pid, area)) %>%
  mutate(s1_coral_prop = if_else(is.na(s1_c0sum), 0, s1_c0sum / (area*multiplier)),
         s2_coral_prop = if_else(is.na(s2_c0sum), 0, s2_c0sum / (area*multiplier)),
         s3_coral_prop = if_else(is.na(s3_c0sum), 0, s3_c0sum / (area*multiplier))#,
         #s4_coral_prop = if_else(is.na(s4_c0sum), 0, s4_c0sum / (area*multiplier))
         ) %>%
  dplyr::select(pid, ends_with("coral_prop"))


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
  dplyr::select(-coral_prop) %>%
  left_join(coral_s0_pid, by = "pid") %>%
  left_join(climate_vals, by = "pid")
base_climate_all

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
## Don't need to read in any new data, just replacing the coral_prop values depending on the scneario above

#scen_climate_all <- newNonClimate %>%
 # left_join(climate_vals, by = "pid")
#scen_climate_all

scen_clim_data <- base_clim_data %>%
  dplyr::select(-coral_prop) %>%
  left_join(newcoral_pid %>% dplyr::select(pid, paste0(anumcor, "_coral_prop"))) %>%
  rename(coral_prop = paste0(anumcor, "_coral_prop"))

summary(scen_clim_data)

identical(base_clim_data$coral_prop, scen_clim_data$coral_prop)

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
## NOTE: All r2r scenarios (rest/prot forest, sust ag/palm) are scenario - baseline 
modeled <- modeled %>%
  mutate(diff_vis = round(preds_scen_clim_vis_future - preds_base_clim_vis_future, 2),
         perc_change = 100*(preds_scen_clim_vis_future - preds_base_clim_vis_future) / preds_base_clim_vis_future) # need to be careful about this line and what it means for each scenario
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
st_write(modeled_sp, paste0("ROOT/R2R_Scenarios/Re_Analysis_2022_Aug/IPMs/mar_r2r_", anumcor, "_rec_", climate, ".geojson"), delete_dsn = TRUE)

# do a tiny bit of clean up to write out as a cleaner shapefile for Jade
modeled_simple <- modeled_sp %>%
  dplyr::select(pid, diff_vis, perc_change)

st_write(modeled_simple, paste0("ROOT/R2R_Scenarios/Re_Analysis_2022_Aug/IPMs/mar_r2r_", anumcor, "_rec_", climate, ".shp"), delete_layer = TRUE)

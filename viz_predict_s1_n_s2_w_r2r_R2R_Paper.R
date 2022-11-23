#####
### R2R paper viz_predict script
### Calculates the effect of implementing watershed strategies in optimal places, both with and without the r2r effects, for jade's paper
### Updated 1/7/22 to read in new forest baseline, and to calculate effects of coral and forest change separately
### Updated 11/18/22 to also work for the s4a and s4b scenarios - reflecting local (country-level) vs
###   regional (MAR-level) planning priorities.
### Forked from viz_predict_s1_s2_restore_protect_forest.R on 1/4/22
###
### NOTE: BOTH The coastal forest "baseline" and the healthy coral "baseline" are being modified in this 
### script. The actual baseline LUC that I used to fit the model
### was updated  to clip out the offshore LUC values, while the coral baseline was updated to remove 
### nonexistent coral around BZ city. But, both happened too late for me to incorporate it throughout 
### (as well as only happening for BZ, HN, and GT, not MX). So... instead, this script:
###    - Reads in the true baseline
###    - Modifies the forest_prop values according to T_AOI_mar_s0_forest.geojson
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
# s1_n_r2r = without_watershed_weighting, s2_w_r2r = with_watershed_weighting
# s4a = local_planning, s4b = regional_planning
anum <- "s4b" 
aname <- "regional_planning"
climate <- "clim0" #Baseline climate = clim0; 25th perc = clim1; 75th perc = clim2
climshort <- "c0"

ipm <- paste0("ipm_", anum)

if(anum == "s1_n_r2r"){
  newNonClimate <- read_csv("mar_tourism/Data/Scenarios/s1_n_r2r_without_watershed_weighting_NonClimatePredictors_20220105.csv")
}else if(anum == "s2_w_r2r"){
  newNonClimate <- read_csv("mar_tourism/Data/Scenarios/s2_w_r2r_with_watershed_weighting_NonClimatePredictors_20220105.csv")  
}else if(anum == "s4a"){
  newNonClimate <- read_csv("mar_tourism/Data/Scenarios/s4a_local_planning_NonClimatePredictors_20221123.csv")
}else if(anum == "s4b"){
  newNonClimate <- read_csv("mar_tourism/Data/Scenarios/s4b_regional_planning_NonClimatePredictors_20221123.csv")
}


## Reading in alternative s0 forest values, and doing some processing on them
## This part forked from `prepping_non_climate_predictors`
forest_alt_s0 <- read_sf("R2R_Paper/Scenarios/BaselineForest/T_AOI_r2r_baseline_forest.geojson")
multiplier <- pull(read.csv("mar_tourism/Data/areaTo30mCellMultiplier.csv"))

forest_s0_pid <- forest_alt_s0 %>%
  st_set_geometry(NULL) %>%
  mutate(forest_prop = fors_sum / (area*multiplier),
         forest_prop = if_else(forest_prop > 1, 1, forest_prop)) %>%
  dplyr::select(pid, forest_prop)

## Reading in alternative s0 coral values & processing
coral_alt_s0 <- read_sf("R2R_Paper/Scenarios/Re_Analysis_2022_Aug/BaselineCoral/T_AOI_r2r_baseline_coral.geojson")
coral_s0_pid <- coral_alt_s0 %>% 
  st_set_geometry(NULL) %>%
  mutate(coral_prop = if_else(is.na(c0_sum), 0, c0_sum / (area*multiplier))) %>%
  dplyr::select(pid, coral_prop)


clim_post <- case_when(climate == "clim0" ~ "0",
                       climate == "clim1" ~ "25",
                       climate == "clim2" ~ "75")

clim_post_c <- case_when(climate == "clim0" ~ "",
                       climate == "clim1" ~ "25",
                       climate == "clim2" ~ "75")


modeled <- baselines %>%
  dplyr::select(pid, vis_log, est_vis) %>%
  arrange(pid)

#### Joining climate onto baselines & REPLACING FOREST & CORAL VALUES!
base_climate_all <- baselines %>%
  dplyr::select(-forest_prop, -coral_prop) %>%
  left_join(forest_s0_pid, by = "pid") %>%
  left_join(coral_s0_pid, by = "pid") %>%
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


scen_climate_all <- newNonClimate %>%
  left_join(climate_vals, by = "pid")
scen_climate_all
base_climate_all

### Below is modified to calculate forest and coral effects separately
# Create tibble of FOREST SCENARIO data in new climate
# looking for forest effect, so replacing CORAL data in the scenario df with coral data from the baseline df
scen_climate_forest_eff <- scen_climate_all %>%
  dplyr::select(-coral_prop) %>%
  left_join(coral_s0_pid, by = "pid")
  
scen_clim_data_forest <- scen_climate_forest_eff %>%
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

preds_scen_clim_forest <- predict(viz_model_raw, newdata = scen_clim_data_forest)
modeled$preds_scen_clim_forest <- preds_scen_clim_forest
modeled$preds_scen_clim_vis_forest <- exp(preds_scen_clim_forest)
modeled


### Now do the same to get the CORAL effect, by replacing FOREST with baseline values
scen_climate_coral_eff <- scen_climate_all %>%
  dplyr::select(-forest_prop) %>%
  left_join(forest_s0_pid, by = "pid")

scen_clim_data_coral <- scen_climate_coral_eff %>%
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

preds_scen_clim_coral <- predict(viz_model_raw, newdata = scen_clim_data_coral)
modeled$preds_scen_clim_coral <- preds_scen_clim_coral
modeled$preds_scen_clim_vis_coral <- exp(preds_scen_clim_coral)
modeled


# Apply future vis multiplier of 2.67
modeled$preds_base_clim_vis_future <- modeled$preds_base_clim_vis * 2.67
modeled$preds_scen_clim_vis_future_forest <- modeled$preds_scen_clim_vis_forest * 2.67
modeled$preds_scen_clim_vis_future_coral <- modeled$preds_scen_clim_vis_coral * 2.67
modeled



# calculate difference
modeled <- modeled %>%
  mutate(diff_vis_forest = round(preds_scen_clim_vis_future_forest - preds_base_clim_vis_future, 2), # Varies by adaptation strategy, but both Protect and rEstore forest are scenario - baseline
         perc_change_forest = 100*(preds_scen_clim_vis_future_forest - preds_base_clim_vis_future) / preds_base_clim_vis_future,
         diff_vis_coral = round(preds_scen_clim_vis_future_coral - preds_base_clim_vis_future, 2), 
         perc_change_coral = 100*(preds_scen_clim_vis_future_coral - preds_base_clim_vis_future) / preds_base_clim_vis_future) 
modeled

# join to spatial 
modeled_sp <- aoi %>%
  dplyr::select(pid, CNTRY_NAME) %>%
  left_join(modeled, by = "pid")

## Drop MX since it's not included in the AOI and is returning some misleading results
modeled_sp_3 <- modeled_sp %>%
  filter(CNTRY_NAME != "Mexico")

ggplot(modeled_sp_3 %>% filter(diff_vis_forest != 0)) +
  geom_sf(aes(fill = diff_vis_forest), size = .1)

ggplot(modeled_sp_3 %>% filter(diff_vis_coral != 0)) +
  geom_sf(aes(fill = diff_vis_coral), size = .1)

summary(modeled_sp_3$diff_vis_forest)
summary(modeled_sp_3$diff_vis_coral)
# ok good, using the updated baseline luc basically took care of my negatives. still a -.03, 
# but i think we can ignore that 


ggplot(modeled_sp_3 %>% filter(diff_vis_forest != 0)) +
  geom_sf(aes(fill = perc_change_forest), size = .1)

ggplot(modeled_sp_3 %>% filter(diff_vis_coral != 0)) +
  geom_sf(aes(fill = perc_change_coral), size = .1)

summary(modeled_sp_3)

## simpler version to share
modeled_sp_tw <- modeled_sp_3 %>%
  dplyr::select(pid, CNTRY_NAME, fors_vis = diff_vis_forest, cor_vis = diff_vis_coral)

# total change?
modeled_sp_tw %>%
  st_drop_geometry() %>%
  summarise(change_fors = sum(fors_vis, na.rm = T),
            change_cor = sum(cor_vis, na.rm = T))

# let's write it out (full file as a geojson, plus a simpler one as a shapefile for jade)
st_write(modeled_sp_3, paste0("R2R_Paper/Scenarios/Re_Analysis_2022_Aug/", anum, "_", aname, "/IPMs/MARwide_", ipm, "_", aname, "_rec_", climate, "_sep.geojson"), delete_dsn = TRUE)
st_write(modeled_sp_tw, paste0("R2R_Paper/Scenarios/Re_Analysis_2022_Aug/", anum, "_", aname, "/IPMs/MARwide_", ipm, "_", aname, "_rec_", climate, "_sep.shp"))#, delete_dsn = TRUE)

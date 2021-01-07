#####
### Using the Viz Model to predict Viz under the s2_protect forest scenario
### Forked from viz_predict_s7_restore_corals.R on 1/7/21

### THIS COPY ONLY ACCOUNTS FOR THE CORAL EFFECT (IGNORES THE ACTUAL LOSS OF FOREST)
###
### First pass test using the corals layer that Jess created
### 4/20/20 SGW

### Updated 5/19 for new corals tests - also working to generalize
### 7/1 Adding climate

### Forked from `viz_predict` on 10/29/20. Not sure we need indiv scripts for each scenario, but trying for nwo

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

# Protect forest is relevant for BZ, GT, and HN
anum <- "02" #protect forest
aname <- "prot_fors"
climate <- "clim2" #Baseline climate = clim0; 25th perc = clim1; 75th perc = clim2
climshort <- "c2"

ipm <- paste0("ipm_", anum)

newNonClimate <- read_csv("mar_tourism/Data/Scenarios/s2_prot_fors_NonClimatePredictors_20210107.csv")


clim_post <- case_when(climate == "clim0" ~ "0",
                       climate == "clim1" ~ "25",
                       climate == "clim2" ~ "75")

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
scen_climate_all <- newNonClimate %>%
  left_join(climate_vals, by = "pid")
scen_climate_all
base_climate_all

scen_clim_data <- scen_climate_all %>%
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

### ACTUALLY, REPLACE FOREST_PROP WITH THE BASELINE VALUES
scen_clim_data <- scen_clim_data %>%
  dplyr::select(-forest_prop) %>%
  left_join(baselines %>% dplyr::select(pid, forest_prop), by = "pid")

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
  mutate(diff_vis = round(preds_scen_clim_vis_future - preds_base_clim_vis_future, 2), # Varies by adaptation strategy
         perc_change = 100*(preds_scen_clim_vis_future - preds_base_clim_vis_future) / preds_base_clim_vis_future) 
modeled

# join to spatial 
modeled_sp <- aoi %>%
  dplyr::select(pid, CNTRY_NAME) %>%
  left_join(modeled, by = "pid")

ggplot(modeled_sp %>% filter(diff_vis != 0)) +
  geom_sf(aes(fill = diff_vis), size = .1)

ggplot(modeled_sp %>% filter(diff_vis != 0)) +
  geom_sf(aes(fill = perc_change), size = .1)

modeled_sp %>%
  filter(diff_vis != 0) %>%
  arrange(desc(perc_change))

summary(modeled_sp)


# let's write it out
st_write(modeled_sp, paste0("ROOT/", anum, "_", aname, "/IPMs/CoralEffectOnly/MARwide_", ipm, "_", aname, "_rec_", climate, "CORAL_EFFECTS_ONLY.geojson"))#, delete_dsn = TRUE)
st_write(modeled_sp, paste0("ROOT/", anum, "_", aname, "/IPMs/CoralEffectOnly/MARwide_", ipm, "_", aname, "_rec_", climate, "CORAL_EFFECTS_ONLY.shp"))#, delete_dsn = TRUE)

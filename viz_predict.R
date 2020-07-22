#####
### Using the Viz Model to predict Viz under different scenarios
###
### First pass test using the corals layer that Jess created
### 4/20/20 SGW

### Updated 5/19 for new corals tests - also working to generalize
### 7/1 Adding climate

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

# Starting with Belize Protect forest
country <- "bz"
countryLong <- "Belize"
ipm <- "ipm_03" #Protect forest
aname <- "prot_fors"
climate <- "clim0" #Baseline climate = clim0; 25th perc = clim1; 75th perc = clim2
climshort <- "c0"

newNonClimate <- read_csv(paste0("ROOT/ProtectForest/NonClimatePredictors_", country, "_", climshort, ".csv"))

clim_post <- case_when(climate == "clim0" ~ "0",
                       climate == "clim1" ~ "25",
                       climate == "clim2" ~ "75")

# Now doing Belize protect coral
#ipm <- "climate" #"ipm_06"
#aname <- "noact" #"prot_corl"
#coral_new <- read_sf("ROOT/ROOT_coral_test_20200519/protect_coral_Tourism_CVmodel/MAR_coral_WGS8416N_eraseBelize.shp")

modeled <- baselines %>%
  dplyr::select(pid, vis_log, est_vis) 

#### Joining climate onto baselines
base_climate_all <- baselines %>%
  left_join(climate_vals, by = "pid")

### Create tibble of baseline values in new climate
base_clim_data <- base_climate_all %>%
  dplyr::select(country, 
                coral_prop, #= corals_new, 
                mangrove, 
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

preds_base_clim <- predict(viz_model_raw, newdata = base_clim_data)
modeled$preds_base_clim <- preds_base_clim
modeled$preds_base_clim_vis <- expm1(preds_base_clim)
modeled

# Create tibble of SCENARIO data in new climate
scen_climate_all <- newNonClimate %>%
  left_join(climate_vals, by = "pid")
scen_climate_all

scen_clim_data <- scen_climate_all %>%
  dplyr::select(country, 
                coral_prop, #= corals_new, 
                mangrove, 
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

preds_scen_clim <- predict(viz_model_raw, newdata = scen_clim_data)
modeled$preds_scen_clim <- preds_scen_clim
modeled$preds_scen_clim_vis <- expm1(preds_scen_clim)
modeled



# calculate difference
modeled <- modeled %>%
  mutate(diff_vis = round(preds_base_clim_vis - preds_scen_clim_vis, 2)) # need to be careful about this line and what it means for each scenario
modeled

# join to spatial 
modeled_sp <- aoi %>%
  dplyr::select(pid, CNTRY_NAME) %>%
  left_join(modeled, by = "pid")

ggplot(modeled_sp) +
  geom_sf(aes(fill = diff_vis), size = .1)

# subset to country
modeled_country <- modeled_sp %>% filter(CNTRY_NAME == countryLong)

# check it out
ggplot(modeled_country) +
  geom_sf(aes(fill = diff_vis))

modeled_country

## Extract the modeled estimates and difference and write it out
country_clean <- modeled_country %>%
  dplyr::select(pid, CNTRY_NAME, est_vis, preds_base_clim_vis, preds_scen_clim_vis, diff_vis)

# write out shp
st_write(country_clean, paste0("ROOT/ProtectForest/IPMs/", country, "_", ipm, "_", aname, "_rec_", climate, ".geojson"))

  
### Convert to Raster (todo: update with new empty raster that Jade shared)
empty_rast <- raster(diff_sp, res = 500)
empty_rast
diff_rast <- rasterize(as(diff_sp, "Spatial"), empty_rast, field = "diff_vis")
plot(diff_rast)

# write it out
writeRaster(diff_rast, 
            paste0("ROOT/ROOT_coral_test_20200519/protect_coral_Tourism_CVmodel/", ipm,"_", aname, "_rec_", climate, ".tif"), 
            format = "GTiff")





# clean up for writing
modeled_tw <- modeled_bz %>%
  select(pid, visWcoral = fitted_vis, visWOcoral = preds_vis, vis_diff = diff_vis)

# write it out
st_write(modeled_tw, "coral_test_tourism.shp")

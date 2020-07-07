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
aoi <- read_sf("GIS/AOI/AOI_v3/Intersected/T_AOI_intersected_pid_32616_no_slivers.shp")

## Getting oriented in naming scheme

# Starting with Belize Restore Coral
country <- "Belize"
#ipm <- "ipm_05" #Restore Coral
#aname <- "rest_corl"
climate <- "clim2" #Baseline climate = clim0; 25th perc = clim1; 75th perc = clim2
#coral_new <- read_sf("ROOT/ROOT_coral_test_20200519/restore_coral_Tourism_CVmodel/MAR_coral_WGS8416N_erase_restored_areasBZ.shp")

# Now doing Belize protect coral
ipm <- "climate" #"imp_06"
aname <- "noact" #"prot_corl"
#coral_new <- read_sf("ROOT/ROOT_coral_test_20200519/protect_coral_Tourism_CVmodel/MAR_coral_WGS8416N_eraseBelize.shp")

#### Joining climate onto baselines
base_climate <- baselines %>%
  left_join(climate_vals, by = "pid")
clim_post <- case_when(climate == "clim0" ~ "",
                       climate == "clim1" ~ "25",
                       climate == "clim2" ~ "75")


##### Coral transforms (hopefully we want this to be in a "preparing_predictors_clean.R" script only, not here) ####
crs(coral_new)
coral_valid <- st_make_valid(coral_new)
#coral
## intersect corals with aoi
# reproject and make valid
#corals_32 <- st_transform(corals_full, crs = 32616)
#corals_valid <- st_make_valid(corals_32)

corals_int <- st_intersection(aoi, coral_valid)

corals_pids <- corals_int$pid

# make a newdata frame
pred2 <- base_climate %>%
  mutate(corals_new = if_else(pid %in% corals_pids, 1, 0))

ggplot(pred2) +
  geom_point(aes(x = jitter(corals), y = jitter(corals_new)))
###################

#### Create shapefile of fitted values (corals_full equivalent)
## Not rerunning the model, since I'm assuming that corals_full is what I built it with
modeled <- baselines %>%
  dplyr::select(pid, vis_log, est_vis) 

modeled$fitted <- viz_model_raw$fitted.values
modeled$fitted_vis <- expm1(modeled$fitted)
modeled

# build newdata frame

newdata <- base_climate %>%
  dplyr::select(country, 
         prop_coral, #= corals_new, 
         mangrove, 
         beach,
         forest, 
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
modeled$preds_vis <- expm1(preds)
modeled

# calculate difference
modeled <- modeled %>%
  mutate(diff_vis = round(preds_vis - fitted_vis, 2)) # need to be careful about this line and what it means for each scenario
modeled

# join to spatial 
modeled_sp <- aoi %>%
  dplyr::select(pid, NAME) %>%
  left_join(modeled, by = "pid")

ggplot(modeled_sp) +
  geom_sf(aes(fill = diff_vis))

# subset to country
modeled_country <- modeled_sp #%>% filter(NAME == country)

# check it out
ggplot(modeled_country) +
  geom_sf(aes(fill = diff_vis))

## Extract just the difference and turn it into a raster
diff_sp <- modeled_country %>% dplyr::select(diff_vis)

# write out shp
st_write(diff_sp, paste0("Scenarios/Climate/", ipm, "_", aname, "_rec_", climate, ".shp"))

  
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

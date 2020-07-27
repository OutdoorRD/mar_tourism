#####
### Using the Viz Model to predict Viz under different scenarios
### This script is optimized for "protect" scenarios that simply remove the entire 
###  footprint of the habitat (mangroves, coral, others?).
### IS NOT APPROPRIATE for protect forest, which doesn't simply remove the entire footprint

## Code has been used to create the mangrove scnearios, and should be ready for protect coral

### Forked from viz_predict.R on 7/27/20

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

aname <- "prot_corl"
anameLong <- "ProtectCoral"
climate <- "clim1" #Baseline climate = clim0; 25th perc = clim1; 75th perc = clim2
climshort <- "c1"

#newNonClimate <- read_csv(paste0("ROOT/ProtectForest/NonClimatePredictors_", country, "_", climshort, ".csv"))

clim_post <- case_when(climate == "clim0" ~ "0",
                       climate == "clim1" ~ "25",
                       climate == "clim2" ~ "75")

# create a special code for coral
clim_post_c <- case_when(climate == "clim0" ~ "",
                       climate == "clim1" ~ "25",
                       climate == "clim2" ~ "75")

modeled <- baselines %>%
  dplyr::select(pid, vis_log, est_vis) 

#### Joining climate onto baselines
base_climate_all <- baselines %>%
  left_join(climate_vals, by = "pid")

### Create tibble of baseline values in new climate
base_clim_data <- base_climate_all %>%
  dplyr::select(country, 
                coral_prop = paste0("coral_prop", clim_post_c), 
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
## Don't need to read in any new data, since we can just overwrite all the mangrove
## pids with zero

#scen_climate_all <- newNonClimate %>%
 # left_join(climate_vals, by = "pid")
#scen_climate_all

if(aname == "prot_mang"){
  scen_clim_data <- base_clim_data %>%
    mutate(mangrove = 0)
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


### Looping through the countries ###
# note that the ipm code changes, so reference the ProtectMangrove README.md to find them (or the ROOT data tracking spreadsheet)

country <- "mx"
countryLong <- "Mexico"
ipm <- "ipm_06" # ProtectMangrove: mx = 02, bz = 06, gt = 06, hn = 10 
# ProtectCoral: mx = 06, bz = 08, hn = 15

# import empty country raster & country aoi outline
country_rast <- raster(paste0("ROOT/CountryAOIs/", country, "_root_aoi.tif"))
country_sf <- read_sf(paste0("ROOT/CountryAOIs/", country, "_root_aoi.shp"))


# subset to country
modeled_country <- modeled_sp %>% filter(CNTRY_NAME == countryLong)

# check it out
ggplot(modeled_country) +
  geom_sf(aes(fill = diff_vis))

modeled_country
summary(modeled_country$diff_vis)

## Extract the modeled estimates and difference and write it out
country_clean <- modeled_country %>%
  dplyr::select(pid, CNTRY_NAME, est_vis, preds_base_clim_vis, preds_scen_clim_vis, diff_vis)

# write out shp
st_write(country_clean, paste0("ROOT/", anameLong, "/IPMs/", country, "_", ipm, "_", aname, "_rec_", climate, ".geojson"))#, delete_dsn = TRUE)


  
### Convert to Raster 

# raster type to float 32 is what matthew thinks will help
# transform
country_tran <- st_transform(country_clean, crs = 26916) 

diff_rast <- fasterize(country_tran, country_rast, field = "diff_vis", background = 0)
diff_rast
#plot(diff_rast)
#dataType(diff_rast) <- "FLT4S"
#dataType(diff_rast)

# mask it
# NOTE: if this throws strange errors/warnings, go to `tmp/R******` and delete all the temp rasters that have been created
diff_masked <- mask(diff_rast, mask = country_sf, datatype = "FLT4S")
diff_masked

# write it out
writeRaster(diff_masked, 
            paste0("ROOT/", anameLong, "/IPMs/", country, "_", ipm,"_", aname, "_rec_", climate, ".tif"), 
            format = "GTiff", datatype = "FLT4S", overwrite = TRUE)




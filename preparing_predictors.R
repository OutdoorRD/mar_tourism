## Preparing predictors for MAR Visitation model ###

library(sf)
library(lwgeom)
library(tidyverse)
library(ggplot2)
library(raster)
library(corrgram)

setwd("~/Documents/MAR/GIS/Predictors/Baseline_Inputs/")

# Import gridded AOI
aoi <- st_read("../../../ModelRuns/baseline_5k_intersect/aoi_viz_exp.shp")
#plot(aoi)
#aoi_valid <- st_make_valid(aoi)

## Starting with habitat layers from coastal vulnerability

## Corals (and barrier reef, which the latest CV run has separated out)
# import corals
coral <- st_read("Corals_1.shp")
barriers <- st_read("BarrierReef_5.shp")
coral_valid <- st_make_valid(coral)

# create an indicator for whether or not corals occur in each grid cell
# check the projection
st_crs(aoi)
st_crs(coral_valid)

# reproject aoi to match coral (and hopefully other CV layers)
aoi_nad83 <- st_transform(aoi, crs = st_crs(coral_valid))

# take intersection of grid and corals
corals_int <- st_intersection(aoi_nad83, coral_valid)

# extracting pids which contain corals
coral_pids <- corals_int$pid

# take intersection of grid and barrier
barrier_int <- st_intersection(aoi_nad83, barriers)
barrier_pid <- barrier_int$pid

predictors <- aoi_nad83 %>% 
  mutate(corals = if_else(pid %in% c(coral_pids, barrier_pid), 1, 0))

ggplot(predictors) + geom_sf(aes(fill = corals))

##### Mangroves
mangroves <- read_sf("Mangroves_2.shp")
mangroves_valid <- st_make_valid(mangroves)

# take intersection of grid and mangroves
mangroves_int <- st_intersection(aoi_nad83, mangroves_valid)
mangroves_pid <- mangroves_int$pid

predictors <- predictors %>% 
  mutate(mangroves = if_else(pid %in% mangroves_pid, 1, 0))

ggplot(predictors) + geom_sf(aes(fill = mangroves))

###### Beaches
beach <- read_sf("beach_from_geomorph_MAR_v4_shift_BZ_MX.shp")
beach_int <- st_intersection(aoi_nad83, beach)
beach_pid <- beach_int$pid

predictors <- predictors %>%
  mutate(beach = if_else(pid %in% beach_pid, 1, 0))

ggplot(predictors) + geom_sf(aes(fill = beach))

################################
#### Climate

# average temp
temp <- raster("MEANTEMP_BASELINE.tif")
#crs(temp) <- crs(aoi)
ex_heat <- raster("MaxTempDaysAbove35C_BASELINE.tif")
precip <- raster("PRECIP_BASELINE.tif")
daysrain <- raster("PrecipDaysAbove1mm_BASELINE.tif")

aoi_centers <- st_centroid(aoi)

climate <- aoi_centers %>%
  mutate(temp = extract(temp, aoi_centers),
         dayshot = extract(ex_heat, aoi_centers),
         precip = extract(precip, aoi_centers),
         daysrain = extract(daysrain, aoi_centers)) %>%
  st_set_geometry(NULL) %>%
  dplyr::select(pid, temp, dayshot, precip, daysrain)

# join to other predictors
predictors2 <- predictors %>%
  left_join(climate, by = "pid") %>%
  mutate(vis_log = log1p(est_vis))


##### In/out of MPA
predictors3 <- predictors2 %>% 
  mutate(protected = if_else(is.na(MPA), 0, 1))


##### % land in polygon
land <- read_sf("landmass_adjusted_clipped_shift_BZ_MX.shp")

## I want to create a predictor which shows what proportion of the grid cell is land

aoi$area <- unclass(st_area(aoi))

crs(land)

land_int <- st_intersection(aoi, land)
plot(land_int)

# calculate the area of each intersected polygon (only includes land)
land_int$area <- unclass(st_area(land_int))

land_areas <- land_int %>% 
  st_set_geometry(NULL) %>%
  group_by(pid) %>% 
  summarise(land_area = sum(area))

# bind on to all pids
aoi_land <- aoi %>%
  left_join(land_areas, by = "pid") %>%
  mutate(prop_land = if_else(is.na(land_area), 0, land_area/area))

# and, add it to the predictors
predictors4 <- predictors3 %>%
  left_join(aoi_land %>% 
              st_set_geometry(NULL) %>% 
              dplyr::select(pid, prop_land),
            by = "pid")


########### Write it out #####
#write_sf(predictors4, "CombinedPredictors_111419_PARTIAL.shp")
## and a csv, for fun
#write_csv(predictors4 %>% st_set_geometry(NULL), "CombinedPredictors_111419_PARTIAL.csv")







# create a non-spatially explicit one to look at corrgram
pred_small <- predictors4 %>% 
  st_set_geometry(NULL) %>%
  dplyr::select(vis_log, est_vis, smud_prop, corals, mangroves, beach, temp, dayshot, 
                precip, daysrain, protected, prop_land)

summary(pred_small)
plot(density(pred_small$est_vis, na.rm = TRUE))
plot(density(pred_small$vis_log, na.rm = TRUE))
plot(density(pred_small$smud_prop))

corrgram(pred_small, lower.panel = panel.cor, upper.panel = panel.pts)
# precipitation and days of rain are highly correlated. Will need to choose just one to include
mod1 <- lm(vis_log ~ corals + mangroves + beach + temp + dayshot + precip + daysrain + protected, 
           data = pred_small)
summary(mod1)
#plot(mod1)

mod2 <- MASS::glm.nb(round(est_vis) ~ corals + mangroves + beach + temp + dayshot + precip + daysrain + protected, 
                     data = pred_small)
summary(mod2)

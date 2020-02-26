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

#ggplot(predictors) + geom_sf(aes(fill = corals))

# let's also recombine them and export for Invest
corals_sm <- coral %>% 
  mutate(type = "coral") %>%
  dplyr::select(type, geometry) %>%
  st_cast("POLYGON") 

barriers_sm <- barriers %>% 
  mutate(type = "barrier") %>%
  dplyr::select(type, geometry)

corals_all <- rbind(corals_sm, barriers_sm)
corals_32616 <- st_transform(corals_all, crs = 32616)

#ggplot(corals_all) +geom_sf()

# write it out
#st_write(corals_all, "corals_all.shp")
#st_write(corals_32616, "ProjectedForInvest/corals_all_32616.shp")


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


### TODO:
# wildlife2.shp (already in baseline_predictors)
# access (roads, airports, ports)
# ruins
# hurricanes (in baseline preds as windset_prob_stats_prob_exceed_C3.shp. Use C3P column)
# sargassum

# read it in
predictors4 <- read_sf("CombinedPredictors_111419_PARTIAL.shp")

# and continue
## Wildlife

wildlife <- read_sf("wildlife2.shp")
st_crs(wildlife)
wildlife_valid <- st_make_valid(wildlife)

aoi_nad27 <- st_transform(aoi, crs = st_crs(wildlife_valid))
aoi_nad27 <- st_make_valid(aoi_nad27)

ggplot() +
  geom_sf(data = aoi_nad27) +
  geom_sf(data = wildlife_valid)

# intersect
wild_int <- st_intersection(aoi_nad27, wildlife_valid)
wild_pid <- wild_int$pid

predictors5 <- predictors4 %>%
  mutate(wildlife = if_else(pid %in% wild_pid, 1, 0))

ggplot(predictors5) + geom_sf(aes(fill = wildlife))

### Hurricanes
hurricanes <- read_sf("windset_prob_stats_prob_exceed_C3.shp")
plot(hurricanes)

# just guessing what the crs is... I htink wgs84 is approp for latlong?
hurricanes_proj <- st_set_crs(hurricanes, value = st_crs(aoi))

# let's do the same as I did for the climate data, and just look at what the
# probability of a C3 hurricane is at the centroid of the tourism hexes

aoi_centers <- st_centroid(aoi)

hurricanes_int <- st_intersection(aoi_centers, hurricanes_proj)

predictors6 <- predictors5 %>%
  left_join(hurricanes_int %>% 
              st_set_geometry(NULL) %>%
              dplyr::select(pid, C3P), by = "pid") 

### Airports
airports <- st_read("airports_MAR.shp")

# for now, only including major and mid aiports
airports_maj <- airports %>% 
  filter(type %in% c("major", "mid")) %>%
  dplyr::select(type, name, abbrev)

# now I want to calculate the distance from the nearest airport for the entire aoi
air_dists <- st_distance(aoi, airports_maj)
air_min_dist <- apply(air_dists, 1, min)

predictors6$air_min_dist <- air_min_dist

#ggplot(predictors6) +
 # geom_sf(aes(fill = air_min_dist))

### Ports
ports <- read_sf("ports_MAR_2.shp")

ports_dists <- st_distance(aoi, ports)
ports_min_dist <- apply(ports_dists, 1, min)

predictors6$ports_min_dist <- ports_min_dist

#ggplot(predictors6) +
 # geom_sf(aes(fill = ports_min_dist))

#### Ruins 
ruins <- st_read("archaeological_sites_combined.shp")

# intersect
ruins_int <- st_intersection(aoi, ruins)
ruins_pid <- ruins_int$pid

predictors7 <- predictors6 %>%
  mutate(ruins = if_else(pid %in% ruins_pid, 1, 0))

#### Sargassum
# just intersect with presence records for now
sargassum <- st_read("sargassum_oceancleaner_present.shp")
# intersect
sarg_int <- st_intersection(aoi, sargassum)
sarg_pid <- sarg_int$pid

predictors8 <- predictors7 %>%
  mutate(sargassum = if_else(pid %in% sarg_pid, 1, 0))

### write it out
#st_write(predictors8, "CombinedPredictors_010320_PARTIAL.shp")

# and as a csv
#write_csv(predictors8 %>% st_set_geometry(NULL), "CombinedPredictors_010320_PARTIAL.csv")

##################################################3
#### Read it in
predictors8 <- read_sf("CombinedPredictors_010320_PARTIAL.shp")


#### Roads 
roads <- read_sf("roads_MAR_clip.shp")

roads_dists <- st_distance(aoi, roads) # too slow (>147 min)

# what about if i run an intersection (to find hexes with roads in them),
# then calculate distance from those hexes to all others?
roads_int <- st_intersection(aoi, roads) # that was quicker
#ggplot(roads_int) + geom_sf()

roads_polys <- aoi %>% filter(pid %in% roads_int$pid)
#ggplot(roads_polys) + geom_sf()

# let's pull out just hexes that don't have roads in them to run the dists on
no_rds_polys <- aoi %>% filter(!pid %in% roads_int$pid)

#### Stopped here on 1/6
roads_dists <- st_distance(no_rds_polys, roads_polys) # started 4:30pm


roads_min_dist <- apply(roads_dists, 1, min)

no_rds_polys$roads_min_dist <- roads_min_dist

ggplot(no_rds_polys) +
  geom_sf(aes(fill = roads_min_dist))

# let's write that one out
st_write(no_rds_polys, "Roads_intermediates/no_roads_dists.shp")

# so right now, the polygons which are adjacent to road polygons are getting a zero
# And the ones which are one away are getting a value of 2667 meters, which is the length
# of one side of my hexes (except for the smaller ones which are intersected with borders)

# So, I want the hexes with roads in them to be zeros, and all others to be slightly more
# Let's add 1000 meters to all values. This isn't perfect, but should work

no_rds_polys$roads_min_dist_adj <- no_rds_polys$roads_min_dist + 1000

predictors9 <- predictors8 %>%
  left_join(no_rds_polys %>% 
              st_set_geometry(NULL) %>%
              dplyr::select(pid, roads_min_dist_adj), by = "pid") %>%
  mutate(roads_min_dist = if_else(pid %in% roads_polys$pid, 0, roads_min_dist_adj)) %>%
  dplyr::select(-roads_min_dist_adj)

## rename with full names (not the shortened .shp ones which came in with predictors8)

predictors9 <- predictors9 %>%
  rename(ud_to_vis = ud_t_vs,
         mangroves = mangrvs,
         daysrain = daysran,
         protected = protctd,
         prop_land = prp_lnd,
         wildlife = wildlif,
         air_min_dist = ar_mn_d,
         ports_min_dist = prts_m_,
         sargassum = sargssm)

# write it out
st_write(predictors9, "CombinedPredictors_010920.shp")
st_write(predictors9, "CombinedPredictors_010920.geojson")
# let's write it to a trackable location as well
write_csv(predictors9 %>% st_set_geometry(NULL), "../../../mar_tourism/CombinedPredictors_010920.csv")
write_csv(predictors9 %>% st_set_geometry(NULL), "CombinedPredictors_010920.csv" )

predictors9 <- st_read("CombinedPredictors_010920.geojson")

######## Using WorldPop as a proxy for development for now (2/11/20) #######

# Note that the units/values in the layer shared by Jess/Stacie seem wonky
# Generally they are almost all <3, but a few pixels in Mexico are up to 571
## The general documentation online suggest that they are meant to be 
##   ppl/100 m2, but this doesn't seem to match well.

## However, the data in the file that stacie created do match the Mexico data
##   when I download them myself. So I think it is meant to be ppl/100m2, but
##   that there may just be some errors created by the data "spreading" method.
##  Though, honestly, they may match my spread viz data fairly well

## I want to truncate the data, and assign all the values < 10?? to be the cutoff
## Then I should add up the values inside each of my hexes to get a value

worldpop <- raster("WorldPop_2019_tourism.tif")
worldpop <- setMinMax(worldpop)

# let's assign each cell to a tourism hex (creates a df ordered according to aoi)
worldaoi <- extract(worldpop, aoi, df = TRUE) ## grr, threw an error after an hour

# instead, how about those points
worldpoints <- rasterToPoints(worldpop, spatial = TRUE)
worldsf <- st_as_sf(worldpoints) # started 1:11

summary(worldsf)

#write it out
#write_sf(worldsf, "WorldPop_intermediates/worldpoints.shp") # 2:31

# let's intersect
testpts <- st_as_sf(rasterToPoints(test, spatial = TRUE))
worldcrs <- st_crs(testpts)
aoi_reproj <- st_transform(aoi, worldcrs) %>% dplyr::select(pid, geometry)

world_int <- st_intersection(worldsf, aoi_reproj) # started 2:42, ended 2:50
world_pid <- world_int %>% 
  st_set_geometry(NULL) %>%
  rename(population = WorldPop_2019_tourism)

# write it out
#write_csv(world_pid, "WorldPop_intermediates/population_by_point_pid.csv")

# Ok. And... summarise (for now wihtout truncating)
pop_by_pid <- world_pid %>%
  group_by(pid) %>%
  summarise(WorldPop = sum(population))

# write it out
#write_csv(pop_by_pid, "WorldPop_intermediates/pop_by_pid_summed.csv")

# let's check it out spatially again
aoi_pop <- aoi %>% left_join(pop_by_pid, by = "pid")

#ggplot() + geom_sf(data = aoi_pop, aes(fill = WorldPop))
# great

# write it out
#st_write(aoi_pop, "WorldPop_intermediates/pop_by_pid_summed_NAs.shp")

# and creat one where ocean gets 0s
aoi_pop_0 <- aoi_pop %>%
  replace_na(list(WorldPop = 0))

# write it out
#st_write(aoi_pop_0, "WorldPop_intermediates/pop_by_pid_summed_0s.shp")

# Ok. Now for actually getting it onto the predictors shp and csv
predictors10 <- predictors9 %>% 
  left_join(pop_by_pid, by = "pid") %>%
  replace_na(list(WorldPop = 0))

# write it out
#st_write(predictors10, "CombinedPredictors_021120.shp")
#st_write(predictors10, "CombinedPredictors_021120.geojson")
# let's write it to a trackable location as well
#write_csv(predictors10 %>% st_set_geometry(NULL), "../../../mar_tourism/CombinedPredictors_021120.csv")
#write_csv(predictors10 %>% st_set_geometry(NULL), "CombinedPredictors_021120.csv" )

#### Adding in development from LULC layer ##########
#################################################

predictors10 <- st_read("CombinedPredictors_021120.geojson")

develop <- st_read("lulc_developed_national_baseline.shp")

# let's make it valid and transform to 32616 for invest
dev_valid <- st_make_valid(develop)
dev_32 <- st_transform(dev_valid, crs = 32616)

# write it out
#st_write(dev_32, "ProjectedForInvestValid/lulc_developed_national_baseline_32616.shp")

# convert aoi as well
aoi_32616 <-  st_transform(aoi, crs = 32616)

aoi_32616$area <- unclass(st_area(aoi_32616))


## Ok. So I want the proportion of each hex which is developed. Let's look at how I did land for this

dev_int <- st_intersection(aoi_32616, dev_32)
plot(dev_int)

# calculate the area of each intersected polygon (only includes developed spaces)
dev_int$area <- unclass(st_area(dev_int))

dev_areas <- dev_int %>% 
  st_set_geometry(NULL) %>%
  group_by(pid) %>% 
  summarise(dev_area = sum(area))

# bind on to all pids
aoi_dev <- aoi_32616 %>%
  left_join(dev_areas, by = "pid") %>%
  mutate(prop_dev = if_else(is.na(dev_area), 0, dev_area/area))

# bind on to predictors
predictors11 <- predictors10 %>% 
  left_join(aoi_dev %>% st_set_geometry(NULL) %>% dplyr::select(pid, prop_dev))

# write it out
st_write(predictors11, "CombinedPredictors_022520.shp")
st_write(predictors11, "CombinedPredictors_022520.geojson")
# let's write it to a trackable location as well
write_csv(predictors11 %>% st_set_geometry(NULL), "../../../mar_tourism/CombinedPredictors_022520.csv")
write_csv(predictors11 %>% st_set_geometry(NULL), "CombinedPredictors_022520.csv" )

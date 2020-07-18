## Creating a forest footprint
## Reads in LULC rasters and extracts forest
## Consider: whether I want to write out rasters or shapes

library(tidyverse)
library(raster)
library(foreign)
library(sf)

setwd("~/Documents/MAR/")

aoi_outline <- read_sf("GIS/AOI/AOI_v4/Tourism_AOI_v4.shp")

# read in baseline LULC. Note that layer=1 just reads in the LUCodes
baseline <- raster("GIS/Predictors/LULC/LULC_all_national_baseline.tif", layer = 1)
plot(baseline)
baseline

# read in smaller BZ scenario layer
bz <- raster("ROOT/ProtectForest/ForestScenarios/bz_s3_prot_fors_luc_names.tif")
bz

# read in joinkey
joinkey <- read_csv("GIS/Predictors/LULC/LULC_Joinkey_coastal_forest.csv")

# create list of forest lulc_codes
# using base r
#forest_lulcs <- joinkey[joinkey$lulc_group == "Forest", "Value"]
#joinkey[!is.na(joinkey$forest_type) & joinkey$forest_type == "coastal", "Value"]
coastal_lulcs <- joinkey %>% filter(forest_type == "coastal") %>% pull(Value)

# Trying the entire baseline witht he coastal joinkey
base_points <- rasterToPoints(baseline, fun=function(x){x %in% coastal_lulcs})
head(base_points)
colnames(base_points) <- c("x","y", "lulc_code")
base_tib <- as_tibble(base_points)
base_sf <- st_as_sf(base_tib, coords = c("x", "y"), crs = 26916)
# clip to aoi_outline



# Ok. This (below), works. The points are still large, and slow. I should add
# an intersection with the AOI outline so that I'm only writing out relevant forest

# how about points?
bz_points <- rasterToPoints(bz, fun=function(x){x %in% forest_lulcs})
bz_points
# great. Now i have a matrix of x, y coords for forest points in BZ
colnames(bz_points) <- c("x","y", "lulc_code")
bz_tib <- as_tibble(bz_points)
bz_forest <- st_as_sf(bz_tib, coords = c("x", "y"), crs = 26916)
# convert
bz_32 <- st_transform(bz_forest, crs = 32616)
# clip by aoi


write_sf(bz_32, "ROOT/ProtectForest/ForestScenarios/bz_forest.shp", delete_layer = TRUE)

# how is it for intersecting with the aoi?
test <- PresAbsFunc(bz_32, aoi)
bz_prot_forest <- test %>% rename(forest = bz_32)
# save it!
write_csv(bz_prot_forest, "ROOT/ProtectForest/ForestScenarios/bz_prot_fors_by_pid.csv")





################################ lumber ########

# select only forest from baseline
# note that this tries to write large rasters to "tmp", and if the computer runs out of space
# it fails with a strange error:
# Error in matrix(unlist(ini), ncol = 2, byrow = TRUE) : 
#'data' must be of a vector type, was 'NULL'
#In addition: There were 50 or more warnings (use warnings() to see the first 50)
# For now, I tried copying my files over to vulpes and running this all there
forest <- baseline %in% forest_lulcs
writeRaster(forest, "baseline_forest.tif", format = "GTiff")


# wait, what about converting it to a shapefile directly?
#rasterToPolygons(x, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)

#bz_poly <- rasterToPolygons(bz, fun=function(x){x %in% forest_lulcs}, digits = 1) # also very slow

# Having done so, I now copied back "baseline_forest.tif
# I want to see how hard it would be to work with

baseline_forest <- raster("GIS/Predictors/LULC/baseline_forest.tif")
baseline_forest
plot(baseline_forest)
rm(baseline_forest)

aoi <- read_sf("ModelRuns/baseline_20200715/T_AOI_v4_5k_32616_pid.shp")

#raster::extract(baseline_forest, aoi) # not fast


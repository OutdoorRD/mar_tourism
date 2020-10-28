## Converting all files to a single crs for InVEST
# let's use WGS84, UTM 16N
# EPSG:32616

# so, it'll just be:
# Read file

library(sf)
library(lwgeom) # for st_make_valid()
library(raster)
#library(fasterize)
library(tidyverse)

#setwd("~/Documents/MAR/GIS/Predictors/Baseline_Inputs/")

inpath <- "~/Documents/MAR/GIS/Predictors/Nature/MAR_mangrove_Oct2020/MAR_Mangrove_Layers/"

# create list of files to transform
shpfiles <- c("MARwide_mangrove_healthy_footprint_Oct2020")

#file1 <- st_read("airports_MAR.shp")
#all(st_is_valid(file1)) == FALSE
#if (all(st_is_valid(file1)) == FALSE) {
#  file1 <- st_make_valid(file1)
#}

# st_zm removes any third dimensional data (z values)

for(shpfile in shpfiles){
  file1 <- st_read(paste0(inpath, shpfile, ".shp"))
  flat <- st_zm(file1)
  if (all(st_is_valid(flat)) == FALSE) {
    flat <- st_make_valid(flat)
  }
  trans <- st_transform(flat, crs = 32616)
  st_write(trans, paste0("ProjectedForInvestValid/", shpfile, "_32616.shp"), delete_layer = TRUE)
}

ggplot(trans) +geom_sf()

# forests isn't working, let's run interactively
# issue seems to be that it was becoming "GEOMETRY" type instead of "multipolygon"
#trans2 <- st_cast(trans, "MULTIPOLYGON")
#st_write(trans2, paste0("ProjectedForInvestValid/", shpfile, "_32616.shp"), delete_layer = TRUE)


# getting some errors. Especially for those that have been commented out above
# Working through them in QGIS. So far: land (fixed), mangroves (fixed), sargassum (fixed), C3
# Let's try them manually here

# and the same for tiffs
tiffs <- c(#"MaxTempDaysAbove35C_BASELINE", 
           #"MEANTEMP_BASELINE",
           #"PRECIP_BASELINE", 
  "WorldPop_2019_tourism")
longproj = "+proj=utm +zone=16 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
for(tiff1 in tiffs){
  file1 <- raster(paste0(tiff1, ".tif"))
  trans <- projectRaster(file1, crs = longproj)
  writeRaster(trans, paste0("ProjectedForInvest/", tiff1, "_32616.tif"))
}

# only worked for worldpop. Did the others in Qgis. Maxtemp, meantemp, precip
## Update 2/25/20. Looks like I did it wrong in QGIS before. So now doing it in 
## preparing_predictors (where I am also writing out correct wgs ones at the same time)

# and actually, what C3 needs is to be converted into a raster. So, let's try it
shpfile <- "windset_prob_stats_prob_exceed_C3"
file1 <- st_read(paste0(shpfile, ".shp"))
trans <- st_set_crs(file1, value = 32616)

trans_r <- raster(trans, res = .01)
c3_r <- fasterize(trans, trans_r, field = "C3P")
trans <- projectRaster(c3_r, crs = longproj)
#plot(test)
writeRaster(c3_r, paste0("ProjectedForInvestValid/", shpfile, "_32616.tif"), overwrite = TRUE)

# And finally, for my aoi
aoi_pid <- st_read("../../../ModelRuns/baseline_5k_intersect/T_AOI_intersected_pid.shp")
aoi_trans <- st_transform(aoi_pid, crs = 32616)

# let's drop that NA field while i'm at it
summary(aoi_trans)

aoi2 <- aoi_trans %>%
  filter(!is.na(FID))

st_write(aoi2, "ProjectedForInvestValid/T_AOI_intersected_pid_32616.shp", delete_layer = TRUE)


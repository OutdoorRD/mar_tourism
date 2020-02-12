## Converting all files to a single crs for InVEST
# let's use WGS84, UTM 16N
# EPSG:32616

# so, it'll just be:
# Read file

library(sf)
library(raster)

setwd("~/Documents/MAR/GIS/Predictors/Baseline_Inputs/")

# create list of files to transform
shpfiles <- c("airports_MAR", "archaeological_sites_combined", "BarrierReef_5",
  "beach_from_geomorph_MAR_v4_shift_BZ_MX", "Corals_1",
  #"landmass_adjusted_clipped_shift_BZ_MX.shp", 
  #"Mangroves_2",
  "ports_MAR_2", "roads_MAR_clip", 
  #"sargassum_oceancleaner_present",
  #"windset_prob_stats_prob_exceed_C3",
  "wildlife2")

for(shpfile in shpfiles){
  file1 <- st_read(paste0(shpfile, ".shp"))
  trans <- st_transform(file1, crs = 32616)
  st_write(trans, paste0("ProjectedForInvest/", shpfile, "_32616.shp"), delete_layer = TRUE)
}

# getting some errors. Especially for those that have been commented out above
# Working through them in QGIS. So far: land, mangroves, sargassum, C3

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

# And finally, for my aoi
aoi_pid <- st_read("../../../ModelRuns/baseline_5k_intersect/T_AOI_intersected_pid.shp")
aoi_trans <- st_transform(aoi_pid, crs = 32616)
st_write(aoi_trans, "ProjectedForInvest/T_AOI_intersected_pid_32616.shp")


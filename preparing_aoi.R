
## Gridded and transforming the AOI for MAR

library(sf)

setwd("~/Documents/MAR/GIS/")

aoi <- st_read("AOI/AOI_v3/Tourism_AOI_v3.shp")

# Make the hexagonal grid across the aoi
aoi_hex <- st_make_grid(aoi, cellsize = 5000, square = FALSE)
#plot(aoi_hex)

# transform gridded aoi to wgs84 for globalrec
aoi_wgs <- st_transform(aoi_hex, crs = 4326)
st_crs(aoi_wgs)

# write it out
#st_write(aoi_wgs, "AOI/AOI_v3/T_aoi_v3_wgs_5k.shp")

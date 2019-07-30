# Combine PUD and TUD at the 5km scale
# note that will need to pull in PUD from pud_results.shp for now, since
# I just pulled those through invest (though may want to repull them through
# globalrec if I decide I want raw counts or anything other than average annuals)
# Or actually those data are in the monthly_table.csv, but still only through
# 2017

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

##
## Trying to convert an IPM into tourists / raster
## 2/2/21
##

library(raster)

setwd("~/Documents/MAR/")

# read in an ipm, and also it's converter. let's do this for c1 and c2, just to look at values
# TODO: teach it to pull out the relevant parts of the file name, and automatically write out the correct file names

ipm <- raster("ROOT/07_rest_corl/IPMs/bz_ipm_07_rest_corl_rec_clim2.tif")
converter <- raster("ROOT/CountryAOIs/Conversions/bz_conversion.tif")

new <- ipm/converter

# is it really this easy?
plot(new)

# let's write it out
writeRaster(new, "ROOT/07_rest_corl/IPMs/SpreadTourists/bz_ipm_07_rest_corl_rec_clim2_spread.tif", 
            format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
  
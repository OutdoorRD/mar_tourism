##
## Trying to convert an IPM into tourists / raster
## 2/2/21
##

library(raster)
library(stringr)

# choose location of files to convert
setwd("~/Documents/MAR/ROOT/06_prot_corl/IPMs/")

# list files to loop through
files <- list.files("HexTourists")

# read in an ipm, and also it's converter. let's do this for c1 and c2, just to look at values
# TODO: teach it to pull out the relevant parts of the file name, and automatically write out the correct file names
#file <- "bz_ipm_06_prot_corl_rec_clim0.tif"
for(file in files){
  ipm <- raster(paste0("HexTourists/", file))
  
  country <- str_extract(file, "[:alpha:]{2}")
  converter <- raster(paste0("../../CountryAOIs/Conversions/", country, "_conversion.tif"))
  
  # convert
  new <- ipm/converter
  
  # write it out
  writeRaster(new, paste0("SpreadTourists/", file), format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
}

  
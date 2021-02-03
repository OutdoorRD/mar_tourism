##
## Making templates so I can "spread" tourists across my IPM raster cells
## Creates Conversions (eg `mx_conversion.tif`) that is a raster where each value is the number
## of raster cells that are contained in the tourism hex
## These conversion rasters can be used to divide out the difference in tourism to get tourists per raster cell
## 2/2/21

library(fasterize)
library(raster)
library(sf)
library(tidyverse)

setwd("~/Documents/MAR")
## read in the tourism aoi that has the count of raster cells from each country aoi in it

t_aoi <- read_sf("ROOT/CountryAOIs/Conversions/T_AOI_v4_5k_ipm_raster_cells.shp")

# let's get rid of any zeros, and parts of a raster cell that overlap, by making the minimum 1
t_aoi2 <- t_aoi %>%
  mutate_at(vars(ends_with("count")), function(x){if_else(x > 1, x, 1)})

# now I want to create a raster for each country with these values, so that I can do simple raster math
# (divide by the number of cells per hex) on any generated ipms

# following code in `create_country_IPMs.R`
country <- "bz"
countries <- c("mx", "gt", "hn")
# import empty country raster & country aoi outline
for(country in countries){
  countryLong <- case_when(country == "mx" ~ "Mexico",
                           country == "bz" ~ "Belize",
                           country == "gt" ~ "Guatemala",
                           country == "hn" ~ "Honduras")
  print(paste("Beginning to process", countryLong))
  
  country_rast <- raster(paste0("ROOT/CountryAOIs/", country, "_root_aoi.tif"))
  country_sf <- read_sf(paste0("ROOT/CountryAOIs/", country, "_root_aoi.shp"))
  
  
  aoi_country <- t_aoi2 %>% filter(CNTRY_NAME == countryLong)
  
  ### Convert to Raster 
  #st_crs(aoi_country)
  
  # raster type to float 32 is what matthew thinks will help
  # transform
  #country_tran <- st_transform(country_clean, crs = 26916) 
  
  # Note background = 1 to avoid divide by 0 issues laters
  aoi_rast <- fasterize(aoi_country, country_rast, field = paste0(country, "_count"), background = 1)
  aoi_rast
  #summary(country_clean)
  plot(aoi_rast)
  #dataType(diff_rast) <- "FLT4S"
  #dataType(diff_rast)
  
  # mask it
  # NOTE: if this throws strange errors/warnings, go to `tmp/R******` and delete all the temp rasters that have been created
  #aoi_masked <- mask(aoi_rast, mask = country_sf, datatype = "FLT4S")
  #aoi_masked
  #plot(aoi_masked)
  
  # write it out
  writeRaster(aoi_rast, 
              paste0("ROOT/CountryAOIs/Conversions/", country, "_conversion.tif"), 
              format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
  
  # remove temp files
  files <- list.files(tempdir(), full.names = TRUE)
  unlink(files, recursive = TRUE)
}

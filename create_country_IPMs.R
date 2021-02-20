##
## Subsetting MARwide ipm geojsons into country-specific IPM rasters
## 12/22/20
## Forked from the bottom of `viz_predict_s4_restore_mangroves.R`
##

library(tidyverse)
library(sf)
library(lwgeom)
library(raster)
library(fasterize)

setwd("~/Documents/MAR/")

# read in MARwide IPM geojson
marwide_path <- "ROOT/04_rest_mang/IPMs/MARwide_ipm_04_rest_mang_rec_clim0.geojson"
modeled_sp <- read_sf(marwide_path)

# pull some relevant pieces out
anum <- str_extract(marwide_path, "[:digit:]{2}")
aname <- str_extract(marwide_path, "(?<=[:digit:]{2}_)[:alpha:]+_[:alpha:]+")
climate <- str_extract(marwide_path, "[:alnum:]*(?=.geojson)")
ipm <- paste0("ipm_", anum)

countries <- c("mx", "bz", "gt", "hn")
#countries <- c("bz")
#countries <- c("bz", "gt", "mx")
#country <- "hn"

for(country in countries){
  # Set Country (protect mangrove gets all 4)
  #country <- "gt"
  #countryLong <- "Guatemala"
  countryLong <- case_when(country == "mx" ~ "Mexico",
                           country == "bz" ~ "Belize",
                           country == "gt" ~ "Guatemala",
                           country == "hn" ~ "Honduras")
  
  print(paste("Beginning to process", countryLong))
  
  # import empty country raster & country aoi outline
  country_rast <- raster(paste0("ROOT/CountryAOIs/", country, "_root_aoi.tif"))
  country_sf <- read_sf(paste0("ROOT/CountryAOIs/", country, "_root_aoi.shp"))
  
  
  modeled_country <- modeled_sp %>% filter(CNTRY_NAME == countryLong)
  
  # check it out
  fig <- ggplot(modeled_country) +
    geom_sf(aes(fill = diff_vis))
  
  print(fig)
  
  modeled_country
  summary(modeled_country)
  
  ## Extract the modeled estimates and difference and write it out
  country_clean <- modeled_country %>%
    dplyr::select(pid, CNTRY_NAME, est_vis, preds_base_clim_vis_future, preds_scen_clim_vis_future, diff_vis, perc_change)
  
  # write out shp
  st_write(country_clean, paste0("ROOT/", anum, "_", aname, "/IPMs/", country, "_", ipm, "_", aname, "_rec_", climate, ".geojson"))#, delete_dsn = TRUE)
  
  
  
  ### Convert to Raster 
  
  # raster type to float 32 is what matthew thinks will help
  # transform
  country_tran <- st_transform(country_clean, crs = 26916) 
  
  diff_rast <- fasterize(country_tran, country_rast, field = "diff_vis", background = 0)
  diff_rast
  summary(country_clean)
  #plot(diff_rast)
  #dataType(diff_rast) <- "FLT4S"
  #dataType(diff_rast)
  
  # mask it
  # NOTE: if this throws strange errors/warnings, go to `tmp/R******` and delete all the temp rasters that have been created
  diff_masked <- mask(diff_rast, mask = country_sf, datatype = "FLT4S")
  diff_masked
  
  # spread tourists according to conversion factor (new 2/19/21)
  converter <- raster(paste0("ROOT/CountryAOIs/Conversions/", country, "_conversion.tif"))
  
  # convert
  diff_spread <- diff_masked/converter
  
  # write it out
  writeRaster(diff_spread, 
              paste0("ROOT/", anum, "_", aname, "/IPMs/", country, "_", ipm,"_", aname, "_rec_", climate, ".tif"), 
              format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
  
  # remove temp files
  files <- list.files(tempdir(), full.names = TRUE)
  unlink(files, recursive = TRUE)
}

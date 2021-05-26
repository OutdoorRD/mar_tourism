###
### Making maps showing the climate variables
### 5/26/21
###

library(tidyverse)
library(sf)
library(raster)
library(rasterVis)

setwd("~/Documents/MAR/GIS/")

# read in data
aoi <- read_sf("AOI/AOI_v4/Tourism_AOI_v4.shp")
coastline <- read_sf("BordersandProtectedAreas/mar_coastline.shp")

precip_base <- raster("Predictors/Climate/Climate from Columbia/RastersSGW_WGS/PRECIP_BASELINE_corrected_2.tif",
                      band = 1)

#coords <- xyFromCell(precip_base, seq_len(ncell(precip_base)))
#precip_base <- stack(as.data.frame(getValues(precip_base)))
#names(precip_base) <- c('value', 'variable')
#
#precip_base <- cbind(coords, precip_base)

aoi_32 <- st_transform(aoi, crs = 32616)

precip_base_df <- as.data.frame(precip_base, xy = TRUE)
precip_base_df
ggplot() +
  geom_raster(data = precip_base_df, aes(x = x, y = y, fill = PRECIP_BASELINE_corrected_2)) +
  geom_sf(data = aoi_32, fill = NA, col = "orange") +
  geom_sf(data = coastline, col = "black") +
  scale_fill_viridis_c(name = "Baseline Annual Precipitation") +
  coord_sf(xlim = c(200000, 700000),
           ylim = c(1700000, 2420000))

precip_base

## Ok. read in climate ones and calculate % change
# Note that choosing "band = 2" pulls in the 25th percentile data. Band = 3 corresponds to 75th percentile
precip_2050_25 <- raster("Predictors/Climate/Climate from Columbia/RastersSGW_WGS/Precip_RCP85_2050s_corrected.tif", 
                         band = 2)
precip_2050_75 <- raster("Predictors/Climate/Climate from Columbia/RastersSGW_WGS/Precip_RCP85_2050s_corrected.tif", 
                         band = 3)

precip_2050_25_df <- as.data.frame(precip_2050_25, xy = TRUE)

#### THIS ISNT WORKING. Full join is not my best bet. maybe try making a raster stack instead
precip_perc_change_clim1 #<- 
precip_2050_25_df %>%
  rename(precip_clim1 = Precip_RCP85_2050s_corrected) %>%
  full_join(precip_base_df) %>%
  rename(precip_clim0 = PRECIP_BASELINE_corrected_2) %>%
  mutate(perc_change = 100* (precip_clim1 - precip_clim0) / precip_clim0) #%>%
  filter(!is.na(perc_change))

precip_perc_change_clim1

ggplot() +
  geom_raster(data = precip_perc_change_clim1, aes(x = x, y = y, fill = perc_change)) #+
  geom_sf(data = aoi_32, fill = NA, col = "orange") +
  geom_sf(data = coastline, col = "black") +
  scale_fill_viridis_c(name = "Baseline Annual Precipitation") +
  coord_sf(xlim = c(200000, 700000),
           ylim = c(1700000, 2420000))

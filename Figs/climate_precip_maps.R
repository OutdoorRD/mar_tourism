###
### Making maps showing the climate variables
### 5/26/21
###

library(tidyverse)
library(sf)
library(raster)
library(rasterVis)
library(fuzzyjoin)
library(knitr)
library(scales)

setwd("~/Documents/MAR/GIS/")

# read in data
aoi <- read_sf("AOI/AOI_v4/Tourism_AOI_v4.shp")
coastline <- read_sf("BordersandProtectedAreas/mar_coastline.shp")
aoi_32 <- st_transform(aoi, crs = 32616)

precip_base <- raster("Predictors/Climate/Climate from Columbia/RastersSGW_WGS/PRECIP_BASELINE_corrected_2.tif",
                      band = 1)

precip_base_df <- as.data.frame(precip_base, xy = TRUE)
precip_base_df

ggplot() +
  geom_raster(data = precip_base_df, aes(x = x, y = y, fill = PRECIP_BASELINE_corrected_2)) +
  geom_sf(data = aoi_32, fill = NA, col = "black") +
  geom_sf(data = coastline, col = "black") +
  scale_fill_distiller(palette = "Blues", 
                       name = "Precipitation \n (mm/year)", 
                       direction = 1,
                       na.value = "gray90") +
  coord_sf(xlim = c(220000, 700000),
           ylim = c(1700000, 2420000)) +
  labs(title = "Baseline") +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.background = element_rect(fill = "gray95"),
        legend.margin = margin(2, 2, 2, 2),
        legend.position = c(.8, .4),
        plot.title = element_text(hjust = .5, size = 15))


# write it out
ggsave("../Deliverables/figs/ClimateMaps/precip_baseline.png", width = 6, height = 6.4, units = "in")
knitr::plot_crop("../Deliverables/figs/ClimateMaps/precip_baseline.png")

rastprecip_base

## Ok. read in climate ones and calculate % change
# Note that choosing "band = 2" pulls in the 25th percentile data. Band = 3 corresponds to 75th percentile
precip_2050_25 <- raster("Predictors/Climate/Climate from Columbia/RastersSGW_WGS/Precip_RCP85_2050s_corrected.tif", 
                         band = 2)
precip_2050_75 <- raster("Predictors/Climate/Climate from Columbia/RastersSGW_WGS/Precip_RCP85_2050s_corrected.tif", 
                         band = 3)

precip_2050_25_df <- as.data.frame(precip_2050_25, xy = TRUE)
precip_2050_75_df <- as.data.frame(precip_2050_75, xy = TRUE)

precip_perc_change <- precip_2050_25_df %>%
  as_tibble() %>%
  rename(precip_clim1 = Precip_RCP85_2050s_corrected) %>%
  difference_full_join(precip_base_df, by = c("x", "y"), max_dist = .001) %>%
  dplyr::select(x = x.x, y = y.x, precip_clim0 = PRECIP_BASELINE_corrected_2, precip_clim1) %>%
  full_join(precip_2050_75_df) %>%
  rename(precip_clim2 = Precip_RCP85_2050s_corrected) %>%
  mutate(perc_change1 = 100* (precip_clim1 - precip_clim0) / precip_clim0,
         perc_change2 = 100* (precip_clim2 - precip_clim0) /precip_clim0) #%>%
  #filter(!is.na(perc_change1))

precip_perc_change

# let do: growths is warm, loss is cool (matches some of jade's slides)
## except... if it gets more rainy, shouldn't it be bluer?

ggplot() +
  geom_raster(data = precip_perc_change, aes(x = x, y = y, fill = perc_change1)) +
  geom_sf(data = aoi_32, fill = NA, col = "black") +
  geom_sf(data = coastline, col = "black") +
  scale_fill_distiller(palette = "YlOrBr", 
                       name = "Precipitation \n(% change)",
                       na.value = "gray90",
                       labels = percent_format(scale = 1, accuracy = 1)) +
  coord_sf(xlim = c(220000, 700000),
           ylim = c(1700000, 2420000)) +
  labs(title = "RCP 8.5 2050s (25th Percentile)") +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.background = element_rect(fill = "gray95"),
        legend.margin = margin(2, 2, 2, 2),
        legend.position = c(.8, .4),
        plot.title = element_text(hjust = .5, size = 15))

# write it out
ggsave("../Deliverables/figs/ClimateMaps/precip_clim1.png", width = 6, height = 6.4, units = "in")
knitr::plot_crop("../Deliverables/figs/ClimateMaps/precip_clim1.png")

ggplot() +
  geom_raster(data = precip_perc_change, aes(x = x, y = y, fill = perc_change2)) +
  geom_sf(data = aoi_32, fill = NA, col = "black") +
  geom_sf(data = coastline, col = "black") +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction =1,
                       name = "Precipitation \n(% change)",
                       na.value = "gray90",
                       labels = percent_format(scale = 1, accuracy = 1)) +
  coord_sf(xlim = c(220000, 700000),
           ylim = c(1700000, 2420000)) +
  labs(title = "RCP 8.5 2050s (75th Percentile)") +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.background = element_rect(fill = "gray95"),
        legend.margin = margin(2, 2, 2, 2),
        legend.position = c(.8, .4),
        plot.title = element_text(hjust = .5, size = 15))

# write it out
ggsave("../Deliverables/figs/ClimateMaps/precip_clim2.png", width = 6, height = 6.4, units = "in")
knitr::plot_crop("../Deliverables/figs/ClimateMaps/precip_clim2.png")

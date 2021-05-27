###
### Making maps showing the hot days climate variables
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

hotdays_base <- raster("Predictors/Climate/Climate from Columbia/RastersSGW_WGS/MaxTempDaysAbove35C_BASELINE.tif",
                      band = 1)

hotdays_base_df <- as.data.frame(hotdays_base, xy = TRUE)
hotdays_base_df

ggplot() +
  geom_raster(data = hotdays_base_df, aes(x = x, y = y, fill = MaxTempDaysAbove35C_BASELINE)) +
  geom_sf(data = aoi_32, fill = NA, col = "black") +
  geom_sf(data = coastline, col = "black") +
  scale_fill_distiller(palette = "Oranges", 
                       name = "Days >35C \n(per year)", 
                       limits = c(-13, 220),
                       direction = 1,
                       na.value = "gray90") +
  coord_sf(xlim = c(220000, 680000),
           ylim = c(1705000, 2420000)) +
  labs(title = "Baseline") +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.background = element_rect(fill = "gray95"),
        legend.margin = margin(2, 2, 7, 2),
        legend.position = c(.82, .425),
        plot.title = element_text(hjust = .5, size = 15))


# write it out
ggsave("../Deliverables/figs/ClimateMaps/hotdays_baseline.png", width = 6, height = 6.4, units = "in")
knitr::plot_crop("../Deliverables/figs/ClimateMaps/hotdays_baseline.png")


## Ok. read in climate ones and calculate % change
# Note that choosing "band = 2" pulls in the 25th percentile data. Band = 3 corresponds to 75th percentile
hotdays_2050_25 <- raster("Predictors/Climate/Climate from Columbia/RastersSGW_WGS/MaxTempDaysAbove35C_RCP85_2050s.tif", 
                         band = 2)
hotdays_2050_75 <- raster("Predictors/Climate/Climate from Columbia/RastersSGW_WGS/MaxTempDaysAbove35C_RCP85_2050s.tif", 
                         band = 3)

hotdays_2050_25_df <- as.data.frame(hotdays_2050_25, xy = TRUE)
hotdays_2050_75_df <- as.data.frame(hotdays_2050_75, xy = TRUE)

hotdays_perc_change <- hotdays_2050_25_df %>%
  as_tibble() %>%
  rename(hotdays_clim1 = MaxTempDaysAbove35C_RCP85_2050s) %>%
  difference_full_join(hotdays_base_df, by = c("x", "y"), max_dist = .001) %>%
  dplyr::select(x = x.x, y = y.x, hotdays_clim0 = MaxTempDaysAbove35C_BASELINE, hotdays_clim1) %>%
  full_join(hotdays_2050_75_df) %>%
  rename(hotdays_clim2 = MaxTempDaysAbove35C_RCP85_2050s) %>%
  mutate(perc_change1 = 100* (hotdays_clim1 - hotdays_clim0) / hotdays_clim0,
         perc_change2 = 100* (hotdays_clim2 - hotdays_clim0) /hotdays_clim0) #%>%
  #filter(!is.na(perc_change1))

hotdays_perc_change

# let do: growths is warm, loss is cool (matches some of jade's slides)
## except... if it gets more rainy, shouldn't it be bluer?

## Right. % change doesn't work if the baseline is 0. Let's plot the actual number of hot days?

ggplot() +
  geom_raster(data = hotdays_perc_change, aes(x = x, y = y, fill = hotdays_clim1)) +
  geom_sf(data = aoi_32, fill = NA, col = "black") +
  geom_sf(data = coastline, col = "black") +
  scale_fill_distiller(palette = "Oranges", 
                       name = "Days >35C \n(per year)", 
                       limits = c(-13, 220),
                       direction = 1,
                       na.value = "gray90") +
  labs(title = "RCP 8.5 2050s (25th Percentile)") +
  coord_sf(xlim = c(220000, 680000),
           ylim = c(1705000, 2420000)) +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.background = element_rect(fill = "gray95"),
        legend.margin = margin(2, 2, 7, 2),
        legend.position = c(.82, .425),
        plot.title = element_text(hjust = .5, size = 15))

# write it out
ggsave("../Deliverables/figs/ClimateMaps/hotdays_clim1.png", width = 6, height = 6.4, units = "in")
knitr::plot_crop("../Deliverables/figs/ClimateMaps/hotdays_clim1.png")

ggplot() +
  geom_raster(data = hotdays_perc_change, aes(x = x, y = y, fill = hotdays_clim2)) +
  geom_sf(data = aoi_32, fill = NA, col = "black") +
  geom_sf(data = coastline, col = "black") +
  scale_fill_distiller(palette = "Oranges", 
                       name = "Days >35C \n(per year)", 
                       limits = c(-13, 220),
                       direction = 1,
                       na.value = "gray90") +
  labs(title = "RCP 8.5 2050s (75th Percentile)") +
  coord_sf(xlim = c(220000, 680000),
           ylim = c(1705000, 2420000)) +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.background = element_rect(fill = "gray95"),
        legend.margin = margin(2, 2, 7, 2),
        legend.position = c(.82, .425),
        plot.title = element_text(hjust = .5, size = 15))

# write it out
ggsave("../Deliverables/figs/ClimateMaps/hotdays_clim2.png", width = 6, height = 6.4, units = "in")
knitr::plot_crop("../Deliverables/figs/ClimateMaps/hotdays_clim2.png")

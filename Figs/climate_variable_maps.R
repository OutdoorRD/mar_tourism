###
### Maps of Climate Variables w/ tourism AOI
### 5/5/21
###

library(sf)
library(tidyverse)

setwd("~/Documents/MAR/")

# read in my climate by pid csv
climate <- read_csv("mar_tourism/Data/Future_Climate_RCP85_2050s_and_Current.csv")

# and aoi
aoi <- read_sf("ModelRuns/baseline_20200715/T_AOI_v4_5k_32616_pid.shp")
aoi_out <- read_sf("GIS/AOI/AOI_v4/Tourism_AOI_v4.shp")

# calculate change from baselines
climate %>%
  mutate(precip25_percchange = 100*(precip25 - precip0) / precip0)

# let's break these apart by variable first
precip <- climate %>%
  select(pid, starts_with("precip")) %>%
  pivot_longer(-c(pid, precip0), names_to = "climate", names_prefix = "precip", values_to = "precip") %>%
  mutate(perc_change = 100*(precip - precip0) / precip0)

# join to spatial and plot
precip_sf <- aoi %>%
  left_join(precip, by = "pid")

ggplot(precip_sf) +
  #geom_sf(data = aoi_out) +
  geom_sf(aes(fill = perc_change), size = 0.05) +
  scale_fill_gradientn(colours = c(scales::muted("red"), "white", scales::muted("blue")),
                       values = c(0, 
                                  scales::rescale(0, from = c(min(precip_sf$perc_change),
                                                              max(precip_sf$perc_change))),
                                  1),
                       name = "% Change in Precipitation",
                       breaks = c(-20, -10, 0, 10, 20)) +
  facet_wrap(~climate)

# maybe better if i just do one at a time?
ggplot(precip_sf %>% filter(climate == "25")) +
  geom_sf(aes(fill = perc_change), size = .1)


# Ok. It may be better to make maps using the rasters I created, plus the country outline
# plus the tourism aoi outline


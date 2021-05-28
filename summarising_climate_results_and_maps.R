####### THIS IS ONLY THE SECOND HALF OF THE OLD SCRIPT...
## NEED TO READ IN THE CLIMATE FILES FROM SCENARIOS/CLIMATE AND BIND THEM TOGETHER
## Then the following visualizing code should work well enough (and can be modified to be more useful)

library(tidyverse)
library(sf)
library(scales)
library(knitr)

setwd("~/Documents/MAR/")
dddd <- gsub("-", "", Sys.Date())

aoi <- read_sf("GIS/AOI/AOI_v4/Tourism_AOI_v4.shp")
coastline <- read_sf("GIS/BordersandProtectedAreas/mar_coastline.shp")
aoi_32 <- st_transform(aoi, crs = 32616)

clim1 <- read_sf("Scenarios/Climate/MARwide_climate_noact_rec_clim1.geojson")
clim1
# right... fitted_vis_current because it's meant to show current visitors, without the multiplier
clim2 <- read_sf("Scenarios/Climate/MARwide_climate_noact_rec_clim2.geojson")

## First, maybe try plotting current fitted vis?
ggplot(clim1) +
  geom_sf(aes(fill = est_vis), size = .01) +
  scale_fill_distiller(name = "Visitors in 2017 \n(Estimated)",
                       palette = "Greens", 
                       trans = "log1p",
                       breaks = c(0, 10, 100, 1000, 10000, 100000),
                       labels = label_comma(scale = 1, accuracy = 1),
                       direction = 1) +
  geom_sf(data = aoi_32, fill = NA) +
  geom_sf(data = coastline, size = .3, col = "gray25") +
  coord_sf(xlim = c(260000, 670000),
           ylim = c(1705000, 2420000)) +
  labs(title = "Baseline") +
  theme_void() +
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.background = element_rect(fill = "white"),
        legend.margin = margin(2, 2, 7, 2),
        legend.position = c(.85, .425),
        plot.title = element_text(hjust = .5, size = 15))

ggsave(paste0("Deliverables/figs/MAR_2017_estimated_vis_", dddd, ".png"), width = 6, height = 6.4, units = "in")
plot_crop(paste0("Deliverables/figs/MAR_2017_estimated_vis_", dddd, ".png"))

### What I do want, is maps of % change

ggplot(clim1) +
  geom_sf(aes(fill = perc_change), size = .01) +
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba'),
                       values = c(0, 
                                  scales::rescale(0, from = c(min(clim1$perc_change),
                                                              max(clim1$perc_change))),
                                  1),
                       name = "Tourism \n(% Change)",
                       breaks = c(-100, -50, 0, 50, 100, 150, 200, 250),
                       labels = percent_format(scale = 1, accuracy = 1)) +
  geom_sf(data = aoi_32, fill = NA) +
  geom_sf(data = coastline) +
  coord_sf(xlim = c(260000, 670000),
           ylim = c(1705000, 2420000)) +
  labs(title = "RCP 8.5 2050s (25th Percentile)") +
  theme_void() +
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=.5),
    legend.background = element_rect(fill = "white"),
    legend.margin = margin(3, 3, 3, 3),
    legend.position = c(.82, .425),
    plot.title = element_text(hjust = .5, size = 15))

ggsave(paste0("Deliverables/figs/futureVis/perc_change_map_clim1_", dddd, ".png"), width = 6, height = 6.4, units = "in")
plot_crop(paste0("Deliverables/figs/futureVis/perc_change_map_clim1_", dddd, ".png"))

ggplot(clim2) +
  geom_sf(aes(fill = perc_change), size = .01) +
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba'),
                       values = c(0, 
                                  scales::rescale(0, from = c(min(clim2$perc_change),
                                                              max(clim2$perc_change))),
                                  1),
                       name = "Tourism \n(% Change)",
                       breaks = c(-100, -50, 0, 50, 100, 150, 200, 250),
                       labels = percent_format(scale = 1, accuracy = 1)) +
  geom_sf(data = aoi_32, fill = NA) +
  geom_sf(data = coastline) +
  coord_sf(xlim = c(260000, 670000),
           ylim = c(1705000, 2420000)) +
  labs(title = "RCP 8.5 2050s (75th Percentile)") +
  theme_void() +
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=.5),
    legend.background = element_rect(fill = "white"),
    legend.margin = margin(3, 3, 3, 3),
    legend.position = c(.82, .425),
    plot.title = element_text(hjust = .5, size = 15))

ggsave(paste0("Deliverables/figs/futureVis/perc_change_map_clim2_", dddd, ".png"), width = 6, height = 6.4, units = "in")
plot_crop(paste0("Deliverables/figs/futureVis/perc_change_map_clim2_", dddd, ".png"))


summary(clim1$perc_change)
summary(clim2$perc_change)

## calculate % change across the mar?
clim1 %>%
  st_drop_geometry() %>%
  summarise_at(vars(fitted_vis_current, preds_vis_future), sum) %>%
  mutate(perc_change = (preds_vis_future - fitted_vis_current) / (fitted_vis_current))
# -28.7% change across the mar in clim1

clim2 %>%
  st_drop_geometry() %>%
  summarise_at(vars(fitted_vis_current, preds_vis_future), sum) %>%
  mutate(perc_change = (preds_vis_future - fitted_vis_current) / (fitted_vis_current))
# -65% change in clim2





################# OLD #####################


# visualizing the percent change
ggplot(modeled_sp) +
  geom_sf(aes(fill = perc_change), size = .1) +
  scale_fill_distiller(palette = "RdBu",
                       name = "Percent Change in Tourism",
                       #limit = max(abs(modeled_sp$perc_change)) * c(-1, 1),
                       limit = c(-100, 100)) +
  facet_wrap(~climate)

# visualizing the percent change
ggplot(modeled_sp) +
  geom_sf(aes(fill = perc_change_mult), size = .1) +
  scale_fill_distiller(palette = "RdBu",
                       limit = c(-200, 200),
                       #limit = max(abs(modeled_sp$perc_change_mult)) * c(-1, 1),
                       name = "Percent Change in Tourism (multi)") +
  facet_wrap(~climate)

## Making individual plots for each climate scen

# no change
ggplot(modeled_sp %>% filter(climate == "nochange")) +
  geom_sf(aes(fill = perc_change_mult), size = .1) +
  scale_fill_distiller(palette = "RdBu",
                       direction = 1,
                       limit = c(-200, 200),
                       #limit = max(abs(modeled_sp$perc_change_mult)) * c(-1, 1),
                       name = "% Change in Tourism") +
  labs(title = "Climate does not change")

ggplot(modeled_sp %>% filter(climate == "nochange")) +
  geom_sf(aes(fill = perc_change_mult), size = .1) +
  #scale_fill_distiller(palette = "RdBu",
  #                     direction = 1,
  #                     limit = c(-100, 300), 
  #                     #limit = max(abs(modeled_sp$perc_change_mult)) * c(-1, 1),
  #                     name = "% Change in Tourism") +
  #scale_fill_gradient2(midpoint = 0)
  scale_fill_gradientn(colours = c(scales::muted("red"), "white", scales::muted("blue")),
                       values = c(0, 
                                  scales::rescale(0, from = c(min(modeled_sp$perc_change_mult[modeled_sp$climate == "75Perc"]),
                                                              max(modeled_sp$perc_change_mult[modeled_sp$climate == "75Perc"]))),
                                  1),
                       name = "% Change in Tourism",
                       breaks = c(-100, -50, 0, 50, 100, 150, 200, 250)) +
  #values = scales::rescale(c(min(modeled_sp$perc_change_mult[modeled_sp$climate == "25Perc"]), 
  #                           0, 
  #                           max(modeled_sp$perc_change_mult[modeled_sp$climate == "25Perc"])))) +
  #scale_fill_gradient2()
  labs(title = "RCP 8.5 - 75th Percentile")

#ggsave("Deliverables/figs/futureVis/perc_change_map_clim0.png", width = 5, height = 5, units = "in")

ggplot(modeled_sp %>% filter(climate == "25Perc")) +
  geom_sf(aes(fill = perc_change_mult), size = .1) +
  #scale_fill_distiller(palette = "RdBu",
  #                     direction = 1,
  #                     limit = c(-100, 300), 
  #                     #limit = max(abs(modeled_sp$perc_change_mult)) * c(-1, 1),
  #                     name = "% Change in Tourism") +
  scale_fill_gradientn(colours = c(scales::muted("red"), "white", scales::muted("blue")),
                       values = c(0, 
                                  scales::rescale(0, from = c(min(modeled_sp$perc_change_mult[modeled_sp$climate == "25Perc"]),
                                                                 max(modeled_sp$perc_change_mult[modeled_sp$climate == "25Perc"]))),
                                  1),
                       name = "% Change in Tourism",
                       breaks = c(-100, -50, 0, 50, 100, 150, 200, 250)) +
                       #values = scales::rescale(c(min(modeled_sp$perc_change_mult[modeled_sp$climate == "25Perc"]), 
                       #                           0, 
                       #                           max(modeled_sp$perc_change_mult[modeled_sp$climate == "25Perc"])))) +
  #scale_fill_gradient2()
  labs(title = "RCP 8.5 - 25th Percentile")

ggsave("Deliverables/figs/futureVis/perc_change_map_clim1.png", width = 5, height = 5, units = "in")

ggplot(modeled_sp %>% filter(climate == "75Perc")) +
  geom_sf(aes(fill = perc_change_mult), size = .1) +
  #scale_fill_distiller(palette = "RdBu",
  #                     direction = 1,
  #                     limit = c(-100, 300), 
  #                     #limit = max(abs(modeled_sp$perc_change_mult)) * c(-1, 1),
  #                     name = "% Change in Tourism") +
  scale_fill_gradientn(colours = c(scales::muted("red"), "white", scales::muted("blue")),
                       values = c(0, 
                                  scales::rescale(0, from = c(min(modeled_sp$perc_change_mult[modeled_sp$climate == "75Perc"]),
                                                              max(modeled_sp$perc_change_mult[modeled_sp$climate == "75Perc"]))),
                                  1),
                       name = "% Change in Tourism",
                       breaks = c(-100, -50, 0, 50, 100, 150, 200, 250)) +
  #values = scales::rescale(c(min(modeled_sp$perc_change_mult[modeled_sp$climate == "25Perc"]), 
  #                           0, 
  #                           max(modeled_sp$perc_change_mult[modeled_sp$climate == "25Perc"])))) +
  #scale_fill_gradient2()
  labs(title = "RCP 8.5 - 75th Percentile")

ggsave("Deliverables/figs/futureVis/perc_change_map_clim2.png", width = 5, height = 5, units = "in")


modeled_sp
# visualizing the log diffs
ggplot(modeled_sp) +
  geom_sf(aes(fill = diff_log)) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~climate)

# visualizing the  diffs
ggplot(modeled_sp) +
  geom_sf(aes(fill = log1p(-diff_vis))) +
  scale_fill_viridis_c() +
  facet_wrap(~climate)

# summarising the range of diff_vis
modeled_climate %>%
  group_by(climate) %>%
  summarise(average_change = mean(diff_vis),
            median_change = median(diff_vis),
            max_positive_change = max(diff_vis),
            max_negative_change = min(diff_vis))

## % change across the mar?
modeled_climate %>%
  group_by(climate) %>%
  summarise_at(vars(fitted_vis, preds_vis_mult), sum) %>%
  mutate(perc_change = (preds_vis_mult - fitted_vis) / (fitted_vis))

# Wow. a drop of 35% across the MAR in the 25th perc pred and almost 70% in the 75th perc pred

# what about if we don't include the 2.4 multiplier up top?
modeled_climate %>%
  group_by(climate) %>%
  summarise_at(vars(fitted_vis, preds_vis), sum) %>%
  mutate(perc_change = (preds_vis - fitted_vis) / (fitted_vis))
# 73% and 87% loss respectively


# these are depressing results. I wonder how far outside of the observed climate I'm predicting?
base_climate

climate_tall <- base_climate %>%
  select(ends_with(c("0", "5")), -starts_with("coral")) %>%
  pivot_longer(cols = precip0:temp75,
               names_to = c("variable", "climate"), 
               names_pattern = "([:alpha:]*)([:digit:]*)",
               values_to = "measure")


ggplot(climate_tall %>% filter(variable == "hotdays")) +
  geom_density(aes(x = measure, col = climate))

ggplot(climate_tall %>% filter(variable == "temp")) +
  geom_density(aes(x = measure, col = climate))

ggplot(climate_tall %>% filter(variable == "precip")) +
  geom_density(aes(x = measure, col = climate))

## histograms
ggplot(climate_tall %>% filter(variable == "hotdays")) +
  geom_histogram(aes(x = measure, fill = climate))

ggplot(climate_tall %>% filter(variable == "temp")) +
  geom_histogram(aes(x = measure, fill = climate))

ggplot(climate_tall %>% filter(variable == "precip")) +
  geom_histogram(aes(x = measure, fill = climate))

## So I'm predicting well out of the range of existing conditions
# let's join back on to conditions to see how this looks in marginal plots
combined <- modeled_climate %>%
  left_join(base_climate)

ggplot(combined) +
  geom_point(aes(x = temp25, y = preds), col = "blue") +
  geom_point(aes(x = temp75, y = preds), col = "red") +
  geom_line(aes(x = temp0, y = fitted)) +
  facet_wrap(~climate)

ggplot(combined) +
  geom_point(aes(x = hotdays25, y = preds), col = "blue") +
  geom_point(aes(x = hotdays75, y = preds), col = "red") +
  geom_line(aes(x = hotdays0, y = fitted)) +
  facet_wrap(~climate)

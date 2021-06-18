#### Creates maps of % change under climate scenarios
## Also planning to add bar charts of change
### Updated 6/16/21

library(tidyverse)
library(sf)
library(scales)
library(knitr)

setwd("~/Documents/MAR/")
dddd <- gsub("-", "", Sys.Date())

aoi <- read_sf("GIS/AOI/AOI_v4/Tourism_AOI_v4.shp")
coastline <- read_sf("GIS/BordersandProtectedAreas/mar_coastline.shp")
aoi_32 <- st_transform(aoi, crs = 32616)
mpa_boundaries <- read_sf("~/Documents/MAR/GIS/BordersandProtectedAreas/MPA/MPA_Updates_July_2020/MPA_Network_WGS8416N_v3.shp")

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
  geom_sf(data = mpa_boundaries, fill = NA, col = "black", lwd = .3) +
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
  geom_sf(data = coastline, size = .3, col = "gray25") +
  geom_sf(data = mpa_boundaries, fill = NA, col = "black", lwd = .5) +
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
  geom_sf(data = coastline, size = .3, col = "gray25") +
  geom_sf(data = mpa_boundaries, fill = NA, col = "black", lwd = .5) +
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

#####
### Ok, let's also try plotting absolute change
library(ggallin)

ggplot(clim1) +
  geom_sf(aes(fill = diff_vis), size = .01) +
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba'),
                       trans = pseudolog10_trans,
                       values = c(0, 
                                  scales::rescale(0, from = c(min(asinh(clim1$diff_vis/2)/log(10)),
                                                              max(asinh(clim1$diff_vis/2)/log(10)))),
                                  1),
                       name = "Tourism \n(Annual Change)",
                       breaks = c(-50000, -1000, -50, 0, 50, 1000)#,
                       #labels = percent_format(scale = 1, accuracy = 1)
                       ) +
  geom_sf(data = aoi_32, fill = NA) +
  #geom_sf(data = coastline, size = .2, col = "gray50") +
  geom_sf(data = coastline, size = .3, col = "gray50") +
  geom_sf(data = mpa_boundaries, fill = NA, col = "black", lwd = .5) +
  coord_sf(xlim = c(260000, 670000),
           ylim = c(1705000, 2420000)) +
  labs(title = "RCP 8.5 2050s (25th Percentile)") +
  theme_void() +
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=.5),
    legend.background = element_rect(fill = "white"),
    legend.margin = margin(3, 3, 3, 3),
    legend.position = c(.82, .425),
    plot.title = element_text(hjust = .5, size = 15))

ggsave(paste0("Deliverables/figs/futureVis/absolute_change_map_clim1_", dddd, ".png"), width = 6, height = 6.4, units = "in")
plot_crop(paste0("Deliverables/figs/futureVis/absolute_change_map_clim1_", dddd, ".png"))

## clim 2
ggplot(clim2) +
  geom_sf(aes(fill = diff_vis), size = .01) +
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba'),
                       trans = pseudolog10_trans,
                       values = c(0, 
                                  scales::rescale(0, from = c(min(asinh(clim2$diff_vis/2)/log(10)),
                                                              max(asinh(clim2$diff_vis/2)/log(10)))),
                                  1),
                       name = "Tourism \n(Annual Change)",
                       breaks = c(-50000, -1000, -50, 0, 50, 1000)#,
                       #labels = percent_format(scale = 1, accuracy = 1)
  ) +
  geom_sf(data = aoi_32, fill = NA) +
  geom_sf(data = coastline, size = .3, col = "gray50") +
  geom_sf(data = mpa_boundaries, fill = NA, col = "black", lwd = .5) +
  coord_sf(xlim = c(260000, 670000),
           ylim = c(1705000, 2420000)) +
  labs(title = "RCP 8.5 2050s (75th Percentile)") +
  theme_void() +
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=.5),
    legend.background = element_rect(fill = "white"),
    legend.margin = margin(3, 3, 3, 3),
    legend.position = c(.82, .425),
    plot.title = element_text(hjust = .5, size = 15))

ggsave(paste0("Deliverables/figs/futureVis/absolute_change_map_clim2_", dddd, ".png"), width = 6, height = 6.4, units = "in")
plot_crop(paste0("Deliverables/figs/futureVis/absolute_change_map_clim2_", dddd, ".png"))

##########
### Ok. And, let's make some bar charts, and test country/mpa level maps

clim1
# first I need to create a single tibble w/ current, fitted, and future for both climates
clim_comb <- clim1 %>%
  mutate(climate = "clim1") %>%
  bind_rows(clim2 %>% mutate(climate = "clim2"))

# first, let's calculate the mar wide summaries
mar_summaries <- clim_comb %>%
  st_drop_geometry() %>%
  group_by(climate) %>%
  summarise_at(vars(est_vis, fitted_vis_current, preds_vis_future), sum) %>%
  mutate(perc_change = (preds_vis_future - fitted_vis_current) / (fitted_vis_current))

ggplot(mar_summaries) +
  geom_col(aes(x = climate, y = preds_vis_future))

## let's make a column for without climate change, and also stack these

# by hand, cuz it's tricky
mar_tall <- mar_summaries %>%
  select(climate, preds_vis_future) %>%
  bind_rows(tibble(climate = c("clim0", "current"), 
                   preds_vis_future = c(mar_summaries$fitted_vis_current[1]*2.67,
                                        mar_summaries$fitted_vis_current[1])))
ggplot(mar_tall) +
  geom_col(aes(x = climate, y = preds_vis_future))

## Note that my fitted vis is 1/4 of the "estimated" current vis. 
## Question: is it legit to use the calculated % changes and then apply them to the estimated vis numbers???

## Then I could make plots that would have the numbers which are more familiar to people

## let's see
# Mar wide
mar_summaries
# I estimate ~8 million visitors

mar_tall_from_est_vis <- tibble(current = mar_summaries$est_vis[1],
       clim0 = current*2.67,
       clim1 = current*as.numeric(1+mar_summaries[mar_summaries$climate == "clim1", "perc_change"]),
       clim2 = current*as.numeric(1+mar_summaries[mar_summaries$climate == "clim2", "perc_change"])) %>%
  pivot_longer(everything(), names_to = "climate", values_to = "estimated_visitors") %>%
  mutate(climate = fct_relevel(climate, levels = c("current", "clim0", "clim1", "clim2")))

ggplot(mar_tall_from_est_vis) +
  geom_col(aes(x = climate, y = estimated_visitors))
# Ok. really the same thing, just with a new legend.

ggplot(mar_tall_from_est_vis) +
  geom_col(aes(x = climate, y = (estimated_visitors/1000000), fill = climate), position = "dodge") +
  scale_x_discrete(name = "Time & Climate Scenario",
                   labels = c("2017", "2050 \n(No climate change)", 
                              "2050 \n(25th Percentile)", "2050 \n(75th Percentile)")) +
  scale_y_continuous(name = "Estimated Annual Visitors (millions)") +
  scale_fill_brewer(palette = "Paired", direction = -1, guide = NULL) +
  labs(title = paste0("Visitation to the MAR region")) +
  theme_classic()
ggsave(paste0("Deliverables/figs/futureVis/abs_change_barchart_MAR.png"), width = 5, height = 4, units = "in")

# can I do the same thing easily at smaller scales?

# countries?
country_summaries <- clim_comb %>%
  st_drop_geometry() %>%
  group_by(climate, CNTRY_NAME) %>%
  summarise_at(vars(est_vis, fitted_vis_current, preds_vis_future), sum) %>%
  mutate(perc_change = (preds_vis_future - fitted_vis_current) / (fitted_vis_current))
country_summaries

# ok... let's calculate future estimates
country_tall_from_est_vis <- country_summaries %>%
  pivot_wider(-c(fitted_vis_current, preds_vis_future), names_from = "climate", values_from = "perc_change") %>%
  rename(current = est_vis) %>%
  mutate(clim0 = current*2.67,
         clim1 = current*(1+ clim1),
         clim2 = current*(1 +clim2)) %>%
  pivot_longer(-c(CNTRY_NAME), names_to = "climate", values_to = "estimated_visitors") %>%
  mutate(climate = fct_relevel(climate, levels = c("current", "clim0", "clim1", "clim2")))


ggplot(country_tall_from_est_vis) +
  geom_col(aes(x = CNTRY_NAME, y = estimated_visitors, fill = climate), position = "dodge") +
  facet_wrap(~CNTRY_NAME, scales = "free")



### Ok, this is feeling ok

# let's clean them up and make one per country

country <- "Mexico"
countries <- c("Belize", "Mexico", "Honduras", "Guatemala")
for(country in countries){
  ggplot(country_tall_from_est_vis %>% filter(CNTRY_NAME == country)) +
    geom_col(aes(x = climate, y = (estimated_visitors/1000000), fill = climate), position = "dodge") +
    scale_x_discrete(name = "Time & Climate Scenario",
                     labels = c("2017", "2050 \n(No climate change)", 
                                "2050 \n(25th Percentile)", "2050 \n(75th Percentile)")) +
    scale_y_continuous(name = "Estimated Annual Visitors (millions)") +
    scale_fill_brewer(palette = "Paired", direction = -1, guide = NULL) +
    labs(title = paste0("Visitation to the MAR region of ", country)) +
    theme_classic()
  ggsave(paste0("Deliverables/figs/futureVis/abs_change_barchart_", country, ".png"), width = 5, height = 4, units = "in") 
}

## how about at the MPA level?
# first, I need to read in a joinkey that has the mpa names
aoi_grid <- read_sf("ModelRuns/baseline_20200715/T_AOI_v4_5k_32616_pid.shp") %>% 
  st_drop_geometry() %>%
  select(-area)
# and the short names
# Read in list of short MPA names
mpas <- read_csv("ModelRuns/baseline_20200715/listofMPAs_v2.csv")

aoi_grid

# join it on, then calculate % changes
mpa_summaries <- clim_comb %>%
  left_join(aoi_grid) %>%
  left_join(mpas %>% select(-Country), by = c("name_2" = "NAME")) %>%
  st_drop_geometry() %>%
  group_by(climate, CNTRY_NAME, MPA = Name_short) %>%
  summarise_at(vars(est_vis, fitted_vis_current, preds_vis_future), sum) %>%
  mutate(perc_change = (preds_vis_future - fitted_vis_current) / (fitted_vis_current))
mpa_summaries

## ok. let's just start by looking at est_vis vs fitted_vis
# how about plotting % change?

ggplot(mpa_summaries) +
  geom_col(aes(x = MPA, y = perc_change, fill = climate), position = "dodge") +
  coord_flip() 

## ok, clean this up and write out by country
country <- "Guatemala"
countries <- c("Belize", "Mexico", "Honduras", "Guatemala")
for(country in countries){
    ggplot(mpa_summaries %>% filter(CNTRY_NAME == country, !is.na(MPA))) +
    geom_col(aes(x = reorder(MPA, perc_change), y = perc_change*100, fill = rev(climate)), position = "dodge") +
    geom_hline(yintercept = 0) +
    scale_fill_brewer(name = "Climate (RCP 8.5 2050s)",
                      palette = "Paired", breaks = c("clim1", "clim2"), 
                      labels = c("75th Percentile", "25th Percentile"), # note this is backwards because of reverse above
                      guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(name = "Percent Change from 2017", labels = percent_format(scale = 1, accuracy = 1)) +
    scale_x_discrete(name = NULL) +
    labs(title = paste0("Estimated % Change to ", country, " MPAs")) +
    coord_flip() +
    theme_classic()
  
  # write it out
  ggsave(paste0("Deliverables/figs/futureVis/perc_change_barchart_", country, ".png"), width = 7, height = 5, units = "in")
}


#### country level maps
# it would be good to make country level maps

# Mexico clim1
ggplot(clim1 %>% filter(CNTRY_NAME == "Mexico")) +
  geom_sf(aes(fill = perc_change), size = .01) +
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba'),
                       values = c(0, 
                                  scales::rescale(0, from = c(min(clim1$perc_change),
                                                              max(clim1$perc_change))),
                                  1),
                       name = "Tourism \n(% Change)",
                       breaks = c(-100, -50, 0, 50, 100, 150, 200, 250),
                       labels = percent_format(scale = 1, accuracy = 1)) +
  geom_sf(data = coastline, col = "gray50") +
  geom_sf(data = mpa_boundaries, fill = NA, col = "black", lwd = .5) +
 # geom_sf(data = aoi_32, fill = NA) +
  coord_sf(xlim = c(-89.0, -86.2), ylim = c(20.8, 22.25), crs = 4326) +
  labs(title = "RCP 8.5 2050s (25th Percentile)") +
  theme_void() +
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=.5),
    legend.background = element_rect(fill = "white"),
    legend.margin = margin(3, 3, 3, 3),
    #legend.position = c(.82, .425),
    plot.title = element_text(hjust = .5, size = 15))
## not used yet...

# is absolute better?

### Mexico Clim2

ggplot(clim2 %>% filter(CNTRY_NAME == "Mexico")) +
  geom_sf(aes(fill = diff_vis), size = .01) +
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba'),
                       trans = pseudolog10_trans,
                       values = c(0, 
                                  scales::rescale(0, from = c(min(asinh(clim2[clim2$CNTRY_NAME=="Mexico",]$diff_vis/2)/log(10)),
                                                              max(asinh(clim2[clim2$CNTRY_NAME=="Mexico",]$diff_vis/2)/log(10)))),
                                  1),
                       name = "Tourism \n(Annual Change)",
                       breaks = c(-50000, -1000, -50, 0, 5, 50, 1000)#,
                       #labels = percent_format(scale = 1, accuracy = 1)
  ) +
  geom_sf(data = aoi_32, fill = NA) +
  geom_sf(data = mpa_boundaries, fill = NA, col = "black", lwd = .5) +
  geom_sf(data = coastline, size = .2, col = "gray50") +
  # geom_sf(data = aoi_32, fill = NA) +
  coord_sf(xlim = c(-89.0, -86.2), ylim = c(20.8, 22.25), crs = 4326) +
  labs(title = "RCP 8.5 2050s (75th Percentile)") +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
    legend.background = element_rect(fill = "white"),
    legend.margin = margin(3, 3, 3, 3),
    #legend.position = c(.82, .425),
    plot.title = element_text(hjust = .5, size = 15))

ggsave(paste0("Deliverables/figs/futureVis/absolute_change_map_clim2_Mexico_", dddd, ".png"), width = 6, height = 6.4, units = "in")
plot_crop(paste0("Deliverables/figs/futureVis/absolute_change_map_clim2_Mexico_", dddd, ".png"))

## Mexico clim1
ggplot(clim1 %>% filter(CNTRY_NAME == "Mexico")) +
  geom_sf(aes(fill = diff_vis), size = .01) +
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba'),
                       trans = pseudolog10_trans,
                       values = c(0, 
                                  scales::rescale(0, from = c(min(asinh(clim1[clim1$CNTRY_NAME=="Mexico",]$diff_vis/2)/log(10)),
                                                              max(asinh(clim1[clim1$CNTRY_NAME=="Mexico",]$diff_vis/2)/log(10)))),
                                  1),
                       name = "Tourism \n(Annual Change)",
                       breaks = c(-50000, -1000, -50, 0, 5, 50, 1000)#,
                       #labels = percent_format(scale = 1, accuracy = 1)
  ) +
  geom_sf(data = aoi_32, fill = NA) +
  geom_sf(data = mpa_boundaries, fill = NA, col = "black", lwd = .5) +
  geom_sf(data = coastline, size = .2, col = "gray50") +
  # geom_sf(data = aoi_32, fill = NA) +
  coord_sf(xlim = c(-89.0, -86.2), ylim = c(20.8, 22.25), crs = 4326) +
  labs(title = "RCP 8.5 2050s (25th Percentile)") +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
    legend.background = element_rect(fill = "white"),
    legend.margin = margin(3, 3, 3, 3),
    #legend.position = c(.82, .425),
    plot.title = element_text(hjust = .5, size = 15))

ggsave(paste0("Deliverables/figs/futureVis/absolute_change_map_clim1_Mexico_", dddd, ".png"), width = 6, height = 6.4, units = "in")
plot_crop(paste0("Deliverables/figs/futureVis/absolute_change_map_clim1_Mexico_", dddd, ".png"))

### Belize clim2
ggplot(clim2 %>% filter(CNTRY_NAME == "Belize")) +
  geom_sf(aes(fill = diff_vis), size = .01) +
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba'),
                       trans = pseudolog10_trans,
                       values = c(0, 
                                  scales::rescale(0, from = c(min(asinh(clim2[clim2$CNTRY_NAME=="Belize",]$diff_vis/2)/log(10)),
                                                              max(asinh(clim2[clim2$CNTRY_NAME=="Belize",]$diff_vis/2)/log(10)))),
                                  1),
                       name = "Tourism \n(Annual Change)",
                       breaks = c(-50000, -1000, -50, 0, 5, 50, 1000)#,
                       #labels = percent_format(scale = 1, accuracy = 1)
  ) +
  geom_sf(data = aoi_32, fill = NA) +
  geom_sf(data = mpa_boundaries, fill = NA, col = "black", lwd = .5) +
  geom_sf(data = coastline, size = .2, col = "gray50") +
  # geom_sf(data = aoi_32, fill = NA) +
  coord_sf(xlim = c(-89.5, -87), ylim = c(15.75, 18.5), crs = 4326) +
  labs(title = "RCP 8.5 2050s (75th Percentile)") +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
    legend.background = element_rect(fill = "white"),
    legend.margin = margin(3, 3, 3, 3),
    #legend.position = c(.82, .425),
    plot.title = element_text(hjust = .5, size = 15))

ggsave(paste0("Deliverables/figs/futureVis/absolute_change_map_clim2_Belize_", dddd, ".png"), width = 6, height = 6.4, units = "in")
plot_crop(paste0("Deliverables/figs/futureVis/absolute_change_map_clim2_Belize_", dddd, ".png"))

## Belize clim1
### Belize clim1
ggplot(clim1 %>% filter(CNTRY_NAME == "Belize")) +
  geom_sf(aes(fill = diff_vis), size = .01) +
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba'),
                       trans = pseudolog10_trans,
                       values = c(0, 
                                  scales::rescale(0, from = c(min(asinh(clim1[clim1$CNTRY_NAME=="Belize",]$diff_vis/2)/log(10)),
                                                              max(asinh(clim1[clim1$CNTRY_NAME=="Belize",]$diff_vis/2)/log(10)))),
                                  1),
                       name = "Tourism \n(Annual Change)",
                       breaks = c(-50000, -1000, -50, 0, 5, 50, 1000)#,
                       #labels = percent_format(scale = 1, accuracy = 1)
  ) +
  geom_sf(data = aoi_32, fill = NA) +
  geom_sf(data = mpa_boundaries, fill = NA, col = "black", lwd = .5) +
  geom_sf(data = coastline, size = .2, col = "gray50") +
  # geom_sf(data = aoi_32, fill = NA) +
  coord_sf(xlim = c(-89.5, -87), ylim = c(15.75, 18.5), crs = 4326) +
  labs(title = "RCP 8.5 2050s (25th Percentile)") +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
    legend.background = element_rect(fill = "white"),
    legend.margin = margin(3, 3, 3, 3),
    #legend.position = c(.82, .425),
    plot.title = element_text(hjust = .5, size = 15))

ggsave(paste0("Deliverables/figs/futureVis/absolute_change_map_clim1_Belize_", dddd, ".png"), width = 6, height = 6.4, units = "in")
plot_crop(paste0("Deliverables/figs/futureVis/absolute_change_map_clim1_Belize_", dddd, ".png"))

### Guatemala clim2
### Guatemala clim2
ggplot(clim2 %>% filter(CNTRY_NAME == "Guatemala")) +
  geom_sf(aes(fill = diff_vis), size = .01) +
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba'),
                       trans = pseudolog10_trans,
                       values = c(0, 
                                  scales::rescale(0, from = c(min(asinh(clim2[clim2$CNTRY_NAME=="Guatemala",]$diff_vis/2)/log(10)),
                                                              max(asinh(clim2[clim2$CNTRY_NAME=="Guatemala",]$diff_vis/2)/log(10)))),
                                  1),
                       name = "Tourism \n(Annual Change)",
                       breaks = c(-50000, -1000, -50, 0, 5, 50, 1000)#,
                       #labels = percent_format(scale = 1, accuracy = 1)
  ) +
  geom_sf(data = aoi_32, fill = NA) +
  geom_sf(data = mpa_boundaries, fill = NA, col = "black", lwd = .5) +
  geom_sf(data = coastline, size = .2, col = "gray50") +
  # geom_sf(data = aoi_32, fill = NA) +
  coord_sf(xlim = c(-89.5, -88.2), ylim = c(15.25, 16.2), crs = 4326) +
  labs(title = "RCP 8.5 2050s (75th Percentile)") +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
    legend.background = element_rect(fill = "white"),
    legend.margin = margin(3, 3, 3, 3),
    #legend.position = c(.82, .425),
    plot.title = element_text(hjust = .5, size = 15))

ggsave(paste0("Deliverables/figs/futureVis/absolute_change_map_clim2_Guatemala_", dddd, ".png"), width = 6, height = 6.4, units = "in")
plot_crop(paste0("Deliverables/figs/futureVis/absolute_change_map_clim2_Guatemala_", dddd, ".png"))

### Guatemala clim1
ggplot(clim1 %>% filter(CNTRY_NAME == "Guatemala")) +
  geom_sf(aes(fill = diff_vis), size = .01) +
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba'),
                       trans = pseudolog10_trans,
                       values = c(0, 
                                  scales::rescale(0, from = c(min(asinh(clim1[clim1$CNTRY_NAME=="Guatemala",]$diff_vis/2)/log(10)),
                                                              max(asinh(clim1[clim1$CNTRY_NAME=="Guatemala",]$diff_vis/2)/log(10)))),
                                  1),
                       name = "Tourism \n(Annual Change)",
                       breaks = c(-50000, -1000, -50, 0, 5, 50, 1000)#,
                       #labels = percent_format(scale = 1, accuracy = 1)
  ) +
  geom_sf(data = aoi_32, fill = NA) +
  geom_sf(data = mpa_boundaries, fill = NA, col = "black", lwd = .5) +
  geom_sf(data = coastline, size = .2, col = "gray50") +
  # geom_sf(data = aoi_32, fill = NA) +
  coord_sf(xlim = c(-89.5, -88.2), ylim = c(15.25, 16.2), crs = 4326) +
  labs(title = "RCP 8.5 2050s (25th Percentile)") +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
    legend.background = element_rect(fill = "white"),
    legend.margin = margin(3, 3, 3, 3),
    #legend.position = c(.82, .425),
    plot.title = element_text(hjust = .5, size = 15))

ggsave(paste0("Deliverables/figs/futureVis/absolute_change_map_clim1_Guatemala_", dddd, ".png"), width = 6, height = 6.4, units = "in")
plot_crop(paste0("Deliverables/figs/futureVis/absolute_change_map_clim1_Guatemala_", dddd, ".png"))


### Honduras
###  clim2
ggplot(clim2 %>% filter(CNTRY_NAME == "Honduras")) +
  geom_sf(aes(fill = diff_vis), size = .01) +
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba'),
                       trans = pseudolog10_trans,
                       values = c(0, 
                                  scales::rescale(0, from = c(min(asinh(clim2[clim2$CNTRY_NAME=="Honduras",]$diff_vis/2)/log(10)),
                                                              max(asinh(clim2[clim2$CNTRY_NAME=="Honduras",]$diff_vis/2)/log(10)))),
                                  1),
                       name = "Tourism \n(Annual Change)",
                       breaks = c(-50000, -1000, -50, 0, 5, 50, 1000)#,
                       #labels = percent_format(scale = 1, accuracy = 1)
  ) +
  geom_sf(data = aoi_32, fill = NA) +
  geom_sf(data = mpa_boundaries, fill = NA, col = "black", lwd = .5) +
  geom_sf(data = coastline, size = .2, col = "gray50") +
  # geom_sf(data = aoi_32, fill = NA) +
  coord_sf(xlim = c(-88.6, -85.65), ylim = c(15.25, 16.75), crs = 4326) +
  labs(title = "RCP 8.5 2050s (75th Percentile)") +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
    legend.background = element_rect(fill = "white"),
    legend.margin = margin(3, 3, 3, 3),
    #legend.position = c(.82, .425),
    plot.title = element_text(hjust = .5, size = 15))

ggsave(paste0("Deliverables/figs/futureVis/absolute_change_map_clim2_Honduras_", dddd, ".png"), width = 6, height = 6.4, units = "in")
plot_crop(paste0("Deliverables/figs/futureVis/absolute_change_map_clim2_Honduras_", dddd, ".png"))

### honduras clim2
ggplot(clim1 %>% filter(CNTRY_NAME == "Honduras")) +
  geom_sf(aes(fill = diff_vis), size = .01) +
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba'),
                       trans = pseudolog10_trans,
                       values = c(0, 
                                  scales::rescale(0, from = c(min(asinh(clim1[clim1$CNTRY_NAME=="Honduras",]$diff_vis/2)/log(10)),
                                                              max(asinh(clim1[clim1$CNTRY_NAME=="Honduras",]$diff_vis/2)/log(10)))),
                                  1),
                       name = "Tourism \n(Annual Change)",
                       breaks = c(-50000, -1000, -50, 0, 5, 50, 1000)#,
                       #labels = percent_format(scale = 1, accuracy = 1)
  ) +
  geom_sf(data = aoi_32, fill = NA) +
  geom_sf(data = mpa_boundaries, fill = NA, col = "black", lwd = .5) +
  geom_sf(data = coastline, size = .2, col = "gray50") +
  # geom_sf(data = aoi_32, fill = NA) +
  coord_sf(xlim = c(-88.6, -85.65), ylim = c(15.25, 16.75), crs = 4326) +
  labs(title = "RCP 8.5 2050s (25th Percentile)") +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
    legend.background = element_rect(fill = "white"),
    legend.margin = margin(3, 3, 3, 3),
    #legend.position = c(.82, .425),
    plot.title = element_text(hjust = .5, size = 15))

ggsave(paste0("Deliverables/figs/futureVis/absolute_change_map_clim1_Honduras_", dddd, ".png"), width = 6, height = 6.4, units = "in")
plot_crop(paste0("Deliverables/figs/futureVis/absolute_change_map_clim1_Honduras_", dddd, ".png"))

####### maybes...
# ok, and then doing the estimated number of people?
# ok... let's calculate future estimates
mpa_tall_from_est_vis <- mpa_summaries %>%
  pivot_wider(-c(fitted_vis_current, preds_vis_future), names_from = "climate", values_from = "perc_change") %>%
  rename(current = est_vis) %>%
  mutate(clim0 = current*2.67,
         clim1 = current*(1+ clim1),
         clim2 = current*(1 +clim2)) %>%
  pivot_longer(-c(CNTRY_NAME, MPA), names_to = "climate", values_to = "estimated_visitors")
mpa_tall_from_est_vis

ggplot(mpa_tall_from_est_vis %>% filter(!is.na(MPA), climate != "clim0")) +
  geom_col(aes(x = MPA, y = estimated_visitors, fill = climate), position = "dodge") +
  facet_wrap(~CNTRY_NAME, scales = "free")

# just BZ
ggplot(mpa_tall_from_est_vis %>% filter(!is.na(MPA), climate != "clim0", CNTRY_NAME == "Belize")) +
  geom_col(aes(x = MPA, y = estimated_visitors, fill = climate), position = "dodge") +
  coord_flip()

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

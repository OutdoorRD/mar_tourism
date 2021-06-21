########## Making maps of tourism for MAR workshop ################
### Starting with baseline maps

library(sf)
library(tidyverse)
library(rnaturalearth) # for country maps
library(rnaturalearthdata)
library(ggspatial)

# grab country outlines from rnaturalearth
#countries1 <- ne_countries(country = c("Mexico", "Belize", "Guatemala", "Honduras"),
 #           returnclass = "sf")
#ggplot(countries) + geom_sf()

# grab tourism aoi, mpas, and viz and expenditure information
countries <- read_sf("~/Documents/MAR/GIS/BordersandProtectedAreas/countries_MAR.shp")
aoi_outline <- read_sf("~/Documents/MAR/GIS/AOI/AOI_v4/Tourism_AOI_v4.shp")
mpas <- read_sf("~/Documents/MAR/GIS/BordersandProtectedAreas/MPA/MPA_Updates_July_2020/MPA_Network_WGS8416N_v3.shp")
vis_per_cell <- read_sf("~/Documents/MAR/ModelRuns/baseline_20200715/aoi_viz_exp.shp")
cities <- read_sf("~/Documents/MAR/GIS/Predictors/Infrastructure/ne_10m_populated_places_simple_MAR.shp")
mpa_names <- read_csv("~/Documents/MAR/ModelRuns/baseline_20200715/listofMPAs_v2.csv")

# filter cities to be a few key ones along the coast
cities_mar <- cities %>% filter(nameascii %in% c("Cancun", "Cozumel", "Chetumal", "Belize City", 
                                   "Puerto Barrios", "San Pedro Sula", "La Ceiba"))

## turn vis_per_cell into lat/long table to plot as dots for propotional
##  bubbles
vis_per_point <- vis_per_cell %>% st_centroid()

st_coordinates(st_centroid(vis_per_cell))

ggplot() +
  geom_sf(data = countries) +
  geom_sf(data = aoi_outline, fill = NA) +
  #geom_sf(data = mpas) +
  geom_sf(data = vis_per_point %>% filter(est_vis > 0), 
          aes(size = est_vis), alpha = .3, col = "darkred",
          show.legend = "point") +
  geom_sf(data = cities_mar) +
  geom_text(data = cities_mar, aes(x = longitude+.7, y = latitude-.1, label = name), size = 3) +
  scale_size_area(name = "Annual Visitors (2017)", 
                  max_size = 15, # size of max bubbles
                  breaks = c(10, 100, 1000, 10000, 100000, 500000),
                  labels = c("1-10", "10-100", "100-1,000", "1,000-10,000", "10,000 - 100,000", "100,000+"),
                  guide = "legend") +
  coord_sf(xlim = c(-89.7, -85.5), ylim = c(15.25, 22.5), crs = 4326) +
  theme_bw()

#ggsave("~/Documents/MAR/Deliverables/August Workshop/figs/MAR_tourism_v4_labels.png",
 #      width = 6, height = 6, units = "in")

###### And... making for individual countries ######
## first, let's read in my MPA buffer and then select only cells within it
#mpa_buffer <- read_sf("~/Documents/MAR/GIS/BordersandProtectedAreas/MPA/MPA_buffer.shp")

#vis_pc_mpas <- vis_per_cell %>% 
#  filter(lengths(st_within(vis_per_cell, mpa_buffer)) > 0)
# write it out
#write_sf(vis_pc_mpas, "~/Documents/MAR/ModelRuns/baseline_5k/aoi_viz_exp_mpa_plus.shp")

### UPDATE - with the intersected polys, I just need to select ones which have MPA names
vis_pc_mpas <- vis_per_cell %>% filter(!is.na(MPA))

# make a column which has cells categorized based on estimated vis
vis_pc_groups <- vis_pc_mpas %>%
  mutate(vis_group = cut(est_vis, 
                         breaks = c(-1, 0, 100, 1000, 10000, 1000000),
                         ordered_result = TRUE))
# make a tibble that has MPA names and coordinates
mpa_pts <- cbind(mpas, st_coordinates(st_centroid(mpas)))
mpa_pts <- mpa_pts %>% 
  left_join(mpa_names %>% select(-Country), by = c("name" = "NAME"))

## Mexico
ggplot() +
  geom_sf(data = countries) +
  geom_sf(data = vis_pc_groups %>% filter(Country == "Mexico"),
          aes(fill = vis_group)) +
  scale_fill_brewer(palette = "Greens",
                    name = "Annual Visitors",
                    labels = c("0", "1-100", "100-1000", "1000-10,000", "10,000+")) +
  #scale_fill_viridis_d("magma") +
  #scale_fill_gradient(breaks = c(10, 100, 1000, 10000, 100000, 500000),
   #                     labels = c("1-10", "10-100", "100-1,000", "1,000-10,000", "10,000 - 100,000", "100,000+")) +
  geom_sf(data = mpas, fill = NA, col = "black", lwd = .5) +
 # geom_text(data = mpa_pts, aes(x =X, y=Y-.25, label = Name_short)) +
  geom_text(aes(x = c(-88.6, -87.95, -87.2, -87.2, -86.5) , y = c(21.75, 21.8, 22, 21.1, 21.1), 
                label = c("Dzilam", "Ria Lagartos", "Tiburon Ballena", "Yum Balam", "Cancun - \nPunta Nizuc"))) +
  #geom_sf(data = cities_mar, shape = 23, fill = "yellow", size = 3.5) +
  #geom_text(data = cities_mar, aes(x = longitude, y = latitude, label = name), 
   #         nudge_x = .1, nudge_y = -.1, fontface = "italic") +
  coord_sf(xlim = c(-89.1, -86.4), ylim = c(20.8, 22.25), crs = 4326) +
  labs(title = "Estimated 2017 Visitation to Mexico MPAs",
       subtitle = paste("Updated", Sys.Date())) +
  annotation_scale() +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw()

#ggsave("~/Documents/MAR/ModelRuns/baseline_20200715/figs/viz_map_mpas_Mexico_label.png",
 #     width = 8, height = 6, unit = "in")


## Honduras
ggplot() +
  geom_sf(data = countries) +
  geom_sf(data = vis_pc_groups %>% filter(Country == "Honduras", !pid %in% c(1383:1384, 1467)),
          aes(fill = vis_group)) +
  scale_fill_brewer(palette = "Greens",
                    name = "Annual Visitors",
                    labels = c("0", "1-100", "100-1000", "1000-10,000", "10,000+")) +
  #scale_fill_viridis_d("magma") +
  #scale_fill_gradient(breaks = c(10, 100, 1000, 10000, 100000, 500000),
  #                     labels = c("1-10", "10-100", "100-1,000", "1,000-10,000", "10,000 - 100,000", "100,000+")) +
  geom_sf(data = mpas %>% filter(Country == "Honduras"), fill = NA, col = "black", lwd = .5) +
  #geom_text(data = mpa_pts, aes(x =X, y=Y-.25, label = Name_short)) +
  geom_text(aes(x = c(-88.6, -87.95, -87.2, -87.2, 
                      -88.25, -88.25, -88.1, -87.7, -87.6, -87.2, -86.5), 
                y = c(21.75, 21.8, 22, 21.1, 
                      15.82, 15.5, 16.0, 15.57, 16.25, 15.57, 15.6), 
                label = c("Dzilam", "Ria Lagartos", "Tiburon Ballena", "Yum Balam",
                          "Cuyamel", "Omoa", "Interconexion", "J. Kawas", "Bahia de Tela",
                          "Punta Izopo", "Cayos Cochinos"))) +
  coord_sf(xlim = c(-88.5, -86), ylim = c(15.25, 16.5), crs = 4326) +
  labs(title = "Estimated 2017 Visitation to Honduras MPAs",
       subtitle = paste("Updated", Sys.Date())) +
  annotation_scale() +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw()

#ggsave("~/Documents/MAR/ModelRuns/baseline_20200715/figs/viz_map_mpas_Honduras.png",
 #     width = 8, height = 5, unit = "in")

## Guatemala
ggplot() +
  geom_sf(data = countries) +
  geom_sf(data = vis_pc_groups %>% filter(Country == "Guatemala"),
          aes(fill = vis_group)) +
  scale_fill_brewer(palette = "Greens",
                    name = "Annual Visitors",
                    labels = c("0", "1-100", "100-1000", "1000-10,000", "10,000+")) +
 geom_sf(data = mpas %>% filter(Country == "Guatemala"), fill = NA, col = "black", lwd = .5) +
#  geom_text(data = mpa_pts, aes(x =X, y=Y-.25, label = Name_short)) +
  coord_sf(xlim = c(-89.5, -88.2), ylim = c(15.25, 16.2), crs = 4326) +
  labs(title = "Estimated 2017 Visitation to Rio Sarstun") +
  annotation_scale() +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw()

#ggsave("~/Documents/MAR/ModelRuns/baseline_20200715/figs/viz_map_mpas_Guatemala.png",
 #    width = 8, height = 6, unit = "in")

## Belize
# Note: 7/15/20, I can't get the labels to work. Come back to this later when it's more important
ggplot() +
  geom_sf(data = countries) +
  geom_sf(data = vis_pc_groups %>% filter(Country == "Belize"),
          aes(fill = vis_group)) +
  scale_fill_brewer(palette = "Greens",
                    name = "Annual Visitors",
                    labels = c("0", "1-100", "100-1000", "1000-10,000", "10,000+")) +
  #scale_fill_viridis_d("magma") +
  #scale_fill_gradient(breaks = c(10, 100, 1000, 10000, 100000, 500000),
  #                     labels = c("1-10", "10-100", "100-1,000", "1,000-10,000", "10,000 - 100,000", "100,000+")) +
  geom_sf(data = mpas %>% filter(Country == "Belize"), fill = NA, col = "black", lwd = .5) +
  geom_spatial_text(data = mpa_pts %>% filter(Country == "Belize", 
                                      !Name_short %in% c("Bacalar Chico", "Laughing Bird Caye",
                                                         "Sapodilla Cayes")), 
            aes(x =X-47000, y=Y, label = Name_short), crs = 32616) +
  geom_text(aes(x = c(-87.45, -87.75, -87.8), 
                y = c(18.15, 16.4, 16.15), 
                label = c("Bacalar Chico", "Laughing Bird Caye", "Sapodilla Cayes"))) +
  coord_sf(xlim = c(-89.5, -87), ylim = c(15.75, 18.5), crs = 4326) +
  labs(title = "Estimated 2017 Visitation to Belize MPAs") +
  annotation_scale() +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw()

#ggsave("~/Documents/MAR/ModelRuns/baseline_20200715/figs/viz_map_mpas_Belize.png",
 #    width = 7, height = 9, unit = "in")

### Creating Wildlife Layer ####

## Pulling in various wildlife layers and combining them into a single one

library(tidyverse)
library(sf)

setwd("~/Documents/MAR/GIS/Predictors/MegaFauna/")

## Note that these data are somewhat described in the Tourism Models/Wildlife googlesheet

# read in data
mpas <- read_sf("../../BordersandProtectedAreas/MPA/MPA_Network_WGS84_4326.shp")
land <- read_sf("../CV_inputs/CV_inputs_v4/landmass_adjusted_clipped_shift_BZ_MX.shp")


#### Already combined wildlife (which went to Aug workshop)
wildlife <- read_sf("wildlife.shp")
ggplot(wildlife) + geom_sf(aes(fill = nombre))
# this has whale sharks from MX, BZ, and HN (so all of them?)
# Turtles from MX
# Manatees from GT
# Sharks from HN and GT

#### Honduras sightseeing locations
HN_locs <- read_sf("Honduras/Wildlife tourism spots.kmz")
# all HN data included here (points from Luis showing where people go)

#### Flamingos - I created this one (only a focal species for MX)
flamingos <- read_sf("Mexico/Flamingos/Flamingos_SGW.shp")

#### Manatees
# BZ
manatee_bz <- read_sf("Belize/manatee_2012.shp")
summary(manatee_bz)
# all of type "waypoint". I believe these are sightings data
# GT data already in "wildlife.shp"
# HN data in HN_locs
# MX? Don't think that Manatees are a focal species in MX

#### Sharks
# BZ
sharks_bz <- read_sf("Belize/SharktourismBelize2016/SharktourismBelize2016.shp")
plot(sharks_bz)
# small polygons around sightseeing areas for sharks
# GT already in wildlife.shp
# HN in HN_locs
# No data for MX - ok?

#### Whale sharks
# all countries already in wildlife.shp. (Though we also have a habitat layer for MX)

#### Sea turtles
# BZ
turtles_bz <- read_sf("Belize/seaturtle_2007.shp")
# sightings/nests
# HN in HN_locs
# MX in wildlife.shp
# Not focal species for GT?


##################################################################

## Ok. So... let's have a look
ggplot() +
  geom_sf(data = flamingos) +
  geom_sf(data = HN_locs) +
  geom_sf(data = manatee_bz) +
  geom_sf(data = sharks_bz) +
  geom_sf(data = turtles_bz) +
  geom_sf(data = wildlife)

# for now, let's use the crs from the bz data
utm_proj <- st_crs(manatee_bz)

###### Clean wildlife and prepare it for merging

wildlife_tidy <- wildlife %>%
  st_transform(crs = utm_proj) %>%
  mutate(Species = case_when(nombre == "Tiburon Ballena" ~ "Whale shark",
                             str_starts(nombre, "Tibur") ~ "Shark",
                             nombre == "Manatee" ~ "Manatee",
                             nombre == "Turtles" ~ "Sea turtle"),
         DataType = if_else(Species == "Sea turtle", "Tourism", "Observations"),
         Source = case_when(str_detect(Species, "hark") ~ "MARFUND",
                            Species == "Manatee" ~ "Fundaeco",
                            Species == "Sea turtle" ~ "Alejandra poly")) %>%
  dplyr::select(Species, DataType, Source, geometry)


##### Tidy flamingos for merging
flamingos_tidy <- flamingos %>%
  st_transform(crs = utm_proj) %>%
  mutate(Species = "Flamingo",
         DataType = "Observations",
         Source = "Sama poly") %>%
  dplyr::select(Species, DataType, Source, geometry)

# Let's draw some buffers around the points, then merge them together

# Starting with the honduras tourism sites. It feels reasonable to give these
# slightly bigger buffers

# transform HN_locs
HN_utm <- st_transform(HN_locs, crs = utm_proj)

# select out the critical species first
HN_wildlife <- HN_utm %>% 
  dplyr::select(Name, geometry) %>%
  filter(Name %in% c("Manati Sightseeing",
                     "Whaleshark tours",
                     "Shark Diving",
                     "See turtles spots")) %>%
  mutate(Species = case_when(Name == "Manati Sightseeing" ~ "Manatee",
                             Name == "Whaleshark tours" ~ "Whale shark",
                             Name == "Shark Diving" ~ "Shark",
                             Name == "See turtles spots" ~ "Sea turtle"),
         DataType = "Tourism",
         Source = "Luis points") %>%
  dplyr::select(-Name)
plot(HN_wildlife)

HN_polys <- st_buffer(HN_wildlife, dist = 5000) # units in meters
ggplot() + 
  #geom_sf(data = land) +
  geom_sf(data = mpas %>% filter(Country == "Honduras")) +
  geom_sf(data = HN_polys)

# that seems fairly reasonable for now

#### Let's do something similar (but with smaller buffers) for the BZ obs data
manatee_bz_polys <- st_buffer(manatee_bz, dist = 1000) # units in meters
plot(manatee_bz_polys)

manatee_bz_tidy <- manatee_bz_polys %>%
  dplyr::select(Name, geometry) %>%
  rename(Species = Name) %>%
  mutate(DataType = "Observations",
         Source = "CZMAI")

# sea turtles
st_crs(turtles_bz)
turtles_bz_polys <- st_buffer(turtles_bz, dist = 1000)
turtles_bz_tidy <- turtles_bz_polys %>%
  dplyr::select(geometry) %>%
  mutate(Species = "Sea turtle",
         DataType = "Observations",
         Source = "CZMAI")

# prep sharks (since it's tourism sites, don't need to buffer)
plot(sharks_bz)
sharks_bz_tidy <- sharks_bz %>%
  dplyr::select(geometry) %>%
  mutate(Species = "Shark",
         DataType = "Tourism",
         Source = "Roberto polys")

# bind them all together!
wildlife_new <- rbind(wildlife_tidy, flamingos_tidy, HN_polys, manatee_bz_tidy, turtles_bz_tidy, sharks_bz_tidy)

ggplot(wildlife_new) +
  geom_sf(aes(fill = Species))

# and write it out
#st_write(wildlife_new, "wildlife2.shp")

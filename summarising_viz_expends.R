#######
###  Summarising Visitation and Expenditures by MAR, country, and MPA
###
###  Requires: Total visitors to the AOI in each country to be distributed out based on PUD and TUD
###     (proportioned_viz_2015.csv from downscaling.r)

library(sf)
library(tidyverse)

# read in total visitation numbers by country (proportioned from overall country numbers into the AOI in 
#  `downscaling.R`)
viz_2017 <- read_csv("~/Documents/MAR/Data/proportioned_viz_2017_AOIv3.csv")

# also, average expenditures (for now, rough numbers). Check out the google sheet 'expenditures' for more options
avg_exp <- tibble(Country = c("Mexico", "Honduras", "Guatemala", "Belize"),
                  AvgExp = c(800, 600, 600, 850))

## First, let's summarise by country (since we're almost already there)

country_summaries <- viz_2017 %>% 
  left_join(avg_exp, by = "Country") %>%
  mutate(Expenditures = vizinAOI*AvgExp) %>%
  select(Country, Year, vizinAOI, AvgExp, Expenditures)

country_summaries 
#write_csv(country_summaries, "~/Documents/MAR/ModelRuns/baseline_5k/viz_and_expends/country_summaries.csv")

## And, for the entire AOI?
mar_summary <- country_summaries %>% 
  summarise(Visitors = sum(vizinAOI),
            Expenditures = sum(Expenditures),
            Year = mean(Year))
#write_csv(mar_summary, "~/Documents/MAR/ModelRuns/baseline_5k/viz_and_expends/mar_summary.csv")

############# And... how about by those MPAs? ###################

# pulling in gridded AOI & MPA shapefiles
aoi <- read_sf("~/Documents/MAR/ModelRuns/baseline_5k/T_aoi_v3_wgs_5k_pid.shp")
mpas <- read_sf("~/Documents/MAR/GIS/BordersandProtectedAreas/MPA/MPA_Network_WGS84_4326.shp")

# let's first try just a simple intersection and see what happens
st_crs(aoi)
st_crs(mpas)

grid_mpas <- st_intersection(aoi, mpas)
plot(grid_mpas, max.plot = 1)

# export
#write_sf(grid_mpas, "~/Downloads/test.shp")

# Let's calculate the proportionn of each grid which falls inside the MPA, then use this as a multplier times the
# avg_ann_ud for that grid cell to get number of posts from "within" the MPA

# what's the area of a single grid cell?

grid_area <- aoi %>%
  mutate(area = unclass(st_area(aoi))) %>%
  st_set_geometry(NULL)

# and, MPA grid areas? Plus calculate the prop_grid (proportion of the grid cell which is inside the MPA)
mpas_props <- grid_mpas %>%
  mutate(area_mpa = unclass(st_area(grid_mpas))) %>%
  left_join(grid_area, by = "pid") %>%
  mutate(prop_grid = area_mpa/area) 

## ok. So... pull in our social media data (by pid)
avg_ann_smud <- read_csv("~/Documents/MAR/ModelRuns/baseline_5k/avg_ann_smud_2005plus.csv")

# and... let's bind it on to mpas_props
mpa_grid_sm <- mpas_props %>% 
  left_join(avg_ann_smud, by = "pid") %>%
  mutate(smud_in_mpa = avg_ann_smud*prop_grid,
         tud_in_mpa = avg_ann_tud*prop_grid,
         pud_in_mpa = avg_ann_pud*prop_grid)
mpa_grid_sm

# and, summarise by mpa
mpa_smud <- mpa_grid_sm %>%
  group_by(NAME, Country) %>%
  summarise(smud_prop = sum(smud_in_mpa),
            smud_tot = sum(avg_ann_smud),
            pud_tot = sum(avg_ann_pud),
            tud_tot = sum(avg_ann_tud)) %>%
  ungroup()

# let's have a look and see if these seems reasonable
mpa_smud_tp <- st_set_geometry(mpa_smud, NULL)
ggplot(mpa_smud_tp)+
  geom_col(aes(x = reorder(NAME, smud_tot), y = smud_prop)) +
  coord_flip()

# how about PUD on its own?
ggplot(mpa_smud_tp)+
  geom_col(aes(x = reorder(NAME, smud_tot), y = pud_tot)) +
  coord_flip()

# and TUD?
ggplot(mpa_smud_tp)+
  geom_col(aes(x = reorder(NAME, smud_tot), y = tud_tot)) +
  coord_flip()


mpa_smud_tall <- mpa_smud %>% 
  st_set_geometry(NULL) %>%
  gather(key = "source", value = "UD", -NAME, -Country)

ggplot(mpa_smud_tall %>% filter(source != "smud_prop")) +
  geom_col(aes(x = NAME, y = UD, fill = source), position = "dodge") +
  coord_flip() 

# can I show proportion of UD that goes to each MPA based on each source?
mpa_smud_tall %>%
  mutate()

# great. So now... how to get from smud to number of visitors and expenditures?

# Let's figure out how many SMUD I have across the entire MAR
total_sm <- avg_ann_smud %>%
  summarise_at(vars(avg_ann_pud, avg_ann_tud, avg_ann_smud), sum)

# and... figure out the total visitor to avg_ann_smud ratio
(viz_to_smud <- mar_summary$Visitors/total_sm$avg_ann_smud)
# ok. each avg_ann_smud is equivalent to ~25 visitors

## so... let's calculate visitors and expenditures by mpa
mpa_smud
mpa_summaries <- mpa_smud %>% 
  left_join(avg_exp, by = "Country") %>%
  mutate(visitors = smud*viz_to_smud,
         expenditures = visitors*AvgExp)
mpa_summaries

## write it out
#write_sf(mpa_summaries, "~/Documents/MAR/ModelRuns/baseline_5k/viz_and_expends/mpa_summaries.shp")


#####################################################
### Now let's make a plot

ggplot(mpa_summaries) +
  geom_col(aes(x = reorder(NAME, visitors), y = visitors)) +
  coord_flip() + 
  xlab(NULL) +
  labs(title = "Annual Visitors to MPAs (based on TUD+PUD Proportion)")

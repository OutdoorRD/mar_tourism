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
            pud_prop = sum(pud_in_mpa),
            tud_tot = sum(avg_ann_tud),
            tud_prop = sum(tud_in_mpa)) %>%
  ungroup()

# let's have a look and see if these seems reasonable
mpa_smud_tp <- st_set_geometry(mpa_smud, NULL)
ggplot(mpa_smud_tp)+
  geom_col(aes(x = reorder(NAME, smud_prop), y = smud_prop)) +
  coord_flip()

# how about PUD on its own?
ggplot(mpa_smud_tp)+
  geom_col(aes(x = reorder(NAME, smud_prop), y = pud_prop)) +
  coord_flip()

# and TUD?
ggplot(mpa_smud_tp)+
  geom_col(aes(x = reorder(NAME, smud_prop), y = tud_prop)) +
  coord_flip()

# plotting together
mpa_smud_tall <- mpa_smud %>% 
  st_set_geometry(NULL) %>%
  gather(key = "source", value = "UD", -NAME, -Country)

ggplot(mpa_smud_tall %>% filter(source %in% c("tud_prop", "pud_prop"))) +
  geom_col(aes(x = reorder(NAME, UD), y = UD, fill = source)) +
  coord_flip() 



###############################
##### Let's check out whether proportions or total UD is better correlated with PUD pulled from the MPA shapefile ####
pud_avg_ann_shp <- read_csv("~/Documents/MAR/ModelRuns/baseline_mpas/pud/avg_ann_pud_pid.csv")

PUD_comps <- pud_avg_ann_shp %>%
  rename(PUD_shape = avg_ann_ud) %>%
  left_join(mpa_smud)

ggplot(PUD_comps, aes(x = PUD_shape)) +
  #geom_point(aes(y = pud_tot)) +
  geom_point(aes(y = pud_prop), col = "green") +
  geom_abline(slope = 1)

# PUD_prop looks much better. Generally good, but there's one point that seems pretty off
PUD_comps %>% arrange(desc(pud_prop))
# Caye Caulker has more viz when calculated as a proportion of grid cells than when calculated using the MPA
# shapefile. I think this is becasue the Caye Caulker MPA doesn't actually include the popular Caye landmass

## Great. Proportions seems pretty good

##################################################################
# great. So now... how to get from smud to number of visitors and expenditures?

# Let's figure out how many SMUD I have across the entire MAR
total_sm <- avg_ann_smud %>%
  summarise_at(vars(avg_ann_pud, avg_ann_tud, avg_ann_smud), sum)

# and... figure out the total visitor to avg_ann_smud ratio
(viz_to_smud <- mar_summary$Visitors/total_sm$avg_ann_smud)
# ok. each avg_ann_smud is equivalent to ~25 visitors
(viz_to_pud <- mar_summary$Visitors/total_sm$avg_ann_pud)
# and each avg_ann_pud is ~1000 visitors

## so... let's calculate visitors and expenditures by mpa
mpa_smud
mpa_summaries <- mpa_smud %>% 
  left_join(avg_exp, by = "Country") %>%
  mutate(visitors_smud = smud_prop*viz_to_smud,
         expenditures_smud = visitors_smud*AvgExp,
         visitors_pud = pud_prop*viz_to_pud)
mpa_summaries

## write it out
#write_sf(mpa_summaries, "~/Documents/MAR/ModelRuns/baseline_5k/viz_and_expends/mpa_summaries.shp")


#####################################################
### Now let's make a plot

### plot of annual viz according to PUD proportions
ggplot(mpa_summaries) +
  geom_col(aes(x = reorder(NAME, visitors_pud), y = visitors_pud)) +
  coord_flip() + 
  xlab(NULL) +
  labs(title = "Annual Visitors to MPAs (based on PUD Proportion)")

### plot of annual viz according to SMUD proportions
ggplot(mpa_summaries) +
  geom_col(aes(x = reorder(NAME, visitors_smud), y = visitors_smud)) +
  coord_flip() + 
  xlab(NULL) +
  labs(title = "Annual Visitors to MPAs (based on TUD+PUD Proportion)")


################### Comparing viz numbers by country to PUD vs TUD and trying to figure out which one is better####
country_summaries
avg_ann_smud

# ok, I need to know which country each pid falls into
countries_aoi <- read_sf("~/Documents/MAR/GIS/Downscaling/CountriesplusTAOI_v3.shp")
countries_aoi
c_aoi_valid <- lwgeom::st_make_valid(countries_aoi)

countries_pid_int <- st_intersection(aoi, c_aoi_valid)
# note that this splits some pids. Below I drop those that are split into two pieces within the same
#   country, but don't worry for now about pids which straddle countries
countries_pid <- countries_pid_int %>% 
  st_set_geometry(NULL) %>%
  dplyr::select(pid, country = CNTRY_NAME) %>%
  distinct()

# combine with socmed and summarise by country
countries_ud <- avg_ann_smud %>% 
  left_join(countries_pid, by = "pid") %>%
  group_by(country) %>%
  summarise_at(vars(avg_ann_pud, avg_ann_tud, avg_ann_smud), sum) %>%
  filter(!is.na(country))


# let's put it together to plot
countries_ud_emp <- countries_ud %>% 
  left_join(country_summaries, by = c("country" = "Country")) %>%
  gather(key = "socmed", value = "avg_ann_ud", c(avg_ann_pud, avg_ann_tud, avg_ann_smud))

ggplot(countries_ud_emp) +
  geom_point(aes(x = log1p(vizinAOI), y = log1p(avg_ann_ud))) +
  geom_abline(slope = 1) +
  facet_wrap(~socmed)

# hmm, they all have the appropriate relationship
# does a linear model help at all?
mod1 <- lm(log1p(country_summaries$vizinAOI) ~ log1p(avg_ann_pud) + log1p(avg_ann_tud), data = countries_ud)
summary(mod1)
plot(mod1)

mod2 <- lm(country_summaries$vizinAOI ~ avg_ann_pud + avg_ann_tud, data = countries_ud)
summary(mod2)
plot(mod2)

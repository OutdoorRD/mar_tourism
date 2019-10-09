#######
###  Summarising Visitation and Expenditures by MAR, country, and MPA
###
###  Requires: 
###     Total visitors to the AOI in each country to be distributed out based on PUD and TUD
###         (proportioned_viz_2015.csv from downscaling.r)
###     Outputs from preparing_socmed.R

library(sf)
library(tidyverse)

setwd("~/Documents/MAR/ModelRuns/baseline_5k_intersect/")

# read in total visitation numbers by country (proportioned from overall country numbers into the AOI in 
#  `downscaling.R`)
viz_2017 <- read_csv("~/Documents/MAR/Data/proportioned_viz_2017_AOIv3.csv")

# also, average expenditures (for now, rough numbers). Check out the google sheet 'expenditures' for more options
# Note that Honduras is from 2016, others are all 2017
# Average expenditures are averaged between day trippers and overnight guests
avg_exp <- tibble(Country = c("Mexico", "Honduras", "Guatemala", "Belize"),
                  AvgExp = c(809, 572, 614, 854))

# Read in AOI and smud info
aoi_smud <- read_sf("aoi_smud.shp")

## First, let's summarise by country (since we're almost already there)

country_summaries <- viz_2017 %>% 
  left_join(avg_exp, by = "Country") %>%
  mutate(Expenditures = vizinAOI*AvgExp) %>%
  dplyr::select(Country, Year, vizinAOI, AvgExp, Expenditures)

country_summaries 
#write_csv(country_summaries, "viz_and_expends/country_summaries.csv")

## And, for the entire AOI?
mar_summary <- country_summaries %>% 
  summarise(Visitors = sum(vizinAOI),
            Expenditures = sum(Expenditures),
            Year = mean(Year))
mar_summary
#write_csv(mar_summary, "viz_and_expends/mar_summary.csv")

############# And... how about by those MPAs? ###################
### As of 10/9/19, this should be much easier since my new AOI is intersected with the MPA boundaries
### See below for old code (used for August workshop summaries), that calculated the proportion
###   of each hex that fell inside the MPA and then proportioned the viz accordingly.
### The new AOI is an improvement since it shouldn't include adjacent cities in MPAs

mpa_smud <- aoi_smud %>%
  group_by(Name_short = Nm_shrt, Country = NAME) %>%
  summarise(smud_prop = sum(smd_prp),
            tud_prop = sum(tud_prp),
            pud_prop = sum(pud_prp)) %>%
  filter(!is.na(Name_short))
plot(mpa_smud)

# great. So now... how to get from smud to number of visitors and expenditures?

## Need to find a SMUD to visitor ratio for each country
## Note: this is still necessary, since my proportions above represent the proportion of ALL tourism to the
##   MAR region which is represented by that cell. But, I want to proportion out the COUNTRY-level tourism
##   numbers

# So, summarise by country
countries_smud <- aoi_smud %>%
  group_by(Country = NAME) %>%
  summarise(smud_prop = sum(smd_prp),
            tud_prop = sum(tud_prp),
            pud_prop = sum(pud_prp)) %>%
  filter(!is.na(Country))


# let's put it together to plot
countries_ud_emp <- countries_smud %>% 
  left_join(country_summaries, by = "Country") %>%
  gather(key = "socmed", value = "prop_ud", c(smud_prop, tud_prop, pud_prop))

# and, calculate the vis to prop_ud ratio for each 
countries_ratios <- countries_ud_emp %>%
  st_set_geometry(NULL) %>%
  mutate(ud_to_vis = vizinAOI/prop_ud) %>% #,
        # socmed = str_extract(socmed, "(?<=_)[:alnum:]*$")) %>%
  dplyr::select(Country, AvgExp, socmed, ud_to_vis)
countries_ratios
# Oof. Those are VERY different by country

##### creating a shapefile that includes estimated visitation per grid cell using those viz ratios ####
## First, just using smud_prop
vis_per_cell <- aoi_smud %>%
  select(pid, smud_prop = smd_prp, Country = NAME, MPA = NAME_2, MPA_short = Nm_shrt, geometry) %>%
  left_join(countries_ratios %>% filter(socmed == "smud_prop"), by = "Country") %>%
  mutate(est_vis = smud_prop*ud_to_vis,
         est_exp = round(est_vis*AvgExp))

## write it out
#write_sf(vis_per_cell, "aoi_viz_exp.shp")

## Let's also do this with tud and pud seperately to see if one of them better matches empirical numbers
vis_per_cell_sep <- aoi_smud %>%
  select(pid, smud_prop = smd_prp, tud_prop = tud_prp, pud_prop = pud_prp, Country = NAME, MPA = NAME_2, MPA_short = Nm_shrt, geometry) %>%
  gather(key = "socmed", value = "prop", tud_prop, pud_prop, smud_prop) %>%
  left_join(countries_ratios, by = c("Country", "socmed")) %>%
  mutate(est_vis = prop*ud_to_vis,
         est_exp = round(est_vis*AvgExp))

## so... let's calculate visitors and expenditures by mpa
mpa_summaries <- vis_per_cell_sep %>%
  filter(!is.na(MPA), !is.na(Country)) %>%
  group_by(Country, MPA, MPA_short, socmed) %>%
  summarise(prop = sum(prop),
            visitors = sum(est_vis),
            expenditures = sum(est_exp))
mpa_summaries
# write it out
#write_csv(mpa_summaries, "viz_and_expends/mpa_summaries.csv")

##### Let's pull in whatever empirical numbers I have for MPA visitors and see how my estimates compare
MPA_emp_viz <- read_csv("~/Documents/MAR/Data/MPA_Emp_Visitation_10_09_19.csv")
# I have multiple estimates/years for Hol Chan and Cayos Cochinos and South Water Caye. Let's drop some
MPA_emp_viz_sub <- MPA_emp_viz %>% 
  filter(!(MPA == "Hol Chan" & Year == 2016),
         !(MPA_short == "Cayos Cochinos" & Year == 2016),
         !(MPA_short == "South Water Caye" & Visitation == 4600))

# bind on to my estimates to plot
mpa_comps <- mpa_summaries %>%
  rename(est_visitors = visitors) %>%
  left_join(MPA_emp_viz_sub)

ggplot(mpa_comps %>% filter(socmed == "smud_prop")) +
  geom_label(aes(x = Visitation, y = est_visitors, label = MPA_short, col = socmed)) +
  geom_point(aes(x = Visitation, y = est_visitors), col = "blue") +
  geom_abline(slope = 1)# +
  facet_grid(rows = vars(socmed))

ggplot(mpa_comps %>% filter(Country == "Belize")) +
  geom_abline(slope = 1) +
  geom_point(aes(x = Visitation, y = est_visitors)) 
  
## Hol Chan is underestimated by all three socmed options


## correlation I think isn't actually what I want... how about RMSE?
mpa_comps %>%
  filter(!is.na(Visitation)) %>%
  mutate(sq_error = (est_visitors - Visitation)^2) %>%
  group_by(socmed) %>%
  summarise(RMSE = mean(sq_error))
# great, smud is actually the lowest, and therefore the best

### Not sure what to do about Hol Chan being low. See notes in MAR notes and thoughts (pg 16)
##   for a few things I investigated but rejected.


############ Plotting MPA Summaries #########
countrytp <- "Honduras"
ggplot(mpa_summaries %>% filter(Country == countrytp, socmed == "smud_prop")) +
  geom_col(aes(x = reorder(MPA_short, visitors), y = visitors), fill = "darkred", width = .7) +
  coord_flip() +
  xlab(NULL) +
  ylab("Annual Visitors") +
  labs(title = paste0("Estimated 2017 Visitation by MPA - ", countrytp),
       subtitle = paste("Updated", Sys.Date())) +
  theme_bw()

#ggsave(paste0("figs/mpas_", countrytp, ".png"),
#      width = 9, height = 7, units = "in", scale = .6)

# let's see if we can add expenditure information in text at the end of the bars
mpa_summaries_tall


## plot all MPAs together
ggplot(mpa_summaries %>% filter(socmed %in% c("smud_prop"))) +
  geom_col(aes(x = reorder(MPA_short, visitors), y = visitors), fill = "darkred", width = .7) +
  coord_flip() +
  xlab(NULL) +
  ylab("Annual Visitors") +
  labs(title = paste0("Estimated 2017 Visitation by MPA")) +
  theme_bw()

# write it out
#ggsave(paste0("~/Documents/MAR/Deliverables/August Workshop/figs/mpas_all.png"), width = 8, height = 8, units = "in")

## Let's do the all MPAs plot, but add in the empirical vis numbers
mpas_comps_tall <- mpa_comps %>% 
  filter(socmed == "smud_prop") %>%
  gather(key = "source", value = "visitation", est_visitors, Visitation)
  
ggplot(mpas_comps_tall) +
  geom_col(aes(x = reorder(MPA_short, prop), y = visitation, color = Country, alpha = source), 
           position = "dodge", width = .8) +
  coord_flip() +
  scale_alpha_discrete(breaks = c("est_visitors", "Visitation"),
                       labels = c("Model estimate", "Empirical Visitation")) +
  labs(title = "Annual Visitation by MPA. Model estimates vs. Empirical viz numbers",
       subtitle = paste("Updated", Sys.Date()))
#ggsave("figs/mpas_smud_v_empirical.png", height = 8, width = 8, unit = "in")


## plot all MPAs together, showing alternative social media combos
ggplot(mpa_summaries_tall %>% filter(socmed %in% c("smud2", "smud", "pud", "tud"),
                                     Country == "Belize")) +
  geom_col(aes(x = reorder(Name_short, visitors), y = visitors, fill = socmed), position = "dodge", width = .7) +
  coord_flip() +
  xlab(NULL) +
  ylab("Annual Visitors") +
  labs(title = paste0("Estimated 2017 Visitation by MPA")) +
  theme_bw()















####################################################
######### Old MPA intersection, proportioning, and summary code ####

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
avg_ann_smud <- read_csv("~/Documents/MAR/ModelRuns/baseline_5k/avg_ann_smud_2005plus_19_101_SMUD.csv")

# and... let's bind it on to mpas_props
mpa_grid_sm <- mpas_props %>% 
  left_join(avg_ann_smud, by = "pid") %>%
  mutate(smud_in_mpa = avg_ann_smud*prop_grid,
         tud_in_mpa = avg_ann_tud*prop_grid,
         pud_in_mpa = avg_ann_pud*prop_grid,
         smud_prop_in_mpa = smud_prop*prop_grid)
mpa_grid_sm

# and, summarise by mpa
mpa_smud <- mpa_grid_sm %>%
  group_by(Name_short, Country) %>%
  summarise(smud_prop = sum(smud_in_mpa),
            #smud_tot = sum(avg_ann_smud),
            #pud_tot = sum(avg_ann_pud),
            pud_prop = sum(pud_in_mpa),
            #tud_tot = sum(avg_ann_tud),
            tud_prop = sum(tud_in_mpa),
            smud2_prop = sum(smud_prop_in_mpa)) %>%
  ungroup()



# let's have a look and see if these seems reasonable
mpa_smud_tp <- st_set_geometry(mpa_smud, NULL)
ggplot(mpa_smud_tp)+
  geom_col(aes(x = reorder(Name_short, smud_prop), y = smud_prop)) +
  coord_flip()

# how about PUD on its own?
ggplot(mpa_smud_tp)+
  geom_col(aes(x = reorder(Name_short, smud_prop), y = pud_prop)) +
  coord_flip()

# and TUD?
ggplot(mpa_smud_tp)+
  geom_col(aes(x = reorder(Name_short, smud_prop), y = tud_prop)) +
  coord_flip()

## how about alternative smud made with proportions?
ggplot(mpa_smud_tp)+
  geom_col(aes(x = reorder(Name_short, smud_prop), y = smud2_prop)) +
  coord_flip()

# plotting together
mpa_smud_tall <- mpa_smud %>% 
  st_set_geometry(NULL) %>%
  gather(key = "source", value = "UD", -Name_short, -Country)

ggplot(mpa_smud_tall %>% filter(source %in% c("tud_prop", "pud_prop"))) +
  geom_col(aes(x = reorder(Name_short, UD), y = UD, fill = source)) +
  coord_flip()

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
  summarise_at(vars(avg_ann_pud, avg_ann_tud, avg_ann_smud, smud_prop), sum) %>%
  filter(!is.na(country))

vis_per_cell <- avg_ann_smud %>% 
  left_join(countries_pid, by = "pid") %>%
  select(pid, smud2 = smud_prop, country) %>%
  left_join(countries_ratios %>% filter(socmed == "smud2"), by = "country") %>%
  mutate(est_vis = smud2*ud_to_vis,
         est_exp = round(est_vis*AvgExp)) %>%
  left_join(aoi, by = "pid")

for(source in c("pud", "tud", "smud", "smud2")){
  plots <- ggplot(countries_ratios %>% filter(socmed == source)) +
    geom_col(aes(x = Country, y = ud_to_vis)) +
    labs(title = source)
  print(plots)
}

plots

################ Old ####
# let's spread this data
countries_ratios %>% select(country, socmed, ud_to_vis) %>%
  spread(key = "socmed", value = "ud_to_vis") %>%
  rename(pud_to_vis = avg_ann_pud,
         smud_to_vis = avg_ann_smud,
         tud_to_vis = avg_ann_tud)

# let's pull out smud ratios alone
smud_ratios <- countries_ratios %>% 
  filter(socmed == "avg_ann_smud") %>%
  rename(smud_to_vis = ud_to_vis) %>%
  select(country, smud_to_vis)
##############

# Let's figure out how many SMUD I have across the entire MAR
#total_sm <- avg_ann_smud %>%
#  summarise_at(vars(avg_ann_pud, avg_ann_tud, avg_ann_smud), sum)

# and... figure out the total visitor to avg_ann_smud ratio
#(viz_to_smud <- mar_summary$Visitors/total_sm$avg_ann_smud)
# ok. each avg_ann_smud is equivalent to ~25 visitors
#(viz_to_pud <- mar_summary$Visitors/total_sm$avg_ann_pud)
# and each avg_ann_pud is ~1000 visitors

mpa_summaries_tall <- mpa_smud_tall %>%
  rename(socmed = source) %>%
  mutate(socmed = str_extract(socmed, ".*(?=_)")) %>%
  left_join(countries_ratios, by = c("Country" = "country", "socmed")) %>%
  mutate(visitors = UD*ud_to_vis,
         expenditures = visitors*AvgExp)
mpa_summaries_tall


#mpa_summaries #<- 
mpa_smud %>% 
  left_join(avg_exp, by = "Country") %>%
  left_join(smud_ratios, by = c("Country" = "country"))
  mutate(visitors_smud = smud_prop*smud_to_vis,
         expenditures_smud = visitors_smud*AvgExp,
         visitors_pud = pud_prop*viz_to_pud)
mpa_summaries

## write it out
## NOTE: this doesn't work now since I haven't recoded mpa_summaries to wide format with geometries
#write_sf(mpa_summaries, "~/Documents/MAR/ModelRuns/baseline_5k/viz_and_expends/mpa_summaries.shp")


################ Old and other, supporting code #########
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



################### Comparing viz numbers by country to PUD vs TUD and trying to figure out which one is better####


ggplot(countries_ud_emp) +
  geom_point(aes(y = log1p(vizinAOI), x = log1p(avg_ann_ud))) +
  geom_abline(slope = 1) +
  facet_wrap(~socmed)

# hmm, they all have the appropriate relationship
# though the two middle countries swap for twitter...
countries_ud_emp %>% arrange(desc(vizinAOI))
# belize and honduras swap. twitter thinks honduras has more viz than belize, which isn't born out by the other data

# does a linear model help at all?
mod1 <- lm(log1p(country_summaries$vizinAOI) ~ log1p(avg_ann_pud) + log1p(avg_ann_tud), data = countries_ud)
summary(mod1)
plot(mod1)

mod2 <- lm(country_summaries$vizinAOI ~ -1 + avg_ann_pud + avg_ann_tud, data = countries_ud)
summary(mod2)
plot(mod2)


#####################################################
### Now let's make a plot - Using the mpa_summaries wide version. Now OLD

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


## and, try to put them on a single plot
mpa_sum_tall <- mpa_summaries %>% 
  gather(key = "socmed", value = "visitors", c("visitors_smud", "visitors_pud"))



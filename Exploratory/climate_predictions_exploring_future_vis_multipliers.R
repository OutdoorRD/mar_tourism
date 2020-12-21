## Experimenting with ways of displaying changes in tourism due to climate

## Forked from viz_predict.R on 7/8/20

#####
### Using the Viz Model to predict Viz under different scenarios
###
### First pass test using the corals layer that Jess created
### 4/20/20 SGW

### Updated 5/19 for new corals tests - also working to generalize
### 7/1 Adding climate

### 7/20 Running with newest model and aoi

library(tidyverse)
library(sf)
library(lwgeom)

## Need to: read in model object, existing predictors,
##    AOI, new data layer, run intersection to get model inputs,
##    scale to match data inputs (relevant for non-binary predictors),
##    create "newdata" df that holds all else equal,
##    predict tourism, write out shapefile

## Additionally, for the ROOT change in service rasters, need to run two scenarios 
##    and subtract them from each other

setwd("~/Documents/MAR/")

baselines <- read_csv("mar_tourism/Data/Predictors_Baseline.csv")
climate_vals <- read_csv("mar_tourism/Data/Future_Climate_RCP85_2050s_and_Current.csv")
viz_model_raw <- read_rds("mar_tourism/Models/viz_model_raw.rds")
aoi <- read_sf("ModelRuns/baseline_20200715/T_AOI_v4_5k_32616_pid.shp")

## Getting oriented in naming scheme

# Starting with Belize Restore Coral
country <- "Belize"
#ipm <- "ipm_05" #Restore Coral
#aname <- "rest_corl"
climate <- "clim0" #Baseline climate = clim0; 25th perc = clim1; 75th perc = clim2
#coral_new <- read_sf("ROOT/ROOT_coral_test_20200519/restore_coral_Tourism_CVmodel/MAR_coral_WGS8416N_erase_restored_areasBZ.shp")

# Now doing Belize protect coral
ipm <- "climate" #"imp_06"
aname <- "noact" #"prot_corl"
#coral_new <- read_sf("ROOT/ROOT_coral_test_20200519/protect_coral_Tourism_CVmodel/MAR_coral_WGS8416N_eraseBelize.shp")

#### Joining climate onto baselines
base_climate <- baselines %>%
  left_join(climate_vals, by = "pid")
clim_post <- case_when(climate == "clim0" ~ "",
                       climate == "clim1" ~ "25",
                       climate == "clim2" ~ "75")


#### Create shapefile of fitted values (corals_full equivalent)
## Not rerunning the model, since I'm assuming that corals_full is what I built it with
modeled <- baselines %>%
  dplyr::select(pid, vis_log, est_vis) 

modeled$fitted <- viz_model_raw$fitted.values
modeled$fitted_vis <- exp(modeled$fitted)
modeled

# renaming to reflec tthe climate scenario (just for these visualizations)
#modeled <- modeled %>% rename(preds_75 = preds, preds_vis_75 = preds_vis, diff_vis_75 = diff_vis, diff_log_75 = diff_log)


# build newdata frame

newdata <- base_climate %>%
  dplyr::select(country, 
                coral_prop, #= corals_new, 
                mangrove_prop, 
                beach,
                forest_prop, 
                temp = paste0("temp", clim_post), 
                hotdays = paste0("hotdays", clim_post), 
                precip = paste0("precip", clim_post), 
                wildlife,
                pa_min_dist, 
                ruins, 
                develop, 
                roads,
                cellarea)

preds <- predict(viz_model_raw, newdata = newdata)
modeled$preds <- preds
modeled$preds_vis <- exp(preds)
modeled

# and then, what if future vis are *1.32?
modeled$preds_vis_mult <- modeled$preds_vis*2.67
modeled

# calculate difference
modeled <- modeled %>%
  mutate(diff_vis = round(preds_vis_mult - fitted_vis, 2),
         diff_log = preds - fitted) # need to be careful about this line and what it means for each scenario

# depending on above climate choice, choose one. Then return to top and rerun with other climate choice
modeled0 <- modeled
#modeled25 <- modeled
#modeled75 <- modeled

modeled0 <- modeled0 %>% mutate(climate = "nochange")
modeled25 <- modeled25 %>% mutate(climate = "25Perc")
modeled75 <- modeled75 %>% mutate(climate = "75Perc")

modeled_climate <- bind_rows(modeled0, modeled25, modeled75)
#modeled_climate <- modeled25

# calculating the percent change in visitors
# (preds_vis - fitted_vis) / fitted_vis
# But, since we have negative predictions, this can be more than losing 100%
# Let's truncate to get rid of those weird values
## TODO: This should be easier to deal with if we're working with additional future visitors
modeled_climate <- modeled_climate %>%
  mutate(perc_change = ((preds_vis) - (fitted_vis)) / (fitted_vis) * 100,
         perc_change = if_else(perc_change < -100, -100, perc_change),
         perc_change_mult = ((preds_vis_mult) - (fitted_vis)) / (fitted_vis) * 100,
         perc_change_mult = if_else(perc_change_mult < -100, -100, perc_change_mult))

# join to spatial 
modeled_sp <- aoi %>%
  dplyr::select(pid) %>%
  left_join(modeled_climate, by = "pid") %>%
  filter(!is.na(climate))

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

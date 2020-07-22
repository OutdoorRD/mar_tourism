### 
### MAR Final Visitation Model
### 3/26/20
###

### Forking from viz_model.R to make a cleaner script
### Updating 7/1/20 to make use of my new cleaner preparing predictors scripts

library(tidyverse)
library(corrgram)
library(coefplot)
library(sf)


modplot <- function(x){
  par(mfrow = c(2,2))
  plot(x, ask = F)
  par(mfrow = c(1,1))
}

setwd("~/Documents/MAR/mar_tourism/Data/")

# read in the prepared predictors
aoi_viz_exp <- read_sf("../../ModelRuns/baseline_20200715/aoi_viz_exp.shp")
predictors <- read_csv("NonClimatePredictors_20200721.csv")
climatepreds <- read_csv("Future_Climate_RCP85_2050s_and_Current.csv")

est_vis <- aoi_viz_exp %>%
  st_set_geometry(NULL) %>%
  dplyr::select(pid, est_vis) %>%
  mutate(vis_log = log1p(est_vis))

## drop pids that are slivers
#slivers <- read_csv("../../AOI/AOI_v3/Intersected/slivers.csv")

# combining, using baseline climate
pred_small <- est_vis %>%
  left_join(predictors) %>%
  left_join(climatepreds %>% dplyr::select(pid, temp = temp0, hotdays = hotdays0, precip = precip0)) %>%
  filter(!is.na(precip)) 
summary(pred_small)

# why do I have negative hotdays? Tracked this back to my rasterizing step.
# TODO: replace these with zeros. But I don't think this has a big impact on anything,
# so leaving as is for now

# look at forest
#predsSF <- aoi_viz_exp %>% left_join(predictors)
#ggplot(predsSF) + geom_sf(aes(fill = forest))


#### Exploring variables
# first limiting those I display & mutating to reflect what's actually going into model
#pred_small <- predictors %>%
 # filter(!is.na(est_vis), !pid %in% slivers$pid, !is.na(temp)) %>%
  #dplyr::select(pid, vis_log, est_vis, 
   #             country = Country, 
    #            corals, mangroves, beach, forest, temp, dayshot,
     #           precip, #protected, prop_land, 
      #          wildlife, #C3P, air_min_dist, ports_min_dist, 
       #         pa_min_dist, ruins, #sargassum, 
        #        roads_min_dist,
         #       prop_dev) %>%
  #mutate(developed = as.integer(prop_dev > 0),
   #      roads = as.integer(roads_min_dist == 0)) %>%
  #dplyr::select(-roads_min_dist, -prop_dev)
corrgram(pred_small, upper.panel = panel.pts, lower.panel = panel.cor, diag.panel = panel.density)

# examining climate only
#corrgram(pred_small %>% dplyr::select(vis_log, temp, hotdays, precip), upper.panel = panel.pts, lower.panel = panel.cor)

# does it work if I drop all NAs? And rescale to get everything 0-1?
scale_func <- function(x) (x - min(x))/(max(x) - min(x))
pred_scaled <- pred_small %>% 
  #filter(!is.na(est_vis) & !is.na(temp)) %>%
  mutate(temp = scale_func(temp),
         hotdays = scale_func(hotdays),
         precip = scale_func(precip),
         #daysrain = scale_func(daysrain),
         #C3P = scale_func(C3P),
         #air_min_dist = scale_func(air_min_dist),
         #ports_min_dist = scale_func(ports_min_dist),
         #roads_min_dist = scale_func(roads_min_dist),
         pa_min_dist = scale_func(pa_min_dist),
         cellarea = scale_func(cellarea))
summary(pred_scaled)

vis_model <- lm(vis_log ~ country + coral_prop + mangrove + beach + forest_prop + temp + I(temp^2) + 
              hotdays + precip  + 
                wildlife +
              pa_min_dist + ruins  + develop + roads + cellarea, 
            data = pred_scaled)
summary(vis_model)
# .445 vs .449
# cellarea seems to be an important controlling variable (sig, and brings me from .44 to .467)
modplot(vis_model)
coefplot(vis_model, decreasing = TRUE)
car::vif(vis_model)

# plotting fitted values annd residuals
modcheck <- pred_scaled
modcheck$fitted <- vis_model$fitted.values
modcheck$resids <- vis_model$residuals

modchecksp <- aoi_viz_exp %>% 
  dplyr::select(pid, geometry) %>%
  left_join(modcheck, by = "pid")

ggplot(modchecksp) + geom_sf(aes(fill = vis_log), size = .1)
ggplot(modchecksp) + geom_sf(aes(fill = fitted), size = .1)
ggplot(modchecksp) + geom_sf(aes(fill = resids), size = .1) + scale_fill_distiller(palette = "RdBu")



# plotting indiv relationships
ggplot(pred_small) +
  geom_point(aes(x = temp, y = vis_log), alpha = .2)

ggplot(pred_small) +
  geom_point(aes(x = hotdays, y = vis_log), alpha = .2)

ggplot(pred_small) +
  geom_point(aes(x = jitter(precip), y = vis_log), alpha = .2)



### Ok. I'd like to get a marginal effect plot for temperature
# First, need to create a df that has mean values for everything else, but a range for temp.
# (also, will need to retransform out of the scaled values)
#... actually, since I'm not comparing magnitudes right now, I'll just rebuild the model using raw values and predict from that
vis_model_raw <- lm(vis_log ~ country + coral_prop + mangrove + beach + forest_prop + temp + I(temp^2) + 
                      hotdays + precip  + 
                      wildlife +
                      pa_min_dist + ruins  + develop + roads + cellarea, 
                data = pred_small)
summary(vis_model_raw)

# let's write out the predictors and model objects for both of these and track them
write_csv(pred_small, "Predictors_Baseline.csv")
write_csv(pred_scaled, "Predictors_Baseline_scaled.csv")

write_rds(vis_model, "../Models/viz_model_scaled.rds")
write_rds(vis_model_raw, "../Models/viz_model_raw.rds")





############## OLD ############
newdata_temp <- tibble(temp = seq(min(pred_small$temp, na.rm = T), max(pred_small$temp, na.rm = T), length.out = 50),
       country = "Belize",
       corals = 1,
       mangroves = 1,
       beach = 1,
       forest = 1,
       dayshot = min(pred_small$dayshot, na.rm = T),
       precip = mean(pred_small$precip, na.rm = T),
       wildlife = 1,
       pa_min_dist = mean(pred_small$pa_min_dist),
       ruins = 1,
       developed = 0,
       roads = 0)

newdata_temp$preds <- predict(vis_model_raw, newdata = newdata_temp)
newdata_temp$preds_vis <- expm1(newdata_temp$preds)
newdata_temp

# note that plotting the points here is not fair, since the line is assuming mean values of many other things
ggplot(newdata_temp) +
  geom_line(aes(x = temp, y = preds_vis)) +
#  geom_point(data = pred_small, aes(x = temp, y = est_vis)) +
 # scale_y_log10() +
  theme_classic()
# peak vis at ~23.75 C

#### Let's do a marginal plot for precip
newdata_precip <- tibble(precip = seq(min(pred_small$precip, na.rm = T), max(pred_small$precip, na.rm = T), length.out = 50),
                       country = "Belize",
                       corals = 1,
                       mangroves = 1,
                       beach = 1,
                       forest = 1,
                       dayshot = min(pred_small$dayshot, na.rm = T),
                       temp = mean(pred_small$temp, na.rm = T),
                       wildlife = 1,
                       pa_min_dist = mean(pred_small$pa_min_dist),
                       ruins = 1,
                       developed = 0,
                       roads = 0)

newdata_precip$preds <- predict(vis_model_raw, newdata = newdata_precip)
newdata_precip$preds_vis <- expm1(newdata_precip$preds)
newdata_precip

# note that plotting the points here is not fair, since the line is assuming mean values of many other things
ggplot(newdata_precip) +
  geom_line(aes(x = precip, y = preds)) +
 # geom_point(data = pred_small, aes(x = precip, y = est_vis)) +
#  scale_y_log10() +
  theme_classic()


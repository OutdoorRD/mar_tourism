### 
### MAR Final Visitation Model
### 3/26/20
###

### Forking from viz_model.R to make a cleaner script

library(tidyverse)
library(corrgram)
library(coefplot)


modplot <- function(x){
  par(mfrow = c(2,2))
  plot(x, ask = F)
  par(mfrow = c(1,1))
}

setwd("~/Documents/MAR/GIS/Predictors/Baseline_Inputs/")

# read in the prepared predictors
predictors <- read_csv("CombinedPredictors_20200320.csv")

## drop pids that are slivers
slivers <- read_csv("../../AOI/AOI_v3/Intersected/slivers.csv")


#### Exploring variables
# first limiting those I display
pred_small <- predictors %>%
  filter(!is.na(est_vis), !pid %in% slivers$pid) %>%
  dplyr::select(pid, vis_log, est_vis, Country, corals, mangroves, beach, forest, temp, dayshot,
                precip, daysrain, protected, prop_land, wildlife, C3P,
                air_min_dist, ports_min_dist, pa_min_dist, ruins, sargassum, roads_min_dist,
                prop_dev) 

# does it work if I drop all NAs? And rescale to get everything 0-1?
scale_func <- function(x) (x - min(x))/(max(x) - min(x))
pred_scaled <- pred_small %>% 
  filter(!is.na(est_vis) & !is.na(temp)) %>%
  mutate(temp = scale_func(temp),
         dayshot = scale_func(dayshot),
         precip = scale_func(precip),
         daysrain = scale_func(daysrain),
         C3P = scale_func(C3P),
         air_min_dist = scale_func(air_min_dist),
         ports_min_dist = scale_func(ports_min_dist),
         roads_min_dist = scale_func(roads_min_dist),
         pa_min_dist = scale_func(pa_min_dist))
summary(pred_scaled)

vis_model <- lm(vis_log ~ Country + corals + mangroves + beach + forest + temp + I(temp^2) + 
              dayshot + precip  + 
                wildlife +
              pa_min_dist + ruins  + I(prop_dev>0) + I(roads_min_dist == 0), 
            data = pred_scaled)
summary(vis_model)
# .445 vs .449
modplot(vis_model)
coefplot(vis_model, decreasing = TRUE)

# plotting indiv relationships
ggplot(pred_small) +
  geom_point(aes(x = temp, y = vis_log), alpha = .2)

ggplot(pred_small) +
  geom_point(aes(x = dayshot, y = vis_log), alpha = .2)

ggplot(pred_small) +
  geom_point(aes(x = jitter(precip), y = vis_log), alpha = .2)

### Ok. I'd like to get a marginal effect plot for temperature
# First, need to create a df that has mean values for everything else, but a range for temp.
# (also, will need to retransform out of the scaled values)
#... actually, since I'm not comparing magnitudes right now, I'll just rebuild the model using raw values and predict from that
vis_model_raw <- lm(vis_log ~ Country + corals + mangroves + beach + forest + temp + I(temp^2) + 
                  dayshot + precip  + 
                  wildlife +
                  pa_min_dist + ruins  + I(prop_dev>0) + I(roads_min_dist == 0), 
                data = pred_small)
summary(vis_model_raw)

newdata_temp <- tibble(temp = seq(min(pred_small$temp, na.rm = T), max(pred_small$temp, na.rm = T), length.out = 50),
       Country = "Belize",
       corals = 1,
       mangroves = 1,
       beach = 1,
       forest = 1,
       dayshot = min(pred_small$dayshot, na.rm = T),
       precip = mean(pred_small$precip, na.rm = T),
       wildlife = 1,
       pa_min_dist = mean(pred_small$pa_min_dist),
       ruins = 1,
       prop_dev = 0,
       roads_min_dist = 0)

newdata_temp$preds <- predict(vis_model_raw, newdata = newdata_temp)
newdata_temp$preds_vis <- expm1(newdata_temp$preds)
newdata_temp

ggplot(newdata_temp) +
  geom_line(aes(x = temp, y = preds_vis)) +
  theme_classic()
# peak vis at ~23.75 C
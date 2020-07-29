###
### Making a marginal plot for temperature 
### (to identify "peak" temp)

### Forked from bottom of viz_model_clean.R on 7/29/20

library(tidyverse)

setwd("~/Documents/MAR/mar_tourism/")

pred_small <- read_csv("Data/Predictors_Baseline.csv")
viz_model_raw <- read_rds("Models/viz_model_raw.rds")

pred_small

## marginal plot for temp
newdata_temp <- tibble(temp = seq(min(pred_small$temp, na.rm = T), max(pred_small$temp, na.rm = T), length.out = 50),
                       country = "Belize",
                       coral_prop = 1,
                       mangrove = 1,
                       beach = 1,
                       forest_prop = 1,
                       hotdays = min(pred_small$hotdays, na.rm = T),
                       precip = mean(pred_small$precip, na.rm = T),
                       wildlife = 1,
                       pa_min_dist = mean(pred_small$pa_min_dist),
                       ruins = 1,
                       develop = 0,
                       roads = 0,
                       cellarea = max(pred_small$cellarea))

newdata_temp$preds <- predict(viz_model_raw, newdata = newdata_temp)
newdata_temp$preds_vis <- expm1(newdata_temp$preds)
newdata_temp

# note that plotting the points here is not fair, since the line is assuming mean values of many other things
ggplot(newdata_temp) +
  geom_line(aes(x = temp, y = preds_vis)) +
  #  geom_point(data = pred_small, aes(x = temp, y = est_vis)) +
  # scale_y_log10() +
  theme_classic()

newdata_temp %>%
  arrange(desc(preds_vis))
# peak vis at 24.6 
# this is invariant to changes in newdata, which just shift the line up or down, not side to side


#### Let's do a marginal plot for precip
newdata_precip <- tibble(precip = seq(min(pred_small$precip, na.rm = T), max(pred_small$precip, na.rm = T), length.out = 50),
                         country = "Belize",
                         coral_prop = 1,
                         mangrove = 1,
                         beach = 1,
                         forest_prop = 1,
                         hotdays = min(pred_small$hotdays, na.rm = T),
                         temp = mean(pred_small$temp, na.rm = T),
                         wildlife = 1,
                         pa_min_dist = mean(pred_small$pa_min_dist),
                         ruins = 1,
                         develop = 0,
                         roads = 0,
                         cellarea = max(pred_small$cellarea))

newdata_precip$preds <- predict(viz_model_raw, newdata = newdata_precip)
newdata_precip$preds_vis <- expm1(newdata_precip$preds)
newdata_precip

# note that plotting the points here is not fair, since the line is assuming mean values of many other things
ggplot(newdata_precip) +
  geom_line(aes(x = precip, y = preds_vis)) +
  # geom_point(data = pred_small, aes(x = precip, y = est_vis)) +
  #  scale_y_log10() +
  theme_classic()

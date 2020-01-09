#### MAR Visitation Model #####
### January 9, 2020
### SGW

library(tidyverse)


modplot <- function(x){
  par(mfrow = c(2,2))
  plot(x, ask = F)
  par(mfrow = c(1,1))
}

setwd("~/Documents/MAR/GIS/Predictors/Baseline_Inputs/")

# read in the prepared predictors
predictors <- read_csv("CombinedPredictors_010920.csv")

summary(predictors)
# hmm. why do i have an NA in the vis numbers?

predictors %>% filter(is.na(est_vis))
# weird. I can't even see it in QGIS, it must be a tiny sliver

#### Exploring variables
# first limiting those I display
pred_small <- predictors %>%
  dplyr::select(pid, vis_log, est_vis, Country, corals, mangroves, beach, temp, dayshot,
         precip, daysrain, protected, prop_land, wildlife, C3P,
         air_min_dist, ports_min_dist, ruins, sargassum, roads_min_dist)

corrgram(pred_small, diag.panel = panel.density, lower.panel = panel.cor, upper.panel = panel.pts)

# precipitation and days of rain are highly correlated. Will need to choose just one to include
mod1 <- lm(vis_log ~ Country + corals + mangroves + beach + temp + I(temp^2) + 
             dayshot + precip + protected + prop_land + wildlife +
             C3P + air_min_dist + ports_min_dist + ruins + sargassum + roads_min_dist, 
           data = pred_small)
summary(mod1)
#plot(mod1)

mod2 <- MASS::glm.nb(round(est_vis) ~ Country + corals + mangroves + beach + temp + I(temp^2) + 
                       dayshot + precip + protected + prop_land + wildlife +
                       C3P + air_min_dist + ports_min_dist + ruins + sargassum + roads_min_dist,
                     data = pred_small)
summary(mod2)
# doesn't work

## Removing "protected"
mod3 <- lm(vis_log ~ Country + corals + mangroves + beach + temp + I(temp^2) + 
             dayshot + precip + prop_land + wildlife +
             C3P + air_min_dist + ports_min_dist + ruins + sargassum + roads_min_dist, 
           data = pred_small)
summary(mod3)

mod3a <- MASS::glm.nb(round(est_vis)~ Country + corals + mangroves + beach + temp + I(temp^2) + 
                        dayshot + precip + prop_land + wildlife +
                        C3P + air_min_dist + ports_min_dist + ruins + sargassum + roads_min_dist, 
                      data = pred_small)

# does it work if I drop all NAs? And rescale to get everything 0-1?
scale_func <- function(x) (x - min(x))/max(x - min(x))
pred_scaled <- pred_small %>% 
  filter(!is.na(est_vis) & !is.na(temp)) %>%
  mutate(temp = scale_func(temp),
         dayshot = scale_func(dayshot),
         precip = scale_func(precip),
         daysrain = scale_func(daysrain),
         C3P = scale_func(C3P),
         air_min_dist = scale_func(air_min_dist),
         ports_min_dist = scale_func(ports_min_dist),
         roads_min_dist = scale_func(roads_min_dist))
summary(pred_scaled)


mod3a <- MASS::glm.nb(round(est_vis)~ as.factor(Country) + corals + mangroves + beach + temp + I(temp^2) + 
                        dayshot + precip + prop_land + wildlife +
                        C3P + air_min_dist + ports_min_dist + ruins + sargassum + roads_min_dist, 
                      data = pred_scaled)
# still no on this one.

# how does the lm look with scaled values?
mod4 <- lm(vis_log ~ Country + corals + mangroves + beach + temp + I(temp^2) + 
             dayshot + precip + prop_land + wildlife +
             C3P + air_min_dist + ports_min_dist + ruins + sargassum + roads_min_dist, 
           data = pred_scaled)
summary(mod4)
# samesies, but now with comparable estimates
modplot(mod4)
# not as bad as I imagined
residuals(mod4)

# I want to look at these spatially, so I'm going to write them out
pred_scaled$resids <- residuals(mod4)
pred_scaled$fitted <- fitted.values(mod4)
write_csv(pred_scaled, "../../../ModelRuns/baseline_5k_intersect/modeling/mod4_010920.csv")

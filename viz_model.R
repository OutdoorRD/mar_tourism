#### MAR Visitation Model #####
### January 9, 2020
### SGW

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
predictors <- read_csv("CombinedPredictors_010920.csv")

summary(predictors)
# hmm. why do i have an NA in the vis numbers?

predictors %>% filter(is.na(est_vis))
# weird. I can't even see it in QGIS, it must be a tiny sliver

#### Exploring variables
# first limiting those I display
pred_small <- predictors %>%
  filter(!is.na(est_vis)) %>%
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
# only R2 = .38
modplot(mod1)
# not great, but not as terrible as they could be...

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
modplot(mod3)

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
#write_csv(pred_scaled, "../../../ModelRuns/baseline_5k_intersect/modeling/mod4_010920.csv")

# Let's use mod4 as our best model for now, and make a coefplot for it
coefplot(mod4)

### And... to try to get the negbin model to fit, let's get starting values from a poisson
mod4pois <- glm(round(est_vis)~ as.factor(Country) + corals + mangroves + beach + temp + I(temp^2) + 
                  dayshot + precip + prop_land + wildlife +
                  C3P + air_min_dist + ports_min_dist + ruins + sargassum + roads_min_dist, 
                data = pred_scaled, family = poisson)
summary(mod4pois)
starting <- coef(mod4pois)

mod4nb <- MASS::glm.nb(round(est_vis)~ as.factor(Country) + corals + mangroves + beach + temp + I(temp^2) + 
                   dayshot + precip + prop_land + wildlife +
                   C3P + air_min_dist + ports_min_dist + ruins + sargassum + roads_min_dist, 
                 data = pred_scaled, start = starting, control = glm.control(maxit = 50, trace = 10))
# no go. does it work with fewer predictors?

modsmall <-  MASS::glm.nb(round(est_vis)~ corals + mangroves + beach + temp + I(temp^2) + 
                            dayshot + precip + prop_land + wildlife +
                            C3P + air_min_dist + ports_min_dist  + roads_min_dist, 
                          data = pred_scaled)
# needs starting values
modsmallpois <- glm(round(est_vis)~ corals + mangroves + beach + temp + I(temp^2) + 
                      dayshot + precip + prop_land + wildlife +
                      C3P + air_min_dist + ports_min_dist  + roads_min_dist, 
                    data = pred_scaled, family = poisson)
startsmall <- coef(modsmallpois)
modsmall <- MASS::glm.nb(round(est_vis)~ corals + mangroves + beach + temp + I(temp^2) + 
                           dayshot + precip + prop_land + wildlife +
                           C3P + air_min_dist + ports_min_dist  + roads_min_dist, 
                         start = unname(startsmall),
                         data = pred_scaled)
# nope. how about smaller still?

modsmaller <-  MASS::glm.nb(round(est_vis)~ temp + I(temp^2) + 
                            prop_land, 
                          data = pred_scaled)
summary(modsmaller)
# wow. I had to get way down there. let's try using its init theta and see if i can go up

modsmaller2 <- MASS::glm.nb(round(est_vis)~ corals + temp + I(temp^2) + 
                              prop_land, 
                            data = pred_scaled, init.theta = 0.077)
# ok. So the real issue is the huge outliers- some cells which are ridiculously greater than
# their surroudings. Neg bin is probably not the right fit either. Let's try exploring a bit

dotchart(round(pred_scaled$est_vis))
summary(pred_scaled$est_vis)
# so the data say that 571,000 people visited by most popular 5km grid cell
# And that over a quarter of the cells saw 0 visitors.
# The first just seems so unlikely.

# But... looking at where these extreme values are, I do belive
# there's a lot of tourism there (cancun, etc)

# Let's try a zero inflated neg bin (ZINB) and see if it works better
# following: https://stats.idre.ucla.edu/r/dae/zinb/

library(pscl)
library(MASS)
library(boot)

# the variables after the | are those that go into the binomial regression
# (eg post vs no post)
modzinb <- zeroinfl(round(est_vis)~ as.factor(Country) + corals + mangroves + beach + temp + I(temp^2) + 
                      dayshot + precip + wildlife +
                      C3P + air_min_dist + ports_min_dist + ruins + sargassum + roads_min_dist |
                      prop_land, 
                    data = pred_scaled,
                    dist = "negbin", 
                    EM = TRUE)
# shocking. Same error as before

modzinb <- MASS::glm.nb(round(est_vis)~ roads_min_dist, 
                    data = pred_scaled)
plot(hist(pred_scaled$roads_min_dist))
plot(est_vis ~ roads_min_dist, data = pred_scaled)
plot(vis_log ~ log1p(roads_min_dist), data = pred_scaled)
plot(hist(log1p(pred_scaled$roads_min_dist)))

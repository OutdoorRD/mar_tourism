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
predictors <- read_csv("CombinedPredictors_20200320.csv")

summary(predictors)
# hmm. why do i have an NA in the vis numbers?

## drop pids that are slivers
slivers <- read_csv("../../AOI/AOI_v3/Intersected/slivers.csv")

predictors %>% filter(is.na(est_vis))
# weird. I can't even see it in QGIS, it must be a tiny sliver

#### Exploring variables
# first limiting those I display
pred_small <- predictors %>%
  filter(!is.na(est_vis), !pid %in% slivers$pid) %>%
  dplyr::select(pid, vis_log, est_vis, Country, corals, mangroves, beach, forest, temp, dayshot,
         precip, daysrain, protected, prop_land, wildlife, C3P,
         air_min_dist, ports_min_dist, pa_min_dist, ruins, sargassum, roads_min_dist,
         prop_dev) 

corrgram(pred_small, diag.panel = panel.density, lower.panel = panel.cor, upper.panel = panel.pts)

# precipitation and days of rain are highly correlated. Will need to choose just one to include
# prop_dev (proportion of cell that is developed), is better than worldpop. let's replace them

mod1 <- lm(vis_log ~ Country + corals + mangroves + beach + forest + temp + I(temp^2) + 
             dayshot + precip + protected + prop_land + wildlife +
             C3P + pa_min_dist + ruins + sargassum + I(prop_dev>0) + I(roads_min_dist==0), 
           data = pred_small)
summary(mod1)
# only R2 = .38. Adding WorldPop helps, but still only .41. Replacing worldpop with prop_dev takes me to .43
# Taking the log of worldpop bumps it to .45
modplot(mod1)
# not great, but not as terrible as they could be...
car::vif(mod1)
# there's some not great correlation in here
# specifically (as of 3/20), roads_min_dist, prop_land, c3p, and dayshot
# c3p is fairly correlated with precip.

# is there a high correlation between roads in cell and dev?
dev_roads <- pred_small %>% 
  dplyr::select(vis_log, prop_dev, roads_min_dist) %>%
  mutate(developed = as.integer(prop_dev >0),
         roads = as.integer(roads_min_dist == 0))

corrgram(dev_roads, diag.panel = panel.density, lower.panel = panel.cor, upper.panel = panel.pts)

cor(dev_roads$developed, dev_roads$roads) # .51. fairly high, but not crazy
glm(dev_roads$developed ~ dev_roads$roads)

# adding roads seems to help (not distance from roads, just whether or not a road is in the hex)
# And while they are correlated (.51), the VIF numbers are not actually that high


# more parsimonious model
mod1a <- lm(vis_log ~ Country + corals + mangroves + beach + forest + temp + I(temp^2) + 
              dayshot + precip  + wildlife +
              pa_min_dist + ruins + sargassum + I(prop_dev>0) + I(roads_min_dist == 0), 
            data = pred_small)
summary(mod1a)
modplot(mod1a)
car::vif(mod1a)
coefplot(mod1a)

mod2 <- MASS::glm.nb(round(est_vis) ~ Country + corals + mangroves + beach + forest + temp + I(temp^2) + 
                       dayshot + precip + wildlife +
                       pa_min_dist + ruins + sargassum  + I(prop_dev>0),
                     data = pred_small)
summary(mod2)
# doesn't work

## Removing "protected"
mod3 <- lm(vis_log ~ Country + corals + mangroves + beach + temp + I(temp^2) + 
             dayshot + precip + prop_land + wildlife +
             I(C3P*100) + air_min_dist + ports_min_dist + ruins + sargassum + 
             roads_min_dist + prop_dev, 
           data = pred_small)
summary(mod3)
modplot(mod3)
coefplot(mod3) #+ xlim(c(-5,5))

mod3a <- MASS::glm.nb(round(est_vis)~ Country + corals + mangroves + beach + temp + I(temp^2) + 
                        dayshot + precip + prop_land + wildlife +
                        C3P + air_min_dist + ports_min_dist + ruins + sargassum + roads_min_dist + prop_dev, 
                      data = pred_small)

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


mod3a <- MASS::glm.nb(round(est_vis)~ as.factor(Country) + corals + mangroves + beach + forest + temp + I(temp^2) + 
                        dayshot + precip + wildlife +
                        pa_min_dist + ruins + sargassum + I(prop_dev>0), 
                      data = pred_scaled)

# still no on this one.

# how does the lm look with scaled values?
mod4 <- lm(vis_log ~ Country + corals + mangroves + beach + forest + temp + I(temp^2) + 
             dayshot + precip  + wildlife +
             pa_min_dist + ruins + sargassum + I(prop_dev>0) + I(roads_min_dist == 0), 
           data = pred_scaled)
summary(mod4)
# samesies, but now with comparable estimates
## Actually, different with the scaled vs unscaled log(worldpop). Better not scaled
##  But still bad if I do everything else scaled, except log(worldpop). Why?
modplot(mod4)
# not as bad as I imagined (though adding worldpop actualy makes it worse)
#residuals(mod4)
car::vif(mod4)

# I want to look at these spatially, so I'm going to write them out
pred_scaled$resids_rds <- residuals(mod4)
pred_scaled$fitted_rds <- fitted.values(mod4)
write_csv(pred_scaled, "../../../ModelRuns/baseline_5k_intersect/modeling/mod4_032020.csv")

# Let's use mod4 as our best model for now, and make a coefplot for it
coefplot(mod4, decreasing = TRUE)
# ha. worldpop

corrgram(pred_scaled, upper.panel = panel.pts, diag.panel = panel.density, lower.panel = panel.cor)
#corrgram(pred_small, upper.panel = panel.pts)

## no sargassum

mod4a <- lm(vis_log ~ Country + corals + mangroves + beach + forest + temp + I(temp^2) + 
             dayshot + precip  + wildlife +
             pa_min_dist + ruins  + I(prop_dev>0) + I(roads_min_dist == 0), 
           data = pred_scaled)
summary(mod4a)
# .445 vs .449
modplot(mod4a)
coefplot(mod4a, decreasing = TRUE)
# beach is a little more important if I drop sargassum. Otherwise equivalent. Probaly this is the better mdoel

## Let's try for more parsimony
# roads_min_dist conflates with a number of others. and prop_land and prop_dev are pretty related
# Also maybe drop country?
mod5 <- lm(vis_log ~ corals + mangroves + beach + temp + I(temp^2) + 
             dayshot + precip  + wildlife + prop_land +
             C3P + air_min_dist  + ruins + sargassum + 
            roads_min_dist +  I(prop_dev>0), 
           data = pred_scaled)
summary(mod5)
modplot(mod5)
# hmm. roads_min_dist seems pretty important (.43 to .40)
# dropping land seems ok (.43 to .42), except that it makes dayshot and precip insig. I wonder if I should have an interaction with precip?
##  Definitely the climate data was not as good offshore
# Country also important - .43 to .408
## Turning Developed into a binary helps (.408 to .426)


#### How well can I model tourism over land? And how much occurs not over land?

land_only <- pred_small %>% filter(prop_land > 0)
ocean_only <- pred_small %>% filter(prop_land == 0)
summary(ocean_only)
dotchart(ocean_only$est_vis)
ocean_only %>%
  arrange(desc(est_vis))
# At least the top couple are adjacent to land. I'd need to modify this to make it more sophisticated

# But for now, let's see if the model runs better on land only
modland <- lm(vis_log ~ Country + corals + mangroves + beach + temp + I(temp^2) + 
             dayshot + precip + wildlife +
             C3P + air_min_dist + ports_min_dist + ruins + sargassum + 
             roads_min_dist + I(prop_dev>0), 
           data = land_only)
summary(modland) # no, actually. bummer
modplot(modland)


###################################################
### And... to try to get the negbin model to fit, let's get starting values from a poisson
mod4pois <- glm(round(est_vis)~ as.factor(Country) + corals + mangroves + beach + temp + I(temp^2) + 
                  dayshot + precip + prop_land + wildlife +
                  C3P + air_min_dist + ports_min_dist + ruins + sargassum + roads_min_dist + WorldPop, 
                data = pred_scaled, family = poisson)
summary(mod4pois)
starting <- coef(mod4pois)

mod4nb <- MASS::glm.nb(round(est_vis)~ as.factor(Country) + corals + mangroves + beach + temp + I(temp^2) + 
                   dayshot + precip + prop_land + wildlife +
                   C3P + air_min_dist + ports_min_dist + ruins + sargassum + roads_min_dist + WorldPop, 
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

# How about a bayesian negbin model?
library(rstanarm)
options(mc.cores = parallel::detectCores())
mod_nb_bayes <- stan_glm.nb(round(est_vis)~ as.factor(Country) + corals + mangroves + beach + forest + temp + I(temp^2) + 
                              dayshot + precip  + wildlife +
                              pa_min_dist + ruins + sargassum + I(prop_dev>0) + I(roads_min_dist == 0), 
            data = pred_scaled)
# started at 1:00, ended 1:15 (with 4 cores) (w/o worldpop)
# started at 3:28, ended at 3:41, (w/ worldpop)
# started at 3:28 (3/20/20), ended at 3:48

summary(mod_nb_bayes)
launch_shinystan(mod_nb_bayes)
# convergence looks pretty good. Really actually happy with all the trace plots
## BUT... adding WorldPop leads to a weird super outlier, that I don't love

plot(mod_nb_bayes)
# some interesting differences from mod4 (linear)
# as of 3/20, actually quite similar to linear model

pp_validate(mod_nb_bayes)

# does this work?
r2 <- bayes_R2(mod_nb_bayes)
plot(density(r2))
# made something... maybe telling me that r2 ~= .54? If so, this is better than the lin mod

# make data of fitted values and resids
mod_bayes_nums <- pred_scaled
mod_bayes_nums$fitted <- fitted(mod_nb_bayes)
mod_bayes_nums$resids <- residuals(mod_nb_bayes)

# write it out
#write_csv(mod_bayes_nums, "../../../ModelRuns/baseline_5k_intersect/modeling/modbayes_032020.csv")

# let's save the model object too
#write_rds(mod_nb_bayes, "../../../ModelRuns/baseline_5k_intersect/modeling/mod_nb_bayes_032020.rds")

plot(predictors$est_vis ~ predictors$WorldPop) 
abline(a = 0, b = 1)


#### Let's do it on the non-scaled predictors (since this seems btter with logWorldPop)
mod_nb_bayes_ns <- stan_glm.nb(round(est_vis)~ as.factor(Country) + corals + mangroves + beach + temp + I(temp^2) + 
                              dayshot + precip + prop_land + wildlife +
                              C3P + air_min_dist + ports_min_dist + ruins + sargassum + 
                              roads_min_dist + log1p(WorldPop), 
                            data = pred_small)
# started at 9:06

summary(mod_nb_bayes_ns)
launch_shinystan(mod_nb_bayes_ns)
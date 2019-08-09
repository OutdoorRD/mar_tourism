###
### Determining best way to combine PUD and TUD to get a viz estimate
### Using the empirical data from Belize district level vistation 


## forked from comparingPUDtoEstVis.R on 8/8/19

library(raster)
library(tidyverse)
library(corrgram)

setwd("~/Documents/MAR/BelizeGuatAdmin/")

# pull in data
pudavgann <- read_csv("flickr/pud/userdays_avg_annual_bypid.csv")
tudavgann <- read_csv("twitter/tud/userdays_avg_annual_bypid.csv")
aoi <- "Guat_Belize_Admin_pid.shp"

shp <- shapefile(aoi)

data <- pudavgann %>% 
  mutate(socmed = "flickr") %>%
  bind_rows(tudavgann %>%
              mutate(socmed = "twitter")) %>%
  left_join(shp@data)
data

##### Belize
# pulling in data from Estimated visitors to destinations by month 2015-2018
# It looks like these data are generated as percentages of some total number of visitors
belize_dists <- read_csv("../Data/Belize_Visitation/Created/Reshaped Estimated Visits to Destinations by Month 2015 - Oct 2018.csv")

district_viz <- belize_dists %>%
  group_by(District) %>%
  summarise(Vis2015 = sum(EstVis2015),
            Vis2016 = sum(EstVis2016),
            Vis2017 = sum(EstVis2017)) %>%
  rowwise() %>%
  mutate(VisAvg = mean(c(Vis2015, Vis2016, Vis2017)),
         VisAvglg = log1p(VisAvg))


belize_socmed_ests <- data %>% 
  inner_join(district_viz, by = c("NAME_1" = "District")) %>%
  mutate(avg_ann_ud_lg = log1p(avg_ann_ud))

belize_socmed_ests

# let's plot it
ggplot(belize_socmed_ests, aes(x = avg_ann_ud_lg, y = VisAvglg, col = socmed)) +
  geom_point() +
  geom_label(aes(label = NAME_1))
# weird. Toledo has more flickr posts than we would expect based on how many people say they go there

# ok, now to spread the data
belize_socmed_wide <- belize_socmed_ests %>% 
  dplyr::select(-avg_ann_ud_lg) %>%
  spread(key = socmed, value = avg_ann_ud) %>%
  rename(TUD = twitter, PUD = flickr) %>%
  mutate(PUDlg = log1p(PUD),
         TUDlg = log1p(TUD))

corrgram(belize_socmed_wide %>% dplyr::select(VisAvg, PUD, TUD), 
         upper.panel = panel.pts, lower.panel = panel.cor)
corrgram(belize_socmed_wide %>% dplyr::select(VisAvglg, PUDlg, TUDlg), 
         upper.panel = panel.pts, lower.panel = panel.cor)

## and... how to combine PUD and TUD to get the best estimate of vis?
lincombo <- lm(VisAvglg ~ -1 + PUDlg + TUDlg, data = belize_socmed_wide)
summary(lincombo)

lincombo_test <- belize_socmed_wide %>%
  mutate(testlin = 0.8182*PUDlg + 1.1081*TUDlg)
ggplot(lincombo_test, aes(x = testlin, y = VisAvglg)) + geom_point() + geom_abline(slope =1)

lin2 <- lm(VisAvg ~ -1 + PUDlg + TUDlg, data = belize_socmed_wide)
summary(lin2)

lin3 <- lm(VisAvg ~ -1 + PUD + TUD, data = belize_socmed_wide)
summary(lin3)
# we're using lin3 coefficients for SMUD!
# So, SMUD = 19*PUD + 101*TUD

lin3_test <- belize_socmed_wide %>%
  mutate(test = 18.76*PUD + 101.32*TUD)
ggplot(lin3_test, aes(x = test, y = VisAvg)) + 
  geom_point() +
  geom_abline(slope = 1) +
  geom_point(aes(x = (.19*PUD + 1.01*TUD)*100), col = "green")


##### And... let's try briefly if I first scale TUD and PUD to each be proportions, and then add them up
# In theory, this should give equal weight to each

prop_test <- belize_socmed_wide %>%
  select(NAME_1, VisAvg, VisAvglg, PUD, TUD, PUDlg, TUDlg) %>%
  mutate(PUD_prop = PUD/ sum(PUD),
         TUD_prop = TUD/ sum(TUD),
         SMUD_prop = PUD_prop + TUD_prop)
ggplot(prop_test) +
  geom_point(aes(x = SMUD_prop, y = VisAvg))

lin4 <- lm(VisAvg ~ -1 + PUD_prop + TUD_prop, data = prop_test)
summary(lin4)

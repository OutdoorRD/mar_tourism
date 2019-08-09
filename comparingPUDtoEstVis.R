###
### Comparing Empirical visitation data from Belize and Guatemala Districts to various social media platforms
###


library(readr)
library(tidyverse)
library(raster)

setwd("~/Documents/MAR/BelizeGuatAdmin/")

# pull in data
pudavgann <- read_csv("flickr/pud/userdays_avg_annual_bypid.csv")
tudavgann <- read_csv("twitter/tud/userdays_avg_annual_bypid.csv")
aoi <- "Guat_Belize_Admin_pid.shp"


### TripAdvisor
TA <- read_csv("tripadvisorTopics.csv")
# NOTE: split topics about the "Belize Cayes" evenly between Belize District
# and Corozal District. So assuming that these posts are primarily about Caye 
# Caulker and surroundings (which are in BZ district), and about Ambergris 
# Caye (which may technically be in BZ district as well, but is classified
# as Corozal district in all other parts of this comparison). The 50/50 split
# comes from the EStimated Visits to destinations by Month spreadsheet, where
# the two Cayes see approx equal vis.
# TODO: However, in that spreadsheet Ambergris is actually a little higher, so
# could check sensitivity of this
TA_grouped <- TA %>%
  group_by(District) %>%
  summarise(TA_topics = sum(TA_topics),
            avg_ann_ud = TA_topics/15) %>% # 2004-2019 timespan, not actually sure of unique users
  mutate(socmed = "tripadvisor",
         TA_topics_lg = log1p(TA_topics),
         avg_ann_ud_lg = log1p(avg_ann_ud))

#### 
shp <- shapefile(aoi)

data <- pudavgann %>% 
  mutate(socmed = "flickr") %>%
  bind_rows(tudavgann %>%
              mutate(socmed = "twitter")) %>%
  left_join(shp@data)


## Guatemala
annvis <- read_csv("districts.csv")
annvis <- annvis %>% rename(domtourists = `2017 domestic tourists (out of 9,416,340 for guatemala)`)


joined <- annvis %>% 
  dplyr::select(-ID_1) %>% 
  inner_join(data)

ggplot(joined) +
  geom_point(aes(x = log1p(domtourists), y = log1p(avg_ann_ud), col = socmed))

mutated <- joined %>% filter(!is.na(domtourists)) %>%
  mutate(domtouristslg = log1p(domtourists),
         avg_ann_ud_lg = log1p(avg_ann_ud))

cor(mutated$domtouristslg, mutated$avg_ann_ud_lg)
# the correlation between log average annual userdays and log domestic tourist estimate 
# by Guatemala department is 0.56. This is not terrible. Not amazing, but not terrible

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

####
belize_TA <- TA_grouped %>% left_join(district_viz)

####

belize_socmed_ests <- data %>% 
  inner_join(district_viz, by = c("NAME_1" = "District")) %>%
  mutate(avg_ann_ud_lg = log1p(avg_ann_ud))

# binding on tripadvisor
belize_3_socmed_ests <- belize_socmed_ests %>%
  dplyr::select(avg_ann_ud, avg_ann_ud_lg, socmed, NAME_1, VisAvg, VisAvglg) %>%
  rename(District = NAME_1) %>%
  bind_rows(belize_TA %>%
              dplyr::select(avg_ann_ud, avg_ann_ud_lg, socmed, District, VisAvg, VisAvglg))
  
ggplot(belize_3_socmed_ests, aes(x = log1p(VisAvg), y = log1p(avg_ann_ud), 
                                 label = District, col = socmed)) +
  geom_point() +
  geom_label() +
  geom_abline(slope =1) +
  scale_x_continuous(limits = c(0, 13)) +
  scale_y_continuous(limits = c(0, 13))

# looking at correlation

belize_flickr <- belize_socmed_ests %>% filter(socmed == "flickr")

cor(belize_flickr$VisAvglg, belize_flickr$avg_ann_ud_lg)
# 0.70 correlation between PUD and estimated viz

# twitter
belize_twitter <- belize_socmed_ests %>% filter(socmed == "twitter")
cor(belize_twitter$VisAvglg, belize_twitter$avg_ann_ud_lg)
# 0.97 correlation with twitter! (Really?)

# tripadvisor
ggplot(belize_TA, aes(x = VisAvglg, y = TA_topics_lg)) +
  geom_point() +
  geom_label(aes(label = District))

cor(belize_TA$VisAvglg, belize_TA$TA_topics_lg)
# 0.98 correlation!

# how about with PUD + TUD + TA?
belize_summed <- belize_3_socmed_ests %>% group_by(District) %>%
  summarise(avg_ann_ud = sum(avg_ann_ud),
            VisAvglg = min(VisAvglg)) %>%
  mutate(socmed = "flickr+twitter+TA",
         avg_ann_ud_lg = log1p(avg_ann_ud))

ggplot(belize_summed) +
  geom_point(aes(x = VisAvglg, y = avg_ann_ud_lg)) +
  geom_abline(slope = 1) +
  scale_y_continuous(limits = c(5, 13)) +
  scale_x_continuous(limits = c(5, 13))

cor(belize_summed$VisAvglg, belize_summed$avg_ann_ud_lg)
# 0.96 correlation between the sum of pud and tud and estimated avg annual viz
# 0.97 corr between sum of pud+tud+TA and estimated avg annual viz

# This seems very good. Twitter is definitely dominating, since its numbers are so
# much larger. Need to think about it...

# what about if I do a SLR to figure out better linear combo?
# reshape data first
belize_wide_lg <- belize_3_socmed_ests %>%
  dplyr::select(-avg_ann_ud) %>%
  spread(key = "socmed", value = "avg_ann_ud_lg")
lincombo <- lm(VisAvglg ~ flickr + tripadvisor + twitter, data = belize_wide_lg)

summary(lincombo)
# hmm. strange that none are sig
plot(lincombo)
pairs(belize_wide_lg[,c("VisAvglg", "flickr", "tripadvisor", "twitter")])

# how about without flickr?
lin2 <- lm(VisAvglg ~ tripadvisor + twitter, data = belize_wide_lg)
summary(lin2)

# maybe it's just the small sample size?

# what about the combined obtion
lin3 <- lm(VisAvglg ~ avg_ann_ud_lg, data = belize_summed)
summary(lin3)



### OLD ####
belize <- joined %>% filter(!is.na(`2015Viz`)) %>%
  mutate(Viz2015lg = log1p(`2015Viz`),
         avg_ann_ud_lg = log1p(avg_ann_ud))

cor(belize$Viz2015lg, belize$avg_ann_ud_lg)

# correlation between these semi random 2015 estimates of viz to each district in belize
# show a 0.75 correlation with PUD from those same districts. 

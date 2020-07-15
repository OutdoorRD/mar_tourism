## Preparing social media response ###
## MAR project

library(sf)
library(tidyverse)
library(lubridate)

setwd("~/Documents/MAR/ModelRuns/baseline_20200715/")

## QUESTION: Timescale. Does it make sense to just add together average annual user-days from two 
# different timeframes (2005-2018 vs 2012-2018)? This would get me the most data. Could be justified
# if we assumed that preferences hadn't changed over time (but gets into questions about the 
# environmental variables remaining at the same scale).

## For now, let's make the code robust enough to allow me to choose later.
# So... pulling in monthly data and I will do my own temporal aggregations
# (monthly data includes all month*pid combos, while daily data does not)

pud_monthly <- read_csv("pud/userdays_total_monthly_bypid.csv")
tud_monthly <- read_csv("tud/userdays_total_monthly_bypid.csv")

# create date column
pud_monthly <- pud_monthly %>% mutate(date = ymd(paste0(year, "-", month, "-01")))
tud_monthly <- tud_monthly %>% mutate(date = ymd(paste0(year, "-", month, "-01")))

# choose PUD timespan
pud_low_date <- ymd("2005-01-01") # change this if desired. Currenlty using all data
pud_sub <- pud_monthly %>% filter(date >= pud_low_date)

# calculating average annual ud
avg_ann_pud <- pud_sub %>% 
  group_by(pid, floor_date(date, unit = "year")) %>%
  summarise(annual_ud = sum(userdays)) %>%
  group_by(pid) %>%
  summarise(avg_ann_pud = mean(annual_ud))

avg_ann_tud <- tud_monthly %>% 
  group_by(pid, floor_date(date, unit = "year")) %>%
  summarise(annual_ud = sum(userdays)) %>%
  group_by(pid) %>%
  summarise(avg_ann_tud = mean(annual_ud))

## combining them
## For now (8/8/19), using the "viz model" for Belize districts which says that 
##  Avg_ann_viz = 19*PUD + 101*TUD
## As of 10/9 (and also before, added this in before the August workshop), also calculating "smud_prop"
##   This one gives equal weight to the distribution of TUD and PUD, and is what I used for the workshop results
## TODO: Figure out this relationship (build a viz model) for the Bahamaian MPAs and Nat Parks

## NOTE: Variability in flickr PUD is totally washed out when just adding PUD + TUD
# (masked by much higher numbers of tweets).
# Maybe we should do something else like scale each from 0-1 then add them together?
avg_ann_ud <- avg_ann_pud %>% 
  left_join(avg_ann_tud, by = "pid") %>%
  mutate(avg_ann_smud = 19*avg_ann_pud + 101*avg_ann_tud,
         tud_prop = avg_ann_tud / sum(avg_ann_tud),
         pud_prop = avg_ann_pud / sum(avg_ann_pud),
         smud_prop = (tud_prop + pud_prop)/2)   # make smud_prop = the proportion of total socmed that is in that gridcell

# write it out
#write_csv(avg_ann_ud, "avg_ann_smud_2005plus_19_101_SMUD.csv")

# and, let's make a shapefile version to write out
aoi <- read_sf("T_AOI_v4_5k_4326_pid.shp")
aoi_smud <- avg_ann_ud %>% left_join(aoi, by = "pid")

# write out
#write_sf(aoi_smud, "aoi_smud.shp")

# and write out a version that only has cells which are greater than 0 smud_prop
aoi_smud_gt_0 <- aoi_smud %>% filter(smud_prop > 0)
#write_sf(aoi_smud_gt_0, "aoi_smud_gt_0.shp")





################################################################
## Old code looking at avg_ann_smud. As of 10/9/19, preference is to use smud_prop
# lets look at our new response, avg_ann_smud
summary(avg_ann_ud)
# great, that brings down the median to 0.07, meaning that fewer than half of our polygons are 0s
# (with 2005+ flickr data). Mean is 39.47

# what proportion of polygons are 0?
sum(avg_ann_ud$avg_ann_smud == 0) / length(avg_ann_ud$avg_ann_smud)
# 0.48 are 0s with 2005+ flickr data
# 0.58 are 0s with 2012+ flickr data

dotchart(avg_ann_ud$avg_ann_smud)
plot(density(avg_ann_ud$avg_ann_smud))

# how about with the log1p
plot(density(log1p(avg_ann_ud$avg_ann_smud)))
# definitely still not normal

## TODO: think about how to model this response. If I multiply the average annual by 14 then I'll be
# back to count data, and a neg bin will feel very appropriate.
# This would make my response "The number of socmed posts in 14 years"... sort of.
# Not quite because twitter only covers 7 years.
# In any case, I could take that response and then divide by 14 to get an estimate of socmed posts
# for the future year. This may be a good thing to do under the hood, since I'll just be applying
# some multiplier anyway to get from posts to people. Really, I could figure out how that "# of posts
# in 14 years" number relates to tourists in 2017 to find the multiplier directly.

# OR, could round average annual UD up, but this loses a lot of variability between 0 and 1. 
# Once I get to model building, let's see if one model is superior to the other
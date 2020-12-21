##
## Future visitation multiplier
##
## 12/21/20
## Meant to be the "final" version, after testing in `mexico_vis_over_time.R` and
##`global_vis_over_time.R`

library(tidyverse)

modplot <- function(x){
  par(mfrow = c(2,2))
  plot(x, ask = F)
  par(mfrow = c(1,1))
}

setwd("~/Documents/MAR/")

nationalviz <- read_csv("Data/NationalVisitationfromGoogleSheet12_21_20.csv")

cruiseOvernight <- nationalviz %>% 
  filter(PopClean == "Cruise" | PopClean == "Overnight") %>%
  group_by(Country, year = Year, Complete) %>%
  summarise(visitors = sum(Visitors))
## NOTE: Guatemala pre 2015 is only Overnight visitors in this tibble

cruiseOvernight

ggplot(cruiseOvernight %>% filter(Country != "Mexico")) +
  geom_point(aes(x = year, y = visitors, col = Country)) +
  facet_wrap(~Country)

# remove BZ 2018, since it's only partial
cruiseOvernight <- cruiseOvernight %>%
  filter(!(Country == "Belize" & Complete == "preliminary")) 

# country level simple models
bz <- cruiseOvernight %>% filter(Country == "Belize")
gt <- cruiseOvernight %>% filter(Country == "Guatemala")
hn <- cruiseOvernight %>% filter(Country == "Honduras")
mx <- cruiseOvernight %>% filter(Country == "Mexico")

bz_mod <- lm(visitors  ~ year, data = bz)
gt_mod <- lm(visitors  ~ year, data = gt)
hn_mod <- lm(visitors  ~ year, data = hn)
mx_mod <- lm(visitors ~ year, data = mx)

summary(bz_mod)
summary(gt_mod) 
summary(hn_mod)
summary(mx_mod)

newdata <- tibble(year = 2009:2050)

newdata$Belize <- predict(bz_mod, newdata)
newdata$Guatemala <- predict(gt_mod, newdata)
newdata$Honduras <- predict(hn_mod, newdata)
newdata$Mexico <- predict(mx_mod, newdata)

## what about if we compare to 2017 preds instead of 2017 observed?
(multipliers_long <- newdata %>%
  #select(-preds) %>%
  gather(key = "country", value = "pred", -year) %>%
  filter(year %in% c(2017, 2050)) %>%
  group_by(country) %>%
  mutate(multiplier = pred/lag(pred)))

multipliers <- multipliers_long %>%
  select(country, multiplier) %>%
  filter(!is.na(multiplier))

# average?
mean(multipliers$multiplier) # 2.67

# let's plot these
ggplot() +
  geom_line(data = newdata, aes(x = year, y = Belize)) +
  geom_point(data = bz, aes(x = year, y = visitors)) +
  labs(title = "Belize")

ggplot() +
  geom_line(data = newdata, aes(x = year, y = Guatemala)) +
  geom_point(data = gt, aes(x = year, y = visitors)) +
  labs(title = "Guatemala")

ggplot() +
  geom_line(data = newdata, aes(x = year, y = Mexico)) +
  geom_point(data = mx, aes(x = year, y = visitors)) +
  labs(title = "Mexico")

ggplot() +
  geom_line(data = newdata, aes(x = year, y = Honduras)) +
  geom_point(data = hn, aes(x = year, y = visitors)) +
  labs(title = "Honduras")

# would it make sense to make a % change from 2017 estimate graph?

preds_2017 <- newdata %>%
  filter(year == 2017)

perc_change <- newdata %>%
  gather(key = "Country", value = "prediction", -year) %>%
  mutate(perc_of_2017 = case_when(Country == "Belize" ~ 100*prediction / preds_2017$Belize,
                                  Country == "Guatemala" ~ 100*prediction / preds_2017$Guatemala,
                                  Country == "Honduras" ~ 100*prediction / preds_2017$Honduras,
                                  Country == "Mexico" ~ 100*prediction / preds_2017$Mexico))

ggplot(perc_change) +
  geom_line(aes(x = year, y = perc_of_2017, col = Country))

# can i add observed points on here?
observed_perc_change <- cruiseOvernight %>%
  mutate(perc_of_2017 = case_when(Country == "Belize" ~ 100*visitors / preds_2017$Belize,
                                  Country == "Guatemala" ~ 100*visitors / preds_2017$Guatemala,
                                  Country == "Honduras" ~ 100*visitors / preds_2017$Honduras,
                                  Country == "Mexico" ~ 100*visitors / preds_2017$Mexico))

ggplot(perc_change) +
  geom_line(aes(x = year, y = perc_of_2017, col = Country)) +
  geom_point(data = observed_perc_change, aes(x = year, y = perc_of_2017, col = Country)) +
  facet_wrap(~ Country)

# ok. may want to consider pretty-ing up some of these for explaining the method.
# But for now, let's go with the average growth of 2.67

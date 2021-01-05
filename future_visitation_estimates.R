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
brewer.pal(4, "Dark2")

ggplot() +
  geom_line(data = newdata, aes(x = year, y = Mexico/1000000), col = "#E7298A") +
  geom_point(data = mx, aes(x = year, y = visitors/1000000), col = "#E7298A") +
  scale_y_continuous(name = "Annual visitors (millions)") +
  labs(title = "Mexico") +
  theme_classic()
ggsave("Deliverables/figs/futureVis/mx_trend.png", width = 3, height = 3, units = "in")


ggplot() +
  geom_line(data = newdata, aes(x = year, y = Honduras/1000000), col = "#7570B3") +
  geom_point(data = hn, aes(x = year, y = visitors/1000000), col = "#7570B3") +
  scale_y_continuous(name = "Annual visitors (millions)") +
  labs(title = "Honduras") +
  theme_classic()
ggsave("Deliverables/figs/futureVis/hn_trend.png", width = 3, height = 3, units = "in")


ggplot() +
  geom_line(data = newdata, aes(x = year, y = Guatemala/1000000), col = "#D95F02") +
  geom_point(data = gt, aes(x = year, y = visitors/1000000), col = "#D95F02") +
  scale_y_continuous(name = "Annual visitors (millions)") +
  labs(title = "Guatemala") +
  theme_classic()
ggsave("Deliverables/figs/futureVis/gt_trend.png", width = 3, height = 3, units = "in")


ggplot() +
  geom_line(data = newdata, aes(x = year, y = Belize/1000000), col = "#1B9E77") +
  geom_point(data = bz, aes(x = year, y = visitors/1000000), col = "#1B9E77") +
  #geom_text(data = (newdata %>% filter(year %in% c(2017, 2050))), 
  #          aes(x = year+3, y = Belize-50000, label = round(Belize, digits = -3)))+
  #geom_point(data = (newdata %>% filter(year %in% c(2017, 2050))), 
  #          aes(x = year, y = Belize))+
  scale_y_continuous(name = "Annual visitors (millions)", breaks = c(1, 1.5, 2, 2.5, 3, 3.5)) +
  labs(title = "Belize") +
  theme_classic()
ggsave("Deliverables/figs/futureVis/bz_trend.png", width = 3, height = 3, units = "in")


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
                                  Country == "Mexico" ~ 100*prediction / preds_2017$Mexico),
         perc_change_2017 = case_when(Country == "Belize" ~ 100*(prediction - preds_2017$Belize)/ preds_2017$Belize,
                                  Country == "Guatemala" ~  100*(prediction - preds_2017$Guatemala)/ preds_2017$Guatemala,
                                  Country == "Honduras" ~   100*(prediction - preds_2017$Honduras)/ preds_2017$Honduras,
                                  Country == "Mexico" ~     100*(prediction - preds_2017$Mexico)/ preds_2017$Mexico))

# create "average" line
perc_change_avg <- perc_change %>%
  pivot_wider(id_cols = year, names_from = Country, values_from = perc_change_2017) %>%
  rowwise() %>%
  mutate(Average = mean(c(Belize, Guatemala, Honduras, Mexico))) %>%
  pivot_longer(-year, names_to = "Country", values_to = "perc_change_2017")


ggplot(perc_change) +
  geom_line(aes(x = year, y = perc_of_2017, col = Country))


ggplot(perc_change_avg) +
  geom_hline(yintercept = 0, col = "gray68") +
  geom_vline(xintercept = 2017, col = "gray68") +
  geom_line(aes(x = year, y = perc_change_2017, col = Country, linetype = Country)) +
  #geom_label(x = 2052, y = 167, label = "167%") +
  scale_color_manual(values = c("black", brewer.pal(4, "Dark2"))) +
  scale_linetype_manual(values = c(1, 2, 2, 2, 2)) +
  scale_x_continuous(breaks = c(2010, 2020, 2030, 2040, 2050), minor_breaks = seq(2009, 2050, by = 1)) +
  ylab("Percent Change in Visitation from 2017 (Estimated)") +
  xlab("Year") +
  theme_classic()

# write it out
ggsave("Deliverables/figs/percent_change_tourism.png", width = 6, height = 5, units = "in")

library(RColorBrewer)
brewer.pal(4, "Dark2")

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

# raw values?
ggplot(perc_change) +
  geom_line(aes(x = year, y = prediction, col = Country)) +
  geom_point(data = observed_perc_change, aes(x = year, y = visitors, col = Country)) +
  facet_wrap(~ Country)

# nope. better each on their own plot

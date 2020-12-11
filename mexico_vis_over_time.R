###
### Visitation over time in Mexico
### Estimating how many visitors there will be in 2050?
###
### 12/10/20
###

library(tidyverse)
library(readxl)

modplot <- function(x){
  par(mfrow = c(2,2))
  plot(x, ask = F)
  par(mfrow = c(1,1))
}

setwd("~/Documents/MAR/")

mex_vis <- read_xlsx("Data/Mexico_Visitation/National_Tourism_database/1_1.xlsx", skip = 2)
mex_vis

# messy, but ok for now.

# pick out the rows I care about
# I focused on Cruise and Overnight passengers to create my current day estimates

cruise_overnight <- mex_vis %>%
  filter(...2 %in% c("Pasajeros en Crucero", "Turistas Internacionales"),
         Unidades %in% c("Miles de Personas", "Pasajeros"),
         str_starts(`1986`, "1")) %>% # to get rid of the 6,000 "Visitantes Internacionales de México al Exterior"
  select(-...1, -...3) %>%
  rename(`2017` = `2017 p/`)

cruise_overnight
# todo: mutate the early years to be numeric, then multiply by 1000 for top row, then add up to 
# get annual totals.

# then do a simple regression

vis_tall <- cruise_overnight %>%
  mutate_at(vars(starts_with(c("1", "2"))), as.numeric) %>%
  gather(key = "year", value = "count", -...2, -Unidades) %>%
  mutate(visitors = if_else(Unidades == "Miles de Personas", count*1000, count),
         year = as.numeric(year))

ggplot(vis_tall) +
  geom_line(aes(x = as.numeric(year), y = visitors, col = Unidades))

# oof, those last couple of years!

# total vis by year
totvis <- vis_tall %>%
  group_by(year) %>%
  summarise(visitors = sum(visitors))

ggplot(totvis) +
  geom_line(aes(x = year, y = visitors))

# simple regression
mexreg <- lm(visitors ~ year, data = totvis)
summary(mexreg); modplot(mexreg)

# predict over time
newdata <- tibble(year = seq(1986, 2050))

newdata$preds <- predict(mexreg, newdata)

newdata

ggplot() +
  geom_point(data = totvis, aes(x = year, y = visitors)) +
  geom_line(data = newdata, aes(x = year, y = preds))  +
  scale_y_continuous(limits = c(0, NA))

# so the last few years (including the official 2017 stat) are not really on that line.

# what would be our multiplier if we just use 2050 pred vs 2017 actual (since this is what
# we used for the baseline)

vis2017 <- totvis$visitors[totvis$year == "2017"]
vis2050 <- newdata$preds[newdata$year == "2050"]

vis2050/vis2017
# 30% growth

# let's throw on some of the other countries trajectorys and see how it might compare
# code below copied from `downscaling.R`
## Let's pull in the national visitation numbers
nationalviz <- read_csv("Data/NationalVisitationfromGoogleSheet8_05_19.csv")

for(country in c("Belize", "Guatemala", "Honduras", "Mexico")){
  fig <- ggplot(nationalviz %>% filter(PopClean == "Cruise" | PopClean == "Overnight",
                                       Country == country)) +
    geom_col(aes(x = Year, y = Visitors, fill = PopClean, col = Complete)) +
    facet_wrap(~Country)
  print(fig)
}
# Note that we don't have cruise and daytrippers separated for Gautemala before 2015

## For now, let's use 2017 as our "baseline" year, since we have data from all countries

cruiseOvernight <- nationalviz %>% 
  filter(PopClean == "Cruise" | PopClean == "Overnight") %>%
  group_by(Country, Year, Complete) %>%
  summarise(`CruiseOvernight` = sum(Visitors))
## NOTE: Guatemala pre 2015 is only Overnight visitors in this tibble

cruiseOvernight

fig1 <- ggplot() +
  geom_point(data = totvis, aes(x = year, y = visitors)) +
  geom_line(data = newdata, aes(x = year, y = preds))  +
  geom_point(data = cruiseOvernight, aes(x = Year, y = CruiseOvernight, col = Country)) +
  scale_y_continuous(limits = c(0, NA))
fig1

fig1 + scale_y_log10()


ggplot() +
  geom_point(data = totvis, aes(x = year, y = visitors)) +
  #geom_line(data = newdata, aes(x = year, y = preds))  +
  geom_point(data = cruiseOvernight, aes(x = Year, y = CruiseOvernight, col = Country)) +
  scale_y_continuous(limits = c(0, NA))


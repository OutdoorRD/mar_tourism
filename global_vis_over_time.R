##
## Global vis trends
## 12/16/20
## Thinking about how best to project future numbers of visitors
## Trying here by calculating a simple linear trend of global international arrivals
##
## Data here: https://docs.google.com/spreadsheets/d/1MglmgHMnbRPsyVOaluKXQfxPhyLTFkkcEAny4p-b5Og/edit#gid=0
## From UNWTO
##

library(tidyverse)

arrivals <- read_sheet("1MglmgHMnbRPsyVOaluKXQfxPhyLTFkkcEAny4p-b5Og")

#arrivals <- read_csv("~/Documents/MAR/Data/Global_Visitation/GlobalVisitation.csv")

arrivals2 <- arrivals %>%
  mutate(tourists = tourists*1000000) %>%
  select(-multiplier)

arrivals2 %>% filter(year > 2008)

# simple linear trend (over the past decade)
mod10 <- lm(tourists ~ year, data = (arrivals2 %>% filter(year > 2009)))
summary(mod10)
#plot(mod)

# predict forward
newdata10 <- tibble(year = 2010:2050)

newdata10$preds <- predict(mod10, newdata10)
newdata10$model <- "10 year trend"

# over the past 2 decades
mod20 <- lm(tourists ~ year, data = (arrivals2 %>% filter(year > 1999)))
summary(mod20)
#plot(mod)

# predict forward
newdata20 <- tibble(year = 2000:2050)

newdata20$preds <- predict(mod20, newdata20)
newdata20$model <- "20 year trend"

#over the past 3 decades
mod30 <- lm(tourists ~ year, data = (arrivals2 %>% filter(year > 1989)))
summary(mod30)
#plot(mod)

# predict forward
newdata30 <- tibble(year = 1990:2050)

newdata30$preds <- predict(mod30, newdata30)
newdata30$model <- "30 year trend"

# over the past 4 decades
mod40 <- lm(tourists ~ year, data = (arrivals2 %>% filter(year > 1979)))
summary(mod40)
#plot(mod)

# predict forward
newdata40 <- tibble(year = 1980:2050)

newdata40$preds <- predict(mod40, newdata40)
newdata40$model <- "40 year trend"

# bind all together
all_lines <- bind_rows(newdata10, newdata20, newdata30, newdata40)


ggplot() +
  geom_point(data = arrivals2, aes(x = year, y = tourists)) +
  geom_line(data = all_lines, aes(x = year, y = preds, col = model)) +
  scale_y_continuous(limits = c(0, NA))

# ok, according to this...
vis2017 <- newdata10[newdata10$year == 2017, "preds"]
vis2050 <- newdata10[newdata10$year == 2050, "preds"]

vis2050/vis2017
# globally, our multiplier would be 2.4 if we use a model parameterized on 2010-2019 data
# Or 1.8 if we used 1980-2019 (and this one looks obviously worse from a linear standpoint)


# can i calculate all at once?
all_lines %>%
  filter(year %in% c(2017, 2050)) %>%
  spread(key = "year", value = "preds") %>%
  mutate(multiplier = `2050`/`2017`)

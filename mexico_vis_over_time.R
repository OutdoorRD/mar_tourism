###
### Visitation over time in Mexico
### Estimating how many visitors there will be in 2050?
###
### 12/10/20
###

library(tidyverse)
library(readxl)

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
  select(-...1, -...3)

cruise_overnight
# todo: mutate the early years to be numeric, then multiply by 1000 for top row, then add up to 
# get annual totals.

# then do a simple regression
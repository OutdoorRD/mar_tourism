## Downscaling National Tourism numbers to in- and out-of-AOI

library(raster)
library(tidyverse)

setwd("~/Documents/MAR/Downscaling/")

# bringing in PUD data to come up with proportions by country
avgann <- read_csv("pud/userdays_avg_annual_bypid.csv")
aoi <- "../GIS/Downscaling/CountriesplusTAOI_pid.shp"

shp <- shapefile(aoi)
data <- merge(shp@data, avgann, by='pid')

# proportions by country
props <- data %>% 
  group_by(CNTRY_NAME) %>%
  mutate(country_pud = sum(avg_ann_ud)) %>%
  select(CNTRY_NAME, id, avg_ann_ud, country_pud) %>%
  filter(id == "1") %>%
  mutate(prop_viz = avg_ann_ud/country_pud)

props
# The flickr data suggests that 53% of visitors to honduras visits this coastal zone,
# 3.5% of visitors to Guatemala,
# 12% of visitors to Mexico,
# 85% of visitors to Belize

## Let's pull in the national visitation numbers
nationalviz <- read_csv("../Data/NationalVisitationfromGoogleSheet8_05_19.csv")

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

# Number of visitors to the coastal zone by country, as divided by PUD
proportioned_viz <- cruiseOvernight %>% filter(Year == 2017) %>%
  left_join(props, by = c("Country" = "CNTRY_NAME")) %>%
  mutate(vizinAOI = CruiseOvernight*prop_viz)

proportioned_viz

# let's write it out
#write_csv(proportioned_viz, "../Data/proportioned_viz_2017.csv")

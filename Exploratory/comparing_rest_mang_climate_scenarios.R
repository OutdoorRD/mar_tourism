###
### Is there a difference between the climate restore mangrove scnearios?
###
library(tidyverse)
library(sf)

setwd("~/Documents/MAR/ROOT/04_rest_mang/IPMs/")

clim0 <- read_sf("MARwide_ipm_04_rest_mang_rec_clim0.geojson")
clim1 <- read_sf("MARwide_ipm_04_rest_mang_rec_clim1.geojson")
clim2 <- read_sf("MARwide_ipm_04_rest_mang_rec_clim2.geojson")

# make nonspatial
clim0_tib <- st_drop_geometry(clim0)
clim1_tib <- st_drop_geometry(clim1)
clim2_tib <- st_drop_geometry(clim2)



clim01 <- inner_join(clim0_tib, clim1_tib, by = c("pid", "CNTRY_NAME"))

test <- clim01 %>%
  filter(perc_change.x != perc_change.y) %>%
  select(starts_with("perc"))
test
# ok, percent change is not always equal. Bu does seem to be functionally equal

plot(clim0_tib$diff_vis ~ clim1_tib$diff_vis)
plot(clim0_tib$perc_change ~ clim1_tib$perc_change)

plot(clim1_tib$diff_vis ~ clim2_tib$diff_vis)
identify(clim1_tib$diff_vis ~ clim2_tib$diff_vis) # identify points

clim1_tib[6555,]

plot(clim01$diff_vis.x, clim01$diff_vis.y)
identify(clim01$diff_vis.x, clim01$diff_vis.y)
clim01[2496,]
clim01[c(2496, 6454), ] #%>%
  select(pid, vis_log.x, est_vis.x, contains("vis"), starts_with("diff_vis"), starts_with("perc"))
    
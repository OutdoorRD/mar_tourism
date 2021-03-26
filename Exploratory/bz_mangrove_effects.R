# calculating the proportion of visitors to bz mar who come because of hte mangroves
# for request from Luis on 3/25/21

library(tidyverse)
library(sf)

bz_ipm <- read_sf("Documents/MAR/ROOT/05_prot_mang/IPMs/bz_ipm_05_prot_mang_rec_clim0.geojson")
bz_ipm
ggplot(bz_ipm) +
  geom_sf(aes(fill = diff_vis))

bz_ipm %>%
  summarise(tot_mang_vis = sum(diff_vis),
            tot_vis = sum(preds_base_clim_vis_future)) %>%
  mutate(prop_mang = tot_mang_vis/tot_vis)
  
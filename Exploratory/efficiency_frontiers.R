library(sf)
library(tidyverse)

prot_mang <- read_csv("~/Documents/MAR/ROOT/Tests/efficiency_frontier_exploration/summary_table.csv")

prot_mang

# efficiency frontiers
ggplot(prot_mang) +
  geom_point(aes(x = cv, y = lob, col = rec)) +
  geom_point(data = prot_mang %>% filter(color != 0), aes(x = cv, y = lob), col = "red")

ggplot(prot_mang) +
  geom_point(aes(x = cv, y = rec, col = lob)) +
  geom_point(data = prot_mang %>% filter(color != 0), aes(x = cv, y = rec), col = "red") #+
 # scale_y_log10() + scale_x_log10()

ggplot(prot_mang) +
  geom_point(aes(x = lob, y = rec, col = cv)) +
  geom_point(data = prot_mang %>% filter(color != 0), aes(x = lob, y = rec), col = "red") #+

prot_mang_sp <- read_sf("~/Documents/MAR/ROOT/Tests/efficiency_frontier_exploration/05_prot_mang_12000ha_all_v1.shp")

prot_mang_val <- prot_mang_sp %>% st_drop_geometry()

ggplot(prot_mang_val) +
  geom_point(aes(x = cv, y = rec, col = lob)) #+
  scale_y_log10() + scale_x_log10()

  
ggplot(prot_mang_val) +
    geom_point(aes(x = cv, y = lob, col = rec)) 


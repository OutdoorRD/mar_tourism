library(sf)
library(tidyverse)


## protect mangrove
prot_mang <- read_csv("~/Documents/MAR/ROOT/Tests/efficiency_frontier_NDC_paper/05_prot_mang_clim0_12000ha/summary_table.csv")

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


#### Underlying data / raw values of sdu plots
## prot mang
prot_mang_sp <- read_sf("~/Documents/MAR/ROOT/Tests/efficiency_frontier_NDC_paper/05_prot_mang_clim0_12000ha/05_prot_mang_12000ha_all.shp")

prot_mang_val <- prot_mang_sp %>% st_drop_geometry()

ggplot(prot_mang_val) +
  geom_point(aes(x = cv, y = rec, col = lob)) #+
scale_y_log10() + scale_x_log10()


ggplot(prot_mang_val) +
  geom_point(aes(x = cv, y = lob, col = rec)) 

ggplot(prot_mang_val) +
  geom_point(aes(x = lob, y = rec))

#########################
## restore mangrove
rest_mang <- read_csv("~/Documents/MAR/ROOT/Tests/efficiency_frontier_NDC_paper/04_rest_mang_clim0_4000ha/summary_table.csv")

# efficiency frontiers
ggplot(rest_mang) +
  geom_point(aes(x = cv, y = lob, col = rec)) +
  geom_point(data = rest_mang %>% filter(color != 0), aes(x = cv, y = lob), col = "red")

ggplot(rest_mang) +
  geom_point(aes(x = cv, y = rec, col = lob)) +
  geom_point(data = rest_mang %>% filter(color != 0), aes(x = cv, y = rec), col = "red") #+
# scale_y_log10() + scale_x_log10()

ggplot(rest_mang) +
  geom_point(aes(x = lob, y = rec, col = cv)) +
  geom_point(data = rest_mang %>% filter(color != 0), aes(x = lob, y = rec), col = "red") #+



### For restore mangrove
rest_mang_sp <- read_sf("~/Documents/MAR/ROOT/Tests/efficiency_frontier_NDC_paper/04_rest_mang_clim0_4000ha/04_rest_mang_4000ha_all.shp")

rest_mang_val <- rest_mang_sp %>% st_drop_geometry()

ggplot(rest_mang_val) +
  geom_point(aes(x = cv, y = rec, col = lob)) +
  ylim(0, 200)
  scale_y_log10() #+ 
  #scale_x_log10()


ggplot(rest_mang_val) +
  geom_point(aes(x = cv, y = lob, col = rec)) 

ggplot(rest_mang_val) +
  geom_point(aes(x = lob, y = rec))



options(stringsAsFactors = F)

library(sf)
library(ggmap)
library(tidyverse)
library(openxlsx)
library(lwgeom)

# 1. Load data

stops <- readRDS('data/stops_data.rds')
routes <- readRDS('data/routes_data.rds')
houses <- readRDS('data/residential_data.rds')

# 2. Set coordinate references

stops_crs <- st_set_crs(stops, 4326) %>% st_transform(crs = 32638)
routes_crs <- st_set_crs(routes, 4326) %>% st_transform(crs = 32638)
houses_crs <- st_as_sf(houses, coords = c("lon", "lat"), crs = 4326) %>% st_transform(crs = 32638)

# 3. Create buffers from transit stops

b <- st_buffer(stops_crs, dist = 500) %>% st_union() # b stands for buffer
ggplot() + geom_sf(data = b, inherit.aes = F, fill = 'red')

# 4. Intersect population data and buffers

i <- st_intersection(houses_crs, b) # computationally expensive operation, be careful
saveRDS(i, 'data/stops_houses_intersect.rds')

glimpse(i)
sum(i$total_ppl)
sum(i$total_ppl) / sum(houses_crs$total_ppl) # 95% of population lives in 500 m radius from transit stops

# 5. Union routes, measure lengths

s <- routes_crs %>% 
  mutate(route_number = str_replace_all(string = route_name, pattern = '[:alpha:]+', replacement = '') %>% as.numeric()) %>% 
  filter(!is.na(route_number), route_number <= 93)

s$len <- st_length(s)

s_union <- s %>% st_union()
st_length(s_union) / sum(s$len)

ggplot() +
  geom_sf(data = s_union, col = 'red', inherit.aes = F)


















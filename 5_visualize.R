
library(tidyverse)
library(sf)
library(ggmap)

basemap <- readRDS('data/basemap.rds')

stops <- readRDS('data/stops_data.rds')
stops <- stops %>% 
  mutate(
    type = ifelse(
      stop_name %in% c(
        'Кутум-ГПЗ',
        'Астрахань 2-Астрахань 1',
        'Астрахань 2-Олейниково',
        'Астрахань 2-Кутум',
        'Кутум-Аксарайская 2'
      ),
      yes = 'train',
      no = 'bus'
    )
  )

astra <- read_sf('data/osm/gis_osm_places_a_free_1.shp') %>% 
  filter(name == 'Астрахань') %>% 
  st_set_crs(value = st_crs(stops))

# 1. Кол-во маршрутов по остановкам

number_of_routes <- stops %>% 
  filter(type == 'bus') %>% 
  group_by(stop_name) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  st_centroid() %>% 
  st_intersection(astra)

ggmap(basemap) +
  geom_sf(data = number_of_routes, aes(size = n), inherit.aes = F, col = 'red', alpha = 0.3, show.legend = 'point')

# 2. Кол-во подвижного состава по маршрутам

df <- readRDS('data/buses_per_route.rds')
routes <- readRDS('data/routes_data.rds')


# 3. Качество сети (с пояснением, какими могут быть интервалы)



# 4. Связи домов с ближайшими остановками (граф)

unique_stops <- stops %>% 
  distinct(geometry)

unique_stops <- stops %>% 
  filter(type == 'bus') %>% 
  group_by(stop_name) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  st_centroid()

houses <- readRDS('data/residential_data.rds') %>% 
  st_as_sf(coords = c('lon', 'lat')) %>% 
  st_set_crs(value = st_crs(stops)) %>% 
  st_intersection(astra)

saveRDS(houses, 'data/houses_clipped.rds')

ggplot() +
  geom_sf(data = houses, inherit.aes = F)

nearest <- st_nearest_feature(x = houses, y = unique_stops)

paths <- st_nearest_points(houses, unique_stops %>% slice(nearest), pairwise = TRUE) %>% 
  st_set_crs(value = st_crs(stops))

ggplot() +
  geom_sf(data = paths, inherit.aes = F, alpha = 0.5)





# 5. Соотношение реестра перевозчиков с реальной маршрутной сетью по 2ГИС




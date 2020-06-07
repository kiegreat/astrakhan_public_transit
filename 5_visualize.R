
library(tidyverse)
library(sf)
library(ggmap)

basemap <- readRDS('data/basemap.rds')

stops <- readRDS('data/stops_data.rds')
stops <- stops %>% 
  st_set_crs(4326) %>% st_transform(crs = 32638) %>% 
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

ggmap(basemap) +
  geom_sf(data = astra, inherit.aes = F)

# 1. Кол-во маршрутов по остановкам

number_of_routes <- stops %>% 
  filter(type == 'bus') %>% 
  group_by(stop_name) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  st_centroid() %>% 
  st_intersection(astra)

ggmap(basemap) +
  geom_sf(data = astra, inherit.aes = F, alpha = 0.3) +
  geom_sf(data = number_of_routes, aes(size = n), inherit.aes = F, col = 'red', alpha = 0.3, show.legend = 'point')

# 2. Кол-во подвижного состава по маршрутам

df <- readRDS('data/buses_per_route.rds')

routes <- readRDS('data/routes_data.rds')
routes <- st_set_crs(routes, 4326) %>% st_transform(crs = 32638)

df2 <- df %>% 
  mutate(
    route_num = str_replace_all(string = route, pattern = '[:alpha:]', replacement = '') %>% 
      as.numeric(),
    direction = str_replace_all(string = route, pattern = '[:digit:]', replacement = ''),
    direction = case_when(
      direction == 'с' ~ 'forward',
      direction == 'р' ~ 'backward'
    )
  )

routes2 <- routes %>% 
  mutate(
    type = ifelse(
      route_name %in% c(
        'Кутум-ГПЗ',
        'Астрахань 2-Астрахань 1',
        'Астрахань 2-Олейниково',
        'Астрахань 2-Кутум',
        'Кутум-Аксарайская 2'
      ),
      yes = 'train',
      no = 'bus'
    ),
    route_num = str_replace_all(string = route_name, pattern = '[:alpha:]', replacement = '') %>% 
      as.numeric()
  )

glimpse(routes2)



# 3. Качество сети (с пояснением, какими могут быть интервалы)



routes3 <- routes2 %>% 
  st_intersection(astra)

routes3$len <- st_length(routes3)

routes_union <- routes3 %>% st_union()
st_length(routes_union) / sum(routes3$len)

ggplot() +
  geom_sf(data = routes_union, col = 'red', inherit.aes = F)

s <- routes2 %>% 
  mutate(route_number = str_replace_all(string = route_name, pattern = '[:alpha:]+', replacement = '') %>% as.numeric()) %>% 
  filter(!is.na(route_number), route_number <= 93)

ggplot() +
  geom_sf(data = s, col = 'red', inherit.aes = F)

s$len <- st_length(s)
s_union <- s %>% st_union()
st_length(s_union) / sum(s$len)

df %>% 
  filter(buses_class == 'МК') %>% 
  summarise(n = sum(buses)) %>% 
  pull(n)

st_length(s_union)

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



# 6. Прямолинейность маршрутов

routes_simplified <- routes2 %>% 
  st_simplify(dTolerance = 0.005) %>% 
  filter(direction == 'forward')

ggmap(basemap) +
  geom_sf(data = routes_simplified, inherit.aes = F)



# 7. Виды подвижного состава / транспорта

r <- routes2 %>% 
  left_join(df2, by = c('route_num', 'direction'))

ggmap(basemap) +
  geom_sf(data = r %>% st_transform(4326), aes(col = type), inherit.aes = F)











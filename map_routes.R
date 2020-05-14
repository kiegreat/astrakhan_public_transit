
options(stringsAsFactors = F)

library(sf)
library(tidyverse)
library(openxlsx)
library(ggmap)

style_string <- '&style=element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road.arterial%7Celement:labels%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels%7Cvisibility:off&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Cvisibility:off&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e'
basemap <- get_googlemap(center = c(lon = 48.033574, lat = 46.347869), zoom = 11, inject = style_string, maptype = 'roadmap')

# - Get geoms ----

get_sf_geoms <- function(file_path, geoms) {
  
  json_data <- jsonlite::fromJSON(file_path)
  directions <- json_data$result$items$directions[[1]]
  
  if(geoms == 'stops') {
    
    result <- data.frame()
    
    for(i in 1:length(directions$type)) {
      
      dir <- directions$type[[i]]
      stops <- directions$platforms[[i]]
      
      df_stops <- data.frame(
        route_name = json_data$result$items$name,
        stop_name = stops$name,
        direction = dir,
        selection = stops$geometry$selection
      )
      result <- rbind(result, df_stops)
    }
    return(result)
    
  } else if (geoms == 'routes') {
    
    result <- data.frame()
    
    for(i in 1:length(directions$type)) {
      
      dir <- directions$type[[i]]
      route <- directions$geometry$selection[i]
      
      df_route <- data.frame(
        route_name = json_data$result$items$name,
        direction = dir,
        selection = route
      )
      result <- rbind(result, df_route)
    }
    return(result)
    
  } else {
    print('Provide correct "geoms" attribute to function: stops or routes')
  }
}
get_sf_geoms_possibly <- possibly(get_sf_geoms, otherwise = NULL)

f <- str_c('data/2gis/', list.files('data/2gis/'))

# Transit stops
df_stops <- map_df(.x = f, .f = ~get_sf_geoms_possibly(file_path = .x, geoms = 'stops'))

geoms <- st_as_sfc(df_stops$selection)
df_stops <- df_stops %>% select(-selection) %>% st_set_geometry(geoms)
st_crs(df_stops) <- "+init=epsg:32638"

# Routest
df_routes <- map_df(.x = f, .f = ~get_sf_geoms_possibly(file_path = .x, geoms = 'routes'))

geoms <- st_as_sfc(df_routes$selection)
df_routes <- df_routes %>% select(-selection) %>% st_set_geometry(geoms)
st_crs(df_routes) <- "+init=epsg:32638"


n_distinct(df_routes$route_name)
rm(geoms)

# - Map data ----

ggmap(basemap) +
  geom_sf(data = df_stops, inherit.aes = F)

ggmap(basemap) +
  geom_sf(data = df_routes, aes(col = route_name), inherit.aes = F) +
  theme(legend.position = 'none')

# 1. Check errors when extacting routes

# err <- df_routes %>% 
#   mutate(
#     route = str_replace_all(string = route_name, pattern = '[:alpha:]*$', replacement = ''),
#     route_n = route %>% as.numeric()
#   ) %>% pull(route_n) %>% unique()
# 
# f <- list.files('data/2gis/') %>% str_replace_all(pattern = '[:alpha:]*.json', replacement = '') %>% as.numeric()
# 
# setdiff(f, err)
# 
# json_data <- jsonlite::fromJSON('data/2gis/41.json')
# directions <- json_data$result$items$directions[[1]]
# 
# dir1 <- directions$type[[1]]
# dir2 <- directions$type[[2]]

# Circle routes only have 1 direction. Because of that function fails

# 2. Download more routes

# Done

# 3. Load population data from reforma-zhkh and 2gis
# 4. Load water layer from OSM, create buffers from transit stops, crop them by water layer

w <- read_sf('data/osm/gis_osm_water_a_free_1.shp') # w stands for water
b <- read_sf('data/osm/gis_osm_places_a_free_1.shp') # b stands for boundaries

b <- b %>% filter(name == 'Астрахань')
w <- st_intersection(b, w)

ggplot() +
  geom_sf(data = w)

buffers <- df_stops %>% 
  st_buffer(dist = 500)

ggmap(basemap) +
  geom_sf(data = w, inherit.aes = F, fill = 'steelblue') +
  geom_sf(data = df_stops %>% slice(1) %>% st_buffer(dist = 500), inherit.aes = F, fill = 'red')
  

# 5. Intersect population data and buffers
# 6. Union routes, measure lengths

























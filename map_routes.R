
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

  dir1 <- directions$type[[1]]
  dir2 <- directions$type[[2]]

  if(geoms == 'stops') {
    
    stops1 <- directions$platforms[[1]]
    stops2 <- directions$platforms[[2]]
    
    df_stops1 <- data.frame(
      route_name = json_data$result$items$name,
      stop_name = stops1$name,
      direction = dir1
    ) %>% 
    st_set_geometry(st_as_sfc(stops1$geometry$selection))
    
    df_stops2 <- data.frame(
      route_name = json_data$result$items$name,
      stop_name = stops2$name,
      direction = dir2
    ) %>% 
    st_set_geometry(st_as_sfc(stops2$geometry$selection))
    
    result <- rbind(df_stops1, df_stops2)
    st_crs(result) <- "+epsg=32638 +units=m +no_defs"
    
    return(result)
    
  } else if (geoms == 'routes') {
    
    route1 <- directions$geometry$selection[1]
    route2 <- directions$geometry$selection[2]
    
    df_route1 <- data.frame(
      route_name = json_data$result$items$name,
      direction = dir1
    ) %>% 
    st_set_geometry(st_as_sfc(route1))
    
    df_route2 <- data.frame(
      route_name = json_data$result$items$name,
      direction = dir1
    ) %>% 
    st_set_geometry(st_as_sfc(route2))
    
    result <- rbind(df_route1, df_route2)
    st_crs(result) <- "+epsg=32638 +units=m +no_defs"
    
    return(result)
    
  } else {
    print('Provide correct "geoms" attribute to function: stops or routes')
  }
  
}
get_sf_geoms_possibly <- possibly(get_sf_geoms, otherwise = NULL)

f <- str_c('data/2gis/', list.files('data/2gis/'))

# Workaround for binding data together
# purrr::map_df fails to keep sf class :-(
df_temp <- get_sf_geoms_possibly(file_path = f[1], geoms = 'stops')
df_stops <- df_temp[0, ]; rm(df_temp); gc()
df_temp <- get_sf_geoms_possibly(file_path = f[1], geoms = 'routes')
df_routes <- df_temp[0, ]; rm(df_temp); gc()

for(i in 1:length(f)) {
  df_temp <- get_sf_geoms_possibly(file_path = f[i], geoms = 'stops')
  df_stops <- rbind(df_stops, df_temp)
}

for(i in 1:length(f)) {
  df_temp <- get_sf_geoms_possibly(file_path = f[i], geoms = 'routes')
  df_routes <- rbind(df_routes, df_temp)
}

# - Map data ----

ggmap(basemap) +
  geom_sf(data = df_stops, inherit.aes = F)

ggmap(basemap) +
  geom_sf(data = df_routes, aes(col = route_name), inherit.aes = F) +
  theme(legend.position = 'none')





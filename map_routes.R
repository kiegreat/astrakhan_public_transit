
library(sf)
library(tidyverse)
library(openxlsx)
library(ggmap)

style_string <- '&style=element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road.arterial%7Celement:labels%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels%7Cvisibility:off&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Cvisibility:off&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e'
basemap <- get_googlemap(center = c(lon = 48.033574, lat = 46.347869), zoom = 11, inject = style_string, maptype = 'roadmap')

# -----

f <- list.files('data/2gis/')



json_data <- jsonlite::fromJSON('data/2gis/1.json')




json_data$result$items$name

directions <- json_data$result$items$directions[[1]]

dir1 <- directions$type[[1]]
dir2 <- directions$type[[2]]

route1 <- directions$geometry$selection[1]
route2 <- directions$geometry$selection[2]

stops1 <- directions$platforms[[1]]
stops2 <- directions$platforms[[2]]

directions$platforms[[1]]



sf_stations <- st_as_sfc(stops1$geometry)

df_stops <- data.frame(
    route_name = json_data$result$items$name,
    stop_name = stops1$name,
    direction = dir1
  ) %>% 
  st_set_geometry(sf_stations)
st_crs(df_stops) <- "+epsg=32638 +units=m +no_defs"



sf_routes <- st_as_sfc(route1)

df_routes <- data.frame(
    route_name = json_data$result$items$name,
    direction = dir1
  ) %>% 
  st_set_geometry(sf_routes)
st_crs(df_stops) <- "+epsg=32638 +units=m +no_defs"


ggmap(basemap) +
  geom_sf(data = df_stops, inherit.aes = F)

st_crs(df_stops)




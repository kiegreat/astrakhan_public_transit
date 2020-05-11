
library(sf)
library(tidyverse)
library(openxlsx)
library(ggmap)

style_string <- '&style=element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road.arterial%7Celement:labels%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels%7Cvisibility:off&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Cvisibility:off&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e'
basemap <- get_googlemap(center = c(lon = 48.033574, lat = 46.347869), zoom = 12, inject = style_string, maptype = 'roadmap')

g <- read.xlsx('geoms.xlsx')

l1 <- g$linestring[1]
l1

l2 <- l1 %>% str_split(pattern = ',')
l3 <- l2[[1]]
l3[2]

r_1c <- data.frame(xy = l3) %>% 
  mutate(
    lon = str_replace_all(xy, pattern = '.*([0-9]{2}.[0-9]{6}).*([0-9]{2}.[0-9]{6}).*', replacement = '\\1'),
    lat = str_replace_all(xy, pattern = '.*([0-9]{2}.[0-9]{6}).*([0-9]{2}.[0-9]{6}).*', replacement = '\\2')
  )

t <- st_linestring(x = matrix(r_1c$lon, r_1c$lat), dim = 'XY')

# -----------------

r1 <- st_as_sfc(g$linestring[1])

ggmap(basemap) +
  geom_sf(data = r1, inherit.aes = FALSE, col = 'red')

plot_route <- function(route_linestring) {
  
  r1 <- st_as_sfc(route_linestring)
  
  ggmap(basemap) +
    geom_sf(data = r1, inherit.aes = FALSE, col = 'red')
  
}

plot_route(g$linestring[2])


options(stringsAsFactors = F)

library(sf)
library(ggmap)
library(tidyverse)
library(openxlsx)
library(XML)

# style_string <- '&style=element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road.arterial%7Celement:labels%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels%7Cvisibility:off&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Cvisibility:off&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e'
# basemap <- get_googlemap(center = c(lon = 48.033574, lat = 46.347869), zoom = 11, inject = style_string, maptype = 'roadmap')
# saveRDS(basemap, 'data/basemap.rds')
basemap <- readRDS('data/basemap.rds')

# Geocoding function ----
#
# geocode_beta <- function(address) {
#   
#   loc <- address %>% 
#     str_replace_all(pattern = ',', replacement = '') %>%
#     str_replace_all(pattern = ' ', replacement = '+')
#   
#   request <- str_c('https://geocode-maps.yandex.ru/1.x/?apikey=', key, '&geocode=', loc) %>% URLencode()
#   
#   xml <- request %>% 
#     readLines(warn = FALSE) %>% 
#     str_c(sep = "\n", collapse = "") %>% 
#     xmlParse(asText = TRUE) %>% 
#     xmlToList()
#   
#   pos <- xml$GeoObjectCollection$featureMember$GeoObject$Point$pos
#   
#   lon <- pos %>% str_replace_all(pattern = ' .*$', replacement = '') %>% as.numeric()
#   lat <- pos %>% str_replace_all(pattern = '^.* ', replacement = '') %>% as.numeric()
#   
#   result <- data.frame('address' = address, 'lon' = lon, 'lat' = lat)
#   print(str_c(address, ' // ', Sys.time()) )
#   
#   return(result)
# }
# geocode <- possibly(.f = geocode_beta, otherwise = NULL)

# 1. Load data from reforma-gkh ----

# mkd <- read_csv2('data/reforma-gkh/export-kr1_1-30-20200501.csv') %>%
#   filter(mun_obr == 'город Астрахань') %>%
#   select(
#     address,
#     total_ppl
#   ) %>%
#   na.omit()
# 
# coords <- map_df(.x = mkd$address, .f = geocode)
# mkd <- mkd %>% left_join(coords, by = 'address')
# saveRDS(mkd, 'data/reforma-gkh/mkd_geocoded.rds')
# 
# mkd <- readRDS('data/reforma-gkh/mkd_geocoded.rds')

# 2. Load data from 2gis ----

# purp <- readRDS('data/bids.rds')
# coords <- readRDS('data/geocoded.rds') %>% mutate(address = str_replace_all(address, pattern = 'Астрахань, ', ''))
# 
# suburbs <- purp %>% 
#   filter(purpose %in% c('Частный дом', 'Коттедж', 'Таунхаус')) %>% 
#   left_join(coords, by = 'address')
# 
# rm(purp, coords); gc()
# 
# ppl_per_house <- (529793 - sum(mkd$total_ppl)) / nrow(suburbs)
# suburbs <- suburbs %>% mutate(total_ppl = ppl_per_house)
# saveRDS(suburbs, 'data/suburbs_geocoded.rds')
# 
# suburbs <- readRDS('data/suburbs_geocoded.rds')

# 3. Union data ----

# df <- mkd %>% rbind(suburbs %>% select(address, total_ppl, lon, lat)) %>% na.omit()
# saveRDS(df, 'data/residential_data.rds')

df <- readRDS('data/residential_data.rds')

# Map data
ggmap(basemap) +
  geom_point(
    data = df, 
    aes(x = lon, y = lat),
    alpha = 0.1
  )





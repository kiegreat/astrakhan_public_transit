
options(stringsAsFactors = F)

library(sf)
library(tidyverse)
library(openxlsx)

mkd <- read_csv2('data/reforma-gkh/export-kr1_1-30-20200501.csv') %>% 
  filter(mun_obr == 'город Астрахань') %>% 
  select(
    address,
    total_ppl
  ) %>% 
  na.omit()

purp <- readRDS('data/bids.rds')
coords <- readRDS('data/geocoded.rds') %>% mutate(address = str_replace_all(address, pattern = 'Астрахань, ', ''))
suburbs <- purp %>% 
  filter(purpose %in% c('Частный дом', 'Коттедж', 'Таунхаус')) %>% 
  left_join(coords, by = 'address')

glimpse(suburbs)
summary(suburbs)
table(suburbs$purpose)

rm(purp, coords); gc()
mkd$total_ppl %>% sum()

ppl_per_house <- (529793 - sum(mkd$total_ppl)) / nrow(suburbs)
suburbs <- suburbs %>% mutate(total_ppl = ppl_per_house)

geocode_beta <- function(address) {
  
  loc <- address %>% 
    str_replace_all(pattern = ',', replacement = '') %>%
    str_replace_all(pattern = ' ', replacement = '+')
  
  request <- str_c('https://geocode-maps.yandex.ru/1.x/?apikey=', key, '&geocode=', loc) %>% URLencode()
  
  xml <- request %>% 
    readLines(warn = FALSE) %>% 
    str_c(sep = "\n", collapse = "") %>% 
    xmlParse(asText = TRUE) %>% 
    xmlToList()
  
  pos <- xml$GeoObjectCollection$featureMember$GeoObject$Point$pos
  
  lon <- pos %>% str_replace_all(pattern = ' .*$', replacement = '') %>% as.numeric()
  lat <- pos %>% str_replace_all(pattern = '^.* ', replacement = '') %>% as.numeric()
  
  result <- data.frame('address' = address, 'lon' = lon, 'lat' = lat)
  print(str_c(address, ' // ', Sys.time()) )
  
  return(result)
}
geocode <- possibly(.f = geocode_beta, otherwise = NULL)

# mkd_geocoded <- map_df(.x = mkd$address, .f = geocode)
# saveRDS(mkd_geocoded, 'data/reforma-gkh/mkd_geocoded')

coords <- readRDS('data/reforma-gkh/mkd_geocoded')
mkd <- mkd %>% left_join(coords, by = 'address')

df_ppl <- mkd %>% rbind(suburbs %>% select(address, total_ppl, lon, lat))

ggmap(basemap) +
  stat_density_2d(
    data = df_ppl, 
    aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), 
    geom = 'polygon', 
    h = NULL, 
    adjust = c(0.2, 0.2)
  )













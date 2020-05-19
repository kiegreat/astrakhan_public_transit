
options(stringsAsFactors = F)

library(sf)
library(tidyverse)

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
saveRDS(df_stops, 'data/stops_data.rds')

# Routest
df_routes <- map_df(.x = f, .f = ~get_sf_geoms_possibly(file_path = .x, geoms = 'routes'))
geoms <- st_as_sfc(df_routes$selection)
df_routes <- df_routes %>% select(-selection) %>% st_set_geometry(geoms)
saveRDS(df_stops, 'data/routes_data.rds')

rm(geoms)
n_distinct(df_routes$route_name)






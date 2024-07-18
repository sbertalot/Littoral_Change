library(dplyr)
library(terra)

aqua_sat <- read.csv("Data/sr_wq_rs_join.csv")
wy <- vect("Data/Shapefiles/Wyoming/GU_StateOrTerritory.shp")
co <- vect("Data/Shapefiles/Colorado/Colorado_State_Boundary.shp")
id <- vect("Data/Shapefiles/Idaho/ID.Boundary.shp")
mt <- vect("Data/Shapefiles/Montana/State_of_Montana__Boundary.shp")
ut

lake_finder <- function(state, df) {
  coords <- vect(df, geom = c("long", "lat"), crs = "EPSG:4326")
  state <- project(state, crs(coords))
  
  bbox <- ext(state)
  long_min <- bbox$xmin
  long_max <- bbox$xmax
  lat_min <- bbox$ymin
  lat_max <- bbox$ymax
  
  points_in_region <- df %>%
    filter(type == "Lake") %>% 
    filter(long >= long_min & long <= long_max & 
             lat >= lat_min & lat <= lat_max) %>% 
    filter(!is.na(secchi))
  
  return(points_in_region)
  
}

wy_data <- lake_finder(wy, aqua_sat)
co_data <- lake_finder(co, aqua_sat)
id_data <- lake_finder(id, aqua_sat)
mt_data <- lake_finder(mt, aqua_sat)

whole_df <- rbind(wy_data, co_data, id_data, mt_data)

write.csv(whole_df, file = "prelim_aquasat.csv" )






library(dplyr)
library(terra)
library(USA.state.boundaries)


#Import AquaSat data frame 
aqua_sat <- read.csv("Data/sr_wq_rs_join.csv")
#Initialize list of states to be included in the query 
state_list <- c("WY", "MT", "CO", "ID", "UT", "WA", "OR", "CA", "NV")

#Function that takes in list of desired states (state) and aqua_sat data frame (df)
#Returns a data frame of all lake match-ups within the given states in the data frame
lake_finder <- function(state, df) {
  
  #Import outline of all states  
  all_states <- vect(state_boundaries_wgs84)
  
  #Subset to just the states in the list
  mountain_states <- all_states[all_states$STATE_ABBR %in% state, ]
  
  #Subsets the aqua_sat dataframe for just lat and long
  coords <- df %>%
    select(long, lat)
  
  #extracts all lat long points from coords that fall within the provided states 
  points_within_index <- terra::extract(mountain_states, coords)
  
  #removes na from the data frame (all points that were not within the perimeter)
  points_within_index <- na.omit(points_within_index)
  
  #extract the indices of the remaining states
  indices <- points_within_index$id.y
  
  #subsets the aqua_sat dataframe based on the set of indices, only lakes, and where there is a secchi depth value
  points_in_region <- df %>%
    slice(indices) %>%
    filter(type == "Lake") %>% 
    filter(!is.na(secchi))
  
  return(points_in_region)
  
}

lake_matchups <- lake_finder(state_list, aqua_sat)

write.csv(lake_matchups, file = "prelim_aquasat.csv" )






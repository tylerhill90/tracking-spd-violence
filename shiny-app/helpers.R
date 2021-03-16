## helpers.R ##
# Helper functions for the shiny app

library(tidyverse)
library(leaflet)
library(sf)
library(htmltools)
library(tigris)
library(highcharter)

# Load the data
source("data.R")

#################
## TERRY STOPS ##
#################
get_terry_years_df <- function(start_y, stop_y) {
  # Subset data by year
  # Report total stops that year
  # Update shapefile accordingly
  terry_year_df <- terry_df %>% 
    filter(Year %in% start_y:stop_y)
  
  # Total number of Terry Stops that year
  n_year_terries <- nrow(terry_year_df)
  assign("n_year_terries", n_year_terries, envir = .GlobalEnv)
  
  # Filter out unknown beat stops
  terry_year_df <- terry_year_df %>% 
    drop_na(Beat)
  assign("terry_year_df", terry_year_df, envir = .GlobalEnv)
  
  # Df of total terry stops that year per beat
  beat_stops <- terry_year_df %>% 
    group_by(Beat) %>% 
    summarise(total_stops = n())
  
  # Reset to a clean shapefile
  beats <- raw_beats()
  
  # Add missing beats data to beat_stops
  dif_beats <- setdiff(beats$beat, beat_stops$Beat)
  for (b in dif_beats) {
    beat_stops <- beat_stops %>% 
      add_row(Beat = b, total_stops = 0)
    }
  
  # Combine terry stop data into the shapefile
  beats <<- geo_join(beats, beat_stops, "beat", "Beat")
}

##################
## USE OF FORCE ##
##################


#########
## OIS ##
#########
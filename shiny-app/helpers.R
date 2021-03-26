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
get_terry_years_df <- function(year) {
  # Subset data by year
  # Report total stops that year
  # Update shapefile accordingly
  terry_year_df <- terry_df %>% 
    filter(Year == year) %>% 
    drop_na(Beat)
  assign("terry_year_df", terry_year_df, envir = .GlobalEnv)
  
  ## Add total terry stops per beat to map ##
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
  beats <- geo_join(beats, beat_stops, "beat", "Beat")
  
  ## Add subject race for map beats ##
  sub_beat_race <- terry_year_df %>% 
    group_by(Beat, Subject.Perceived.Race) %>% 
    summarise(n = n()) %>% 
    pivot_wider(names_from = Subject.Perceived.Race, values_from = n, values_fill = 0)
  
  # Handle missing data due to differential reporting over the years
  # Convert to proportions
  if (year == 2020) {
    sub_beat_race$`Multi-Racial` <- "NA"
    sub_beat_race$Hispanic <- "NA"
  } else {
    sub_beat_race$`Nat Hawaiian/Oth Pac Islander` <- "NA"
  } 
  
  # Combine terry stop data into the shapefile
  beats <- geo_join(beats, sub_beat_race, "beat", "Beat")
  assign("beats", beats, envir = .GlobalEnv)
}

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
  beats <- geo_join(beats, beat_stops, "beat", "Beat")
  assign("beats", beats, envir = .GlobalEnv)
}


get_terry_map <- function() {
  # Generate a chloropleth map according to what year is selected
  # Set chloropleth color palette
  bins <- c(0, 20, 50, 90, 140, 200, 270, Inf)
  pal <- colorBin("YlGnBu", domain = beats$total_stops, bins = bins)
  
  beat_info <- paste(
    "Beat: ", beats$beat,"<br/>",
    "Terry Stops: ", beats$total_stops, "<br/>",
    sep="") %>%
    lapply(htmltools::HTML)
  
  leaflet(options = leafletOptions(minZoom = 10)) %>% 
    addTiles() %>% 
    addPolygons(data = beats,
                stroke = 1,
                weight = 1,
                color = "grey",
                fillColor = ~pal(total_stops),
                fillOpacity = 0.5,
                label = beat_info,
                highlight = highlightOptions(
                  weight = 3,
                  color = "#666",
                  fillOpacity = 0.7,
                  bringToFront = TRUE)) %>%
    addLegend(pal = pal,
              values = beats$total_stops,
              opacity = 0.6,
              title = NULL,
              position = "bottomright") %>% 
    setView((-122.4597 + -122.2244)/2, (47.48173+47.74913)/2, 10) %>% 
    setMaxBounds(-122.4597, 47.74913,  -122.2244, 47.48173)
}


##################
## USE OF FORCE ##
##################


#########
## OIS ##
#########
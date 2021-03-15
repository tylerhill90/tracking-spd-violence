## app.R ##

# Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(sf)
library(htmltools)
library(tigris)
library(highcharter)
library(RColorBrewer)

# Load data and helper functions
source("helpers.R")


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
      fluidRow(
        box(
          h2("Terry Stops"),
          p("Chloropleth of where SPD conducts Terry Stops for the given year."),
          p("Map sectioned by SPD beats. Select a beat to see it's summary statistics."),
          sliderInput("year", h4("Select Year"),
                      min = 2018, max = 2020, value = 2018,
                      sep = ""),
          leafletOutput("map")
        ),
        
        box(
          h2("Summary Statistics"),
          highchartOutput("subject_race", height = "300px"),
          highchartOutput("officer_race", height = "300px")
        )
        
      )
      
    )
  
)

server <- function(input, output) {
  # Update Terry data when slider changes
  terry_map_data <- eventReactive( input$year, {
    get_terry_years_df(as.integer(input$year), as.integer(input$year))
  })
  
  # Generate a chloropleth map according to what year is selected
  # Set chloropleth color palette
  output$map <- renderLeaflet({
    terry_map_data()
    bins <- c(0, 25, 50, 100, 150, 200, 300, Inf)
    pal <- colorBin("Oranges", domain = beats$total_stops, bins = bins)
    
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
                  fillOpacity = 0.6,
                  label = beat_info,
                  layerId = ~beats$beat,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE)) %>%
      addLegend(pal = pal,
                values = beats$total_stops,
                opacity = 0.7,
                title = NULL,
                position = "bottomright") %>% 
      setView((-122.4597 + -122.2244)/2, (47.48173+47.74913)/2, 10) %>% 
      setMaxBounds(-122.5597, 47.84913,  -122.1244, 47.38173)
  })
  
  # Zoom in on selected beat
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    leafletProxy("map") %>% 
      setView(lng = click$lng, lat = click$lat, zoom = 12)
  })
  
  # Plot subject race pie chart
  output$subject_race <- renderHighchart({
    terry_map_data()
    chart_data <- terry_year_df %>% 
      group_by(Subject.Perceived.Race) %>% 
      summarise(count = n())
    
    # Add unrepresented races back in for coloring purposes
    dif_races <- setdiff(unique(terry_df$Officer.Race), chart_data$Subject.Perceived.Race)
    for (b in dif_races) {
      chart_data <- chart_data %>%
        add_row(Subject.Perceived.Race = b, count = 0)
    }
    
    chart_data %>% 
      arrange(Subject.Perceived.Race) %>% 
      hchart(
        "pie", hcaes(x = Subject.Perceived.Race, y = count),
        name = "Frequency"
      ) %>% 
      hc_title(text = "Subject Race") %>% 
      hc_colors(race_color_set)
  })
  
  # Plot officer race pie chart
  output$officer_race <- renderHighchart(({
    terry_map_data()
    chart_data <- terry_year_df %>% 
      group_by(Officer.Race) %>% 
      summarise(count = n())
    
    # Add unrepresented races back in for coloring purposes
    dif_races <- setdiff(unique(terry_df$Subject.Perceived.Race), chart_data$Officer.Race)
    for (b in dif_races) {
      chart_data <- chart_data %>%
        add_row(Officer.Race = b, count = 0)
    }
    
    chart_data %>% 
      arrange(Officer.Race) %>% 
      hchart(
        "pie", hcaes(x = Officer.Race, y = count),
        name = "Frequency"
      ) %>% 
      hc_title(text = "Officer Race") %>% 
      hc_colors(race_color_set)
  }))
  
  # Plot time of day polar bar chart
  output$time <- renderHighchart({
    terry_map_data()
    terry_year_df %>% 
      group_by(Time) %>% 
      summarise(count = n()) %>%
      hchart(
        "column", hcaes(
          x = Time,
          y = count
        ),
        name = "Frequency"
      ) %>% 
      hc_chart(polar = TRUE) %>% 
      hc_title(text = "Terry Stop Time of Day (24 hr)") %>% 
      hc_xAxis(
        categories = as.list(paste(0:23, ":00", sep = ""))
      )
  })
}

shinyApp(ui, server)
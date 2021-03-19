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

ui <- fluidPage(
      fluidRow(
        box(
          h2("Terry Stops"),
          p("Chloropleth of where Seattle Police Department conducts Terry Stops for the given year."),
          p("Map sectioned by SPD beats. Hover over a beat to see it's summary statistics. Click a beat to zoom in."),
          sliderInput("year", h4("Select Year"),
                      min = 2018, max = 2020, value = 2018,
                      sep = "",
                      width = "100%"),
          leafletOutput("map")
        ),
        
        box(
          h2("Summary Statistics for the Year"),
          selectInput("plot_type", "Stat Types:", 
                      choices=c("Race", "Age", "Other")
          ),
          conditionalPanel(
            condition = "input.plot_type == 'Race'",
            highchartOutput("race_compare"),
            p("While the total proportion of SPD officers conducting Terry Stops seems to somewhat accurately reflect Seattle demographics as a whole, there is a clear disparity in the subjects who get stopped in Terry Stops not reflecting the overall city's demographics. This is most notabe in the significant disparity between black subjects of Terry Stops and the overall black proportion of residents in Seattle."),
            helpText("NOTE: The Seattle demographic data is from a 2014-2018 American Community Survey (ACS) 5-Year Estimates (U.S. Census Bureau)")          
            )

        )
        
      )
      
    )

server <- function(input, output) {
  # Update Terry data when slider changes
  terry_map_data <- eventReactive( input$year, {
    get_terry_years_df(as.integer(input$year), as.integer(input$year))
  })
  
  #####################
  ## Chloropleth map ##
  #####################
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
  
  ##############################
  ## Zoom in on selected beat ##
  ##############################
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    leafletProxy("map") %>% 
      setView(lng = click$lng, lat = click$lat, zoom = 12)
  })
  
  #####################################################
  ## Plot line graph showing disparity between races ##
  #####################################################
  output$race_compare <- renderHighchart(({
    terry_map_data()
    
    # Get officer data
    officer_data <- terry_year_df %>% 
      group_by(Officer.Race) %>% 
      summarise(n = n()) %>% 
      mutate(Officer = round((n / sum(n))*100, 1)) %>% 
      select(-n) %>% 
      rename(race = Officer.Race)
    
    # Add unrepresented races back in for coloring purposes
    dif_races <- setdiff(unique(terry_df$Subject.Perceived.Race), officer_data$race)
    for (b in dif_races) {
      officer_data <- officer_data %>%
        add_row(race = b, Officer = 0)
    }
    
    # Get subject data
    subject_data <- terry_year_df %>% 
      group_by(Subject.Perceived.Race) %>% 
      summarise(n = n()) %>% 
      mutate(Subject = round((n / sum(n))*100, 1)) %>% 
      select(-n) %>% 
      rename(race = Subject.Perceived.Race)
    
    # Add unrepresented races back in for coloring purposes
    dif_races <- setdiff(unique(terry_df$Officer.Race), subject_data$race)
    for (b in dif_races) {
      subject_data <- subject_data %>%
        add_row(race = b, Subject = 0)
    }
    
    # Seattle Demo Data
    # https://www.seattle.gov/opcd/population-and-demographics/about-seattle#raceethnicity
    s_demo_data <- data.frame(
      "race" = c("American Indian/Alaska Native",
                 "Asian",
                 "Black",
                 "Nat Hawaiian/Oth Pac Islander",
                 "Unknown",
                 "White",
                 "Multi-Racial",
                 "Hispanic"),
      "Seattle" = c(0.5, 14.9, 6.8, 0.3, 0.3, 64.5, 6.0, 6.6)
    )
    
    # Join the data, pivot longer, and chart it
    chart_data <- inner_join(officer_data, s_demo_data, by = "race") 
    chart_data <- inner_join(chart_data, subject_data, by = "race")%>%
      pivot_longer(!race, names_to = "Type", values_to = "Percent")
    chart_data %>% 
      hchart(
        "line",
        hcaes(x = Type, y = Percent, group = race)
      ) %>% 
      hc_title(text = "Comparing Officer and Subject Race Differentials")
  
  }))
  
  ######################################
  ## Plot time of day polar bar chart ##
  ######################################
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
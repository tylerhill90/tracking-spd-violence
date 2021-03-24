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
library(shinyWidgets)
library(shinyjs)

# Load data and helper functions
source("helpers.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "Examining Seatle Police Department Terry Stop Data",
    titleWidth = 600
  ),
  dashboardSidebar(),
  dashboardBody(
    useShinyjs(),
    fluidPage(
      setBackgroundColor(
        color = c("#1A5276 ", "#34495E"),
        gradient = c("linear", "radial"),
        direction = c("bottom", "top", "right", "left"),
        shinydashboard = TRUE
      ),
      fluidRow(
        box(
          h2(textOutput("map_title")),
          tags$div("Below is a choropleth of where Seattle Police Department (SPD) conducts Terry Stops for the given year.",
                   tags$br(),
                   "The map is sectioned by SPD beats. Hover over a beat to see it's name and total number of stops.",
                   tags$br(),
                   "Click a beat to zoom in."),
          br(),
          leafletOutput("map"),
          selectInput("year", h4("Select a year"),
                      choices = list("2018" = 2018, "2019" = 2019, "2020" = 2020),
                      selected = 2018)
        ),
        
        box(
          h2(textOutput("stats_title")),
          selectInput("plot_type", "Stat Types:", 
                      choices=c("Race", "Age", "Gender", "Time of Day")
          ),
          ## Race ##
          conditionalPanel(
            condition = "input.plot_type == 'Race'",
            highchartOutput("race_compare"),
            tags$div(
              "This plot shows how the proportion of officer and subject race in Terry Stops compares to ",
              tags$a(href = "https://www.seattle.gov/opcd/population-and-demographics/about-seattle#raceethnicity",
                     "Seattle's racial demographics"),
              " as a whole. 
              The slope between points shows how over or under represented a race is. 
              White police officers seem to be over represented where as whitie subjects of Terry Stops are under represented. 
              While black officers invlovled in Terry Stops seem to be accurately reflected (flat slope), black subjects of Terry Stops are clearly over represented when compared to Seattle demographics as a whole.
              ")
            ),
          
          ## Age ##
          conditionalPanel(
            condition = "input.plot_type == 'Age'",
            highchartOutput("age"),
            p("LOrum ipsum unum
              ")
          ),
          
          ## Gender ##
          conditionalPanel(
            condition = "input.plot_type == 'Gender'",
            highchartOutput("gender"),
            p("LOrum ipsum unum
              ")
          ),
          
          ## Other ##
          conditionalPanel(
            condition = "input.plot_type == 'Time of Day'",
            highchartOutput("time"),
            p("LOrum ipsum unum
              ")
          )
          
        )
      )
    )
  )
)

server <- function(input, output) {
  addClass(selector = "body", class = "sidebar-collapse")
  
  #####################################
  ## Update the data with user input ##
  #####################################
  terry_map_data <- eventReactive( input$year, {
    get_terry_years_df(as.integer(input$year))
  })

  ###############
  ## Map Title ##
  ###############
  output$map_title <- renderText({
    terry_map_data()
    n_stops <- formatC(nrow(terry_year_df), format = "d", big.mark = ",")
    paste("Terry Stops in ", input$year, ": ", n_stops, sep = "")
  })
  
  #################
  ## Stats Title ##
  #################
  output$stats_title <- renderText({
    terry_map_data()
    n_stops <- formatC(nrow(terry_year_df), format = "d", big.mark = ",")
    paste("Summary Statistics for", input$year)
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
    
    leaflet(options = leafletOptions(
      minZoom = 10,
      zoomControl = FALSE
      )) %>% 
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
      setMaxBounds(-122.5597, 47.84913,  -122.1244, 47.38173) %>%
      addProviderTiles(providers$Stamen.TonerLite)
  })
  
  ##############################
  ## Zoom in on selected beat ##
  ##############################
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    leafletProxy("map") %>% 
      setView(lng = click$lng, lat = click$lat, zoom = 12)
  })
  
  ################################################
  ## Line graph showing disparity between races ##
  ################################################
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
    
    # Join the data, pivot longer, and chart it
    chart_data <- inner_join(officer_data, s_demo_data, by = "race") 
    chart_data <- inner_join(chart_data, subject_data, by = "race") %>%
      pivot_longer(!race, names_to = "Type", values_to = "Percent")
    chart_data %>% 
      hchart(
        "line",
        hcaes(x = Type, y = Percent, group = race)
      ) %>% 
      hc_title(text = "Comparing officer and subject race in Terry Stops to Seattle demographics")
  
  }))
  
  ####################
  ## Age  bar chart ##
  ####################
  output$age <- renderHighchart(({
    terry_map_data()
    
    # Get subject data
    subject_data <- terry_year_df %>% 
      group_by(Subject.Age.Group) %>% 
      summarise(Subject = n()) %>% 
      rename(
        age = "Subject.Age.Group"
      )
    
    # Get officer data
    officer_data <- terry_year_df %>% 
      group_by(Officer.Age) %>%
      mutate(Officer.Age = cut(Officer.Age, breaks = c(1, 17, 25, 35, 45, 55, Inf))) %>% 
      summarise(Officer = n()) %>% 
      rename(
        age = "Officer.Age"
      ) %>% 
      mutate(
        age = gsub("\\(17.+", "18 - 25", age),
        age = gsub("\\(25.+", "26 - 35", age),
        age = gsub("\\(35.+", "36 - 45", age),
        age = gsub("\\(45.+", "46 - 55", age),
        age = gsub("\\(55.+", "56 +", age)
      )
    
    # Join the data and chart
    chart_data <- inner_join(subject_data, officer_data, by = "age") %>% 
      pivot_longer(!age, names_to = "Type", values_to = "Frequency")
    chart_data %>% 
      hchart(
        "column", hcaes(
          x = age,
          y = Frequency,
          group = Type
        )
      ) %>% 
      hc_yAxis(title = list(text = "Frequency")) %>% 
      hc_title(text = "Age Distribution")
  }))
  
  ######################
  ## Gender pie chart ##
  ######################
  output$gender <- renderHighchart(({
    terry_map_data()
    
  }))
  
  #################################
  ## Time of day polar bar chart ##
  #################################
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
        title = "",
        categories = as.list(paste(0:23, ":00", sep = ""))
      ) %>% 
      hc_yAxis(title = "")
  })
}

shinyApp(ui, server)
## app.R ##
# Examining SPD Terry Stop data

# Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(sf)
library(htmltools)
library(tigris)
library(highcharter)
library(shinyjs)
library(xts)

# Load data and helper functions
source("helpers.R")

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Examining Seatle Police Department Terry Stop Data",
    titleWidth = 600
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Background", tabName = "explain", icon = icon("question")),
      menuItem("Explore the data", tabName = "data", icon = icon("search"))
    )
  ),
  
  dashboardBody(
    tags$style(
      HTML("
      img {
        display: block;
        margin-left: auto;
        margin-right: auto;
      }
    ")
    ),
    
    tabItems(
      ################
      ## Background ##
      ################
      tabItem(tabName = "explain",
        box(
          title = "What is a Terry Stop?", status = "primary", solidHeader = TRUE, width = 6,
          tags$div(
            'In the landmark case Terry v. Ohio, the state of Ohio characterized a Terry Stop as
            "the right of a police officer... to make an on-the-street stop, interrogate
            and pat down for weapons (known in street vernacular as \'stop and frisk\')."[1]
            These detentions must occur with ',
            tags$a(href = "https://en.wikipedia.org/wiki/Reasonable_suspicion",
                   "reasonable suspicion"),
            '. This legal standard of proof falls below the standard of ',
            tags$a(href = "https://en.wikipedia.org/wiki/Probable_cause",
                   "probable cause"),
            ', which is needed to make an arrest, and roughly means that an officer can stop
            and frisk a subject whom they believeto pose a threat based on "specific and articulable facts...
            taken together with rational inferences from those facts." [1] You
            may be wondering if this is in direct conflict with our ',
            tags$a(href = "https://en.wikipedia.org/wiki/Fourth_Amendment_to_the_United_States_Constitution",
                   "Fourth Amendment"),
            'rights. The Supreme Court has ruled in on this grey area, debating
            on issues such as traffic stops in Ohio v. Robinette [2] and pretextual stops in
            Whren v. United States. [3] For more information you can read the ',
            tags$a(href = "https://en.wikipedia.org/wiki/Terry_stop",
                   "Terry Stop"),
            'Wikipedia article.', 
            tags$br(),
            tags$br(),
            tags$img(src = "t-stop.jpg", width = "50%", ),
            tags$br(),
            tags$br()
          ),
          helpText("References:"),
          helpText("[1] ", tags$a(href = "https://scholar.google.com/scholar_case?case=17773604035873288886",
                          "United States Supreme Court - Terry v. Ohio - Court Documents")),
          helpText("[2] ", tags$a(href = "https://tile.loc.gov/storage-services/service/ll/usrep/usrep519/usrep519033/usrep519033.pdf",
                          "United States Supreme Court - Ohio v. Robinette - Court Documents")),
          helpText("[3] ", tags$a(href = "https://scholar.google.com/scholar_case?case=3416424011044753637",
                          "United States Supreme Court - Whren v. United States - Court Documents"))
        ),
        box(
          title = "Where does the data come from?", status = "primary", solidHeader = TRUE, width = 6,
          tags$div(
            'The data for this project comes from ',
            tags$a(href = "https://data.seattle.gov/",
                   "The City of Seattle's Open Data Portal"),
            ', specifically the ',
            tags$a(href = "https://data.seattle.gov/Public-Safety/Terry-Stops/28ny-9ts8",
                   "Terry Stops"),
            'data from the ',
            tags$a(href = "http://www.seattle.gov/police",
            "Seattle Polic Department"),
            '. The map of SPD beats come from ',
            tags$a(href = "https://data.seattle.gov/dataset/Seattle-Police-Beats-2018-Present/ex83-w2uk",
                   "this"),
            'dataset, from the City of Seattle GIS Program.',
            tags$br(),
            tags$br(),
            tags$img(src = "SPD.png", width = "50%")
          )
        )
      ),
      
      ##############
      ## Data tab ##
      ##############
      tabItem(tabName = "data",
              fluidPage(
                fluidRow(
                  box(
                    title = textOutput("map_title"), status = "primary", solidHeader = TRUE,
                    selectInput("year", label = "Change the year",
                                choices = list("2018" = 2018, "2019" = 2019, "2020" = 2020),
                                selected = 2018),
                    leafletOutput("map", height = 420),
                    tags$br(),
                    tags$div('Above is a choropleth of where Seattle Police Department (SPD) conduct Terry Stops for the given year. 
                             The map is sectioned by the "beats" that SPD officers are assigned to work. Hover over a beat to see
                             it\'s nameand total number of stops for the selected year. Click a beat to center and zoom in.')
                  ),
                  
                  box(
                    title = textOutput("stats_title"), status = "primary", solidHeader = TRUE,
                    selectInput("plot_type", "Stat Types", 
                                choices=c("Frequency", "Resolution", "Call Type", "Race", "Age", "Gender", "Time of Day")
                    ),
                    
                    ## Time Series ##
                    conditionalPanel(
                      condition = "input.plot_type == 'Frequency'",
                      highchartOutput("time_series"),
                      tags$div("Examining the frequency of Terry Stops throughout the year shows a moderate trend towards more stops
                                being performed in the spring time when we look at the weekly sum. When we look at day to day counts
                                we see a more stochastic nature to the data. That said the days with higher counts stil tend to be in
                                warmer months whereas the days with lower counts tend to be in the colder months. An interesting
                                finding comes from the 2020 plot, which shows a clear decrease in Terry Stops starting in June right
                                after George Floyd's death on May 25th.")
                    ),
                    
                    ## Resolution ##
                    conditionalPanel(
                      condition = "input.plot_type == 'Resolution'",
                      highchartOutput("resolution"),
                      tags$div("From 2018 to 2020 the category Offense Report steadily declines whereas the category 
                                Field Contact steadily increases. The Arrest category remains about the same over this time span.
                               This may indicate a that there was a change in policy for SPD as to how Terry Stops are to be
                               approached by the officer, opting for more leniency.")
                    ),
                    
                    ## Call Type ##
                    conditionalPanel(
                      condition = "input.plot_type == 'Call Type'",
                      highchartOutput("call_type"),
                      tags$div("Here we see how the Terry Stop was originally dispatched or initiated by the officer.
                               While there is a lot of categories here we can still easily see that the majority of stops
                               are Unknown, which may indicate an officer not wanting to disclose why they stopped a subject.
                               Other leading categories include Suspicous Person, Prowler - Trespass, and Disturbance - Other
                               all of which could be interpretted as vague. It seems like a substantial number of these categories
                               warrant police intervention but there are also a large proportion of possibly dubious reasons
                               to initiate a Terry Stop present.")
                    ),
                    
                    ## Race ##
                    conditionalPanel(
                      condition = "input.plot_type == 'Race'",
                      highchartOutput("race_compare"),
                      tags$div("This plot shows how the proportion of officer and subject race in Terry Stops compares to ",
                               tags$a(href = "https://www.seattle.gov/opcd/population-and-demographics/about-seattle#raceethnicity",
                                     "Seattle's racial demographics"),
                               " as a whole.
                               White police officers seem to be over represented whereas white subjects
                               of Terry Stops are under represented. While black officers invlovled in Terry
                               Stops seem to be accurately reflected, black subjects of Terry Stops
                               are clearly over represented when compared to Seattle's demographics as a whole.
                               Asian officers and subjects are both under represented. This data indicates that
                               SPD has too many white officers and should try harder to employ a more diverse
                               police force. More importantly, it also clearly shows that black people are disproportionately
                               involved in Terry Stops compared to their overall representation in Seattle.")
                    ),
                    
                    ## Age ##
                    conditionalPanel(
                      condition = "input.plot_type == 'Age'",
                      highchartOutput("age"),
                      p("Examining officer and subject age in Terry Stops reveals that their distributions are closely
                        related. Most Terry Stops are conducted by officers in their late 20's to early 30's and most Terry
                        Stop subjects fall into this age group as well.")
                    ),
                    
                    ## Gender ##
                    conditionalPanel(
                      condition = "input.plot_type == 'Gender'",
                      fluidRow(
                        column(6, highchartOutput("off_gender")),
                        column(6, highchartOutput("sub_gender"))
                      ),
                      p("Males dominate both the officer and subject sides of a Terry Stop. It is interesting to see
                        how under represented woman are in SPD, assuming this Terry Stop data loosely follows the overall
                        SPD demographic data.")
                    ),
                    
                    ## Time of Day ##
                    conditionalPanel(
                      condition = "input.plot_type == 'Time of Day'",
                      highchartOutput("time"),
                      p("In 2018 and 2019 there are clear spikes in Terry Stops at 3am and 7pm. This leads me to think that
                        there is something like as a shift change occuring at these times. Another interesting finding to note is
                        the clear dip in Terry Stops around noon, during lunch time.")
                    )
                  )
                )
              )
      )
    )
    
  )
)

server <- function(input, output) {
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
  
  ####################
  ## Choropleth map ##
  ####################
  output$map <- renderLeaflet({
    terry_map_data()
    
    bins <- c(0, 25, 50, 100, 150, 200, 300, Inf)
    pal <- colorBin("Oranges", domain = beats$total_stops, bins = bins)
    
    beat_info <- paste(
      "<strong>Beat:</strong> ", beats$beat, "<br/>",
      "Terry Stops: ", beats$total_stops, "<br/>",
      "<strong>Subject Race</strong><br/>",
      "Asian: ", beats$Asian, "<br/>",
      "Black: ", beats$Black, "<br/>",
      "Hispanic: ", beats$Hispanic, "<br/>",
      "White: ", beats$White, "<br/>",
      "American Indian: ", beats$`American Indian/Alaska Native`, "<br/>",
      "Nat Hawaiian: ", beats$`Nat Hawaiian/Oth Pac Islander`, "</br>",
      "Multi-racial: ", beats$`Multi-Racial`, "<br/>",
      "Unknown: ", beats$Unknown, "<br/>",
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
      leaflet::addLegend(pal = pal,
                values = beats$total_stops,
                opacity = 0.7,
                title = "No. of Terry Stops",
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
  
  ##################################
  ## Frequency by Time Line Graph ##
  ##################################
  output$time_series <- renderHighchart(({
    terry_map_data()
    
    chart_data <- terry_year_df %>% 
      group_by(Reported.Date) %>% 
      summarise(n = n())
    
    # Convert to xts object and chart
    chart_data <- xts(chart_data[-1], order.by = chart_data$Reported.Date)
    hchart(chart_data, type = "column", name = "Count") %>%
      hc_title(text = "Frequency of Terry Stops Throughout the Year")
  }))
  
  ##########################
  ## Resolution pie chart ##
  ##########################
  output$resolution <- renderHighchart(({
    terry_map_data()
    
    terry_year_df %>%
      group_by(Stop.Resolution) %>% 
      summarise(count = n()) %>%
      hchart(
        "pie", hcaes(x = Stop.Resolution, y = count),
        name = "Frequency"
      ) %>% 
      hc_title(text = "Terry Stop Resolutions")
  }))
  
  #########################
  ## Call type pie chart ##
  #########################
  output$call_type <- renderHighchart(({
    terry_map_data()
    
    terry_year_df %>%
      group_by(Final.Call.Type) %>% 
      summarise(count = n()) %>%
      hchart(
        "pie", hcaes(x = Final.Call.Type, y = count),
        name = "Frequency"
      ) %>% 
      hc_title(text = "Terry Stop Call Type")
  }))
  
  ######################
  ## Mosaic race plot ##
  ######################
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
        "column",
        hcaes(x = Type, y = Percent, group = race),
        stacking = "normal"
      ) %>% 
      hc_yAxis(max = 100) %>% 
      hc_title(text = "Comparing Officer and Subject Race to Seattle Demographics")
  
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
  
  #######################
  ## Gender pie charts ##
  #######################
  output$sub_gender <- renderHighchart(({
    terry_map_data()
    
    terry_year_df %>%
      group_by(Subject.Perceived.Gender) %>% 
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      hchart(
        "pie", hcaes(x = Subject.Perceived.Gender, y = count),
        name = "Frequency",
        size = 225
      ) %>% 
      hc_title(text = "Subject Perceived Gender")
  }))
    
    output$off_gender <- renderHighchart(({
      terry_map_data()
    
      terry_year_df %>%
        group_by(Officer.Gender) %>% 
        summarise(count = n()) %>%
        arrange(desc(count)) %>% 
        hchart(
          "pie", hcaes(x = Officer.Gender, y = count),
          name = "Frequency",
          size = 225
        ) %>% 
        hc_title(text = "Officer Gender")
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

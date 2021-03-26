## data.R ##
# Fetch and clean all the data
# https://data.seattle.gov/

library(tidyverse)
library(lubridate)
library(sf)

#####################
## BEATS SHAPEFILE ##
#####################
# https://data.seattle.gov/dataset/Seattle-Police-Beats-2018-Present/ex83-w2uk
raw_beats <- function() {
  # Reset to clean shapefile
  beats <- st_read("data/Beats/SPD_Beats_WGS84.shp") 
  `%notin%` <- Negate(`%in%`)
  beats <- beats %>% 
    filter(beat %notin% c("H1", "H2", "H3", "99"))
  return  (beats)
}


#################
## TERRY STOPS ##
#################
# https://data.seattle.gov/Public-Safety/Terry-Stops/28ny-9ts8
terry_df <- read.csv("data/Terry_Stops.csv")

terry_df <- terry_df %>% 
  select( # No need for these columns for my analysis
    -c(GO...SC.Num, Initial.Call.Type, Officer.Squad,
       Call.Type, Sector, Precinct, Officer.ID,
       Subject.ID, Terry.Stop.ID, Arrest.Flag)
  ) %>% 
  mutate(
    Beat = trimws(ifelse(Beat == "-", NA, Beat)),
    Subject.Age.Group = ifelse(Subject.Age.Group == "-", "Unknown", Subject.Age.Group),
    Weapon.Type = ifelse(Weapon.Type == "-", "Not Reported", Weapon.Type),
    Final.Call.Type = ifelse(Final.Call.Type == "-", "Unknown", Final.Call.Type),
    Subject.Perceived.Race = ifelse(Subject.Perceived.Race == "-", "Unknown", Subject.Perceived.Race),
    Subject.Perceived.Race = gsub("Native Hawaiian or Other Pacific Islander", "Nat Hawaiian/Oth Pac Islander", Subject.Perceived.Race),
    Subject.Perceived.Race = gsub("American Indian or Alaska Native", "American Indian/Alaska Native", Subject.Perceived.Race),
    Subject.Perceived.Race = gsub("Black or African American", "Black", Subject.Perceived.Race),
    Subject.Perceived.Race = gsub("Other", "Unknown", Subject.Perceived.Race),
    Subject.Perceived.Gender = ifelse(Subject.Perceived.Gender == "-", "Unknown", Subject.Perceived.Gender),
    Frisk.Flag = ifelse(Frisk.Flag == "-", "Unknown", Frisk.Flag),
    Final.Call.Type = gsub("--", "", Final.Call.Type),
    Reported.Date = as_date(Reported.Date),
    Reported.Time =  as.integer(substr(Reported.Time, 1, 2)), # Keep only the hour as an int
    Year =  as.integer(substr(Reported.Date, 1, 4)), # Keep only the year as an int
    Subject.Age.Group = gsub("56 and Above", "56 +", Subject.Age.Group),
    Officer.Age = 2021 - Officer.YOB,
    Officer.Gender = gsub("M", "Male", Officer.Gender),
    Officer.Gender = gsub("F", "Female", Officer.Gender),
    Officer.Race = gsub("Two or More Races", "Multi-Racial", Officer.Race),
    Officer.Race = gsub("Black or African American", "Black", Officer.Race),
    Officer.Race = gsub("Hispanic or Latino", "Hispanic", Officer.Race),
    Officer.Race = gsub("Not Specified", "Unknown", Officer.Race),
    Frisk.Flag = ifelse(Frisk.Flag == "Y", "Yes", "No")
  ) %>%
  rename(
    Time = Reported.Time
  ) %>% 
  filter(Year %in% 2018:2020, # Only keep observations after 2017
         Officer.YOB != 1900) %>%  # Erroneous data
  select(-Officer.YOB) # Don't need this
assign("terry_df", terry_df, envir = .GlobalEnv)

race_color_set <- c("American Indian/Alaska Native"="red", "Asian"="yellow", 
                    "Black"="black", "Hispanic"="orange", 
                    "Multi-Racial"="pink", "Nat Hawaiian/Oth Pac Islander"="blue",
                    "Unknown"="grey", "White"="green")

n_total_terries <- nrow(terry_df)
assign("n_total_terries", n_total_terries, envir = .GlobalEnv)


################################
## # Seattle Demographic Data ##
################################
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

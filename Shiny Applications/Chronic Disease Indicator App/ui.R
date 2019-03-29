#
# This is the user-interface of a shiny app to visualize US CDC chronic diseases
# This app will be used to inform decisions about needs and scope of populations
# and identify a target demographic and area that can benefit from a chronic disease
# management program

install.packages("shiny")
install.packages("tidyverse")
install.packages("RColorBrewer")
install.packages("fiftystater")
library(shiny)
library(tidyverse)
# Define UI for application that draws a barplot of chronic diseases given a set of conditions
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Chronic Disease Report Generator"),

  #Sidebar with a checboxgroupinput for data value type and stratification1 values
  #dropbox select input for chronic disease type
  sidebarLayout(
    sidebarPanel(
       #data to observe
       selectInput("DataValueType", "Data Type",
                          c("Age-adjusted Rate" = "Age-adjusted Rate",
                            "Crude Rate" = "Crude Rate",
                            "Total Number" = "Number")), 
       #can compare multiple demographics
       selectInput("Stratification1", "Demographic",
                            c("Overall" =  "Overall",
                            "Male" = "Male",
                            "Female" = "Female",
                            "American Indian/Alaska Native" = "American Indian or Alaska Native",
                            "Asian/Pacific Islander" = "Asian or Pacific Islander",
                            "non-Hispanic Black" = "Black, non-Hispanic",
                            "Hispanic" = "Hispanic", 
                            "non-Hispanic White" = "White, non-Hispanic"
                            )),
       #Selecting one chronic disease at a time
       selectInput("Topic", "Chronic Disease",
                   c("Asthma" = "Asthma",
                     "Chronic Obstructive Pulmonary Disease" = "Chronic Obstructive Pulmonary Disease",
                     "Cardiovascular Disease" = "Cardiovascular Disease",
                     "Diabetes" = "Diabetes",
                     "Older Adults" = "Older Adults")), 
       #Radio button for map view or bar graph view
       radioButtons("view", label = h4("View Option"),
                    choices = list("Map View" = 1, "Bar View" = 2), 
                    selected = 1),
      #download the data presented 
      downloadButton("downloadData", "Download Report")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("ChartUSCD")
    )
  )
))


library(shiny); library(ggplot2); library(lubridate); library(stringr)

shinyUI(fluidPage(
  titlePanel("NYC Bikes"),
  
  # Escull el layout
  sidebarLayout(
    
    # Panell lateral
    sidebarPanel(
      dateInput("from_date", "From:", min="2013-06-01", value="2020-12-29", max="2020-12-31"),
      dateInput("to_date", "To:", min="2013-06-01", value="2020-12-31", max="2020-12-31"),
      actionButton("load", "Load"),
      actionButton("start", "Calculate"),
      sliderInput("sampleSize", "Sample size",
                  value = 5, min = 0, max = 100, step = 1, round = 0),
      sliderInput("mapScale", "Map scale",
                  value = 12, min = 0, max = 18, step = 1, round = 0),
      radioButtons(
        "from_to", "From/To", choiceValues=c(TRUE, FALSE), choiceNames=c("From", "To")
      ),
      selectInput(
        "station_name", "Station", choices=vroom("station_names.csv")$station_names
      ),
    ),
    # Panell central
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Graphs", plotOutput("plot")),
                  tabPanel("Map", plotOutput("map")),
                  tabPanel("Data", dataTableOutput("table"))
      )
    )
  )
))
      
  
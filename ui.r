library(shiny); library(ggplot2); library(lubridate); library(stringr); library(vroom); library(tidyverse)

Color_opt=c('PuBuGn','YlGn','YlGnBu','Spectral','Blues','Greys','Reds')
x_opt=c('Age','Trip duration')

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
      sliderInput("bins",
                  "Number of bins:",
                  min = 5,
                  max = 20,
                  value = 10),
      sliderInput('x_bin','x ticks gap',min=2, max=20,value=10),
      selectInput('Color_opt', 'Palette option', Color_opt),
      selectInput('x_opt','Select X variable',x_opt),
      selectInput(inputId = 'color_var',
                  label = 'Select color variable',
                  choices= c('Gender','User type')),
      sliderInput("mapScale", "Map scale",
                  value = 12, min = 0, max = 18, step = 1, round = 0),
      radioButtons(
        "from_to", "From/To", choiceValues=c(TRUE, FALSE), choiceNames=c("From", "To")
      ),
      selectInput(
        "station_name", "Station", choices=vroom("station_names.csv", delim=",")$station_names
      ),
      numericInput("topStations", "Top/Bottom fraction",
                  value = 5, min = 0),
      radioButtons(
        "top_bottom", "", choiceValues=c(TRUE, FALSE), choiceNames=c("Top", "Bottom")
      ),
      selectInput(
        "criterion", "Map color criterion", choices=c("Trip duration", "Trip count", "User age")
      ),
    ),
    # Panell central
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Data", dataTableOutput("table")),
                  tabPanel("Graphs", plotOutput("plot")),
                  tabPanel("Map", plotOutput("map"))
      )
    )
  )
))
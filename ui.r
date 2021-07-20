library(shiny); library(ggplot2); library(lubridate); library(stringr)

last_date <- format(as.Date(paste(format(Sys.Date(), "%Y-%m"), "-01", sep=""))-1, "%Y-%m-%d")

shinyUI(fluidPage(
  titlePanel("Diamonds"),
  
  # Escull el layout
  sidebarLayout(
    
    # Panell lateral
    sidebarPanel(
      dateInput("from_date", "From:", min="2013-06-01", value="2013-06-01", max="2020-12-31"),
      dateInput("to_date", "To:", min="2013-06-01", value="2020-12-31", max="2020-12-31"),
      actionButton("load", "Load"),
      actionButton("start", "Calculate"),
      sliderInput("sampleSize", "Mida de la mostra",
                  value = 20, min = 10, max = 100, step = 1, round = 0),
    ),
    
    # Panell central
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Graphs", plotOutput("plot")),
                  tabPanel("Data", dataTableOutput("table"))
      )
    )
  )
))
      
  
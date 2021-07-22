library(lubridate); library(stringr); library(vroom); library(tidyverse)

shinyServer(function(input, output) {
  
  variables <- reactiveValues(bike_data=vroom("data_structure.csv"))

  observeEvent(input$load, {
      date_seq <- seq(input$from_date, input$to_date, "months")
      temp <- tempfile()
      year <- format(date_seq[1], "%Y")
      month <- format(date_seq[1], "%m")
      download.file(paste("https://s3.amazonaws.com/tripdata/", year, month, "-citibike-tripdata.csv.zip", sep=""), temp)
      variables$bike_data <- vroom(unz(temp, paste(year, month, "-citibike-tripdata.csv", sep="")))
      if(length(date_seq) > 1){
        for(i in 2:(length(date_seq))){
          temp <- tempfile()
          year <- format(date_seq[i], "%Y")
          month <- format(date_seq[i], "%m")
          download.file(paste("https://s3.amazonaws.com/tripdata/", year, month, "-citibike-tripdata.csv.zip", sep=""), temp)
          temp_bike_data <- vroom(unz(temp, paste(year, month, "-citibike-tripdata.csv", sep="")))
          variables$bike_data <- rbind(variables$bike_data, temp_bike_data)
        }
      }
      variables$bike_data$gender <- as.factor(variables$bike_data$gender)
      variables$bike_data$gender <- recode_factor(variables$bike_data$gender, "0" = "Unknown", "1" = "Male", "2" = "Female")
      variables$bike_data$bikeid <- as.factor(variables$bike_data$bikeid)
      variables$bike_data$`start station id` <- as.factor(variables$bike_data$`start station id`)
      variables$bike_data$`end station id` <- as.factor(variables$bike_data$`end station id`)
      variables$bike_data$usertype <- as.factor(variables$bike_data$usertype)
      birthyear_outliers <- boxplot(na.omit(variables$bike_data)$`birth year`, plot=FALSE)$out
      variables$bike_data <- variables$bike_data[-which(variables$bike_data$`birth year` %in% birthyear_outliers), ]
      variables$bike_data <- variables$bike_data %>% filter(starttime > input$from_date & stoptime > input$to_date)
  })
    
  DrawChart <- eventReactive(input$start, {
    bike_data_sample <- sample_frac(variables$bike_data, input$sampleSize/100)
    chart <- ggplot(bike_data_sample) 
    chart <- chart + aes_string(x = bike_data_sample$gender, y = bike_data_sample$tripduration) + geom_boxplot()
    print(chart)
      
  })
  
  output$plot <- renderPlot({
    DrawChart()
  }, width = 1200, height = 720)
  
  output$table <- renderDataTable(sample_frac(variables$bike_data, input$sampleSize/100))
})
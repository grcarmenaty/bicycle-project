library(lubridate); library(stringr); library(vroom); library(tidyverse)

shinyServer(function(input, output) {
  
  bike_data <- eventReactive(input$load, {
      date_seq <- seq(input$from_date, input$to_date, "months")
      temp <- tempfile()
      year <- format(date_seq[1], "%Y")
      month <- format(date_seq[1], "%m")
      download.file(paste("https://s3.amazonaws.com/tripdata/", year, month, "-citibike-tripdata.csv.zip", sep=""), temp)
      bike_data <- vroom(unz(temp, paste(year, month, "-citibike-tripdata.csv", sep="")))
      if(length(date_seq) > 1){
        for(i in 2:length(date_seq)){
          temp <- tempfile()
          year <- format(date_seq[i], "%Y")
          month <- format(date_seq[i], "%m")
          download.file(paste("https://s3.amazonaws.com/tripdata/", year, month, "-citibike-tripdata.csv.zip", sep=""), temp)
          temp_bike_data <- vroom(unz(temp, paste(year, month, "-citibike-tripdata.csv", sep="")))
          bike_data <- rbind(bike_data, temp_bike_data)
        }
      }
      bike_data$gender <- as.factor(bike_data$gender)
      bike_data$gender <- recode_factor(bike_data$gender, "0" = "Unknown", "1" = "Male", "2" = "Female")
      bike_data$bikeid <- as.factor(bike_data$bikeid)
      bike_data$`start station id` <- as.factor(bike_data$`start station id`)
      bike_data$`end station id` <- as.factor(bike_data$`end station id`)
      bike_data$usertype <- as.factor(bike_data$usertype)
      birthyear_outliers <- boxplot(na.omit(bike_data)$`birth year`, plot=FALSE)$out
      bike_data <- bike_data[-which(bike_data$`birth year` %in% birthyear_outliers), ]
      bike_data <- bike_data %>% filter(starttime > input$from_date & stoptime > input$to_date)
      bike_data <- sample_n(bike_data, nrow(bike_data)*(input$sampleSize/100))
      print(bike_data)
  })
    
  DrawChart <- eventReactive(input$start, {
    chart <- ggplot(bike_data()) 
    chart <- chart + aes_string(x = bike_data()$gender, y = bike_data()&tripduration) + geom_boxplot()
    print(chart)
      
  })
  
  output$plot <- renderPlot({
    DrawChart()
  }, width = 1200, height = 720)
  
  output$table <- renderDataTable(bike_data())
})
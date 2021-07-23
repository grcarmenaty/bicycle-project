library(lubridate); library(stringr); library(vroom); library(tidyverse); library(ggmap)

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
      variables$bike_data <- variables$bike_data %>% filter(starttime > input$from_date & stoptime < input$to_date)
  })
    
  DrawChart <- eventReactive(input$start, {
    bike_data_sample <- sample_frac(variables$bike_data, input$sampleSize/100)
    chart <- ggplot(bike_data_sample, aes(x = gender, y = tripduration)) + geom_boxplot()
    print(chart)
  })

  DrawMap <- eventReactive(input$start, {
    bike_data_sample <- sample_frac(variables$bike_data, input$sampleSize/100)
    if(input$from_to){
      if(input$criterion=="Trip count"){
        bike_data_count <- bike_data_sample %>%
          filter(`start station name`==input$station_name) %>%
          group_by(`end station name`) %>%
          summarise(
            Criterion = n(),
            start_lon=mean(`start station longitude`),
            start_lat=mean(`start station latitude`),
            lon=mean(`end station longitude`),
            lat=mean(`end station latitude`)) %>%
          arrange(desc(Criterion))
      } else if(input$criterion=="Trip duration"){
        bike_data_count <- bike_data_sample %>%
          filter(`start station name`==input$station_name) %>%
          group_by(`end station name`) %>%
          summarise(
            Criterion = mean(tripduration)/60,
            start_lon=mean(`start station longitude`),
            start_lat=mean(`start station latitude`),
            lon=mean(`end station longitude`),
            lat=mean(`end station latitude`)) %>%
          arrange(desc(Criterion))
      } else {
        bike_data_count <- bike_data_sample %>%
          filter(`start station name`==input$station_name) %>%
          group_by(`end station name`) %>%
          summarise(
            Criterion = as.numeric(format(Sys.Date(), "%Y")) - mean(`birth year`),
            start_lon=mean(`start station longitude`),
            start_lat=mean(`start station latitude`),
            lon=mean(`end station longitude`),
            lat=mean(`end station latitude`)) %>%
          arrange(desc(Criterion))
      }
      
      if(input$top_bottom){
        bike_data_count <- head(bike_data_count, (input$topStations))
      } else {
        bike_data_count <- tail(bike_data_count, (input$topStations))
      }
    } else {
      if(input$criterion=="Trip count"){
        bike_data_count <- bike_data_sample %>%
          filter(`end station name`==input$station_name) %>%
          group_by(`start station name`) %>%
          summarise(
            Criterion = n(),
            start_lon=mean(`end station longitude`),
            start_lat=mean(`end station latitude`),
            lon=mean(`start station longitude`),
            lat=mean(`start station latitude`)) %>%
          arrange(desc(Criterion))
      } else if(input$criterion=="Trip duration"){
        bike_data_count <- bike_data_sample %>%
          filter(`end station name`==input$station_name) %>%
          group_by(`start station name`) %>%
          summarise(
            Criterion = mean(tripduration)/60,
            start_lon=mean(`end station longitude`),
            start_lat=mean(`end station latitude`),
            lon=mean(`start station longitude`),
            lat=mean(`start station latitude`)) %>%
          arrange(desc(Criterion))
      } else {
        bike_data_count <- bike_data_sample %>%
          filter(`end station name`==input$station_name) %>%
          group_by(`start station name`) %>%
          summarise(
            Criterion = as.numeric(format(Sys.Date(), "%Y")) - mean(`birth year`),
            start_lon=mean(`end station longitude`),
            start_lat=mean(`end station latitude`),
            lon=mean(`start station longitude`),
            lat=mean(`start station latitude`)) %>%
          arrange(desc(Criterion))
      }
      
      if(input$top_bottom){
        bike_data_count <- head(bike_data_count, (input$topStations))
      } else {
        bike_data_count <- tail(bike_data_count, (input$topStations))
      }
    }
    margin <- 0.01
    verticalSize <- (max(c(bike_data_count$lat, bike_data_count$start_lat)) + margin) - (min(c(bike_data_count$lat, bike_data_count$start_lat)) - margin)
    manhattan_bb <- c(
      left = mean(c(bike_data_count$lon, bike_data_count$start_lon), na.rm = TRUE) - verticalSize/2,
      bottom = min(c(bike_data_count$lat, bike_data_count$start_lat)) - margin,
      right = mean(c(bike_data_count$lon, bike_data_count$start_lon), na.rm = TRUE) + verticalSize/2,
      top = max(c(bike_data_count$lat, bike_data_count$start_lat)) + margin
    )
    newyorkmap <- get_stamenmap(bbox=manhattan_bb, maptype="toner", zoom=input$mapScale)
    chart <- ggmap(newyorkmap) +
      geom_point(data=bike_data_count, aes(x=start_lon, y=start_lat), color="cadetblue4", size=6) +
      geom_point(data=bike_data_count, aes(x=lon, y=lat, color=Criterion), size=6) + 
      geom_segment(data=bike_data_count, aes(
                    x=start_lon,y=start_lat,
                    xend=lon, yend=lat,
                    color=Criterion),
                   size=1.25, alpha=0.75) +
      theme(axis.ticks=element_blank(), axis.text=element_blank()) +
      xlab("") + ylab("")
    print(chart)
  })
  
  output$plot <- renderPlot({
    DrawChart()
  }, width = 1200, height = 720)
  
  output$map <- renderPlot({
    DrawMap()
  }, width = 890, height = 890)

  output$table <- renderDataTable(sample_frac(variables$bike_data, input$sampleSize/100))
})
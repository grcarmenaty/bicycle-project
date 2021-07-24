install.packages("vroom")
install.packages("ggmap")
install.packages("tidyverse")
library(tidyverse)
library(vroom)
library(ggmap)

temp <- tempfile()
download.file("https://s3.amazonaws.com/tripdata/202012-citibike-tripdata.csv.zip", temp)
data <- vroom(unz(temp, "202012-citibike-tripdata.csv"))
data$gender <- as.factor(data$gender)
data$gender <- recode_factor(data$gender, "0" = "Unknown", "1" = "Male", "2" = "Female")
data$bikeid <- as.factor(data$bikeid)
data$`start station id` <- as.factor(data$`start station id`)
data$`end station id` <- as.factor(data$`end station id`)
data$usertype <- as.factor(data$usertype)
birthyear_outliers <- boxplot(na.omit(data)$`birth year`, plot=FALSE)$out
data <- data[-which(data$`birth year` %in% birthyear_outliers), ]
str(data)
write.csv(data, "data.csv")
write.csv(sample_n(data, 1e4), "data_sample.csv")
station_names <- unique(c(data$`start station name`, data$`end station name`))
write.csv(station_names, "station_names.csv")

bike_data_sample <- sample_n(data, 1e4)

input$sampleSize <- 100
input

    bike_data_sample <- sample_frac(variables$bike_data, input$sampleSize/100)
    if(input$from_to){
      bike_data_count <- bike_data_sample %>%
        filter(`start station name`==input$station_name) %>%
        group_by(`end station name`) %>%
        summarise(
          color = n(),
          start_lon=mean(`start station longitude`),
          start_lat=mean(`start station latitude`),
          lon=mean(`end station longitude`),
          lat=mean(`end station latitude`)) %>%
        arrange(desc(color))
      if(input$top_bottom){
        bike_data_count <- head(bike_data_count, (5))
      } else {
        bike_data_count <- tail(bike_data_count, (5))
      }
    } else {
      bike_data_count <- bike_data_sample %>%
        filter(`end station name`==input$station_name) %>%
        group_by(`start station name`) %>%
        summarise(
          color = n(),
          start_lon=mean(`end station longitude`),
          start_lat=mean(`end station latitude`),
          lon=mean(`start station longitude`),
          lat=mean(`start station latitude`)) %>%
        arrange(desc(color)) %>%
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
      geom_point(data=bike_data_count, aes(x=lon, y=lat, color=color), size=6) + 
      geom_segment(data=bike_data_count, aes(
                    x=start_lon,y=start_lat,
                    xend=lon, yend=lat,
                    color=color),
                   size=1.25, alpha=0.75) +
      theme(axis.ticks=element_blank(), axis.text=element_blank()) +
      xlab("") + ylab("")
    print(chart)

fileConn<-file(".gitignore")
writeLines(c("data.csv", "data_sample.csv"), fileConn)
close(fileConn)
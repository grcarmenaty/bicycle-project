install.packages("vroom")
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

bike_data_count <- bike_data_sample %>% filter(`start station name`==station_names[1]) %>% group_by(`end station name`) %>% summarise(trip_count = n(), start_lon=mean(`start station longitude`), start_lat=mean(`start station latitude`), lon=mean(`end station longitude`), lat=mean(`end station latitude`))
    margin <- 0.01
    verticalSize <- (max(bike_data_sample$`start station latitude`) + margin) - (min(bike_data_sample$`start station latitude`) - margin)
    manhattan_bb <- c(
      left = mean(bike_data_sample$`start station longitude`, na.rm = TRUE) - verticalSize,
      bottom = min(bike_data_sample$`start station latitude`) - margin,
      right = mean(bike_data_sample$`start station longitude`, na.rm = TRUE) + verticalSize,
      top = max(bike_data_sample$`start station latitude`) + margin
    )
    newyorkmap <- get_stamenmap(bbox=manhattan_bb, maptype="toner", zoom=12)
    chart <- ggmap(newyorkmap) +
      geom_point(data=bike_data_sample, aes(x=`start station longitude`,y=`start station latitude`), color='red',size=2) +
      geom_segment(data=bike_data_count, aes(
                    x=start_lon, y=start_lat,
                    xend=lon, yend=lat,
                    color=trip_count),
                   size=1, alpha=0.75) +
      theme(axis.ticks = element_blank(), axis.text = element_blank())+
      xlab('')+ylab('')
    chart
fileConn<-file(".gitignore")
writeLines(c("data.csv", "data_sample.csv"), fileConn)
close(fileConn)
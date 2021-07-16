install.packages("vroom")
install.packages("tidyverse")
library(tidyverse)
library(vroom)

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

fileConn<-file(".gitignore")
writeLines(c("data.csv", "data_sample.csv"), fileConn)
close(fileConn)
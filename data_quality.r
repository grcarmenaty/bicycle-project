install.packages("vroom")
install.packages("tidyverse")
library(tidyverse)
library(vroom)

temp <- tempfile()
download.file("https://s3.amazonaws.com/tripdata/202012-citibike-tripdata.csv.zip", temp)
data <- vroom(unz(temp, "202012-citibike-tripdata.csv"))
str(data)

birthyear_outliers <- boxplot(na.omit(data)$birth.year, plot=FALSE)$out
max(birthyear_outliers)


a=data %>% filter(birth.year<1930) %>% group_by(birth.year) %>% summarise(count=n()) %>% arrange(birth.year)
sum(a$count)
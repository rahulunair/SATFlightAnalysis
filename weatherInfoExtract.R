# script to get weather data from wunderground.com

# Library used to get functions, date, day and month
library(lubridate)

calendar <- seq(as.Date("2013/1/1"), as.Date("2014/1/1"), by = "day") # change values here to get data for the time period

# Making list of URLs for the period specified above

urls <- sapply( calendar, function(c_date){
  if (!exists("x"))
    x <- paste("http://www.wunderground.com/history/airport/KSAT",year(c_date),month(c_date),day(c_date), "DailyHistory.html?format=1", sep = "/")
  else {
    temp_x <- paste("http://www.wunderground.com/history/airport/KSAT",year(c_date),month(c_date),day(c_date), "DailyHistory.html?format=1", sep = "/")
    x <- rbind(x, temp_x)
    remove(temp_x)
  }
  x})
  
#Creating a data frame of the downloded data for the period
weather_table <- rbindlist(lapply(urls, read.csv),use.names = TRUE, fill = TRUE) 

# Adding a new column, Date to the dataset
weather_table <- cbind("Date", as.data.frame(substr(weather_table$DateUTC.br..., 1, 10)), weather_table)

# writing the dataset to a file
write.csv(weather_table, "../datasets/weather/weather_data.csv")

# Reading the raw data, and formatting for my need
weather_table_1 <- read.csv("../datasets/weather_data.csv", stringsAsFactors = FALSE) 

# Combining two columns with CST and CDT data into one
weather_table_1$Time <- weather_table_1$TimeCST
weather_table_1$Time[!is.na(weather_table_1$TimeCDT)] =  weather_table_1$TimeCDT[!is.na(weather_table_1$TimeCDT)]

# Rearranging order of the columns
weather_table_f <- weather_table_1[c(c("Date", "Time"), setdiff(names(weather_table_1), c("Date", "Time")))]  

# Removing the count column X
weather_table_f <- weather_table_f[,-3]

# Rearranging order of the columns - moving TimeCST to the end of the columns
weather_table_f <- weather_table_f[c(setdiff(names(weather_table_f), c("TimeCST")), c("TimeCST"))]  

# Writing the edited file to a new dataset - formatted weather data
write.csv(weather_table_f, "../datasets/weather/f_weather_data.csv")




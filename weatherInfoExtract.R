# script to get weather data from wunderground.com

# Library used to get functions, date, day and month
library(lubridate)
library (data.table)

calendar <- seq(as.Date("2005/1/1"), as.Date("2015/1/1"), by = "day") # change values here to get data for the time period

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
View(weather_table)

# Adding a new column for utc date and time to the dataset
weather_table_1 <- cbind(as.data.frame(substr(weather_table$DateUTC.br..., 1, 19)), weather_table)
colnames(weather_table_1)[1] <- "UTCDateTime"

View(weather_table_1)

# converting UTC timezone to CST TImeZone
DateTime_UTC <- as.POSIXct(weather_table_1$UTCDateTime,tz="UTC")
head(DateTime_UTC)
DateTime_CST <- format(DateTime_UTC, tz = "America/Chicago", usetz = TRUE)
head(DateTime_CST, 24)

# adding date, time columns to weather_table_1
weather_table_1$date <- substr(DateTime_CST, 1, 10)
weather_table_1$time <- substr(DateTime_CST, 12, 16)

# adding date_hour column to weather_table_1
weather_table_1$date_hour <- paste(weather_table_1$date, substr(strptime(weather_table_1$time, "%H"), 12, 13), sep = "-")  
View(weather_table_1)

# Removing unused attributes and formatting
weather_table_2 <- weather_table_1 %>%  select(-c(UTCDateTime, TimeCST, Gust.SpeedMPH, PrecipitationIn, DateUTC.br..., TimeCDT))
names(weather_table_2)[names(weather_table_2) == "time"] <- 'hour'
View(weather_table_2)

# Selecting attributes important for analysis and ordering columns
weather_data <- weather_table_2 %>%  select(date_hour, date, hour, TemperatureF, VisibilityMPH, Wind.SpeedMPH, Wind.Direction, WindDirDegrees, Humidity, Sea.Level.PressureIn, Conditions, Events)
View(weather_data)





# writing the dataset to a file
write.csv(weather_data, "../datasets/orginals/h_weather_data.csv")
write.csv(weather_data, "../datasets/weather/h_weather_data.csv")


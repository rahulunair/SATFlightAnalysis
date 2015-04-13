library(tidyr)
library(dplyr)
library(sqldf) # a very nice library to do sql like queries with data frames
# Reading in weather and flight data weather data

weather_table_24 = read.csv("../datasets/weather/weather_data_24.csv")
f_dataset = read.csv("../datasets/SAT_flights.csv")

View (f_dataset)
View (weather_table_24)

# making a few columns as factors

f_dataset$Cancelled <-  as.factor(f_dataset$Cancelled)
f_dataset$Diverted <-  as.factor(f_dataset$Diverted)
f_dataset$Quarter <-  as.factor(f_dataset$Quarter)
f_dataset$Month <-  as.factor(f_dataset$Month)
f_dataset$DayofMonth <-  as.factor(f_dataset$DayofMonth)
f_dataset$DayOfWeek <-  as.factor(f_dataset$DayOfWeek)
f_dataset$DestAirportID <-  as.factor(f_dataset$DestAirportID)
f_dataset$DestAirportSeqID <-  as.factor(f_dataset$DestAirportSeqID)
f_dataset$DestCityMarketID <-  as.factor(f_dataset$DestCityMarketID)
f_dataset$ArrDel15 <-  as.factor(f_dataset$ArrDel15)


# getting the hour from departure time
f_dataset$hour <- substr(f_dataset$DepTime, 1, (nchar(f_dataset$DepTime)-2))

dim(f_dataset)
f_dataset$hour <- as.factor(f_dataset$hour)
summary (f_dataset$DepDelay)

dim(f_dataset)

#names(flights_2)[names(flights_2) == "data=e.hour"] <- 'date.hour'

# removing NA from departure delay and hour column and unites data and hour as date.hour
f_data <- f_dataset %>% filter(DepDelay!= "NA", hour!= "NA") %>%  unite("date.hour", FlightDate, hour, sep = "-")
 
dim(f_data)
str(f_data)


# Average delay 

avg_delay <- f_data %>% group_by (date.hour) %>% filter( DepDel15 == 1) 
    %>% summarize (Avg_DepDelayMinutes = mean(DepDelayMinutes), count = n())
    %>% filter ( count > 3 )
avg_delay

# max of average delay for more than 3 flights

HighestAvgDelayDest <- sqldf("SELECT `date.hour`,Avg_DepDelayMinutes,count from avg_delay WHERE Avg_DepDelayMinutes =  (
                SELECT max(Avg_DepDelayMinutes) FROM avg_delay);")

ScndHighestAvgDelayDest <- sqldf("SELECT `date.hour`,Avg_DepDelayMinutes,count from avg_delay WHERE Avg_DepDelayMinutes =  (
                SELECT max(Avg_DepDelayMinutes) FROM avg_delay
                WHERE Avg_DepDelayMinutes NOT IN (SELECT max(Avg_DepDelayMinutes) FROM avg_delay));")



# When was max delay for a date
max_delay <- f_data %>% group_by (date.hour) %>% filter( DepDel15 == 1) %>% summarize (Max_DepDelayMinutes = max(DepDelayMinutes))

names(max_delay)[names(max_delay) == "date.hour"] <- 'date_hour'



# Departure delay table with date, max amount of delay in minutes for the date, carrier ID and destination city
# Using this table, we can see when was the most delay and its details
# when was the least delay and details
# carrier with most number of delay
# carrier with least number of delay
# destination with most number of delay
# destination with least number of delay

max_delay_destination <- sqldf("select f_data.date_hour,Max_DepDelayMinutes,UniqueCarrier, DestCityName, Distance FROM max_delay 
                                inner join f_data  on f_data.date_hour = max_delay.date_hour 
                                and f_data. DepDelayMinutes = max_delay.Max_DepDelayMinutes
                                order by max_delay.Max_DepDelayMinutes DESC;") 
View(max_delay_destination)

# finding the highest and second highest delay information
HighestDelayDest <- sqldf("SELECT date_hour,Max_DepDelayMinutes,DestCityName,Distance from max_delay_destination WHERE Max_DepDelayMinutes =  (
                SELECT max(Max_DepDelayMinutes) FROM max_delay_destination);")

scndHighstDelayDest <- sqldf("SELECT date_hour,Max_DepDelayMinutes,DestCityName,Distance from max_delay_destination 
                              WHERE Max_DepDelayMinutes =  (SELECT max(Max_DepDelayMinutes) FROM max_delay_destination 
                              WHERE Max_DepDelayMinutes NOT IN (SELECT max(Max_DepDelayMinutes) FROM max_delay_destination));")


scndHighstDelayDest <- sqldf("SELECT date_hour,Max_DepDelayMinutes,DestCityName,Distance from max_delay_destination 
                              WHERE Max_DepDelayMinutes =  (SELECT max(Max_DepDelayMinutes) FROM max_delay_destination 
                              WHERE Max_DepDelayMinutes NOT IN (SELECT max(Max_DepDelayMinutes) FROM max_delay_destination));")

# when was the least delay


LowestDelayDest <- sqldf("SELECT date_hour,Max_DepDelayMinutes,DestCityName,Distance from max_delay_destination WHERE Max_DepDelayMinutes =  (
                SELECT min(Max_DepDelayMinutes) FROM max_delay_destination);")


scndLowestDelayDest <- sqldf("SELECT date_hour,Max_DepDelayMinutes,DestCityName,Distance from max_delay_destination 
                              WHERE Max_DepDelayMinutes =  (SELECT min(Max_DepDelayMinutes) FROM max_delay_destination 
                              WHERE Max_DepDelayMinutes NOT IN (SELECT min(Max_DepDelayMinutes) FROM max_delay_destination));")
                                                                                       

# Departure delay table with date, avg amount of delay in minutes for the date, carrier ID and destination city
# Using this table, we can see when was the most delay and its details
# when was the least delay and details
# carrier with most number of delay
# carrier with least number of delay
# destination with most number of delay
# destination with least number of delay



Destination1 =  labels(which.max(table(f_data$DestCityName)))
table(f_data$DestCityName)



# Weather Data
w_data <- weather_table_24 %>% filter (Time != "NA")
w_data$Time_24 <- as.character(w_data$Time_24)

class(w_data$Time_24)

# creating a new column hour for weather data
w_data$hour <- substr((w_data$Time_24), 1, (nchar(w_data$Time_24)-2))
w_data_2 <- w_data %>% filter(Date!= "NA", Time!= "NA")

# hour 24 is represented as empty character, thus the substr will return empty character for 24, 
# using sapply and update those back to 24

w_data_2$hour = sapply(w_data_2$hour, function (x) if(x == "") x = "24" else x)

# combining date and hour
w_data_2 <- w_data_2 %>% unite("date_hour", Date, hour, sep = "-")
View(w_data_2)
dim(w_data_2)

#Selecting attibutes fom weather data
w_data_3 <- w_data_2 %>% filter(date_hour != "NA", TemperatureF != "NA", Wind.SpeedMPH != "NA", VisibilityMPH != "NA")
dim(w_data_3)


# Average weather - Averaging all values in a 24 hr period, getting max of weather conditions and events if any
avg_weather <-  w_data_3 %>% group_by(date_hour) %>% summarise(Avg_Temperature_F = mean(TemperatureF),
                                                               Avg_wind_speed_MPH = mean (Wind.SpeedMPH),
                                                               Avg_Visibility = mean(VisibilityMPH),
                                                               conditions = labels(which.max(table(Conditions))),
                                                               events = labels(which.max(table(Events))))
                                                            

dim (avg_weather)


# combined weather and flight data


flight_nd_weather <- merge(f_)






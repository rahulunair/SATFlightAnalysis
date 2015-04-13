library(tidyr)
library(dplyr)
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



# When was max delay
max_delay <- f_data %>% group_by (date.hour) %>% filter( DepDel15 == 1) %>% summarize (Max_DepDelayMinutes = max(DepDelayMinutes))
                                                                                       
as.character(f_data[which.max(f_data[, "DepDelayMinutes"]), "DestCityName"])
opt <- which.max(f_data[, "DepDelayMinutes"])

index = which.max(max_delay$Max_DepDelayMinutes)

max_delay_date <- max_delay[index, 1]
max_delay_date_delay <-  max_delay[index, 2]



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
w_data_2 <- w_data_2 %>% unite("date.hour", Date, hour, sep = "-")
View(w_data_2)
dim(w_data_2)

#Selecting attibutes fom weather data
w_data_3 <- w_data_2 %>% filter(date.hour != "NA", TemperatureF != "NA", Wind.SpeedMPH != "NA", VisibilityMPH != "NA")
dim(w_data_3)


# Average weather - Averaging all values in a 24 hr period, getting max of weather conditions and events if any
avg_weather <-  w_data_3 %>% group_by(date.hour) %>% summarise(Avg_Temperature_F = mean(TemperatureF),
                                                               Avg_wind_speed_MPH = mean (Wind.SpeedMPH),
                                                               Avg_Visibility = mean(VisibilityMPH),
                                                               conditions = labels(which.max(table(Conditions))),
                                                               events = labels(which.max(table(Events))))
                                                            

dim (avg_weather)
View(avg_weather)labels(which.max(table(w_data_3$Conditions)))

# combined weather and flight data


flight_nd_weather <- merge(f_)






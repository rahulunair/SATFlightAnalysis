install_github("hadley/devtools")
require(devtools)
devtools::install_git("https://github.com/jbryer/makeR.git", branch = "master")

library(tidyr)
library(dplyr)
library(sqldf) # a very nice library to do sql like queries with data frames
library(makeR) # for calendarHeat function
library("leaflet") # for mapping

# Reading in weather and flight data weather data
weather_data = read.csv("../datasets/weather/weather_data.csv")
f_dataset = read.csv("../datasets/SAT_flights.csv")

# Viewing both datasets
View (f_dataset)
View (weather_data)

# Removing the first column from the datasets, which was automatically added when written as csv
weather_data <- weather_data[,-1]
flight_data <- f_dataset[,-1]

# making a few columns as factors

flight_data$Cancelled <-  as.factor(flight_data$Cancelled)
flight_data$Diverted <-  as.factor(flight_data$Diverted)
flight_data$Quarter <-  as.factor(flight_data$Quarter)
flight_data$Month <-  as.factor(flight_data$Month)
flight_data$DayofMonth <-  as.factor(flight_data$DayofMonth)
flight_data$DayOfWeek <-  as.factor(flight_data$DayOfWeek)
flight_data$DestAirportID <-  as.factor(flight_data$DestAirportID)
flight_data$DestAirportSeqID <-  as.factor(flight_data$DestAirportSeqID)
flight_data$DestCityMarketID <-  as.factor(flight_data$DestCityMarketID)
flight_data$ArrDel15 <-  as.factor(flight_data$ArrDel15)

# view flight_data

View(flight_data)


# Appending a zero to the front of depTime, so that hour format extracted can be easily matched with weather
for (i in seq(nrow(flight_data))) {
  if(nchar(flight_data$DepTime[i]) == 3)
    flight_data$DepTime[i] = paste0("0",flight_data$DepTime[i], sep = "")
  else if(nchar(flight_data$DepTime[i]) == 2)
    flight_data$DepTime[i] = paste0("00", flight_data$DepTime[i], sep = "")
  else 
    flight_data$DepTime[i] = flight_data$DepTime[i]
  #print (flight_data$DepTime[i])
}

View (flight_data)

# getting the hour from departure time
flight_data$hour <- substr(flight_data$DepTime, 1, 2)

dim(flight_data)



f_dataset$hour <- as.factor(f_dataset$hour)
summary (f_dataset$DepDelay)

dim(f_dataset)

#names(flights_2)[names(flights_2) == "data=e.hour"] <- 'date.hour'

# removing NA from departure delay and hour column and unites data and hour as date.hour
f_data <- flight_data %>% filter(DepDelay!= "NA") %>%  unite("date_hour", FlightDate, hour, sep = "-")

# Viewing attributes of the dataset 
dim(f_data)
str(f_data)
View(f_data)


# Average delay grouped based on carriers
Avg_carriers_delay <-  f_data %>%  filter( DepDel15 == 1) %>%  group_by(UniqueCarrier) %>%  summarize(Mean_Delay = mean(DepDelayMinutes), count = n()) %>% arrange(desc(Mean_Delay))
View(Avg_carriers_delay) 

# Max delay grouped based on carriers
Max_carriers_delay <- sqldf("SELECT UniqueCarrier, Max(DepDelayMinutes) as Max_Delay_minutes, date_hour from f_data group by UniqueCarrier Order By Max_Delay_minutes DESC;")
View(Max_carriers_delay)


# number of flights grouped based on Destination
flight_Destination <- f_data %>% group_by(Year, DestCityName) %>%  summarize(Number_of_flights = n()) %>% arrange(desc(Number_of_flights))
str(flight_Destination)

# basic plot of number of flights for each Destination
barchart(Number_of_flights ~ DestCityName, data = flight_Destination)


# Delay grouped based on Destination
Avg_delay_Destination <- f_data %>% filter( DepDel15 == 1) %>% group_by(Year, DestCityName) %>%  summarize(Mean_Delay = mean(DepDelayMinutes), Number_of_flights = n()) %>% arrange(desc(Number_of_flights))
View(Avg_delay_Destination) 



# Average delay  - atleast one flight was delayed for 15 minutes 
avg_delay <- f_data %>% group_by (date_hour) %>% filter( DepDel15 == 1)%>% summarize (Avg_DepDelayMinutes = mean(DepDelayMinutes), count = n()) 

# Exploring avg_delay subset
avg_delay
View(avg_delay)
dim(avg_delay)


# max of average delay from the grouped average delay avg_delay. Please note here, even if there is only one flight, that is also grouped.
# using sqldf which enables running sql like query against data frames in R. It is being very useful and flexible.

HighestAvgDelayDest <- sqldf("SELECT `date_hour`,Avg_DepDelayMinutes,count from avg_delay WHERE Avg_DepDelayMinutes =  (
                             SELECT max(Avg_DepDelayMinutes) FROM avg_delay);")

ScndHighestAvgDelayDest <- sqldf("SELECT `date_hour`,Avg_DepDelayMinutes,count from avg_delay WHERE Avg_DepDelayMinutes =  (
                                 SELECT max(Avg_DepDelayMinutes) FROM avg_delay
                                 WHERE Avg_DepDelayMinutes NOT IN (SELECT max(Avg_DepDelayMinutes) FROM avg_delay));")



# When was max delay for each date_hour, where the departure delay was atleast 15 mins
max_delay <- f_data %>% group_by (date_hour) %>% filter( DepDel15 == 1) %>% summarize (Max_DepDelayMinutes = max(DepDelayMinutes))

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





# Weather Data

dim(weather_data)
#Selecting attibutes fom weather data
weather_data_1 <- weather_data %>% filter(date_hour != "NA", TemperatureF != "NA", Wind.SpeedMPH != "NA", VisibilityMPH != "NA")
dim(weather_data_1)

View(w_data_3)


# Average weather - Averaging all values in a 24 hr period, getting max of weather conditions and events if any
avg_weather <-  weather_data_1 %>% group_by(date_hour) %>% summarise(Avg_Temperature_F = mean(TemperatureF),
                                                                     Avg_wind_speed_MPH = mean (Wind.SpeedMPH),
                                                                     Avg_Visibility = mean(VisibilityMPH),
                                                                     conditions = labels(which.max(table(Conditions))),
                                                                     events = labels(which.max(table(Events))))


dim (avg_weather)
dim (avg_delay)

# combined weather and flight data
View(avg_weather)
str(avg_delay)
flight_nd_w
flight_nd_weather <- merge(avg_delay, avg_weather, by = "date_hour")

View(flight_nd_weather)

dim(flight_nd_weather)


flight_nd_weather$hour <- substr(flight_nd_weather$date_hour, 12, 13)
str(flight_nd_weather)
# sample figures

xyplot(Avg_DepDelayMinutes ~ as.numeric(hour), data = flight_nd_weather)




#http://blog.revolutionanalytics.com/2009/11/charting-time-series-as-calendar-heat-maps-in-r.html
#http://stackoverflow.com/questions/26171068/add-dates-to-calendar-heat-map-r

# Calendar Heat Map
# uses MakeR library calendarHeat function

calendarHeat(as.POSIXct(substr(flight_nd_weather$date_hour,1, 10 ), format = "%Y-%m-%d"), flight_nd_weather$Avg_Temperature_F)
View(flight_Destination)


avg_flt_delay_per_day <-  f_data %>% group_by (Year, Month, DayofMonth) %>% filter( DepDel15 == 1)%>% summarize (Avg_DepDelayMinutes = mean(DepDelayMinutes), count = n()) 
View(avg_flt_delay_per_day)

# POSIX date 
each_dt_as_char <- paste0(as.character(avg_flt_delay_per_day$Year), "-",as.character(avg_flt_delay_per_day$Month),"-", as.character(avg_flt_delay_per_day$DayofMonth))
each_dt <- as.POSIXct(each_dt_as_char,format = "%Y-%m-%d")



calendarHeat2<- makeR::calendarHeat

#insert line to calculate day number
bl<-as.list(body(calendarHeat2))
body(calendarHeat2) <- as.call(c(
  bl[1:14], 
  quote(caldat$dom <- as.numeric(format(caldat$date.seq, "%d"))),
  bl[-(1:14)]
))

#change call to level plot
lp<-as.list(body(calendarHeat2)[[c(32,2,3)]])
lp$dom <- quote(caldat$dom)
lp$panel <- quote(function(x,y,subscripts,dom,...) {
  str(list(...))
  panel.levelplot(x,y,subscripts=subscripts,...)
  panel.text(x[subscripts],y[subscripts],labels=dom[subscripts])
})
body(calendarHeat2)[[c(32,2,3)]]<-as.call(lp)

calendarHeat(each_dt, avg_flt_delay_per_day$Avg_DepDelayMinutes, ncolors = 50, color = "r2b")
calendarHeat2(each_dt, avg_flt_delay_per_day$Avg_DepDelayMinutes,color = "r2b")



#  maps



library(ggmap)


#Destnation 
destination <- f_data %>%  group_by(DestCityName) %>% summarise(n = n())

# using google geocode service to get the lattitude and longitude
latLong <- geocode (as.character(destination$DestCityName), 'latlona')

# df with long , lat and address
latLong <- rbind(latLong, geocode ('san antonio, tx', 'latlona'))
latLong$id <-1:dim(latLong)[1] 
latLong <- latLong %>%  select(id, lon, lat, address)

#View(latLong)


destination$sourceCity <- 'san antonio, tx'
# df with source, destination and number of flights
destination <- destination %>%  select(sourceCity, DestCityName, n)
colnames(destination) <- c('source', 'dest', 'count_flights')

#View(destination)


# instead of this we could use  destination$id <-1:dim(destination)[1] 
for (i in seq(nrow(destination))) {
    destination$id[i] <-  i
}


# creating the source table to show lines
src <- data.frame(lon = character(30), lat = character(30))
src$id <-  1:dim(src)[1]
src$lon <- as.numeric(-98.49363) 
src$lat <- as.numeric(29.42412)
src  <- src %>% select(id, lon, lat)

View(src)




dst <- latLong %>%  filter(id != 31) %>%  select (id, lon, lat)
dst$lat <- sapply ( dst$lat, as.numeric)
dst$lon <- sapply ( dst$lon, as.numeric)
str (dst)
View(dst)


# route_lat_lon
route_lat_lon <- rbind(src, dst)
View(route_lat_lon)

mapData <- get_googlemap(center = 'us', zoom = 4, maptype = 'roadmap')
usmap <- ggmap(mapData, darken = 0, extent = 'device') 
airports <- usmap +  geom_point(data=latLong,aes(x=lon,y=lat),colour = 'red4',alpha=0.8, size = 3)
route <- airports + geom_line(data = route_lat_lon, aes(x=lon, y=lat, group = id), size = 0.5 ,alpha = 0.4,color = 'red4')
route

str(latLong)
























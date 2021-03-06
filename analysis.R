install_github("hadley/devtools")
require(devtools)

devtools::install_git("https://github.com/jbryer/makeR.git", branch = "master")

library(tidyr)
library(dplyr)
library(sqldf) # a very nice library to do sql like queries with data frames
library(makeR) # for calendarHeat function


# Reading in weather and flight data weather data
weather_data = read.csv("../datasets/weather/h_weather_data.csv")
f_dataset = read.csv("../datasets/SAT_flights.csv")

# Viewing both datasets
#View (f_dataset)
#View (weather_data)

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
flight_data$DestAirportSeqID <-
  as.factor(flight_data$DestAirportSeqID)
flight_data$DestCityMarketID <-
  as.factor(flight_data$DestCityMarketID)
flight_data$ArrDel15 <-  as.factor(flight_data$ArrDel15)
flight_data$DepDel15 <- as.factor(flight_data$DepDel15)
# view flight_data

View(flight_data)


# Appending a zero to the front of depTime, so that hour format extracted can be easily matched with weather
for (i in seq(nrow(flight_data))) {
  if (nchar(flight_data$DepTime[i]) == 3)
    flight_data$DepTime[i] = paste0("0",flight_data$DepTime[i], sep = "")
  else if (nchar(flight_data$DepTime[i]) == 2)
    flight_data$DepTime[i] = paste0("00", flight_data$DepTime[i], sep = "")
  else
    flight_data$DepTime[i] = flight_data$DepTime[i]
  #print (flight_data$DepTime[i])
}



# Appending a zero to the front of ArrTime, so that hour format extracted can be easily matched with weather
for (i in seq(nrow(flight_data))) {
  if (nchar(flight_data$ArrTime[i]) == 3)
    flight_data$ArrTime[i] = paste0("0",flight_data$ArrTime[i], sep = "")
  else if (nchar(flight_data$ArrTime[i]) == 2)
    flight_data$ArrTime[i] = paste0("00", flight_data$ArrTime[i], sep = "")
  else
    flight_data$ArrTime[i] = flight_data$ArrTime[i]
  #print (flight_data$DepTime[i])
}


View (flight_data)

# getting the hour from departure time
flight_data$hour <- substr(flight_data$DepTime, 1, 2)
flight_data$arr_hour <- substr(flight_data$ArrTime,1, 2)
dim(flight_data)



f_dataset$hour <- as.factor(f_dataset$hour)
# summary of flight data
summary (f_dataset$DepDelay)

dim(f_dataset)



# removing NA from departure delay and hour (departure Time) column and unites data and hour as date.hour
f_data <-
  flight_data %>% filter(DepDelay != "NA") %>%  unite("date_hour", FlightDate, hour, sep = "-")

# Viewing attributes of the dataset
dim(f_data)
str(f_data)
View(f_data)


# Average delay grouped based on carriers
# combined
Avg_carriers_delay <-
  f_data %>%  filter(DepDel15 == 1) %>%  
  group_by(UniqueCarrier) %>%  
  summarize(Mean_Delay = mean(DepDelayMinutes),
            count = n()) %>% 
  arrange(desc(Mean_Delay))

View(Avg_carriers_delay)



# Average arrival Delay
Avg_arr_delay_carriers <-  f_data %>%  filter(ArrDel15 == 1) %>%  
  group_by(UniqueCarrier) %>%  
  summarize(Mean_Arr_Delay = mean(ArrDelayMinutes),
            count = n()) %>% 
  arrange(desc(Mean_Arr_Delay))

View(Avg_arr_delay_carriers)

# bar chart of arr delay and flights
barchart(Mean_Arr_Delay ~  UniqueCarrier, data = Avg_arr_delay_carriers ,
         ylab = "Mean Arrival Delay (in minutes)", xlab = "Unique Carrier Codes", main = 
           "Arrival Delay Statistic of carrier from SAT")


# Arrival delay for carriers and count
levelplot(Mean_Arr_Delay ~ count * UniqueCarrier, data = Avg_arr_delay_carriers, main =
            "Arrival Delay vs number of flights per carrier" )




# 2013
Avg_carriers_delay_2013 <-  f_data %>%
  filter(DepDel15 == 1, Year == 2013) %>%
  group_by(UniqueCarrier) %>%
  summarize(Mean_Delay = mean(DepDelayMinutes), count = n()) %>%
  arrange(desc(Mean_Delay))

View(Avg_carriers_delay_2013)

# 2014
Avg_carriers_delay_2014 <-  f_data %>%
  filter(DepDel15 == 1, Year == 2014) %>%
  group_by(UniqueCarrier) %>%
  summarize(Mean_Delay = mean(DepDelayMinutes), count = n()) %>%
  arrange(desc(Mean_Delay))

View(Avg_carriers_delay_2014)





require(mosaic)
str(Avg_carriers_delay)

# plots

# bar chart of delay and flights
barchart(Mean_Delay ~ UniqueCarrier, data = Avg_carriers_delay_2013 ,
         ylab = "Mean Departure Delay (in minutes)", xlab = "Unique Carrier Codes", main = 
           "Flight Delay Statistic of carrier from SAT for 2013")

# 2013
levelplot(Mean_Delay ~ count * UniqueCarrier, data = Avg_carriers_delay_2013, main =
            "Delay vs number of flights per carrier for 2013" )

#2014
levelplot(Mean_Delay ~ count * UniqueCarrier, data = Avg_carriers_delay_2014, main =
            "Delay vs number of flights per carrier for 2014")

require(sqldf)
# Max delay grouped based on carriers
Max_carriers_delay <-
  sqldf(
    "SELECT UniqueCarrier, Max(DepDelayMinutes) as Max_Delay_minutes, date_hour from f_data group by UniqueCarrier Order By Max_Delay_minutes DESC;"
  )
View(Max_carriers_delay)



library(dplyr)

# Top 4 destinations for each year
flight_Destination_4 <-   f_data %>% group_by(Year, Dest) %>%  summarize(Number_of_flights = n()) 
flight_Destination_4 <- flight_Destination %>% filter (Number_of_flights > 2000) %>% arrange(desc(Number_of_flights))


# list of destinations and numbr of flights

flight_Destination <-   f_data %>% filter (Year < 2015) %>%  group_by(Year, Dest) %>%  summarize(Number_of_flights = n()) 
flight_Destination <- flight_Destination %>% arrange(desc(Number_of_flights))



View(flight_Destination)

# plot of number of flights for each Destination- Airport
barchart(Number_of_flights ~ Dest,groups = Year,stack = T, auto.key=list(space='right'), data = 
           flight_Destination, ylab = "Number of flights", xlab = "Airport")


# Delay grouped based on Destination
Avg_delay_Destination <-
  f_data %>% filter(DepDel15 == 1, Year != 2015) %>% 
  group_by(Year, DestCityName, Dest) %>%
  summarize(Mean_Delay = mean(DepDelayMinutes), Number_of_flights = n()) %>%
  arrange(desc(Number_of_flights))

View(Avg_delay_Destination)

# plot of Avg delay for each Destination- Airport
barchart(Mean_Delay ~ Dest,groups = Year,stack = F, auto.key=list(space='right'), data = 
           Avg_delay_Destination, ylab = "Mean Delay", xlab = "Airport", main = 
           "Average Delay to different destinations")

bwplot(Mean_Delay ~ Dest, data = Avg_delay_Destination, fill = "orange", pch="|",
       xlab = "Airport", ylab = "Mean Delay", main = "Average Delay Spread to diff destinations")


str(Avg_delay_Destination)

# Average delay  - atleast one flight was delayed for 15 minutes
avg_delay <-
  f_data %>% group_by (date_hour) %>% filter(DepDel15 == 1) %>% summarize (Avg_DepDelayMinutes = mean(DepDelayMinutes), count = n())

# Exploring avg_delay subset
avg_delay
View(avg_delay)
dim(avg_delay)


# max of average delay from the grouped average delay avg_delay. Please note here, even if there is only one flight, that is also grouped.
# using sqldf which enables running sql like query against data frames in R. It is being very useful and flexible.

HighestAvgDelayDest <-
  sqldf(
    "SELECT `date_hour`,Avg_DepDelayMinutes,count from avg_delay WHERE Avg_DepDelayMinutes =  (
    SELECT max(Avg_DepDelayMinutes) FROM avg_delay);"
    )

ScndHighestAvgDelayDest <-
  sqldf(
    "SELECT `date_hour`,Avg_DepDelayMinutes,count from avg_delay WHERE Avg_DepDelayMinutes =  (
    SELECT max(Avg_DepDelayMinutes) FROM avg_delay
    WHERE Avg_DepDelayMinutes NOT IN (SELECT max(Avg_DepDelayMinutes) FROM avg_delay));"
  )



#  max delay for each date_hour, where the departure delay was atleast 15 mins
max_delay <-
  f_data %>% group_by (date_hour) %>% filter(DepDel15 == 1) %>% summarize (Max_DepDelayMinutes = max(DepDelayMinutes))
View(max_delay)


max_delay_350_mins <- max_delay %>%  filter (Max_DepDelayMinutes > 350)

max_delay_350_mins$Max_DepDelayMinutes <- gsub(",","",max_delay_350_mins$Max_DepDelayMinutes)
max_delay_350_mins$Max_DepDelayMinutes <- as.numeric(max_delay_350_mins$Max_DepDelayMinutes)
max_delay_350_mins$date_hour <- as.POSIXct(max_delay_350_mins$date_hour, format = '%Y-%m-%d-%H')

View(max_delay_350_mins)

# xy plot
xyplot(Max_DepDelayMinutes ~ date_hour, data = max_delay_350_mins, type = c("p", "l"))


avg_delay <-
  f_data %>% group_by (date_hour) %>% filter(DepDel15 == 1) %>%
  summarize (Avg_DepDelayMinutes = mean(DepDelayMinutes))

View(avg_delay)


avg_delay_200_mins <- avg_delay %>%  filter (Avg_DepDelayMinutes > 200)

View(avg_delay_200_mins)

avg_delay_200_mins$Max_DepDelayMinutes <- gsub(",","",avg_delay_200_mins$Avg_DepDelayMinutes)
avg_delay_200_mins$Max_DepDelayMinutes <- as.numeric(avg_delay_200_mins$Avg_DepDelayMinutes)
avg_delay_200_mins$date_hour <- as.POSIXct(avg_delay_200_mins$date_hour, format = '%Y-%m-%d-%H')

# xy plot
xyplot(Avg_DepDelayMinutes ~ date_hour, data = avg_delay_200_mins, type = c("p", "l"))

# Departure delay table with date, max amount of delay in minutes for the date, carrier ID and destination city
# Using this table, we can see when was the most delay and its details
# when was the least delay and details
# carrier with most number of delay
# carrier with least number of delay
# destination with most number of delay
# destination with least number of delay

max_delay_destination <-
  sqldf(
    "select f_data.date_hour,Max_DepDelayMinutes,UniqueCarrier, DestCityName, Distance FROM max_delay
    inner join f_data  on f_data.date_hour = max_delay.date_hour
    and f_data. DepDelayMinutes = max_delay.Max_DepDelayMinutes
    order by max_delay.Max_DepDelayMinutes DESC;"
  )


max_delay_destination <- max_delay_destination %>% filter (Max_DepDelayMinutes > 596)
View(max_delay_destination)

# finding the highest and second highest delay information
HighestDelayDest <-
  sqldf(
    "SELECT date_hour,Max_DepDelayMinutes,DestCityName,Distance from max_delay_destination WHERE Max_DepDelayMinutes =  (
    SELECT max(Max_DepDelayMinutes) FROM max_delay_destination);"
  )

scndHighstDelayDest <-
  sqldf(
    "SELECT date_hour,Max_DepDelayMinutes,DestCityName,Distance from max_delay_destination
    WHERE Max_DepDelayMinutes =  (SELECT max(Max_DepDelayMinutes) FROM max_delay_destination
    WHERE Max_DepDelayMinutes NOT IN (SELECT max(Max_DepDelayMinutes) FROM max_delay_destination));"
  )


# when was the least delay


LowestDelayDest <-
  sqldf(
    "SELECT date_hour,Max_DepDelayMinutes,DestCityName,Distance from max_delay_destination WHERE Max_DepDelayMinutes =  (
    SELECT min(Max_DepDelayMinutes) FROM max_delay_destination);"
  )


scndLowestDelayDest <-
  sqldf(
    "SELECT date_hour,Max_DepDelayMinutes,DestCityName,Distance from max_delay_destination
    WHERE Max_DepDelayMinutes =  (SELECT min(Max_DepDelayMinutes) FROM max_delay_destination
    WHERE Max_DepDelayMinutes NOT IN (SELECT min(Max_DepDelayMinutes) FROM max_delay_destination));"
  )



# Weather Data

dim(weather_data)
#Selecting attibutes fom weather data
weather_data_1 <-
  weather_data %>% filter(date_hour != "NA", TemperatureF != "NA", Wind.SpeedMPH != "NA", VisibilityMPH != "NA")
dim(weather_data_1)
View (weather_data_1)

weather_data_full <- weather_data_1

weather_data_full$year <- as.numeric(substr(weather_data_1$date, 1, 4))

weather_data_1 <- weather_data_full %>% filter (year >2012)
View(weather_data_1)

# Average weather - Averaging all values in a 24 hr period, getting max of weather conditions and events if any
avg_weather <-
  weather_data_1 %>% group_by(date_hour) %>% summarise(
    Avg_Temperature_F = mean(TemperatureF),
    Avg_wind_speed_MPH = mean (Wind.SpeedMPH),
    Avg_Visibility = mean(VisibilityMPH),
    conditions = labels(which.max(table(Conditions))),
    events = labels(which.max(table(Events)))
  )

View(avg_weather)





dim (avg_weather)
dim (avg_delay)

# combined weather and flight data
View(avg_weather)
str(avg_delay)


avg_weather <- avg_weather %>% 
  filter (Avg_Temperature_F > -25)

avg_weather_2013 <- avg_weather %>% 
  filter (Avg_Temperature_F > -25, year == 2013)

avg_weather_2014 <- avg_weather %>% 
  filter (Avg_Temperature_F > -25, year == 2014)


# full weather
library(lubridate)
summary(weather_data_full)

## takes a long time, dont run

**## Not run:**
weather_data_full$month <- month(weather_data_full$date)
weather_data_full$group <- "past"
for ( i in seq(nrow(weather_data_full))) {
    if (weather_data_full[i, 'year'] > 2012)
    weather_data_full[i, 'group'] = weather_data_full[i, 'year']
}
## End(**Not run**)

weather_data_full$TemperatureF <- as.numeric(weather_data_full$TemperatureF)
weather_data_full <- weather_data_full %>% filter (TemperatureF > -10, year < 2015)
write.csv(weather_data_full, "../datasets/weather/weather_data_full.csv")


# plot of weather - historical and current
bwplot(TemperatureF ~ as.factor(month)|group, data = weather_data_full, pch = '|',
       type = c("l", "smooth"), grid = TRUE, col.line = "darkorange",main = 
         "Temperature across 12 months",xlab = "Months",
       ylab = "Temperature in F")


# plot of weather 2013
xyplot(Avg_Temperature_F ~ date_hour, data = avg_weather_2013,
       type = c("l", "smooth"), grid = TRUE, col.line = "darkorange",main = 
         "Temperature accross the year for 2013",xlab = "Date-hour across the year",
       ylab = "Temperature in F", scales=list(x=list(at=NULL)))


# plot of weather 2014
xyplot(Avg_Temperature_F ~ date_hour, data = avg_weather_2014,
       type = c("l", "smooth"), grid = TRUE, col.line = "darkblue",main = 
         "Temperature accross the year for 2014",xlab = "Date-hour across the year",
       ylab = "Temperature in F", scales=list(x=list(at=NULL)))


# calendar plot of weather
flight_nd_weather_no_2015 <- flight_nd_weather %>% 
  filter(substr(flight_nd_weather$date_hour,1, 4) != 2015) 

View(flight_nd_weather_no_2015)
calendarHeat(as.POSIXct(substr(flight_nd_weather_no_2015$date_hour,1, 10), format = "%Y-%m-%d"),
             flight_nd_weather_no_2015$Avg_Temperature_F,
             col = 'r2b', varname = "Temperature in degree F")


flight_nd_weather <- merge(avg_delay, avg_weather, by = "date_hour")

View(flight_nd_weather)
dim(flight_nd_weather)

flight_nd_weather$hour <-
  substr(flight_nd_weather$date_hour, 12, 13)
str(flight_nd_weather)

# sample figures

# Delay vs Time 
xyplot(Avg_DepDelayMinutes ~ as.numeric(hour), 
       data = flight_nd_weather, xlab = "Time (in Hrs)", 
       ylab = "Average Departure Delay in Minutes", 
       main = "Departure Delay across the day for 2013 - 2014")

#Delay vs avg Temperature
xyplot(Avg_DepDelayMinutes ~ as.numeric(Avg_Temperature_F), 
       data = flight_nd_weather, xlab = "Temperature in F", type = c("p"),
       ylab = "Average Departure Delay in Minutes", 
       main = "Departure Delay vs Temperature for 2013 - 2014")

# Delay vs Wind speed
xyplot(Avg_DepDelayMinutes ~ as.numeric(Avg_wind_speed_MPH), 
       data = flight_nd_weather, xlab = "Wind Speed in MPH", type = c("p"),
       ylab = "Average Departure Delay in Minutes", 
       main = "Departure Delay vs Wind Speed in MPH 2013 - 2014")

# There is a stray visibility value of -1000, removing it
flight_nd_weather <-  flight_nd_weather %>%  filter(Avg_Visibility > 0)

# Delay vs Visibility
xyplot(Avg_DepDelayMinutes ~ (Avg_Visibility), 
       data = flight_nd_weather, xlab = "Visibility in Miles", type = c("p"),
       ylab = "Average Departure Delay in Minutes", 
       main = "Departure Delay vs Visibility 2013 - 2014")

#http://blog.revolutionanalytics.com/2009/11/charting-time-series-as-calendar-heat-maps-in-r.html
#http://stackoverflow.com/questions/26171068/add-dates-to-calendar-heat-map-r

# Calendar Heat Map
# uses MakeR library calendarHeat function

calendarHeat(as.POSIXct(substr(flight_nd_weather$date_hour,1, 10), format = "%Y-%m-%d"),
             flight_nd_weather$Avg_Temperature_F)
View(flight_Destination)


avg_flt_delay_per_day <-
  f_data %>% group_by (Year, Month, DayofMonth) %>% filter(DepDel15 == 1) %>% summarize (Avg_DepDelayMinutes = mean(DepDelayMinutes), count = n())
View(avg_flt_delay_per_day)

# POSIX date
each_dt_as_char <-
  paste0(
    as.character(avg_flt_delay_per_day$Year), "-",as.character(avg_flt_delay_per_day$Month),"-", as.character(avg_flt_delay_per_day$DayofMonth)
  )
each_dt <- as.POSIXct(each_dt_as_char,format = "%Y-%m-%d")


calendarHeat(
  each_dt, avg_flt_delay_per_day$Avg_DepDelayMinutes, ncolors = 50, color = "r2b"
)


#  maps
# http://xccds1977.blogspot.com/2012/07/blog-post_26.html
library(ggmap)
#Destnation
destination <-
  f_data %>%  group_by(DestCityName) %>% summarise(n = n())

# using google geocode service to get the lattitude and longitude
latLong <-
  geocode (as.character(destination$DestCityName), 'latlona')

# df with long , lat and address
latLong <- rbind(latLong, geocode ('san antonio, tx', 'latlona'))
latLong$id <- 1:dim(latLong)[1]
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

#View(src)

dst <- latLong %>%  filter(id != 31) %>%  select (id, lon, lat)
dst$lat <- sapply (dst$lat, as.numeric)
dst$lon <- sapply (dst$lon, as.numeric)
str (dst)
#View(dst)


# route_lat_lon
route_lat_lon <- rbind(src, dst)
#View(route_lat_lon)

mapData <-
  get_googlemap(center = 'us', zoom = 4, maptype = 'roadmap')
usmap <- ggmap(mapData, darken = 0, extent = 'device')
airports <-
  usmap +  geom_point(
    data = latLong,aes(x = lon,y = lat),colour = 'red4',alpha = 0.8, size = 3
  )
route <-
  airports + geom_line(
    data = route_lat_lon, aes(x = lon, y = lat, group = id), size = 0.5 ,alpha = 0.4,color = 'red4'
  )

# map of different destinations from SAT.
route


# plot of weather 2013
xyplot(Avg_Temperature_F ~ date_hour, data = avg_weather_2013,
       type = c("l", "smooth"), grid = TRUE, col.line = "darkorange",main = 
         "Temperature accross the year for 2013",xlab = "Date-hour across the year",
       ylab = "Temperature in F", scales=list(x=list(at=NULL)))

View (f_data)

#A model to predict departure delay minutes#


# cleaning the flight dataset for modelling departure delay

f_data_modelling <-  f_data %>%  select (-Div5TotalGTime, -Div5WheelsOn ,
                                         -Div5AirportSeqID,-DepDelay,
                                         -Div5WheelsOff,-Div5TailNum, 
                                         -Div5LongestGTime, -DepartureDelayGroups,
                                         -DepDelayMinutes, -V110, -OriginCityName, -OriginState, 
                                         -OriginStateFips, -OriginStateName,
                                         -OriginCityMarketID, -Origin, 
                                         -OriginAirportID,-OriginAirportSeqID,
                                         -Div2TailNum,-Div2Airport,
                                         -DivAirportLandings, -Diverted,
                                         -Cancelled, -ArrDel15, -ArrDelay,
                                         -Flights, -OriginWac, -Div1Airport,
                                         -Div1TailNum, -DestCityName,
                                         -DestCityMarketID, -DestAirportSeqID )

# Removing variables with only NA's
f_data_modelling <- f_data_modelling[,colSums(is.na(f_data_modelling))<60000]
nrow (f_data_modelling)
f_data_mdl <- na.omit(f_data_modelling);

library (data.table)
setattr(f_data_mdl$arr_hour,"levels",c("00", "01", "02", "03", "05", "06", "07", "08", "09", "10",
                              "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 
                              "21", "22", "23", "24"))


f_data_mdl$ArrMin <- substr(f_data_mdl$ArrTime, nchar (f_data_mdl$ArrTime) -1
                           , nchar (f_data_mdl$ArrTime) )
f_data_mdl$DepMin <- substr(f_data_mdl$DepTime, nchar (f_data_mdl$DepTime) -1
                            , nchar (f_data_mdl$DepTime) )


f_data_mdl$Year <- as.factor(f_data_mdl$Year)
f_data_mdl$ArrMin <- as.integer(f_data_mdl$ArrMin )
f_data_mdl$DepMin <- as.integer(f_data_mdl$DepMin)
f_data_mdl$hour <- substr(f_data_mdl$date_hour, 12,13 ) 
f_data_mdl$hour <- as.factor(f_data_mdl$hour)
setattr(f_data_mdl$hour,"levels",c("00", "01", "04", "05", "06", "07", "08", "09", "10",
                                       "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 
                                       "21", "22", "23", "24"))

colnames(f_data_mdl)
summary(f_data_mdl$DepDel15)
summary (f_data_mdl$ArrDelayMinutes)
f_data_mdl$ArrDel15 <- f_data_mdl$ArrDelayMinutes > 15
f_data_mdl$ArrDel15 <- ifelse ( f_data_mdl$ArrDel15 == TRUE, 1, 0 )
f_data_mdl$ArrDel15 <- factor(f_data_mdl$ArrDel15)
f_data_mdl.train <- na.omit(f_data_mdl.train)
f_data_mdl.test <- na.omit(f_data_mdl.test)

colnames(f_data_mdl)


f_data_mdl$DistanceGroup <- as.factor(f_data_mdl$DistanceGroup)
flight_formula <- as.factor(DepDel15) ~ ArrDel15 + Quarter + Month + DayOfWeek +       UniqueCarrier + Dest + DepTime + AirTime + hour + arr_hour + Distance + DayofMonth + 
  TaxiIn + ArrDelayMinutes + DepMin + DistanceGroup




f_data_mdl$arr_hour <- as.integer(f_data_mdl$arr_hour)
f_data_mdl$ArrTime<- as.integer(f_data_mdl$ArrTime)

f_data_mdl$ArrDelayMinutes


ind <- sample(2, nrow(f_data_mdl), replace=TRUE, prob=c(0.7, 0.3))
f_data_mdl.train <- f_data_mdl[ind==1,]
f_data_mdl.test <- f_data_mdl[ind==2,]





##### RANDOM FOREST #######
library (randomForest)
rf_model <- randomForest(flight_formula, data = f_data_mdl.train, importance = TRUE, ntree = 500)

delay.prediction <- predict(rf_model, f_data_mdl.test )
table (f_data_mdl.test[,'DepDel15'],delay.prediction )

varImpPlot(rf_model)








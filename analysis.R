# function to load the dataset

read_in <- function (location) {
  read.csv(location, header = TRUE, stringsAsFactors = FALSE)
}

file_1401 <- read_in("/home/rahul/programming/R/project/datasets/On_Time_On_Time_Performance_2014_1.csv")

filenames <- c('01', '02', '03', '04','05', '06', '07', '08', '09', '10', '11', '12')

for  (i in 1:length(filenames)){
  
  pathName <- "/home/rahul/programming/R/weather/UTSA Weather/2015/"
  regexP <- paste('^weather 2_',filenames[i],'.*\\.csv$', sep = "")
  csv_file <- list.files(path=pathName, pattern = regexP)
  myread.csv = function (x) read.csv(x, header = FALSE, skip = 11, nrows = 285)
  readThis <- paste(pathName , csv_file, sep = "")
  mydataFrame = paste("Weather_", filenames[i], sep = "")
  mydataFrame <- do.call("rbind", lapply(readThis, myread.csv))
  colnames(mydataFrame) <- c('Date', 'Time',	'CO2', 'DIRECTION',	'HG',	'WINDSPD',	'OAE')
  write.csv (mydataFrame, file = paste(pathName, "monthly/Weather_2015_", filenames[i], ".csv", sep = ""))
  
  print (readThis) # list of file location
  
}
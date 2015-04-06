
# fuction to read in files
read_in <- function (file_name) {

    # using fread function from read.table as read.csv was taking long time to read in all files
    fread(file_name, header = TRUE, stringsAsFactors = FALSE)
  }

# unzip the zipped files into an appropriate directory

process_files <- function() {
  
  # Timer start 
  ptm <- proc.time()  
  
  # Directory where files are unzipped
  zipdir <- "../datasets/flight_data/"
  zip_files_list <- list.files(path="../datasets/orginals/", pattern = ".\\.zip$")
  
  # unzipping to specified directory zipdir
  for (zip_file in zip_files_list) {
    zip_file = paste("../datasets/orginals", zip_file, sep = "/")
    cat("Extracting file: \"",zip_file, "\"\n")
    unzip( zip_file, exdir = zipdir)
  }
  
  # Getting the list of files in the directory
  files_list = list.files(path = zipdir, , pattern = ".\\.csv$")
  
  #  Reading in one csv at a time and filtering for Origin = "SAT"
  for ( file_name in files_list) {
    
    file_name <- paste(zipdir,file_name, sep ="")
    cat("Reading in file: \"",file_name, "\"\n") # prints out the current file name
    
    if( !exists ("f_dataset"))
      
      {
      f_dataset <- read_in(file_name)
      f_dataset <- f_dataset %>%  filter(grepl('SAT', Origin))
    }
    
    else
      
      {
        
      temp_f_dataset <- read_in(file_name)
      temp_f_dataset <- temp_f_dataset %>%  filter(grepl('SAT', Origin))
      
      f_dataset <- rbind(f_dataset,temp_f_dataset)
      rm(temp_f_dataset)
      
    } 
      
  }
  cat("Number of data points: \"",nrow(f_dataset), "\"\n") # Number of rows of the filtered dataset
    write.csv(f_dataset, "../datasets/SAT_flights.csv")
    proc.time() - ptm # timing the save the file
  
}


# When this method is called, flight datasets zipped are unzipped,
# filtered for datapoints with Origin == 'SAT' and saved as SAT_flights.csv

process_files()
  
  


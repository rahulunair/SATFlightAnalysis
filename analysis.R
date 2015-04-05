# function to load the dataset
library(data.table)


# fuction to read in files
read_in <- function (file_name) {
  # using fread function from read.table as read.csv was taking long time to read in all files
  fread(paste("/home/rahul/programming/R/project/datasets/", file_name, sep = ""), header = TRUE, stringsAsFactors = FALSE)

}
# Start the clock!


files_list <- list.files(path="/home/rahul/programming/R/project/datasets/", pattern = ".\\.csv$")
#dataset <- rbindlist(lapply(files_list,read_in))





ptm <- proc.time()

for ( file in files_list) {
  cat("Reading in file: \"",file, "\"\n") # prints out the current file name
  if( !exists ("dataset")){
    dataset <- read_in(file)
    dataset <- dataset %>%  filter(grepl('SAT', Origin))
  }
  else {
    temp_dataset <- read_in(file)
    temp_dataset <- temp_dataset %>%  filter(grepl('SAT', Origin))
    dataset <- rbind(dataset,temp_dataset[-1,])
    rm(temp_dataset)
  } 
  
}
proc.time() - ptm # timing the save the file
nrow(dataset)
View(dataset)


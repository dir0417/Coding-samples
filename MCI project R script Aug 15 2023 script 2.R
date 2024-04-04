# Load required libraries
library(readxl)
library(dplyr)
library(lubridate)
library(purrr)

# Set your working directory to the folder containing Excel files
setwd("C:/Users/dair/Documents/R Scripts/Report/")

# Function to process a single Excel file
process_excel_file <- function(file_path) {
  # Read the Excel file
  data <- read_excel(file_path, skip = 1)
  
  # Extract the filename without extension
  filename <- tools::file_path_sans_ext(basename(file_path))
  
  # Remove header and add filename column
  colnames(data) <- c("Column1", "Column2", "Column3")  # Rename columns as needed
  data <- data[-1, ]
  data$Filename <- filename
  
  # Add date event started and ended columns
  data$EventStarted <- Sys.time()  # Replace with actual event start time
  data$EventEnded <- Sys.time() + minutes(nrow(data))  # Replace with actual event end time
  
  return(data)
}

# Get a list of all Excel files in the folder
excel_files <- list.files(pattern = "*.xlsx")

# Process all Excel files using the function and bind them into one dataframe
all_data <- map_df(excel_files, process_excel_file)

# Merge with another file (replace "other_file_path" with the actual path)
other_data <- read_excel("C:/Users/dair/Documents/R Scripts/facilities.xlsx")
merged_data <- bind_rows(all_data, other_data)

# Save the merged data to an output file
write_excel_csv(merged_data, "output_file.xlsx")


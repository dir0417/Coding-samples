install.packages("stringr")
install.packages("dplyr")
install.packages("readxl")
install.packages("lubridate")
install.packages("tidyr")
install.packages("openxlsx")
library(stringr)
library(dplyr)
library(readxl)
library(lubridate)
library(tidyr)
library(openxlsx)

# Set the folder path where your Excel files are located
folder_path <- "C:/Users/dair/Documents/R Scripts/Report/"
# Get a list of Excel file names in the folder
file_names <- list.files(path = folder_path, pattern = "*.xlsx", full.names = TRUE)

# Function to process each file
process_file <- function(file_names) {
  df <- read_excel(file_names, skip = 1)  # Assuming the header is in the second row
 # df <- read_excel("C:/Users/hapappul/Desktop/MOCK MCI/Test/report 1519.xlsx", skip = 1)
  df <- df %>% rename('EMResource Region' = 'Resource Type', 'Facility Name' = 'Resource')  # Rename columns
  file_date <- as.Date(gsub(".xlsx", "", basename(file_names)), format = "%Y-%m-%d")  # Extract date from filename
  
  #capturing the header to extract/Sub string the date
  df1 <- read_excel(file_names) #Reading the file 
  header <- colnames(df1[1]) #first row from excel file
  str1 <- header %>% str_extract("\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}") #sub string the date (string) from header
  date_collected <- gsub('/', '-',str1, fixed = TRUE) %>% mdy_hm(tz="America/Denver") #converting to date format in Mountain Time

  # (Nicole's add) Extract time from filename using a regular expression
 military_time <- sub(".+\\s(\\d+)\\.xlsx", "\\1", basename(file_names))
  
  # (Nicole's add) Convert military time to standard time (HH:MM:SS)
  standard_time <- sprintf("%04d", as.numeric(military_time))
  standard_time <- paste(substr(standard_time, 1, 2), substr(standard_time, 3, 4), "00", sep = ":")  # Format as HH:MM:SS
  
  # (Nicole's add) Create a standard timestamp
  timestamp <- as.POSIXct(paste(file_date, standard_time), format = "%Y-%m-%d %H:%M:%S")
  
  # (Nicole's add) Add the times as a new column
  df <- df %>% mutate(Timestamp = timestamp)
  
  # Add columns for event start and end dates
  df <- df %>% mutate('Event Started' = "2023-07-13 15:04", 'Event Ended' = "2023-07-13 16:04",'Date Collected' = date_collected)  # Assuming 1 hour duration
  
  # Add a filename column
  df <- df %>% mutate('FileName' = basename(file_names))
  
  # Add a column to rename files based on filename, time, and date
  #df <- df %>% mutate('Date Collected' = date_collected)
  
  return(df)
}

# Process each file and combine them into one data frame
combined_df <- lapply(file_names, process_file) %>% bind_rows()

# Load the additional file for merging
additional_file <- read_excel("C:/Users/dair/Documents/R Scripts/facilities.xlsx")

# Merge the combined_df with the additional file
final_merged_df <- merge(combined_df, additional_file, by.x = "Facility Name", by.y = "Resource Name")

# Removing columns
final_merged_remove_col_df <- final_merged_df %>%
  select('Facility Name','EMResource Region', 'Red Now', 'Yellow Now', 'Green Now', 'Last Update', 'Event Started', 
         'Event Ended','FileName', 'Date Collected','Address', 'County', 'Latitude', 'Longitude', 'License #  (State will populate)' )

# renaming columns from facilities
final_merged_remove_col_df <- final_merged_remove_col_df %>%
  rename('License #' = 'License #  (State will populate)')

# Save the final merged dataframe to an output file
output_file_path <- "C:/Users/dair/Documents/R Scripts/final_merged_output.xlsx"
write.xlsx(final_merged_remove_col_df, output_file_path, rowNames = FALSE)

cat("Script execution completed.\n")


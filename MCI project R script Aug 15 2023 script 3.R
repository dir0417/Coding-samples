library(dplyr)
library(readxl)
library(lubridate)
library(tidyr)

# Set the folder path where your Excel files are located
folder_path <- "C:/Users/dair/Documents/R Scripts/Report/"

# Get a list of Excel file names in the folder
file_names <- list.files(path = folder_path, pattern = ".xlsx", full.names = TRUE)

# Function to process each file
process_file <- function(file_name) {
  df <- read_excel(file_name, skip = 1)  # Assuming the header is in the second row
  df <- df %>% rename('EMResource Region+' = 'Resource Type', 'Facility Name' = 'Resource')  # Rename columns
  file_date <- as.Date(gsub(".xlsx", "", basename(file_name)), format = "%Y-%m-%d")  # Extract date from filename
  
  # Add columns for event start and end dates
  df <- df %>% mutate(EventStarted = file_date, EventEnded = file_date + days(1))
  
  # Add a filename column
  df <- df %>% mutate(FileName = basename(file_name))
  
  # Add a column to rename files based on filename, time, and date
  df <- df %>% mutate(RenamedColumn = paste(FileName, format(Sys.time(), "%Y%m%d%H%M%S"), sep = "_"))
  
  return(df)
}

# Process each file and combine them into one data frame
combined_df <- lapply(file_names, process_file) %>% bind_rows()

# Load the additional file for merging
additional_file <- read_excel("C:/Users/dair/Documents/R Scripts/facilities.xlsx")

# Merge the combined_df with the additional file
final_merged_df <- merge(combined_df, additional_file, by = "common_column")

# Save the final merged dataframe to an output file
output_file <- "path/to/your/output/final_merged_output.xlsx"
write.xlsx(final_merged_df, output_file, row.names = FALSE)

cat("Script execution completed.\n")

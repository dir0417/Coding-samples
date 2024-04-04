# Load required packages
library(readxl)
library(dplyr)
library(lubridate)

# Set your working directory to the folder containing Excel files
setwd("C:/Users/dair/Documents/R Scripts/Report/")

# Get a list of all Excel files in the folder
excel_files <- list.files(pattern = "\\.xlsx$")

# Create an empty data frame to store the merged data
merged_data <- data.frame()

# Loop through each Excel file
for (file in excel_files) {
  # Read Excel file
  data <- read_excel(file, skip = 1)  # Assuming header is in the first row
  
  # Add filename column
  data$Filename <- file
  
  # Rename columns
  colnames(data) <- c("Col1", "Col2", "Col3", "Col4", "Col5", "Filename")
  
  # Add date event started and ended columns
  data$EventStarted <- NA
  data$EventEnded <- NA
  
  # Add column for renaming files based on filename, time, and date
  data$RenamedFilename <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", file)
  
  # Append data to the merged_data dataframe
  merged_data <- bind_rows(merged_data, data)
}

# Merge with another file (replace 'other_data_file.xlsx' with your file)
other_data <- read_excel("C:/Users/dair/Documents/R Scripts/facilities.xlsx")

final_merged_data <- bind_rows(merged_data, other_data)

# Save the merged data to an output file
write.xlsx(final_merged_data, "output_merged_data.xlsx", row.names = FALSE)

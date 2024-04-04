setwd("C:/Users/hapappul/Documents/R_Scripts/RScripts_PROD")
readRenviron("C:/Users/hapappul/Documents/R_Scripts/RScripts_PROD/.Renviron")

args = commandArgs(trailingOnly = TRUE)
message(sprintf("Hello %s", args[1L]))

# load libraries
# pacman::p_load(
#   pacman,
#   readxl,
#   RSelenium,
#   lubridate,
#   dplyr,
#   rvest,
#   RODBC,
#   gsubfn,
#   magrittr,
#   stringr,
#   wdman
# )

install.packages("readxl")
install.packages("pacman")
install.packages("RSelenium")
install.packages("lubridate")
install.packages("dplyr")
install.packages("rvest")
install.packages("RODBC")
install.packages("gsubfn")
install.packages("magrittr")
install.packages("stringr")
install.packages("wdman")
install.packages("tidyverse")
library(readxl)
library("pacman")
library("RSelenium")
library("lubridate")
library("dplyr")
library("rvest")
library("RODBC")
library("gsubfn")
library("magrittr")
library("stringr")
library("wdman")
library("tidyverse")

xcel_file <- read_excel("C:/Users/hapappul/Documents/R_Scripts/report_20123.xlsx")


class(xcel_file)

my_df <- as.data.frame(xcel_file)
my_df
class(my_df)

colnames(my_df) <- my_df[1,]
colnames(my_df)

my_df <- my_df[-1, ]
my_df

regionTables <- my_df

# lapply(regionTables, function(x) {
#   names(x)
# })

## Data elements going into Event_snapshot_daily table -------------------------------------------------------------------------
# select specific columns from EMResource tables (this is done because some regions have additional data points beyond what we need)
regionTables <-   select(regionTables,"Resource", 
                         "Confirmed COVID-19 - Total", 
                         "Confirmed COVID Hospitalized - Age 0-4", 
                         "Confirmed COVID Hospitalized - Age 5-17", 
                         "Conf COVID-19 Fully Vaxxed - Total",
                         "Conf COVID-19 Fully Vaxxed - Age 5-17",
                         "Total # of Staffed ICU Beds", 
                         "ICU Bed Availability (current)",
                         "PICU - Available (current)",
                         "NICU - Available (current)",
                         "ICU Bed Shortage (anticipated)", 
                         "Med/Surgical Bed Availability (current)", 
                         "Baseline Staffed-bed Capacity",
                         "Ped Med/Surgical - Available (current)",
                         "Med/Surgical Bed Shortage (anticipated)",
                         "License #  (State will populate)", 
                         "Comment", 
                         "Last Update", 
                         "By User", 
                         "Resource Type")



# renaming columns to match SQL table
colnames <- c("Resource_facility_name", 
              "Num_Confirmed_COVID19", 
              "Confirmed_COVID19_Hospitalized_Age0to4",
              "Confirmed_COVID19_Hospitalized_Age5to17",
              "Conf_COVID19_Fully_Vaxxed_Total",
              "Conf_COVID19_Fully_Vaxxed_Age_5to17", 
              "Total_Num_Staffed_ICU_Beds",
              "ICU_Bed_Availability_current",
              "PICU_Bed_Available_current",
              "NICU_Bed_Available_current",
              "ICU_Bed_Shortage_anticipated", 
              "MedSurgical_Bed_Availability_current",
              "Baseline_Staffed_Bed_Capacity",
              "Ped_MedSurgical_Bed_Available_current",
              "MedSurgical_Bed_Shortage_anticipated",
              "RegulatoryID", 
              "Comment", 
              "Record_date", 
              "By_User", 
              "Resource_Type")



colnames(regionTables) <- colnames
colnames(regionTables)



# add column for Event_Snapshot_Datetime_Daily
regionDF <- dplyr:::mutate.data.frame(regionTables, Event_Snapshot_Datetime_Daily = "2023-02-01 10:00:00.000")

# check column classes
sapply(regionDF, class)


## MAKE SURE YOU DO THIS
# # converting certain columns to numeric
cols = c(2:10,12,13,14)
regionDF[, cols] %<>% lapply(function(x) as.numeric(as.character(x)))


# converting to datetime
regionDF$Record_date <- parse_date_time(regionDF$Record_date, c("mdY!_HM", "Y!md_HMS", "dmY!_HM", "db!Y!_HM"))
regionDF$Event_Snapshot_Datetime_Daily <- parse_date_time(regionDF$Event_Snapshot_Datetime_Daily, c("mdY!_HM", "Y!md_HMS", "dmY!_HM", "db!Y!_HM"))


# fixing preceding zeros and converting to character 
regionDF$RegulatoryID[regionDF$RegulatoryID == ""] <- NA

regionDF$RegulatoryID <- stringr::str_pad(regionDF$RegulatoryID, 6, pad = "0")

regionDF$RegulatoryID <- as.character(regionDF$RegulatoryID)

# # NAs to blanks
regionDF$Record_date[is.na(regionDF$Record_date)] <- parse_date_time("1900-01-01 05:00:00", c("Y!-%m-%d %H:%M-%S"), tz = "MST")

regionDF[is.na(regionDF)] <- 0

# # double-check column classes
sapply(regionDF, class)

glimpse(regionDF)



## connect to Event_snapshot_daily SQL DB ------ 

SQLusername <- Sys.getenv("sql_username")
SQLpassword <- Sys.getenv("sql_password")

#SQLconn <-odbcDriverConnect(connection="Driver={ODBC Driver 17 for SQL Server};server=CDPHESQD04;database=EMResourceCOVID19_Dev;trusted_connection=Yes;")
SQLconn <-odbcConnect("CDPHESQP04", uid=SQLusername, pwd=SQLpassword)
serverINFO <-odbcGetInfo(SQLconn) #get odbc db info

# output test 4 ------
print(serverINFO)

## inserting data -----
# distinct number of DailyRecordID in Event_snapshot_daily ==> should be same count as number of all records
sqlQuery(SQLconn, 
         "SELECT COUNT(DISTINCT DailyRecordID)
         FROM Event_snapshot_daily;") # 0 records | pulled on Mar 31 2020

## insert data into Event_snapshot_daily table 
# get data as string and sub ' for ''
EMResourceData <- paste0(apply(regionDF, 1, function(x) paste0("('", paste(gsub("'","''",x), collapse = "', '"), "')")), collapse = ", ")


saveData <- function(regionDF) {
  table <- "Event_snapshot_daily"
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES %s",
    table,
    paste(names(regionDF), collapse = ", "),
    paste(EMResourceData, collapse = "', '")
  )
  # Submit the update query and disconnect
  sqlQuery(SQLconn, query, stringsAsFactors = FALSE)
  # dbDisconnect(db)
}


saveData(regionDF)


# final records checks
# sqlQuery(SQLconn, "SELECT *
#   FROM [EMResourceCOVID19].[dbo].[Event_snapshot_daily]
#   ORDER BY Event_Snapshot_Datetime_Daily DESC;")
sqlQuery(SQLconn, 
         "SELECT COUNT(DISTINCT DailyRecordID)
         FROM Event_snapshot_daily;")



## Data elements going into Event_snapshot_weekly table -------------------------------------------------------------------------
# data organizing and cleanup ------
# grabbing only region tables (leaving summary table) and making new Region column

# lapply(regionTables_week, function(x) {
#   names(x)
# })

# renaming columns to match SQL table

# select specific columns from EMResource tables (this is done because some regions have additional data points beyond what we need)

regionTables_week <- my_df

regionTables_week <-   select(regionTables_week, "Resource", 
                              "Total # of ICU Capable Beds", 
                              # "Maximum Acute Care Beds",
                              "Total # of Acute Care Beds",
                              "Acute Care Inpatient Beds In-use",
                              "Total # of PICU Beds",
                              "Total # of NICU Beds",
                              "License #  (State will populate)" ,
                              "Comment",
                              "Last Update",
                              "By User",
                              "Resource Type" )

# renaming columns to match SQL table
colnames_week <- c("Resource_facility_name",
                   "Total_Num_ICU_Capable_Beds",
                   # "Maximum_Acute_Care_Beds",
                   "Total_Num_Acute_Care_Beds",
                   "Acute_Care_Inpatient_Beds_In_Use",
                   # "Vent_Medications", 
                   # "N95_Respirators",
                   # "Surgical_Procedure_Masks",
                   # "PAPR_and_supplies",
                   # "Eye_protection",
                   # "Gloves",
                   # "Gowns",
                   # "Facility_Testing_internal_processing",
                   # "Fatality_Cass_Loads",
                   # "Total_Num_Adult_Neg_Pressure_Iso_Beds",
                   # "Total_Num_Ped_Neg_Pressure_Iso_Beds",
                   # "Total_Num_ED_Beds",
                   "Total_Num_PICU_Beds",
                   "Total_Num_NICU_Beds",
                   # "Total_Num_Ped_MedicalSurgical_Beds",
                   # "Total_Num_LTC_Beds",
                   "RegulatoryID",
                   "Comment",
                   "Record_date",
                   "By_User",
                   "Resource_Type")

colnames(regionTables_week) <- colnames_week
colnames(regionTables_week)



# add column for Event_Snapshot_Datetime_Daily
regionDF_week <- dplyr:::mutate.data.frame(regionTables_week, Event_Snapshot_Weekly_datetime = "2023-02-01 10:00:00.000")


# check column classes
sapply(regionDF_week, class)

## MAKE SURE YOU DO THIS
# # converting certain columns to numeric
cols_week = c(2,3,4,5,6)
regionDF_week[, cols_week] %<>% 
  lapply(function(x) as.numeric(as.character(x)))
sapply(regionDF_week, class)

# converting to datetime
regionDF_week$Record_date <- parse_date_time(regionDF_week$Record_date, c("mdY!_HM", "Y!md_HMS", "dmY!_HM", "db!Y!_HM"))

regionDF_week$Event_Snapshot_Weekly_datetime <- parse_date_time(regionDF_week$Event_Snapshot_Weekly_datetime, c("mdY!_HM", "Y!md_HMS", "dmY!_HM", "db!Y!_HM"))

# fixing preceding zeros and converting to character 
regionDF_week$RegulatoryID[regionDF_week$RegulatoryID == ""] <- NA

regionDF_week$RegulatoryID <- stringr::str_pad(regionDF_week$RegulatoryID, 6, pad = "0")

regionDF_week$RegulatoryID <- as.character(regionDF_week$RegulatoryID)


# # NAs to blanks
regionDF_week$Record_date[is.na(regionDF_week$Record_date)] <- parse_date_time("1900-01-01 05:00:00", c("Y!-%m-%d %H:%M-%S"), tz = "MST")

#regionDF_week <- replace(regionDF_week, is.na(regionDF_week), "")

regionDF_week[is.na(regionDF_week)] <- 0

# # double-check column classes
sapply(regionDF_week, class)


glimpse(regionDF_week)

## connect to Event_snapshot_weekly SQL DB ------
SQLusername <- Sys.getenv("sql_username")
SQLpassword <- Sys.getenv("sql_password")
SQLconn <-odbcConnect("CDPHESQP04", uid=SQLusername, pwd=SQLpassword)
#SQLconn <-SQLconn <- odbcDriverConnect(connection="Driver={ODBC Driver 17 for SQL Server};server=CDPHESQD04;database=EMResourceCOVID19_Dev;trusted_connection=Yes;")
serverINFO <-odbcGetInfo(SQLconn) #get odbc db info

# output test 5 ------
print(serverINFO)

## explore DB
# # number of records in Event_snapshot_daily
# AllDailyEMResourceRecords <- sqlQuery(SQLconn, "SELECT * FROM Event_snapshot_daily;") # 0 records | pulled on Mar 31 2020

# distinct number of DailyRecordID in Event_snapshot_daily ==> should be same count as number of all records
sqlQuery(SQLconn, 
         "SELECT COUNT(DISTINCT WeeklyRecordID)
         FROM Event_snapshot_weekly;") # 0 records | pulled on Mar 31 2020


## insert data into Event_snapshot_daily table 
# get data as string and sub ' for ''
EMResourceData_week <- paste0(apply(regionDF_week, 1, function(x) paste0("('", paste(gsub("'","''",x), collapse = "', '"), "')")), collapse = ", ")



saveData <- function(regionDF_week) {
  table <- "Event_snapshot_weekly"
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES %s",
    table,
    paste(names(regionDF_week), collapse = ", "),
    paste(EMResourceData_week, collapse = "', '")
  )
  # Submit the update query and disconnect
  sqlQuery(SQLconn, query, stringsAsFactors = FALSE)
  # dbDisconnect(db)
}

saveData(regionDF_week)


# final records check
sqlQuery(SQLconn, 
         "SELECT COUNT(DISTINCT WeeklyRecordID)
         FROM Event_snapshot_weekly;")


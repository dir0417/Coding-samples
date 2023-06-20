#!/usr/bin/env Rscript

# Written by Adreanna Early, CDPHE
# adreanna.early@state.co.us
#
setwd("C:/Users/dair/Documents/R Scripts")
readRenviron("C:/Users/dair/Documents/R Scripts/.Renviron")

#output test 1 ------------
args = commandArgs(trailingOnly = TRUE)
message(sprintf("Hello %s", args[1L]))

# load libraries
pacman::p_load(
  pacman,
  RSelenium,
  lubridate,
  dplyr,
  rvest,
  RODBC,
  gsubfn,
  magrittr,
  stringr,
  wdman
)

# Chrome troubleshooting ----
# This block of code is to run if the chrome version won't work after you update to your version below
# getChromeDriverVersion <- function(versions = binman::list_versions("chromedriver")) {
#   if ( xfun::is_unix() ) {
#     chrome_driver_version <-
#       system2(command = ifelse(xfun::is_macos(),
#                                "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
#                                "google-chrome-stable"),
#               args = "--version",
#               stdout = TRUE,
#               stderr = TRUE) %>%
#       stringr::str_extract(pattern = "(?<=Chrome )(\\d+\\.){3}")
# 
#     ## on Windows a plattform-specific bug prevents us from calling the Google Chrome binary directly to get its version number
#     ## cf. https://bugs.chromium.org/p/chromium/issues/detail?id=158372
#   } else if ( xfun::is_windows() ) {
#     chrome_driver_version <-
#       system2(command = "wmic",
#               args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
#               stdout = TRUE,
#               stderr = TRUE) %>%
#       stringr::str_extract(pattern = "(?<=Version=)(\\d+\\.){3}")
# 
#   } else rlang::abort(message = "Your OS couldn't be determined (Linux, macOS, Windows) or is not supported!")
# 
#   # ... and determine most recent ChromeDriver version matching it
#   chrome_driver_version %>%
#     magrittr::extract(!is.na(.)) %>%
#     stringr::str_replace_all(pattern = "\\.",
#                              replacement = "\\\\.") %>%
#     paste0("^",  .) %>%
#     stringr::str_subset(string = dplyr::last(versions)) %>%
#     as.numeric_version() %>%
#     max() %>%
#     as.character()
# }
# 
# 
# rsStuff <- local({
#   rD <- NULL
#   driver <- function(port = 4567L, force = FALSE, verbose = FALSE) {
#     if (force)
#       rD <<- NULL
#     if (!is.null(rD))
#       return(rD)
#     versions <- binman::list_versions("chromedriver")
#     versions <- c(versions$mac64, getChromeDriverVersion(versions))
#     v <- length(versions) + 1
#     while (v && (is.null(rD) || inherits(rD, "condition"))) {
#       v <- v - 1  # Try each value
#       rD <<- tryCatch(rsDriver(verbose = verbose,
#                                port = port + sample(0:1000, 1),
#                                chromever=versions[v],
#                                geckover = NULL,
#                                phantomver = NULL), error = function(e) e,
#                       message = function(m) m)
#     }
#     rD
#   }
#   kill <- function() {
#     try(rD$server$stop())
#     rD <<- NULL
#   }
#   list(driver = driver, kill = kill)
# })
# 
# getrsDriver <- rsStuff$driver
# killrsDriver <- rsStuff$kill
# rm(rsStuff)
# 
# 
# remDr <- getrsDriver()$client
# 


# starting a Selenium chrome browser ---------

# define some log in variables
chromever <- Sys.getenv("chrome_version")
username <- Sys.getenv("juvare_username")
password <- Sys.getenv("juvare_password")

# connect to chrome
rD <- rsDriver(browser=c("chrome"), chromever=chromever, port = 4444L)
# check chrome versions:
binman::list_versions("chromedriver")
# if port blocked or need to use random port, use port = netstat::free_port() 

remDr <- rD[["client"]]

# if error:  rsDriver(check = FALSE)

# go to EMResource
remDr$navigate("https://login.juvare.com/home/bookmark/0oa166je2j3mvG8Zu4x7/2557?fromHome=true")

# logging in
userElem <- remDr$findElement(value = "//*[(@id = 'idp-discovery-username')]")
userElem$clearElement()
userElem$sendKeysToElement(list(username, key = "enter"))

#give it time to think
Sys.sleep(3)

passElem <- remDr$findElement(value = "//*[(@id = 'okta-signin-password')]")
passElem$clearElement()
passElem$sendKeysToElement(list(password, key = "enter"))

#give it time to think
Sys.sleep(3)

# end first check: select/run everything above this before moving on ----
# 
# region select only necessary if you have access to multiple EMResource regions/domains
#regionSelect <- remDr$findElement(value = "/html/body/div[2]/form/p[2]/input[5]")
#regionSelect$clickElement()

# getting to Event Reports -----
# selet 'reports'
remDr$navigate("https://emresource.juvare.com/EMSystem?uc=GENERAL&nextStep=menuItemList&nextStepDetail=REPORT")

# select 'event snapshot'
remDr$navigate("https://emresource.juvare.com/EMSystem?uc=REPORT&nextStep=ES_01")



# popup checker ----
# if successful (no popups found), a error message will result with "Selenium message:no such element:" THIS IS A GOOD THING
e <- simpleError("test error")

tryCatch(
  stop({
    noticeButton <- remDr$findElement(value = "//*[(@id = 'TB_iframeContent')]")
    noticeButton$clickElement()
  }), 
  error = function(e) e,
  finally =
    message("test done")
)

tryCatch(
  stop({
    startDate <- remDr$findElement(value = "//*[(@id = 'searchStartDate')]")
    startDate$clearElement()
    startDate$sendKeysToElement(list(format.Date(today()-1, "%m/%d/%Y"), key = "tab"))
  }),
  error = function(e) e, 
  finally = 
    message("test done")
)


# set date range for report ----
endDate <- remDr$findElement(value = "//*[(@id = 'searchEndDate')]")
endDate$clearElement()
endDate$sendKeysToElement(list(format.Date(today(), "%m/%d/%Y"), key = "tab")) 

reportFormat <- remDr$findElement(value = "//*[@id='mainContainer']/form/table/tbody/tr[4]/td[2]/input[@value='HTML']")
reportFormat$clickElement()

Event <- remDr$findElement(value = "//*[@id='mainContainer']/form/table/tbody/tr[5]/td[2]/div[15]/input[@value='661847']")
Event$clickElement()

Next <- remDr$findElement(value = "//*[@id='mainContainer']/form/table/tbody/tr[6]/td[2]/input[@value='Next']")
Next$clickElement()

# old code - input value changed
# EventSelect <- remDr$findElement(value = "//*[@id='mainContainer']/form/table/tbody/tr[2]/td/table/tbody/tr/td[1]/input[@value='662169']")
# EventSelect$clickElement()

# if there are 2 radio buttons:
# EventSelect <- remDr$findElement(value = "//*[@id='mainContainer']/form/table/tbody/tr[2]/td/table/tbody/tr[2]/td[1]/input")

# if there is 1 radio button
EventSelect <- remDr$findElement(value = "//*[@id='mainContainer']/form/table/tbody/tr[2]/td/table/tbody/tr/td[1]/input")

EventSelect$clickElement()

snapshotDate <- remDr$findElement(value = "//*[(@id = 'snapshotDate')]")
snapshotDate$clearElement()
snapshotDate$sendKeysToElement(list(format.Date(today(), "%m/%d/%Y"), key = "tab"))

snapshotHour <- remDr$findElement(value = "//*[@id='mainContainer']/form/table/tbody/tr[6]/td[2]/select/option[@value='10']")
snapshotHour$clickElement()

#snapshotHour <- remDr$findElement(value = "//*[@id='mainContainer']/form/table/tbody/tr[7]/td[2]/select/option[@value='10']")
#snapshotHour$clickElement()

generateReport <- remDr$findElement(value = "//*[@id='mainContainer']/form/table/tbody/tr[8]/td[2]/input[@value='Generate Report']")
generateReport$clickElement()

# switch to new tab
myswitch <- function (remDr, windowId) 
{
  qpath <- sprintf("%s/session/%s/window", remDr$serverURL, 
                   remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

# try to switch to new window
check_handle <- FALSE
count <- 0
while(!check_handle || count > 20){
  count <- count + 1
  windows_handles <- remDr$getWindowHandles()
  if(length(windows_handles) < 2){
    Sys.sleep(1)
  }else{
    check_handle <- TRUE
  }
}
myswitch(remDr, windows_handles[[2]])

#give it time to download
Sys.sleep(30)

# grab page html -----
page <- read_html(remDr$getPageSource()[[1]])

#output test 2 ------
print(page)


# close and stop the driver/browser -----------
remDr$close()
rD[["server"]]$stop()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

# if user forgets to stop server it will be garbage collected upon removal.
rm(rD)
# gc(rD)
gc()

# working with the data ---------------
# creating data frame
# 20 = License # and needs to be character to keep leading zeros
# rvest custom code doesn't work from command line so will need to add back preceding zeros another way
# allTables <- page %>% html_nodes(xpath = "//table") %>% html_table(header=TRUE, col_classes=list(`20`='character',`29`='character'))

allTables <- page %>%
  html_nodes(xpath = "//table") %>%
  html_table(header=TRUE)

# output test 3 ----------
print(allTables[[1]][1])

# data organizing and cleanup ------
# grabbing only region tables (leaving summary table) and making new Region column
regionTables <- lapply(allTables[c(1:18)], function(x) {
  mutate(x, Region = colnames(x)[1])
})

regionTables <- lapply(regionTables, function(x) {
  rename(x, "Resource_facility_name" = 1)
})


lapply(regionTables, function(x) {
  names(x)
})

## Data elements going into Event_snapshot_daily table -------------------------------------------------------------------------
# select specific columns from EMResource tables (this is done because some regions have additional data points beyond what we need)
regionTables <- lapply(regionTables, function(x) {
  select(x,"Resource_facility_name", 
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
         "Region")
})


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



regionTables <- 
  lapply(regionTables, setNames, colnames)

# binding all the region tables into one data.frame
regionDF <- as.data.frame(data.table::rbindlist(regionTables))

#remove Summary rows
regionDF <- filter(regionDF, Resource_facility_name != "Summary")

# extracting snapshot datetime from page header
snapshotDatetimeHeader <- page %>%
  html_node(xpath = "/html/body/h2") %>%
  html_text() %>%
  strapplyc("\\d+/\\d+/\\d+\\s+\\d+:\\d+", simplify = TRUE)

# add column for Event_Snapshot_Datetime_Daily
regionDF <- mutate(regionDF, Event_Snapshot_Datetime_Daily = snapshotDatetimeHeader)

# check column classes
sapply(regionDF, class)


## MAKE SURE YOU DO THIS
# # converting certain columns to numeric
cols = c(2:10, 12, 13, 14)
regionDF[, cols] %<>% lapply(function(x) as.numeric(as.character(x)))


# converting to datetime
regionDF$Record_date <- parse_date_time(regionDF$Record_date, c("mdY!_HM", "Y!md_HMS", "dmY!_HM", "db!Y!_HM"))
regionDF$Event_Snapshot_Datetime_Daily <- mdy_hm(regionDF$Event_Snapshot_Datetime_Daily)


# fixing preceding zeros and converting to character 
regionDF$RegulatoryID[regionDF$RegulatoryID == ""] <- NA

regionDF$RegulatoryID <- stringr::str_pad(regionDF$RegulatoryID, 6, pad = "0")

regionDF$RegulatoryID <- as.character(regionDF$RegulatoryID)

# # NAs to blanks
regionDF$Record_date[is.na(regionDF$Record_date)] <- parse_date_time("1900-01-01 05:00:00", c("Y!-%m-%d %H:%M-%S"), tz = "MST")

regionDF[is.na(regionDF)] <- ""

# # double-check column classes
sapply(regionDF, class)

glimpse(regionDF)



## connect to Event_snapshot_daily SQL DB ------ 

SQLusername <- Sys.getenv("sql_username")
SQLpassword <- Sys.getenv("sql_password")

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
regionTables_week <- lapply(allTables[c(1:18)], function(x) {
  mutate(x, Region = colnames(x)[1])
})

regionTables_week <- lapply(regionTables_week, function(x) {
  rename(x, "Resource_facility_name" = 1)
})

# lapply(regionTables_week, function(x) {
#   names(x)
# })

# select specific columns from EMResource tables (this is done because some regions have additional data points beyond what we need)
regionTables_week <- lapply(regionTables_week, function(x) {
  select(x, "Resource_facility_name", 
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
         "Region" )
})

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

regionTables_week <- 
  lapply(regionTables_week, setNames, colnames_week)


# binding all the region tables into one data.frame
regionDF_week <- as.data.frame(data.table::rbindlist(regionTables_week))


#remove Summary rows
regionDF_week <- filter(regionDF_week, Resource_facility_name != "Summary")

# extracting snapshot datetime from page header
snapshotDatetimeHeader_week <- page %>%
  html_node(xpath = "/html/body/h2") %>%
  html_text() %>%
  strapplyc("\\d+/\\d+/\\d+\\s+\\d+:\\d+", simplify = TRUE)

# add column for Event_Snapshot_Datetime_Daily
regionDF_week <- mutate(regionDF_week, Event_Snapshot_Weekly_datetime = snapshotDatetimeHeader_week)


# check column classes
sapply(regionDF_week, class)

## MAKE SURE YOU DO THIS
# # converting certain columns to numeric
cols_week = c(2,3,4,5,6)
regionDF_week[, cols_week] %<>% 
  lapply(function(x) as.numeric(as.character(x)))

# converting to datetime
regionDF_week$Record_date <- parse_date_time(regionDF_week$Record_date, c("mdY!_HM", "Y!md_HMS", "dmY!_HM", "db!Y!_HM"))
regionDF_week$Event_Snapshot_Weekly_datetime <- mdy_hm(regionDF_week$Event_Snapshot_Weekly_datetime)

# fixing preceding zeros and converting to character 
regionDF_week$RegulatoryID[regionDF_week$RegulatoryID == ""] <- NA

regionDF_week$RegulatoryID <- stringr::str_pad(regionDF_week$RegulatoryID, 6, pad = "0")

regionDF_week$RegulatoryID <- as.character(regionDF_week$RegulatoryID)


# # NAs to blanks
regionDF_week$Record_date[is.na(regionDF_week$Record_date)] <- parse_date_time("1900-01-01 05:00:00", c("Y!-%m-%d %H:%M-%S"), tz = "MST")

regionDF_week[is.na(regionDF_week)] <- ""

# # double-check column classes
sapply(regionDF_week, class)


glimpse(regionDF_week)

## connect to Event_snapshot_weekly SQL DB ------
SQLusername <- Sys.getenv("sql_username")
SQLpassword <- Sys.getenv("sql_password")

SQLconn <-odbcConnect("CDPHESQP04", uid=SQLusername, pwd=SQLpassword)
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
#!/usr/bin/env Rscript

# Written by Adreanna Early, CDPHE
# adreanna.early@state.co.us
#
setwd("C:/Users/dair/Documents/R Scripts")
readRenviron("C:/Users/dair/Documents/R Scripts/.Renviron")

#output test 1 ------------
args = commandArgs(trailingOnly = TRUE)
message(sprintf("You're working on Event_snapshot_GHICF"))
# Event_snapshot_GHICF
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


# starting a Selenium chrome browser ---------

# define some log in variables
chromever <- Sys.getenv("chrome_version")
username <- Sys.getenv("juvare_username")
password <- Sys.getenv("juvare_password")

# connect to chrome
rD <- rsDriver(browser=c("chrome"), chromever=chromever, port = 4444L)
# check chrome versions:
# binman::list_versions("chromedriver")
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

# region select only necessary if you have access to multiple EMResource regions/domains
#regionSelect <- remDr$findElement(value = "/html/body/div[2]/form/p[2]/input[5]")
#regionSelect$clickElement()

# getting to Event Reports -----
remDr$navigate("https://emresource.juvare.com/EMSystem?uc=GENERAL&nextStep=menuItemList&nextStepDetail=REPORT")

remDr$navigate("https://emresource.juvare.com/EMSystem?uc=REPORT&nextStep=ES_01")


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

endDate <- remDr$findElement(value = "//*[(@id = 'searchEndDate')]")
endDate$clearElement()
endDate$sendKeysToElement(list(format.Date(today(), "%m/%d/%Y"), key = "tab")) 

reportFormat <- remDr$findElement(value = "//*[@id='mainContainer']/form/table/tbody/tr[4]/td[2]/input[@value='HTML']")
reportFormat$clickElement()

Event <- remDr$findElement(value = "//*[@id='mainContainer']/form/table/tbody/tr[5]/td[2]/div[24]/input[@value='675218']")


Event$clickElement()

Next <- remDr$findElement(value = "//*[@id='mainContainer']/form/table/tbody/tr[6]/td[2]/input[@value='Next']")
Next$clickElement()

# if there are multiple radio buttons:
# EventSelect <- remDr$findElement(value = "//*[@id='mainContainer']/form/table/tbody/tr[2]/td/table/tbody/tr[3]/td[1]/input")
# 
# EventSelect <- remDr$findElement(value = "//*[@id='mainContainer']/form/table/tbody/tr[2]/td/table/tbody/tr[6]/td[1]/input")

# if there is 1 radio button
EventSelect <- remDr$findElement(value = "//*[@id='mainContainer']/form/table/tbody/tr[2]/td/table/tbody/tr/td[1]/input")

EventSelect$clickElement()

snapshotDate <- remDr$findElement(value = "//*[(@id = 'snapshotDate')]")
snapshotDate$clearElement()
snapshotDate$sendKeysToElement(list(format.Date(today(), "%m/%d/%Y"), key = "tab"))

snapshotHour <- remDr$findElement(value = "//*[@id='mainContainer']/form/table/tbody/tr[6]/td[2]/select/option[@value='10']")
snapshotHour$clickElement()

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
pageGHICF <- read_html(remDr$getPageSource()[[1]])

#output test 2 ------
print(pageGHICF)


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

allTables <- pageGHICF %>%
  html_nodes(xpath = "//table") %>%
  html_table(header=TRUE)

# output test 3 ----------
print(allTables[[1]][1])

# data organizing and cleanup ------
# grabbing only region tables (leaving summary table) and making new `Resource Type`  column
regionTables <- lapply(allTables[1], function(x) {
  mutate(x, `Resource Type` = colnames(x)[1])
})

regionTables <- lapply(regionTables, function(x) {
  rename(x, "Resource_facility_name" = 1)
})


lapply(regionTables, function(x) {
  names(x)
})

# write.csv(regionTables, "ghicf.csv")


## Data elements going into Event_snapshot_GHICF table -------------------------------------------------------------------------
# select specific columns from EMResource tables (this is done because some regions have additional data points beyond what we need)
regionTables <- lapply(regionTables, function(x) {
  select(x,
         "Resource_facility_name",
         # "Beds- Male Available",
         # "Beds- Female Available",
         #"CDPHE COVID-19 Vaccine Clinic Assistance",
         "COVID-19 Vaccine Provider",
         "COVID-19 Treatment Provider",
         "COVID-19 Vaccine Clinic Date",
         "IPC First and Last Name",
         "IPC Email Address",
         "IPC Training and/or Certifications",
         "Beds Available",
         "24 Hour Nursing Supervisor Phone Number",
         "Ability to Accept COVID-19+ Residents",
         # "Staffed COVID-19 + beds available",
         # "Acute Isolation Bed Avail.", 
         # "PPE",
         # "PPE (explain)",
         # "Supplies - general (explain)",	
         # "Staff",
         "Staffing Level",
         "Staffing Shortage (General)",
         # "Staffing Shortage Nurses",
         # "Staffing Shortage Direct Care/Caregivers",
         # "Staffing Shortage Aides",
         # "Staffing Shortage Other Staff",
         # "Visitation in Place",
         "Current Residents",
         "Resident Updated Bivalent Doses",
         "Up-to-Date Residents",
         "Not Up-to-Date Residents",
         "Not Vaccinated Residents",
         "Current Staff",
         "Up-to-Date Staff",
         "Not Up-to-Date Staff",
         "Not Vaccinated Staff",
         # "Current Residents Eligible for Booster",
         # "Current Vaccinated Residents",
         # "Additional Dose or Booster for Resident",
         # "Current Staff Eligible for Booster",
         # "Additional Dose or Booster for Staff",
         "Flu Vaccinated Staff",
         "Flu Vaccinated Residents",
         "License #  (State will populate)",
         # "Oxygen",
         # "Oxygen Concentrators",
         # "Transportation resources", 
         "Comment",
         "Last Update",
         "By User",
         "Resource Type")
})


# renaming columns to match SQL table
colnames <- c("Resource_facility_name",
              # "Beds_Male_Available",
              # "Beds_Female_Available",
              #"Vax_Clinic_Assistance",
              "[COVID-19_Vax_Provider]",
              "[COVID-19_Treat_Provider]",
              "[COVID-19_Vax_Clinic_Date]",
              "IPC_Name",
              "IPC_Email",
              "IPC_Train_Certs",
              "Beds_Available",
              "Nursing_Supervisor_Phone_Number_24Hour",
              "Ability_to_Accept_COVID19_Residents",
              # "Staffed_COVID19_Beds_Availabl",
              # "Acute_Isolation_Beds_Available", 
              # # "PPE",
              # "PPE_explain",
              # "Supplies_general_explain", 
              # "Staff",
              "Staffing_Level",
              "Staffing_Shortage_General",
              # "Staffing_Shortage_Nurses",
              # "Staffing_Shortage_Direct_Care_Caregivers",
              # "Staffing_Shortage_Aides",
              # "Staffing_Shortage_Other_Staff",
              # # "Visitation_in_Place",
              "Current_Total_Residents",
              "Bivalent_Doses_Residents",
              "Up_to_Date_Residents",
              "Not_Up_to_Date_Residents",
              "Not_Vaxxed_Residents",
              "Current_Staff",
              "Up_to_Date_Staff",
              "Not_Up_to_Date_Staff",
              "Not_Vaxxed_Staff",
              # "Current_Resident_Eligible_for_Booster",
              # "Current_Vaxxed_Residents",
              # "Addtl_Dose_Booster_Resident",
              # "Current_Staff_Eligible_for_Booster",
              # "Addtl_Dose_Booster_Staff",
              "Flu_Vaccinated_Staff",
              "Flu_Vaccinated_Residents",	
              "RegulatoryID", 
              # "Oxygen",
              # "Oxygen_Concentrators",
              # "Transportation_Resources", 
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
snapshotDatetimeHeader <- pageGHICF %>% 
  html_node(xpath = "/html/body/h2") %>% 
  html_text() %>% strapplyc("\\d+/\\d+/\\d+\\s+\\d+:\\d+", simplify = TRUE)

# add column for Event_Snapshot_Datetime_GHICF
regionDF <- mutate(regionDF, Event_Snapshot_Datetime_GHICF = snapshotDatetimeHeader)

# check column classes
sapply(regionDF, class)


## MAKE SURE YOU DO THIS
# # converting certain columns to numeric
cols = c(8,9,13:23)
regionDF[, cols] %<>% 
  lapply(function(x) as.numeric(as.character(x)))
regionDF[regionDF=="--"]<-NA

# converting to datetime
regionDF$Record_date <- parse_date_time(regionDF$Record_date, c("mdY!_HM", "Y!md_HMS", "dmY!_HM", "db!Y!_HM"))

# regionDF$First_Vax_Clinic_Date <- parse_date_time(regionDF$First_Vax_Clinic_Date, c("mdY!_HM", "Y!md_HMS", "dmY!_HM", "db!Y!_HM", "m/d/Y!"))
# 
# regionDF$Second_Vax_Clinic_Date <- parse_date_time(regionDF$Second_Vax_Clinic_Date, c("mdY!_HM", "Y!md_HMS", "dmY!_HM", "db!Y!_HM", "m/d/Y!"))
# 
# regionDF$Third_Vax_Clinic_Date <- parse_date_time(regionDF$Third_Vax_Clinic_Date, c("mdY!_HM", "Y!md_HMS", "dmY!_HM", "db!Y!_HM", "m/d/Y!"))

regionDF$Event_Snapshot_Datetime_GHICF <- mdy_hm(regionDF$Event_Snapshot_Datetime_GHICF)


# fixing preceeding zeros and converting to character 
regionDF$RegulatoryID[regionDF$RegulatoryID == ""] <- NA

regionDF$RegulatoryID <- str_pad(regionDF$RegulatoryID, 6, pad = "0")

regionDF$RegulatoryID <- as.character(regionDF$RegulatoryID)

# # NAs to blanks
regionDF$Record_date[is.na(regionDF$Record_date)] <- parse_date_time("1900-01-01 05:00:00", c("Y!-%m-%d %H:%M-%S"), tz = "MST")

# regionDF$First_Vax_Clinic_Date[is.na(regionDF$First_Vax_Clinic_Date)] <- parse_date_time("1900-01-01 05:00:00", c("Y!-%m-%d %H:%M-%S"), tz = "MST")
# 
# regionDF$Second_Vax_Clinic_Date[is.na(regionDF$Second_Vax_Clinic_Date)] <- parse_date_time("1900-01-01 05:00:00", c("Y!-%m-%d %H:%M-%S"), tz = "MST")
# 
# regionDF$Third_Vax_Clinic_Date[is.na(regionDF$Third_Vax_Clinic_Date)] <- parse_date_time("1900-01-01 05:00:00", c("Y!-%m-%d %H:%M-%S"), tz = "MST")

regionDF[is.na(regionDF)] <- ""

# # double-check column classes
sapply(regionDF, class)

glimpse(regionDF)

#write.csv(regionTables, "ghicf1.csv")
## connect to SQL DB ------

SQLusername <- Sys.getenv("sql_username")
SQLpassword <- Sys.getenv("sql_password")
# 
# SQLconn <-odbcConnect("EMResourceCOVID19", uid=SQLusername, pwd=SQLpassword)
#SQLconn <- odbcDriverConnect(connection="Driver={ODBC Driver 17 for SQL Server};server=CDPHESQD04;database=EMResourceCOVID19_Dev;trusted_connection=Yes;")
SQLconn <-odbcConnect("CDPHESQP04", uid=SQLusername, pwd=SQLpassword) 
serverINFO <-odbcGetInfo(SQLconn) #get odbc db info
# 
# # output test 4 ------
print(serverINFO)

## inserting data -----
# distinct number of DailyRecordID in Event_snapshot_GHICF ==> should be same count as number of all records
sqlQuery(SQLconn, 
         "SELECT COUNT(DISTINCT DailyRecordID)
         FROM Event_snapshot_GHICF;") # 0 records | pulled on Dec 1 2020

## insert data into Event_snapshot_GHICF table 
# get data as string and sub ' for ''
EMResourceData <- paste0(apply(regionDF, 1, function(x) paste0("('", paste(gsub("'","''",x), collapse = "', '"), "')")), collapse = ", ")

saveData <- function(regionDF) {
  table <- "Event_snapshot_GHICF"
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
#   FROM [EMResourceCOVID19].[dbo].[Event_snapshot_GHICF]
#   ORDER BY Event_Snapshot_Datetime_GHICF DESC;")
sqlQuery(SQLconn, 
         "SELECT COUNT(DISTINCT DailyRecordID)
         FROM Event_snapshot_GHICF;")

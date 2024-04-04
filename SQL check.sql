select 
sum(Conf_COVID19_Fully_Vaxxed_Age_5to17) Conf_COVID19_Fully_Vaxxed_Age_5to17,
sum(Conf_COVID19_Fully_Vaxxed_Total) Conf_COVID19_Fully_Vaxxed_Total,
sum(Confirmed_COVID19_Hospitalized_Age0to4) Confirmed_COVID19_Hospitalized_Age0to4,
sum(Confirmed_COVID19_Hospitalized_Age5to17) Confirmed_COVID19_Hospitalized_Age5to17,
sum(Num_Confirmed_COVID19) Num_Confirmed_COVID19,
sum(NICU_Bed_Available_current) NICU_Bed_Available_current,
sum(PICU_Bed_Available_current) PICU_Bed_Available_current,
sum(Ped_MedSurgical_Bed_Available_current) Ped_MedSurgical_Bed_Available_current,
sum(Total_Num_Staffed_ICU_Beds) Total_Num_Staffed_ICU_Beds,
sum(ICU_Bed_Availability_current) ICU_Bed_Availability_current,
sum(Baseline_Staffed_Bed_Capacity) Baseline_Staffed_Bed_Capacity,
sum(MedSurgical_Bed_Availability_current) MedSurgical_Bed_Availability_current
from Event_snapshot_daily
where Event_Snapshot_Datetime_Daily = '2023-02-27 10:00:00.000'


select
sum(Total_Num_NICU_Beds) Total_Num_NICU_Beds,
sum(Total_Num_PICU_Beds) Total_Num_PICU_Beds,
sum(Total_Num_ICU_Capable_Beds) Total_Num_ICU_Capable_Beds,
sum(Total_Num_Acute_Care_Beds) Total_Num_Acute_Care_Beds,
sum(Acute_Care_Inpatient_Beds_In_Use) Acute_Care_Inpatient_Beds_In_Use
from Event_snapshot_weekly
where Event_Snapshot_Weekly_datetime = '2023-02-27 10:00:00.000'




Select Event_snapshot_Datetime_Daily, Resource_facility_name, ICU_Bed_Availability_current from
Event_snapshot_daily
where Event_Snapshot_Datetime_Daily > '2023-02-26 10:00:00.000' 
Order by Resource_facility_name


Select Event_snapshot_Datetime_Daily, Resource_facility_name, ICU_Bed_Availability_current from
Event_snapshot_daily
where Event_Snapshot_Datetime_Daily > '2023-02-28 10:00:00.000' 
Order by Resource_facility_name


Select Event_snapshot_Datetime_Daily, Resource_facility_name, MedSurgical_Bed_Availability_current from
Event_snapshot_daily
where Event_Snapshot_Datetime_Daily > '2022-06-22 10:00:00.000'
and Resource_facility_name like 'St Anthony North Hospital%'


Select Event_snapshot_Datetime_Daily, Resource_facility_name, MedSurgical_Bed_Availability_current from
Event_snapshot_daily
where Event_Snapshot_Datetime_Daily > '2022-12-07 10:00:00.000'
and Resource_facility_name like 'Sky Ridge Medical Center%'


Select Event_snapshot_Datetime_Daily, Resource_facility_name, MedSurgical_Bed_Availability_current from
Event_snapshot_daily
where Event_Snapshot_Datetime_Daily > '2022-12-21 10:00:00.000'
and Resource_facility_name like 'UCHealth Memorial Hospital Central%'

Select Event_snapshot_Datetime_Daily, Resource_facility_name, MedSurgical_Bed_Availability_current from
Event_snapshot_daily
where Event_Snapshot_Datetime_Daily > '2022-12-21 10:00:00.000'
and Resource_facility_name like 'Centura% Anthony Hospital%'



Select Event_snapshot_Datetime_Daily, Resource_facility_name, ICU_Bed_Availability_current from
Event_snapshot_daily
where Event_Snapshot_Datetime_Daily > '2022-12-07 10:00:00.000'
and Resource_facility_name like 'Centura% Mercy Hospital%'

Select Event_snapshot_Datetime_Daily, Resource_facility_name, ICU_Bed_Availability_current from
Event_snapshot_daily
where Event_Snapshot_Datetime_Daily > '2022-12-07 10:00:00.000'
and Resource_facility_name like 'Children% Hospital Colorado% I-RPTC'


Select Event_snapshot_Datetime_Daily, Resource_facility_name, MedSurgical_Bed_Availability_current from
Event_snapshot_daily
where Event_Snapshot_Datetime_Daily > '2022-12-07 10:00:00.000'
and Resource_facility_name like 'Centura%Porter Adventist%'



Select Event_snapshot_Datetime_Daily, Resource_facility_name, ICU_Bed_Availability_current from
Event_snapshot_daily
where Event_Snapshot_Datetime_Daily > '2022-12-13 10:00:00.000'
and Resource_facility_name like 'St Anthony North Hospital%'

Select Event_snapshot_Datetime_Daily, Resource_facility_name, MedSurgical_Bed_Availability_current from
Event_snapshot_daily
where Event_Snapshot_Datetime_Daily > '2022-12-13 10:00:00.000'
and Resource_facility_name like 'St Anthony North Hospital%'

Select
Sum(Beds_Available) Beds_Available,
sum(Current_Total_Residents) Current_Total_Residents,
sum(Omicron_Bivalent_Doses_Residents) Omicron_Bivalent_Doses_Residents,
sum(Up_to_Date_Residents) Up_to_Date_Residents,
sum(Not_Up_to_Date_Residents) Not_Up_to_Date_Residents,
sum(Not_Vaxxed_Residents) Not_Vaxxed_Residents,
sum(Current_Staff) Current_Staff,
sum(Up_to_Date_Staff) Up_to_Date_Staff,
sum(Not_Up_to_Date_Staff) Not_Up_to_Date_Staff,
sum(Not_Vaxxed_Staff) Not_Vaxxed_Staff,
sum(Flu_Vaccinated_Residents) Flu_Vaccinated_Residents,
sum(Flu_Vaccinated_Staff) Flu_Vaccinated_Staff
from Event_snapshot_ALR
where Event_Snapshot_Datetime_ALR = '2023-03-07 10:00:00.000'

Select
Sum(Beds_Available) Beds_Available,
sum(Current_Total_Residents) Current_Total_Residents,
sum(Omicron_Bivalent_Doses_Residents) Omicron_Bivalent_Doses_Residents,
sum(Up_to_Date_Residents) Up_to_Date_Residents,
sum(Not_Up_to_Date_Residents) Not_Up_to_Date_Residents,
sum(Not_Vaxxed_Residents) Not_Vaxxed_Residents,
sum(Current_Staff) Current_Staff,
sum(Up_to_Date_Staff) Up_to_Date_Staff,
sum(Not_Up_to_Date_Staff) Not_Up_to_Date_Staff,
sum(Not_Vaxxed_Staff) Not_Vaxxed_Staff,
sum(Flu_Vaccinated_Residents) Flu_Vaccinated_Residents,
sum(Flu_Vaccinated_Staff) Flu_Vaccinated_Staff
from Event_snapshot_GHICF
where Event_Snapshot_Datetime_GHICF = '2023-03-07 10:00:00.000'

Select
Sum(Beds_Available) Beds_Available,
sum(Current_Total_Residents) Current_Total_Residents,
sum(Omicron_Bivalent_Doses_Residents) Omicron_Bivalent_Doses_Residents,
sum(Up_to_Date_Residents) Up_to_Date_Residents,
sum(Not_Up_to_Date_Residents) Not_Up_to_Date_Residents,
sum(Not_Vaxxed_Residents) Not_Vaxxed_Residents,
sum(Current_Staff) Current_Staff,
sum(Up_to_Date_Staff) Up_to_Date_Staff,
sum(Not_Up_to_Date_Staff) Not_Up_to_Date_Staff,
sum(Not_Vaxxed_Staff) Not_Vaxxed_Staff,
sum(Flu_Vaccinated_Residents) Flu_Vaccinated_Residents,
sum(Flu_Vaccinated_Staff) Flu_Vaccinated_Staff
from Event_snapshot_SNF
where Event_Snapshot_Datetime_SNF = '2023-03-07 10:00:00.000'


Update Event_snapshot_daily
set Confirmed_COVID19_Hospitalized_Age0to4 = 6
where Event_Snapshot_Datetime_Daily = '2023-03-23 10:00:00.000'
and Resource_facility_name like 'Children% Hospital Colorado% I-RPTC'


select Event_Snapshot_Datetime_Daily, Import_Created_By, count(*) from Event_snapshot_daily
group by Event_Snapshot_Datetime_Daily, Import_Created_By
order by Event_Snapshot_Datetime_Daily desc

select Event_Snapshot_Weekly_datetime, Import_Created_By, count(*) from Event_snapshot_weekly
group by Event_Snapshot_Weekly_datetime, Import_Created_By
order by Event_Snapshot_Weekly_datetime desc



Select Event_Snapshot_Weekly_datetime, Resource_facility_name, Total_Num_PICU_Beds 
from Event_snapshot_weekly
where Event_Snapshot_Weekly_datetime > '2022-06-22 10:00:00.000'
and Resource_facility_name like 'Children% Hospital Colorado Spgs%'



Select Event_snapshot_Datetime_Daily, Resource_facility_name, PICU_Bed_Available_current
from Event_snapshot_daily
where Event_Snapshot_Datetime_Daily > '2022-06-22 10:00:00.000'
and Resource_facility_name like 'Children% Hospital Colorado Spgs%'
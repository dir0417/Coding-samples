Select Event_snapshot_Datetime_Daily, Resource_facility_name, Ped_MedSurgical_Bed_Available_current from
Event_snapshot_daily
where Event_Snapshot_Datetime_Daily > '2022-06-22 10:00:00.000'
and  like 'St Anthony North Hospital%'


Select Event_snapshot_Datetime_Daily, Resource_facility_name, MedSurgical_Bed_Availability_current from
Event_snapshot_daily
where Event_Snapshot_Datetime_Daily > '2022-06-22 10:00:00.000'
and Resource_facility_name like 'UCHealth Poudre Valley Hospital%'




select Event_Snapshot_Datetime_Daily, Import_Created_By, count(*) from Event_snapshot_daily
group by Event_Snapshot_Datetime_Daily, Import_Created_By
order by Event_Snapshot_Datetime_Daily desc

select Event_Snapshot_Weekly_datetime, Import_Created_By, count(*) from Event_snapshot_weekly
group by Event_Snapshot_Weekly_datetime, Import_Created_By
order by Event_Snapshot_Weekly_datetime desc
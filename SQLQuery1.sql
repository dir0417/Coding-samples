Select Event_snapshot_Datetime_Daily, Resource_facility_name, MedSurgical_Bed_Availability_current from
Event_snapshot_daily
where Event_Snapshot_Datetime_Daily > '2022-06-22 10:00:00.000'
and Resource_facility_name like 'St Anthony North Hospital%'
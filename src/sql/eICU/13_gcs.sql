drop table if exists `db_name.icu_elos.gcs`;
create table `db_name.icu_elos.gcs` as

-- This query extracts the Glasgow Coma Scale, a measure of neurological function.
-- The query has a few special rules:
--    (1) The verbal component can be set to 0 if the patient is ventilated.
--    This is corrected to 5 - the overall GCS is set to 15 in these cases.
--    (2) Often only one of three components is documented. The other components
--    are carried forward.

-- Note:
--  The GCS for sedated patients is defaulted to 15 in this code.
--  This is in line with how the data is meant to be collected.
--  e.g., from the SAPS II publication:
--    For sedated patients, the Glasgow Coma Score before sedation was used.
--    This was asncrtained either from interviewing the physician who ordered the sedation,
--    or by reviewing the patient's medical record.

with base as (
  select
  nc.patientunitstayid
  , nc.nursingchartoffset

  -- pivot each value into its own column
  , min(case
  when nursingchartcelltypevallabel = 'Glasgow coma score'
  and nursingchartcelltypevalname = 'GCS Total'
  and REGEXP_CONTAINS(nursingchartvalue, '^[-]?[0-9]+[.]?[0-9]*$')
  and nursingchartvalue not in ('-','.')
  then cast(nursingchartvalue as numeric)
  when nursingchartcelltypevallabel = 'Score (Glasgow Coma Scale)'
  and nursingchartcelltypevalname = 'Value'
  and REGEXP_CONTAINS(nursingchartvalue, '^[-]?[0-9]+[.]?[0-9]*$')
  and nursingchartvalue not in ('-','.')
  then cast(nursingchartvalue as numeric)
  else null end)
  as gcs_total

  , min(case
  when nursingchartcelltypevallabel = 'Glasgow coma score'
  and nursingchartcelltypevalname = 'Motor'
  and REGEXP_CONTAINS(nursingchartvalue, '^[-]?[0-9]+[.]?[0-9]*$')
  and nursingchartvalue not in ('-','.')
  then cast(nursingchartvalue as numeric)
  else null end)
  as gcsmotor

  , min(case
  when nursingchartcelltypevallabel = 'Glasgow coma score'
  and nursingchartcelltypevalname = 'Verbal'
  and REGEXP_CONTAINS(nursingchartvalue, '^[-]?[0-9]+[.]?[0-9]*$')
  and nursingchartvalue not in ('-','.')
  then cast(nursingchartvalue as numeric)
  else null end)
  as gcsverbal
  
  , min(case
  when nursingchartcelltypevallabel = 'Glasgow coma score'
  and nursingchartcelltypevalname = 'Eyes'
  and REGEXP_CONTAINS(nursingchartvalue, '^[-]?[0-9]+[.]?[0-9]*$')
  and nursingchartvalue not in ('-','.')
  then cast(nursingchartvalue as numeric)
  else null end)
  as gcseyes

 -- convert the data into a number, reserving a value of 0 for ET/Trach
  , max(case
-- endotrach/vent is assigned a value of 0
-- flag it here to later parse specially
  when nursingchartvalue = 'Unable to score due to medication' then 1
  else 0 end)
  as endotrachflag
  , ROW_NUMBER ()
  OVER (PARTITION BY nc.patientunitstayid ORDER BY nc.nursingchartoffset ASC) as rn

  from `physionet-data.eicu_crd.nursecharting` nc

  -- speed up by only looking at a subset of charted data
  where nursingchartcelltypecat in
  ('Scores', 'Other Vital Signs and Infusions')
  group by patientunitstayid, nursingchartoffset
  )

, gcs as (
  select b.*
  , b2.gcs_total as gcs_totalprev
  , b2.gcsverbal as gcsverbalprev
  , b2.gcsmotor as gcsmotorprev
  , b2.gcseyes as gcseyesprev
  -- Calculate GCS, factoring in special case when they are intubated and prev vals
  -- note that the coalesce are used to implement the following if:
  --  if current value exists, use it
  --  if previous value exists, use it
  --  otherwise, default to normal
  , case
      -- replace GCS during sedation with 15
      when b.endotrachflag = 1
        then 15
      when b.endotrachflag = 0 and b2.endotrachflag = 0
        then 15
      -- if previously they were intub, but they aren't now, do not use previous GCS values
      when b2.endotrachflag = 0
        then
            coalesce(b.gcsmotor,6)
          + coalesce(b.gcsverbal,5)
          + coalesce(b.gcseyes,4)
      -- otherwise, add up score normally, imputing previous value if none available at current time
      else
            coalesce(b.gcsmotor,coalesce(b2.gcsmotor,6))
          + coalesce(b.gcsverbal,coalesce(b2.gcsverbal,5))
          + coalesce(b.gcseyes,coalesce(b2.gcseyes,4))
      end as gcs_corr

  from base b
  -- join to itself within 6 hours to get previous value
  left join base b2
    on b.patientunitstayid = b2.patientunitstayid
    and b.rn = b2.rn+1
    and b2.nursingchartoffset > b.nursingchartoffset+360 -- 6 hours interval == 360 minutes 
)

-- combine components with previous within 6 hours
-- filter down to cohort which is not excluded
-- truncate nursingchartoffset to the hour
, gcs_stg as
(
  select
  gs.patientunitstayid, gs.nursingchartoffset
  , gcs_corr
  , gcs_total
  , coalesce(gcsmotor, gcsmotorprev) as gcsmotor
  , coalesce(gcsverbal, gcsverbalprev) as gcsverbal
  , coalesce(gcseyes, gcseyesprev) as gcseyes
  , case when coalesce(gcsmotor, gcsmotorprev) is null then 0 else 1 end
  + case when coalesce(gcsverbal, gcsverbalprev) is null then 0 else 1 end
  + case when coalesce(gcseyes, gcseyesprev) is null then 0 else 1 end
    as components_measured
  , endotrachflag
  from gcs gs
)

select
  gs.patientunitstayid, gs.nursingchartoffset
  , gcs_total
  , gcs_corr AS gcs_total_corr
  , gcsmotor AS gcs_motor
  , gcsverbal AS gcs_verbal
  , gcseyes AS gcs_eyes
  , endotrachflag AS gcs_unable
from gcs_stg gs

ORDER BY patientunitstayid, nursingchartoffset ASC
;

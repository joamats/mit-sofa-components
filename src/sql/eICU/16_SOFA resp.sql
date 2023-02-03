DROP TABLE IF EXISTS `db_name.icu_elos.sofa_resp` ;
CREATE TABLE `db_name.icu_elos.sofa_resp` AS

WITH vent_table AS (
-- Ventiliation -> Note: This information is stored in multiple tables
-- Create unified vent_table first
-- Queries are duplicated -> day 1 and day 7, where applicable (excluding APACHE tables)
    
    -- 1: use pat table as base 
    SELECT pat.patientunitstayid, vent1_d1, vent1_d7, vent2_d1, vent3_d1, vent4_d1, vent4_d7
    FROM `physionet-data.eicu_crd.patient` AS pat

    -- 2: ventilation_events table -> day 1
      LEFT JOIN(
        SELECT vent_events_d1.patientunitstayid
        , CASE WHEN ( MAX(event) = "mechvent start" OR MAX(event) = "mechvent end") THEN 1
        ELSE NULL
        END as vent1_d1
        
        FROM `physionet-data.eicu_crd_derived.ventilation_events` AS vent_events_d1
        
        -- ventilation_events has no offset information -> fetch info from patient table by inner join
        INNER JOIN (
        SELECT patientunitstayid, hospitalAdmitOffset
        FROM `physionet-data.eicu_crd.patient`
        WHERE hospitalAdmitOffset BETWEEN 0 AND (24*60) -- convert from minutes to 1 day
        ) AS innerjoin
        ON innerjoin.patientunitstayid = vent_events_d1.patientunitstayid
        
        GROUP BY patientunitstayid
  )
  AS vent_events_d1
  ON vent_events_d1.patientunitstayid = pat.patientunitstayid 

    -- 2: ventilation_events table -> day 7
      LEFT JOIN(
        SELECT vent_events_d7.patientunitstayid
        , CASE WHEN ( MAX(event) = "mechvent start" OR MAX(event) = "mechvent end") THEN 1
        ELSE NULL
        END as vent1_d7
        
        FROM `physionet-data.eicu_crd_derived.ventilation_events` AS vent_events_d7
        
        -- ventilation_events has no offset information -> fetch info from patient table by inner join
        INNER JOIN (
        SELECT patientunitstayid, hospitalAdmitOffset
        FROM `physionet-data.eicu_crd.patient`
        WHERE hospitalAdmitOffset BETWEEN (6*24*60) AND (7*24*60) -- convert from minutes to day 7
        ) AS innerjoin
        ON innerjoin.patientunitstayid = vent_events_d7.patientunitstayid
        
        GROUP BY patientunitstayid
  )
  AS vent_events_d7
  ON vent_events_d7.patientunitstayid = pat.patientunitstayid 

    -- 3: apacheapsvar table -> only day 1 available
    LEFT JOIN(
      SELECT patientunitstayid, intubated as vent2_d1
      FROM `physionet-data.eicu_crd.apacheapsvar` AS apacheapsvar
      WHERE (intubated = 1)
  )
  AS apacheapsvar
  ON apacheapsvar.patientunitstayid = pat.patientunitstayid 
  
    -- 4: apachepredvar table -> only day 1 available
    LEFT JOIN(
      SELECT patientunitstayid, oobintubday1 as vent3_d1
      FROM `physionet-data.eicu_crd.apachepredvar` AS apachepredvar
      WHERE (oobintubday1 = 1)
  )
  AS apachepredvar
  ON apachepredvar.patientunitstayid = pat.patientunitstayid 
    
    
    -- 5: respiratory care table -> day 1
    LEFT JOIN(
      SELECT patientunitstayid, 
      CASE
      WHEN COUNT(airwaytype) >= 1 THEN 1
      WHEN COUNT(airwaysize) >= 1 THEN 1
      WHEN COUNT(airwayposition) >= 1 THEN 1
      WHEN COUNT(cuffpressure) >= 1 THEN 1
      WHEN COUNT(setapneatv) >= 1 THEN 1
      ELSE NULL
      END AS vent4_d1
      FROM `physionet-data.eicu_crd.respiratorycare` AS resp_care_d1
      WHERE (respCareStatusOffset > 0 AND respCareStatusOffset <= 1440) -- day 1
      GROUP BY patientunitstayid
  )
  AS resp_care_d1
  ON resp_care_d1.patientunitstayid = pat.patientunitstayid 

    -- 5: respiratory care table -> day 7
    LEFT JOIN(
      SELECT patientunitstayid, 
      CASE
      WHEN COUNT(airwaytype) >= 1 THEN 1
      WHEN COUNT(airwaysize) >= 1 THEN 1
      WHEN COUNT(airwayposition) >= 1 THEN 1
      WHEN COUNT(cuffpressure) >= 1 THEN 1
      WHEN COUNT(setapneatv) >= 1 THEN 1
      ELSE NULL
      END AS vent4_d7
      FROM `physionet-data.eicu_crd.respiratorycare` AS resp_care_d7
      WHERE respCareStatusOffset BETWEEN (60*6*24) AND (60*7*24) -- day 7
      GROUP BY patientunitstayid
  )
  AS resp_care_d7
  ON resp_care_d7.patientunitstayid = pat.patientunitstayid 

)

, o2_table AS (

-- 1: use pat table as base 
SELECT pat.patientunitstayid, fio2_aps_d1, fio2_pred_d1, fio2_bg_d1, pao2_bg_d1, fio2_bg_d7, pao2_bg_d7
FROM `physionet-data.eicu_crd.patient` AS pat

-- apacheapsvar table -> only day 1 available
    LEFT JOIN(
      SELECT patientunitstayid as apsvar_id, fio2 as fio2_aps_d1
      FROM `physionet-data.eicu_crd.apacheapsvar`
      WHERE fio2 > 20
  )
AS apacheapsvar
ON apacheapsvar.apsvar_id = pat.patientunitstayid 

-- apachepredvar table -> only day 1 available
    LEFT JOIN(
      SELECT patientunitstayid as predvar_id, fio2 as fio2_pred_d1
      FROM `physionet-data.eicu_crd.apachepredvar` 
      WHERE fio2 > 20
  )
AS apachepredvar
ON apachepredvar.predvar_id = pat.patientunitstayid 

-- SaO2 information

-- pivoted bg table -> day 1
    LEFT JOIN(
      SELECT patientunitstayid as bg_id
      , AVG(fio2) AS fio2_bg_d1
      , AVG(pao2) AS pao2_bg_d1
      FROM `physionet-data.eicu_crd_derived.pivoted_bg` AS bg_d1
      WHERE (chartoffset BETWEEN 0 AND 1440) -- day 1 
      OR (fio2 between 20 AND 100)
      OR (pao2 between 0 AND 700)
      GROUP BY patientunitstayid
  )
AS bg_d1
ON bg_d1.bg_id = pat.patientunitstayid 

-- pivoted bg table -> day 7
    LEFT JOIN(
      SELECT patientunitstayid as bg_id
      , AVG(fio2) AS fio2_bg_d7
      , AVG(pao2) AS pao2_bg_d7
      FROM `physionet-data.eicu_crd_derived.pivoted_bg` AS bg_d7
      WHERE (chartoffset BETWEEN (60*24*6) AND (60*24*7)) -- day 17
      OR (fio2 between 20 AND 100)
      OR (pao2 between 0 AND 700)
      GROUP BY patientunitstayid
  )
AS bg_d7
ON bg_d7.bg_id = pat.patientunitstayid 

)

  -- Merge temporary tables together
, merged_tables AS (

SELECT patientunitstayid, fio2_d1, fio2_d7, 
pao2_bg_d1, pao2_bg_d7, 
pao2_bg_d1/fio2_d1 AS pf_ratio_d1,
pao2_bg_d7/fio2_d7 AS pf_ratio_d7
, COALESCE(vent1_d1,vent2_d1, vent3_d1, vent4_d1) AS vent_flag_d1
, COALESCE(vent1_d7, vent4_d7) AS vent_flag_d7
FROM vent_table

LEFT JOIN(
      SELECT patientunitstayid AS o2_id
      , COALESCE(fio2_aps_d1, fio2_pred_d1, fio2_bg_d1) AS fio2_d1
      , pao2_bg_d1
      , pao2_bg_d7
      , fio2_bg_d7 AS fio2_d7
      FROM o2_table
  )
AS o2_table
ON o2_table.o2_id = vent_table.patientunitstayid 
)

SELECT *,
CASE WHEN pf_ratio_d1 < 100 AND vent_flag_d1 = 1 THEN 4
WHEN pf_ratio_d1 BETWEEN 100 AND 199 AND vent_flag_d1 = 1 THEN 3
WHEN pf_ratio_d1 BETWEEN 200 AND 299 THEN 2
WHEN pf_ratio_d1 BETWEEN 300 AND 399 THEN 1
ELSE 0 
END AS sofa_resp_d1

, CASE WHEN pf_ratio_d7 < 100 AND vent_flag_d7 = 1 THEN 4
WHEN pf_ratio_d7 BETWEEN 100 AND 199 AND vent_flag_d7 = 1 THEN 3
WHEN pf_ratio_d7 BETWEEN 200 AND 299 THEN 2
WHEN pf_ratio_d7 BETWEEN 300 AND 399 THEN 1
ELSE 0 
END AS sofa_resp_d7
FROM merged_tables


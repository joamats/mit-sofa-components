DROP TABLE IF EXISTS `db_name.my_eICU.aux_treatments`;
CREATE TABLE `db_name.my_eICU.aux_treatments` AS

SELECT 
  icu.patientunitstayid
  , vent_1
  , vent_2
  , vent_3
  , vent_4
  , vent_5
  , vent_6
  , rrt_1
  , pressor_1
  , pressor_2
  , pressor_3
  , pressor_4

FROM `db_name.eicu_crd_derived.icustay_detail` as icu


-- ventilation events
LEFT JOIN(
    SELECT 
        patientunitstayid
      , COUNT(event) as vent_1
    FROM `physionet-data.eicu_crd_derived.ventilation_events` 
    WHERE (event = "mechvent start" OR event = "mechvent end")
    GROUP BY patientunitstayid
)
AS v1
ON v1.patientunitstayid= icu.patientunitstayid

-- apache aps vars
LEFT JOIN(
    SELECT 
      patientunitstayid
      , COUNT(intubated) as vent_2
    FROM `physionet-data.eicu_crd.apacheapsvar`
    WHERE intubated = 1
    GROUP BY patientunitstayid
)
AS v2
ON v2.patientunitstayid= icu.patientunitstayid

-- apache pred vars
LEFT JOIN(
    SELECT 
      patientunitstayid
    , COUNT(oobintubday1) as vent_3
    FROM `physionet-data.eicu_crd.apachepredvar`
    WHERE oobintubday1 = 1
    GROUP BY patientunitstayid
)
AS v3
ON v3.patientunitstayid= icu.patientunitstayid

-- debug vent tags
LEFT JOIN(
    SELECT 
      patientunitstayid
    , COUNT(intubated) as vent_4
    , COUNT(extubated) as vent_5
    FROM `physionet-data.eicu_crd_derived.debug_vent_tags`
    WHERE intubated = 1 OR extubated = 1
    GROUP BY patientunitstayid
)
AS v45
ON v45.patientunitstayid= icu.patientunitstayid

-- respiratory care table
LEFT JOIN(
    SELECT 
      patientunitstayid
    , CASE
        WHEN COUNT(airwaytype) >= 1 THEN 1
        WHEN COUNT(airwaysize) >= 1 THEN 1
        WHEN COUNT(airwayposition) >= 1 THEN 1
        WHEN COUNT(cuffpressure) >= 1 THEN 1
        WHEN COUNT(setapneatv) >= 1 THEN 1
        ELSE NULL
      END AS vent_6

  FROM `physionet-data.eicu_crd.respiratorycare`
  GROUP BY patientunitstayid
)
AS v6
ON v6.patientunitstayid= icu.patientunitstayid

-- treatment table to get RRT
LEFT JOIN(
    SELECT 
      patientunitstayid
    , COUNT(treatmentstring) as rrt_1
    FROM `physionet-data.eicu_crd.treatment` 
    WHERE (
      treatmentstring LIKE "renal|dialysis|C%" OR 
      treatmentstring LIKE "renal|dialysis|hemodialysis|emergent%" OR 
      treatmentstring LIKE "renal|dialysis|hemodialysis|for acute renal failure" OR
      treatmentstring LIKE "renal|dialysis|hemodialysis"
      )
    GROUP BY patientunitstayid
)
AS rrt1
ON rrt1.patientunitstayid= icu.patientunitstayid

-- pivoted infusions table to get vasopressors
LEFT JOIN(
    SELECT 
      patientunitstayid
    , CASE
        WHEN COUNT(dopamine) >= 1 THEN 1
        WHEN COUNT(dobutamine) >= 1 THEN 1
        WHEN COUNT(norepinephrine) >= 1 THEN 1
        WHEN COUNT(phenylephrine) >= 1 THEN 1
        WHEN COUNT(epinephrine) >= 1 THEN 1
        WHEN COUNT(vasopressin) >= 1 THEN 1
        WHEN COUNT(milrinone) >= 1 THEN 1
        ELSE NULL
      END AS pressor_1  

  FROM `physionet-data.eicu_crd_derived.pivoted_infusion`
  GROUP BY patientunitstayid
)
AS vp1
ON vp1.patientunitstayid= icu.patientunitstayid

-- infusions table to get vasopressors
LEFT JOIN(
    SELECT 
        patientunitstayid
      , COUNT(drugname) as pressor_2
    FROM `physionet-data.eicu_crd.infusiondrug`
    WHERE(
         LOWER(drugname) LIKE '%dopamine%' 
      OR LOWER(drugname) LIKE '%dobutamine%'
      OR LOWER(drugname) LIKE '%norepinephrine%'
      OR LOWER(drugname) LIKE '%phenylephrine%'
      OR LOWER(drugname) LIKE '%epinephrine%'
      OR LOWER(drugname) LIKE '%vasopressin%'
      OR LOWER(drugname) LIKE '%milrinone%'
      OR LOWER(drugname) LIKE '%dobutrex%' 
      OR LOWER(drugname) LIKE '%neo synephrine%'
      OR LOWER(drugname) LIKE '%neo-synephrine%'
      OR LOWER(drugname) LIKE '%neosynephrine%' 
      OR LOWER(drugname) LIKE '%neosynsprine%'
    )
    GROUP BY patientunitstayid
)
AS vp2
ON vp2.patientunitstayid= icu.patientunitstayid

-- medication
LEFT JOIN(
    SELECT  
        patientunitstayid
      , COUNT(drugname) as pressor_3
    FROM `physionet-data.eicu_crd.medication`
    WHERE(
        LOWER(drugname) LIKE '%dopamine%' 
      OR LOWER(drugname) LIKE '%dobutamine%'
      OR LOWER(drugname) LIKE '%norepinephrine%' 
      OR LOWER(drugname) LIKE '%phenylephrine%'
      OR LOWER(drugname) LIKE '%epinephrine%'
      OR LOWER(drugname) LIKE '%vasopressin%'
      OR LOWER(drugname) LIKE '%milrinone%'
      OR LOWER(drugname) LIKE '%dobutrex%' 
      OR LOWER(drugname) LIKE '%neo synephrine%' 
      OR LOWER(drugname) LIKE '%neo-synephrine%' 
      OR LOWER(drugname) LIKE '%neosynephrine%'
      OR LOWER(drugname) LIKE '%neosynsprine%'
    )
    GROUP BY patientunitstayid
)
AS vp3
ON vp3.patientunitstayid= icu.patientunitstayid


-- pivoted med
LEFT JOIN(
    SELECT  
        patientunitstayid
      , CASE
          WHEN SUM(dopamine) >= 1 THEN 1
          WHEN SUM(dobutamine) >= 1 THEN 1
          WHEN SUM(norepinephrine) >= 1 THEN 1
          WHEN SUM(phenylephrine) >= 1 THEN 1
          WHEN SUM(epinephrine) >= 1 THEN 1
          WHEN SUM(vasopressin) >= 1 THEN 1
          WHEN SUM(milrinone) >= 1 THEN 1
          ELSE NULL
       END AS pressor_4

    FROM `physionet-data.eicu_crd_derived.pivoted_med`
    GROUP BY patientunitstayid
)
AS vp4
ON vp4.patientunitstayid= icu.patientunitstayid

ORDER BY patientunitstayid
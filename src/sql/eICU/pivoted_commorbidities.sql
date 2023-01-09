DROP TABLE IF EXISTS `db_name.my_eICU.pivoted_commorbidities`;
CREATE TABLE `db_name.my_eICU.pivoted_commorbidities` AS

WITH temp_table AS (

  SELECT icu.patientunitstayid, 
  dx.*, ph.*

  FROM `db_name.eicu_crd_derived.icustay_detail` as icu

  -- get missing values from diagnosistring
  LEFT JOIN(
    SELECT patientunitstayid AS patientunitstayid_dx

    , STRING_AGG(icd9code) AS icd_codes

    , MAX(
      CASE
        WHEN LOWER(diagnosisstring) LIKE "%hypertension%"
        AND LOWER(diagnosisstring) NOT LIKE "%pulmonary hypertension%" THEN 1
        ELSE NULL
      END)
      AS hypertension_1

    ,MAX(
    CASE
      WHEN LOWER(diagnosisstring) LIKE "%heart fail%" THEN 1
      ELSE NULL
    END)
    AS heart_failure_1

    , MAX(
    CASE
      WHEN LOWER(diagnosisstring) LIKE "%renal%"
      AND LOWER(diagnosisstring) LIKE "%stage 1%" THEN 1
      ELSE NULL
    END)
    AS renal_11

    , MAX(
    CASE
      WHEN LOWER(diagnosisstring) LIKE "%renal%"
      AND LOWER(diagnosisstring) LIKE "%stage 2%" THEN 2
      ELSE NULL
    END)
    AS renal_12

    , MAX(
    CASE
      WHEN LOWER(diagnosisstring) LIKE "%renal%"
      AND LOWER(diagnosisstring) LIKE "%stage 3%" THEN 3
      ELSE NULL
    END)
    AS renal_13

    , MAX(
    CASE
      WHEN LOWER(diagnosisstring) LIKE "%renal%"
      AND LOWER(diagnosisstring) LIKE "%stage 4%" THEN 4
      ELSE NULL
    END)
    AS renal_14

    , MAX(
    CASE
      WHEN LOWER(diagnosisstring) LIKE "%renal%"
      AND LOWER(diagnosisstring) LIKE "%stage 5%" THEN 5
      WHEN LOWER(diagnosisstring) LIKE "%renal%"
      AND LOWER(diagnosisstring) LIKE "%esrd%" THEN 5
      ELSE NULL
    END)
    AS renal_15

    , MAX( 
    CASE
      WHEN LOWER(diagnosisstring) LIKE "%copd%" THEN 1
      ELSE NULL
    END)
    AS copd_1

    , MAX(
    CASE
      WHEN LOWER(diagnosisstring) LIKE "%asthma%" THEN 1
      ELSE NULL
    END)
    AS asthma_1

    FROM `physionet-data.eicu_crd.diagnosis`
    GROUP BY patientunitstayid
  )
  AS dx
  ON dx.patientunitstayid_dx = icu.patientunitstayid


  -- get missing values from past history
  LEFT JOIN(
    SELECT patientunitstayid AS patientunitstayid_ph
    
    , MAX(
    CASE
      WHEN LOWER(pasthistorypath) LIKE "%hypertension%"
      AND LOWER(pasthistorypath) NOT LIKE "%pulmonary hypertension%" THEN 1
      ELSE NULL
    END)
    AS hypertension_2
    
    , MAX(
    CASE
      WHEN LOWER(pasthistorypath) LIKE "%heart fail%" THEN 1
      ELSE NULL
    END)
    AS heart_failure_2
    
    , MAX(
    CASE
      WHEN LOWER(pasthistorypath) LIKE "%renal%"
      AND LOWER(pasthistorypath) LIKE "%creatinine 1-2%" THEN 2
      ELSE NULL
    END)
    AS renal_22

    , MAX(
    CASE
      WHEN LOWER(pasthistorypath) LIKE "%renal%"
      AND LOWER(pasthistorypath) LIKE "%creatinine 2-3%" THEN 3
      WHEN LOWER(pasthistorypath) LIKE "%renal%"
      AND LOWER(pasthistorypath) LIKE "%creatinine 3-4%" THEN 3
      ELSE NULL
    END)
    AS renal_23

    , MAX(
    CASE
      WHEN LOWER(pasthistorypath) LIKE "%renal%"
      AND LOWER(pasthistorypath) LIKE "%creatinine 4-5%" THEN 4
      ELSE NULL
    END)
    AS renal_24

    , MAX(
    CASE
      WHEN LOWER(pasthistorypath) LIKE "%renal%"
      AND LOWER(pasthistorypath) LIKE "%creatinine > 5%" THEN 5
      WHEN LOWER(pasthistorypath) LIKE "%renal failure%" THEN 5
      ELSE NULL
    END)
    AS renal_25
    
    , MAX(
    CASE
      WHEN LOWER(pasthistorypath) LIKE "%copd%" THEN 1
      ELSE NULL
    END)
    AS copd_2

    ,MAX(
    CASE
      WHEN LOWER(pasthistorypath) LIKE "%asthma%" THEN 1
      ELSE NULL
    END)
    AS asthma_2

    FROM `physionet-data.eicu_crd.pasthistory`
    GROUP BY patientunitstayid
  )
  AS ph
  ON ph.patientunitstayid_ph = icu.patientunitstayid

)

SELECT temp_table.patientunitstayid

  , CASE
    WHEN hypertension_1 IS NOT NULL
    OR hypertension_2 IS NOT NULL 
    OR icd_codes LIKE "%I10%"
    OR icd_codes LIKE "%I11%"
    OR icd_codes LIKE "%I12%"
    OR icd_codes LIKE "%I13%"
    OR icd_codes LIKE "%I14%"
    OR icd_codes LIKE "%I15%"
    OR icd_codes LIKE "%I16%"
    OR icd_codes LIKE "%I70%"
    THEN 1
    ELSE NULL
    END AS hypertension_present

  , CASE 
    WHEN heart_failure_1 IS NOT NULL
    OR heart_failure_2 IS NOT NULL 
    OR icd_codes LIKE "%I50%"
    OR icd_codes LIKE "%I110%"
    OR icd_codes LIKE "%I27%"
    OR icd_codes LIKE "%I42%"
    OR icd_codes LIKE "%I43%"
    OR icd_codes LIKE "%I517%"
    THEN 1
    ELSE NULL
    END AS heart_failure_present

  , CASE 
    WHEN asthma_1 IS NOT NULL
    OR asthma_2 IS NOT NULL
    OR icd_codes LIKE "%J841%"
    THEN 1
    ELSE NULL
    END AS asthma_present

  , CASE 
    WHEN copd_1 IS NOT NULL
    OR copd_2 IS NOT NULL
    OR icd_codes LIKE "%J41%"
    OR icd_codes LIKE "%J42%"
    OR icd_codes LIKE "%J43%"
    OR icd_codes LIKE "%J44%"
    OR icd_codes LIKE "%J45%"
    OR icd_codes LIKE "%J46%"
    OR icd_codes LIKE "%J47%"
    THEN 1
    ELSE NULL
    END AS copd_present

  , CASE 
    WHEN renal_11 IS NOT NULL
      OR icd_codes LIKE "%N181%" 
    THEN 1
    WHEN renal_12 IS NOT NULL
      OR renal_22 IS NOT NULL
      OR icd_codes LIKE "%N182%"
    THEN 2
    WHEN renal_13 IS NOT NULL
      OR renal_23 IS NOT NULL
      OR icd_codes LIKE "%N183%"
    THEN 3
    WHEN renal_14 IS NOT NULL
      OR renal_24 IS NOT NULL
      OR icd_codes LIKE "%N184%"
    THEN 4
    WHEN renal_15 IS NOT NULL
      OR renal_25 IS NOT NULL
      OR icd_codes LIKE "%N185%"
      OR icd_codes LIKE "%N186%"
    THEN 5
    ELSE NULL
    END AS ckd_stages

  FROM temp_table
  ORDER BY patientunitstayid

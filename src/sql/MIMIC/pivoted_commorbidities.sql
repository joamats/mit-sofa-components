DROP TABLE IF EXISTS `db_name.my_MIMIC.pivoted_commorbidities`;
CREATE TABLE `db_name.my_MIMIC.pivoted_commorbidities` AS

SELECT DISTINCT
    icu.hadm_id

  , CASE WHEN (
       icd_codes LIKE "%I10%"
    OR icd_codes LIKE "%I11%"
    OR icd_codes LIKE "%I12%"
    OR icd_codes LIKE "%I13%"
    OR icd_codes LIKE "%I14%"
    OR icd_codes LIKE "%I15%"
    OR icd_codes LIKE "%I16%"
    OR icd_codes LIKE "%I70%"
  ) THEN 1
    ELSE NULL
  END AS hypertension_present

  , CASE WHEN (
       icd_codes LIKE "%I50%"
    OR icd_codes LIKE "%I110%"
    OR icd_codes LIKE "%I27%"
    OR icd_codes LIKE "%I42%"
    OR icd_codes LIKE "%I43%"
    OR icd_codes LIKE "%I517%"
  ) THEN 1
    ELSE NULL
  END AS heart_failure_present

  , CASE WHEN (
       icd_codes LIKE "%J41%"
    OR icd_codes LIKE "%J42%"
    OR icd_codes LIKE "%J43%"
    OR icd_codes LIKE "%J44%"
    OR icd_codes LIKE "%J45%"
    OR icd_codes LIKE "%J46%"
    OR icd_codes LIKE "%J47%"
  ) THEN 1
    ELSE NULL
  END AS copd_present

  , CASE WHEN 
      icd_codes LIKE "%J841%"
      THEN 1
      ELSE NULL
  END AS asthma_present

  , CASE 
      WHEN icd_codes LIKE "%N181%" THEN 1
      WHEN icd_codes LIKE "%N182%" THEN 2
      WHEN icd_codes LIKE "%N183%" THEN 3
      WHEN icd_codes LIKE "%N184%" THEN 4
      WHEN (
           icd_codes LIKE "%N185%" 
        OR icd_codes LIKE "%N186%"
      )
      THEN 5
    ELSE NULL
  
  END AS ckd_stages

FROM `physionet-data.mimiciv_derived.icustay_detail` AS icu

LEFT JOIN(
  SELECT hadm_id, STRING_AGG(icd_codes) AS icd_codes
  FROM `db_name.my_MIMIC.aux_icd10codes`
  GROUP BY hadm_id
)
AS diagnoses_icd10 
ON diagnoses_icd10.hadm_id = icu.hadm_id

-- Elective admission

-- New approach -> extract ICD diagnoses of elective admissions in MIMIC
-- Map these to the eICU diagnoses
-- Assumptions: only use first 2 diagnoses

DROP TABLE IF EXISTS `db_name.my_eICU.pivoted_elective`;
CREATE TABLE `db_name.my_eICU.pivoted_elective` AS

WITH 

  admission_type AS (

  SELECT subject_id, hadm_id, admission_type

  FROM `physionet-data.mimiciv_hosp.admissions` 
  WHERE  admission_type = "ELECTIVE" 
  OR admission_type = "SURGICAL SAME DAY ADMISSION"

)

, diagnoses_code AS (

  SELECT hadm_id, icd_code, icd_version

  FROM `physionet-data.mimiciv_hosp.diagnoses_icd` 
  WHERE seq_num <= 2

)

, diagnoses_text AS (

  SELECT icd_code, long_title
  FROM `physionet-data.mimiciv_hosp.d_icd_diagnoses` 

)

, MIMIC_elective AS (

  SELECT 
  admission_type.subject_id, admission_type.hadm_id, admission_type.admission_type,
  diagnoses_code.icd_code, diagnoses_code.icd_version,
  diagnoses_text.long_title
  FROM admission_type

  LEFT JOIN diagnoses_code
  ON diagnoses_code.hadm_id = admission_type.hadm_id

  LEFT JOIN diagnoses_text
  ON diagnoses_text.icd_code = diagnoses_code.icd_code

  WHERE diagnoses_code.icd_code IS NOT NULL

)

, eICU_diagnosis AS (

  SELECT patient.patientunitstayid, apacheadmissiondx, icd9code
  FROM `physionet-data.eicu_crd.patient` AS patient

    LEFT JOIN (
    SELECT diagnosis.patientunitstayid, diagnosisstring, icd9code
    FROM `physionet-data.eicu_crd.diagnosis` AS diagnosis
    WHERE icd9code IS NOT NULL
    )
    AS diagnosis
    ON diagnosis.patientunitstayid = patient.patientunitstayid

)

SELECT patientunitstayid, STRING_AGG(icd9code) AS icdcodes, 1 AS adm_elective
FROM eICU_diagnosis
WHERE REGEXP_EXTRACT(eICU_diagnosis.icd9code, r'^[^,]*') IN(
  SELECT REGEXP_EXTRACT(MIMIC_elective.icd_code, r'^[^,]*')
  FROM MIMIC_elective
)
OR REGEXP_EXTRACT(eICU_diagnosis.icd9code, r',(.*)') IN(
  SELECT  REGEXP_EXTRACT(MIMIC_elective.icd_code, r',(.*)')
  FROM MIMIC_elective
)
OR REGEXP_EXTRACT(eICU_diagnosis.icd9code, r'^[^,]*') IN(
  SELECT REGEXP_EXTRACT(MIMIC_elective.icd_code, r',(.*)')
  FROM MIMIC_elective
)

GROUP BY patientunitstayid

UNION ALL -- has to be (and is) the same result as DISTINCT

SELECT patientunitstayid, STRING_AGG(icd9code) AS icdcodes, 0 AS adm_elective
FROM eICU_diagnosis
WHERE REGEXP_EXTRACT(eICU_diagnosis.icd9code, r'^[^,]*') NOT IN(
  SELECT REGEXP_EXTRACT(MIMIC_elective.icd_code, r'^[^,]*')
  FROM MIMIC_elective
)
OR REGEXP_EXTRACT(eICU_diagnosis.icd9code, r',(.*)') NOT IN(
  SELECT  REGEXP_EXTRACT(MIMIC_elective.icd_code, r',(.*)')
  FROM MIMIC_elective
)
OR REGEXP_EXTRACT(eICU_diagnosis.icd9code, r'^[^,]*') NOT IN(
  SELECT REGEXP_EXTRACT(MIMIC_elective.icd_code, r',(.*)')
  FROM MIMIC_elective
)

GROUP BY patientunitstayid
ORDER BY patientunitstayid


/*

-- Approach with mapping on source of admission
-- Problem: Deprecated due to too many elective admission

-- Mapping
-- Assume emergency admission if patient came from
-- Emergency Department, Direct Admit, Chest Pain Center, Other Hospital, or Observation
-- Assume elective admission if patient from other place, e.g. operating room, floor, etc.

DROP TABLE IF EXISTS `db_name.my_eICU.pivoted_elective`;
CREATE TABLE `db_name.my_eICU.pivoted_elective` AS

WITH elective_admission AS (

    -- 1: pat table as base for patientunitstayid  
    SELECT pat.patientunitstayid, adm_elective2
      , CASE
      WHEN unitAdmitSource LIKE "Emergency Department" THEN 0
      WHEN unitAdmitSource LIKE "Chest Pain Center" THEN 0
      WHEN unitAdmitSource LIKE "Other Hospital" THEN 0
      WHEN unitAdmitSource LIKE "Other" THEN 0
      WHEN unitAdmitSource LIKE "Observation" THEN 0
      WHEN unitAdmitSource LIKE "Direct Admit" THEN 0
      ELSE 1
      END AS adm_elective1
      FROM `physionet-data.eicu_crd.patient` AS pat

    -- 2: apachepredvar table
    LEFT JOIN (
    SELECT apache.patientunitstayid, electivesurgery AS adm_elective2
    FROM `physionet-data.eicu_crd.apachepredvar` AS apache
    )
    AS apache
    ON pat.patientunitstayid = apache.patientunitstayid

)


  SELECT patientunitstayid
  , CASE
    WHEN adm_elective1 = 1 THEN 1
    WHEN adm_elective2 = 1 THEN 1
    ELSE 0
    END AS adm_elective
  FROM elective_admission
*/
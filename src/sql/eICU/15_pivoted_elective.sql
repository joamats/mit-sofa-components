-- Elective admission

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


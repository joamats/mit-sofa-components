DROP TABLE IF EXISTS `db_name.my_eICU.pivoted_codes`;
CREATE TABLE `db_name.my_eICU.pivoted_codes` AS

WITH codes AS(
  
  SELECT  
    patientunitstayid
  , CASE 
      WHEN code_seq = MIN(code_seq) OVER(PARTITION BY patientunitstayid) THEN cplitemvalue
    END AS first_code
  , CASE 
      WHEN code_seq = MAX(code_seq) OVER(PARTITION BY patientunitstayid) THEN cplitemvalue
    END AS last_code

  FROM (
    SELECT
        icu.patientunitstayid
      , cplitemoffset
      , cplitemvalue
      , ROW_NUMBER() OVER(PARTITION BY icu.patientunitstayid ORDER BY cplitemoffset ASC) AS code_seq

    FROM `physionet-data.eicu_crd_derived.icustay_detail`
    AS icu

    LEFT JOIN (
      SELECT patientunitstayid, cplitemoffset, cplgroup, cplitemvalue
      FROM `physionet-data.eicu_crd.careplangeneral`) 
    AS care
    ON care.patientunitstayid = icu.patientunitstayid
    AND LOWER(cplgroup) LIKE "%care limitation%"
  )

  ORDER BY patientunitstayid

)

SELECT icu.patientunitstayid, codes_1.first_code, codes_2.last_code
FROM `physionet-data.eicu_crd_derived.icustay_detail`
AS icu

LEFT JOIN (
  SELECT patientunitstayid, STRING_AGG(first_code) AS first_code
  FROM codes 
  GROUP BY patientunitstayid
)
AS codes_1
ON codes_1.patientunitstayid = icu.patientunitstayid

LEFT JOIN (
  SELECT patientunitstayid, STRING_AGG(last_code) AS last_code
  FROM codes 
  GROUP BY patientunitstayid 
)
AS codes_2
ON codes_2.patientunitstayid = icu.patientunitstayid

ORDER BY patientunitstayid

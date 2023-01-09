DROP TABLE IF EXISTS `db_name.my_eICU.pivoted_cancer`;
CREATE TABLE `db_name.my_eICU.pivoted_cancer` AS
  
SELECT icu.patientunitstayid

, CASE WHEN 
    icd_codes LIKE "%C__%"
    THEN 1
    ELSE NULL
  END AS has_cancer

, CASE WHEN (
        icd_codes LIKE "%C0%" 
    OR icd_codes LIKE "%C1%"
    OR icd_codes LIKE "%C2%"
    OR icd_codes LIKE "%C3%" 
    OR icd_codes LIKE "%C4%"
    OR icd_codes LIKE "%C5%"
    OR icd_codes LIKE "%C6%"
    OR icd_codes LIKE "%C70%"
    OR icd_codes LIKE "%C71%"
    OR icd_codes LIKE "%C72%"
    OR icd_codes LIKE "%C73%"
    OR icd_codes LIKE "%C74%"
    OR icd_codes LIKE "%C75%"
    OR icd_codes LIKE "%C76%"
) THEN 1
  ELSE NULL
END AS cat_solid

, CASE WHEN (
        icd_codes LIKE "%C77%" 
    OR icd_codes LIKE "%C7B%"
    OR icd_codes LIKE "%C78%"
    OR icd_codes LIKE "%C79%" 
    OR icd_codes LIKE "%C79%"
) THEN 1
  ELSE NULL
END AS cat_metastasized

, CASE WHEN (
        icd_codes LIKE "%C8%"  
    OR icd_codes LIKE "%C90%" 
    OR icd_codes LIKE "%C91%" 
    OR icd_codes LIKE "%C92%" 
    OR icd_codes LIKE "%C93%" 
    OR icd_codes LIKE "%C94%" 
    OR icd_codes LIKE "%C95%" 
) THEN 1
  ELSE NULL
END AS cat_hematological

, CASE WHEN (
      icd_codes LIKE "%C17%"
  OR icd_codes LIKE "%C18%"
  OR icd_codes LIKE "%C19%"
  OR icd_codes LIKE "%C20%"
  OR icd_codes LIKE "%C21%"
) THEN 1
  ELSE NULL
END AS loc_colon_rectal

, CASE WHEN (
      icd_codes LIKE "%C22%"
) THEN 1
  ELSE NULL
END AS loc_liver_bd

, CASE WHEN (
      icd_codes LIKE "%C25%"
) THEN 1
  ELSE NULL
END AS loc_pancreatic

, CASE WHEN (
      icd_codes LIKE "%C34%"
) THEN 1
  ELSE NULL
END AS loc_lung_bronchus

, CASE WHEN (
      icd_codes LIKE "%C43%"
) THEN 1
  ELSE NULL
END AS loc_melanoma

, CASE WHEN (
      icd_codes LIKE "%C50%"
) THEN 1
  ELSE NULL
END AS loc_breast

, CASE WHEN (
      icd_codes LIKE "%C53%"
  OR  icd_codes LIKE "%C54%"
  OR  icd_codes LIKE "%C55%"
) THEN 1
  ELSE NULL
END AS loc_endometrial

, CASE WHEN (
      icd_codes LIKE "%C61%"
) THEN 1
  ELSE NULL
END AS loc_prostate

, CASE WHEN (
      icd_codes LIKE "%C64%"
  OR icd_codes LIKE "%C65%"
) THEN 1
  ELSE NULL
END AS loc_kidney

, CASE WHEN (
      icd_codes LIKE "%C67%"
) THEN 1
  ELSE NULL
END AS loc_bladder

, CASE WHEN (
      icd_codes LIKE "%C73%"
) THEN 1
  ELSE NULL
END AS loc_thyroid

, CASE WHEN (
      icd_codes LIKE "%C82%"
  OR icd_codes LIKE "%C83%"
  OR icd_codes LIKE "%C84%"
  OR icd_codes LIKE "%C85%"
  OR icd_codes LIKE "%C86%"
) THEN 1
  ELSE NULL
END AS loc_nhl

, CASE WHEN (
      icd_codes LIKE "%C91%"
  OR icd_codes LIKE "%C92%"
  OR icd_codes LIKE "%C93%"
  OR icd_codes LIKE "%C94%"
  OR icd_codes LIKE "%C95%"
) THEN 1
  ELSE NULL
END AS loc_leukemia

FROM `db_name.eicu_crd_derived.icustay_detail` as icu

LEFT JOIN(
  SELECT patientunitstayid AS patientunitstayid_dx
        , STRING_AGG(icd9code) AS icd_codes
  FROM `physionet-data.eicu_crd.diagnosis`
  GROUP BY patientunitstayid
)
    
AS dx
ON dx.patientunitstayid_dx = icu.patientunitstayid

ORDER BY patientunitstayid


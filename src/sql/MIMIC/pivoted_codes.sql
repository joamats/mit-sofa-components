DROP TABLE IF EXISTS `db_name.my_MIMIC.pivoted_codes`;
CREATE TABLE `db_name.my_MIMIC.pivoted_codes` AS


WITH codes AS(
  
  SELECT  
    stay_id
  , CASE 
      WHEN code_seq = MIN(code_seq) OVER(PARTITION BY stay_id) THEN value
    END AS first_code
  , CASE 
      WHEN code_seq = MAX(code_seq) OVER(PARTITION BY stay_id) THEN value
    END AS last_code

  FROM (
    SELECT
        icu.stay_id
      , charttime
      , value
      , ROW_NUMBER() OVER(PARTITION BY icu.stay_id ORDER BY charttime ASC ) AS code_seq

    FROM `physionet-data.mimiciv_derived.icustay_detail`
    AS icu

    LEFT JOIN (
      SELECT stay_id, charttime, itemid, value
      FROM `physionet-data.mimiciv_icu.chartevents`) 
    AS chart
    ON chart.stay_id = icu.stay_id
    AND itemid = 223758
  )

  ORDER BY stay_id

)

SELECT icu.stay_id, codes_1.first_code, codes_2.last_code
FROM `physionet-data.mimiciv_derived.icustay_detail`
AS icu

LEFT JOIN (
  SELECT stay_id, STRING_AGG(first_code) AS first_code
  FROM codes 
  GROUP BY stay_id
)
AS codes_1
ON codes_1.stay_id = icu.stay_id

LEFT JOIN (
  SELECT stay_id, STRING_AGG(last_code) AS last_code
  FROM codes 
  GROUP BY stay_id 
)
AS codes_2
ON codes_2.stay_id = icu.stay_id

ORDER BY stay_id

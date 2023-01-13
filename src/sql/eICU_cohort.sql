WITH
  sofa24 AS (
    SELECT patientunitstayid AS s24_id,
            sofa_resp AS resp_24,
            sofa_gcs AS cns_24,
            sofa_circ AS cv_24,
            sofa_liver AS liver_24,
            sofa_hematology AS coag_24,
            sofa_renal AS renal_24
    FROM `icu_elos.itu_sofa_day`
    WHERE day = 1
)
, sofa168 AS (
    SELECT patientunitstayid AS s168_id,
           sofa_resp AS resp_168,
           sofa_gcs AS cns_168,
           sofa_circ AS cv_168,
           sofa_liver AS liver_168,
           sofa_hematology AS coag_168,
           sofa_renal AS renal_168
    FROM `icu_elos.itu_sofa_day`
    WHERE day = 7
)
, vent24 AS (
    SELECT patientunitstayid AS mv24_id
    FROM `icu_elos.invasive`
    WHERE starttime < 24 * 60
    AND endtime > 24 * 60
)
, vent168 AS (
    SELECT patientunitstayid AS mv168_id
    FROM `icu_elos.invasive`
    WHERE starttime < 168 * 60
    AND endtime > 168 * 60
)
, patient as (
    SELECT patientunitstayid AS p_id,
           hospitaldischargelocation,
           unitdischargestatus,
           hospitaldischargestatus
    FROM `physionet-data.eicu_crd.patient`
)
, cabg_adm AS (
    SELECT distinct patientunitstayid AS cabg_id
    FROM `physionet-data.eicu_crd.patient`
    WHERE apacheadmissiondx IN (
                                "CABG alone, coronary artery bypass grafting",
                                "CABG redo with valve repair/replacement",
                                "CABG with mitral valve replacement",
                                "CABG alone, redo",
                                "CABG with double valve repair/replacement",
                                "CABG with aortic valve replacement",
                                "CABG with other operation",
                                "CABG with mitral valve repair",
                                "CABG redo with other operation",
                                "CABG with pulmonic or tricuspid valve repair or replacement ONLY.",
                                "CABG, minimally invasive; mid-CABG"
                                )
)
, cirrhosis AS (
    SELECT DISTINCT *
    FROM (
        SELECT distinct patientunitstayid AS cirrhosis_id
        FROM `physionet-data.eicu_crd.pasthistory` 
        WHERE pasthistorypath
        IN ("notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Cirrhosis/jaundice",
            "notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Cirrhosis/UGI bleeding",
            "notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Cirrhosis/encephalopathy",
            "notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Cirrhosis/ascites",
            "notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Cirrhosis/varices",
            "notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Cirrhosis/biopsy proven",
            "notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Cirrhosis/clinical diagnosis",
            "notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Cirrhosis/coma"
        )
    UNION ALL
    SELECT distinct patientunitstayid
    FROM `physionet-data.eicu_crd.apachepredvar`
    WHERE cirrhosis = 1

    UNION ALL
    SELECT distinct patientunitstayid
    FROM `physionet-data.eicu_crd.diagnosis`
    WHERE diagnosisstring
    IN ("gastrointestinal|hepatic disease|hepatic dysfunction|with cirrhosis",
        "gastrointestinal|hepatic disease|hepatic dysfunction|with cirrhosis|biliary",
        "gastrointestinal|hepatic disease|hepatic dysfunction|with cirrhosis|alcoholic",
        "gastrointestinal|hepatic disease|hepatic dysfunction|with cirrhosis|cryptogenic"
       )
    )
)
, esrd as (
    SELECT distinct patientunitstayid as esrd_id
    FROM (
        SELECT distinct patientunitstayid
        FROM `physionet-data.eicu_crd.pasthistory`
        WHERE pasthistorypath
        IN ("notes/Progress Notes/Past History/Organ Systems/Renal  (R)/Renal Failure/renal failure - hemodialysis")
        UNION ALL
        SELECT distinct patientunitstayid
        FROM `physionet-data.eicu_crd.diagnosis` 
        WHERE diagnosisstring
        IN ("renal|disorder of kidney|ESRD (end stage renal disease)")
    )
)
 
, charlson as (
    SELECT patientunitstayid as charlson_id, final_charlson_score as charlson
    FROM `icu_elos.charlson_comorbidity_index`
    )

, sepsis3 as (
    SELECT patientunitstayid as sepsis_id, 
    CASE 
    WHEN patientunitstayid IS NOT NULL THEN "TRUE"
    ELSE NULL
    END AS sepsis3
    FROM `icu_elos.sepsis_adult_eicu`
    )

, first_tbl as (
SELECT patientunitstayid as service_id, specialty AS first_service
    FROM 
    (
        SELECT patientunitstayid, managingphysician, careprovidersaveoffset, specialty,
        ROW_NUMBER() OVER(PARTITION BY patientunitstayid ORDER BY careprovidersaveoffset ASC) AS service_seq
        FROM `physionet-data.eicu_crd.careplancareprovider`
        WHERE specialty NOT IN 
        ('nurse','nurse practitioner', 'social work', 'ethics')
    )
    WHERE managingphysician = 'Managing'
    AND service_seq = 1
    AND careprovidersaveoffset BETWEEN -(60*6) AND (24*60) -- analogous to SOFA rule, pick -6 and +24h from admission to unit
    )

, apache AS (
    SELECT patientunitstayid AS apache_id, physicianspeciality
 FROM `physionet-data.eicu_crd.apachepatientresult`
)

, first_service AS (
    Select patientunitstayid AS first_service_id, MIN(COALESCE(physicianspeciality, first_service)) AS specialty
    From `physionet-data.eicu_crd.patient` AS patient

    LEFT JOIN first_tbl
    ON patient.patientunitstayid = first_tbl.service_id

    LEFT JOIN apache
    ON patient.patientunitstayid = apache.apache_id

    GROUP BY patientunitstayid
    ORDER BY patientunitstayid
)



SELECT distinct *
FROM `physionet-data.eicu_crd_derived.icustay_detail` AS cohort 

LEFT JOIN sofa24
ON cohort.patientunitstayid = sofa24.s24_id

LEFT JOIN sofa168
ON cohort.patientunitstayid = sofa168.s168_id

LEFT JOIN vent24
ON cohort.patientunitstayid = vent24.mv24_id

LEFT JOIN vent168
ON cohort.patientunitstayid = vent168.mv168_id

LEFT JOIN patient
ON cohort.patientunitstayid = patient.p_id

LEFT JOIN cabg_adm
ON cohort.patientunitstayid = cabg_adm.cabg_id

LEFT JOIN cirrhosis
ON cohort.patientunitstayid = cirrhosis.cirrhosis_id

LEFT JOIN esrd
ON cohort.patientunitstayid = esrd.esrd_id

LEFT JOIN charlson
ON cohort.patientunitstayid = charlson.charlson_id

LEFT JOIN sepsis3
ON cohort.patientunitstayid = sepsis3.sepsis_id

LEFT JOIN first_service
ON cohort.patientunitstayid = first_service.first_service_id

LEFT JOIN(
  SELECT *
  FROM `db_name.my_eICU.pivoted_comorbidities`
)
AS comms
ON cohort.patientunitstayid = comms.patientunitstayid

LEFT JOIN(
  SELECT *
  FROM `db_name.my_eICU.pivoted_codes`
)
AS codes
ON cohort.patientunitstayid = codes.patientunitstayid

LEFT JOIN(
  SELECT patientunitstayid, adm_elective
  FROM `db_name.my_eICU.pivoted_elective`
)
AS adm
ON cohort.patientunitstayid = adm.patientunitstayid

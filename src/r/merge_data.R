###### STUB ############
###### STUB ############
###### STUB ############
###### STUB ############

library(magrittr) 
library(dplyr)
library(tidyr)
library(gdata)
library(forcats)

load_data <- function(cohort, hour){

  file_path <- paste0("data/", cohort, "_", hour, ".csv")

# eicu varnames
	...1	uniquepid	patienthealthsystemstayid	patientunitstayid	unitvisitnumber	hospitalid	region	unittype	
  hospitaladmitoffset	hospitaldischargeoffset	unitadmitoffset	unitdischargeoffset	apache_iv	hospitaldischargeyear	age	hosp_mort	
  gender	ethnicity	admissionheight	admissionweight	dischargeweight	icu_los_hours	
  s24_id	cns_24	cv_24	liver_24	coag_24	renal_24	s168_id	cns_168	cv_168	liver_168	coag_168	renal_168	
  resp_sofa_id	resp_24	resp_168	mv24_id	mv168_id	p_id	
  hospitaldischargelocation	unitdischargestatus	hospitaldischargestatus	unitvisitnumber_1	
  los_icu	cabg_id	cirrhosis_id	esrd_id	charlson_id	charlson	sepsis_id	sepsis3	first_service_id	
  specialty	patientunitstayid_1	hypertension_present	heart_failure_present	asthma_present	copd_present	cad_present	ckd_stages	diabetes_types	
  patientunitstayid_2	first_code	patientunitstayid_3	adm_elective	out_in	discharge_in	icudeath	medical	cirr_present	full_therapy	newvent24

# mimic varnames
	...1	subject_id	hadm_id	stay_id	gender	dod	admittime	dischtime	los_hospital	admission_age	race	
  hospital_expire_flag	hospstay_seq	first_hosp_stay	icu_intime	icu_outtime	los_icu	icustay_seq	first_icu_stay	
  stay_id_1	resp_24	coag_24	liver_24	cv_24	cns_24	renal_24	stay_id_2	resp_168	coag_168	liver_168	cv_168	cns_168	renal_168
  	stay_id_3	max_hr	resp_last	coag_last	liver_last	cv_last	cns_last	renal_last	stay_id_4	vent_24	stay_id_5	vent_168	stay_id_6	vent_last	
    hadm_id_1	deathtime	admission_type	adm_elective	admission_location	discharge_location	subject_id_1	stay_id_7	antibiotic_time	culture_time	
    suspected_infection_time	sofa_time	sofa_score	respiration	coagulation	liver	cardiovascular	cns	renal	sepsis3	cabg_id	cirrhosis_id	
    ESRD_id	hadm_id_2	charlson	hadm_id_3	first_service	hadm_id_4	hypertension_present	heart_failure_present	copd_present	asthma_present	
    cad_present	ckd_stages	diabetes_types	stay_id_8	first_code	age	esrd_id	out_in	death_in	discharge_in	ethnicity	medical	cirr_present	
    full_therapy	combinedeathtime	icudeath	newvent24

  # Load Data  
  data <- read.csv(file_path, header = TRUE, stringsAsFactors = TRUE)
 
  # Return just keeping columns of interest
  data <- data[, c("sex_female", "race_group", "anchor_age",
                  "mech_vent", "rrt", "vasopressor",  
                  "CCI", "CCI_ranges", 
                  "ethno_white", "language", "lang_eng",
                  "SOFA", "SOFA_ranges", "los_icu",
                  "mortality_in", "mortality_90",
                  "has_cancer", "cat_solid", "cat_hematological", "cat_metastasized",
                  "loc_colon_rectal", "loc_liver_bd", "loc_pancreatic", "loc_lung_bronchus",
                  "loc_melanoma", "loc_breast", "loc_endometrial", "loc_prostate",
                  "loc_kidney", "loc_bladder", "loc_thyroid", "loc_nhl", "loc_leukemia",
                  "com_hypertension_present", "com_heart_failure_present", "com_asthma_present",
                  "com_copd_present", "com_ckd_stages",
                  "is_full_code_admission", "is_full_code_discharge")
             ]

  return(data)

}

get_merged_datasets <- function() {
  
  hour <- ("24", "168")
  mimic <- load_data("MIMIC", hour)
  eicu <- load_data("eICU", hour)
  
  # merge cohorts
  data_all <- combine(mimic, eicu)

  write.csv(data_all, paste0("data/merged", hour, ".csv"))

  return (data_all)
}

get_merged_datasets()
library(magrittr) 
library(dplyr)
library(tidyr)
library(gdata)
library(forcats)

load_data <- function(cohort){

  file_path <- paste0("data/", cohort, "_data.csv")

  # Load Data  
  data <- read.csv(file_path, header = TRUE, stringsAsFactors = TRUE)
  data$ethno_white <- data$race_group 
  data <- data %>% mutate(ethno_white = ifelse(race_group=="White", 1, 0))

  data$lang_eng <- data$language 
  data <- data %>% mutate(lang_eng = ifelse(language=="ENGLISH", 1, 0))


  # Replace all NAs in cancer types with 0
  cancer_list <- c("has_cancer", "cat_solid", "cat_hematological", "cat_metastasized",
                    "loc_colon_rectal", "loc_liver_bd", "loc_pancreatic", "loc_lung_bronchus",
                    "loc_melanoma", "loc_breast", "loc_endometrial", "loc_prostate",
                    "loc_kidney", "loc_bladder", "loc_thyroid", "loc_nhl", "loc_leukemia")
  data <- data %>% mutate_at(cancer_list, ~ replace_na(., 0))

  # Encode CKD stages as binary
  data <- within(data, com_ckd_stages <- factor(com_ckd_stages, levels = c(0, 1, 2, 3, 4, 5)))
  data <- within(data, com_ckd_stages <- fct_collapse(com_ckd_stages,"0"=c("0", "1", "2"), "1"=c("3", "4", "5")))


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
  write.csv(data, file_path)

  return(data)

}

get_merged_datasets <- function() {

  mimic <- load_data("MIMIC")
  eicu <- load_data("eICU")

  # merge 3 cohorts
  data_all <- combine(mimic, eicu)

  write.csv(data_all, "data/cohorts_merged.csv")

  return (data_all)
}

get_merged_datasets()
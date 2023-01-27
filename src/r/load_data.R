library(magrittr) 
library(dplyr)
library(tidyr)
library(gdata)
library(forcats)

load_data <- function(cohort){

  file_path <- paste0("data/", cohort, "_data.csv")

  # Load Data  
  data <- read.csv(file_path, header = TRUE, stringsAsFactors = TRUE)

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
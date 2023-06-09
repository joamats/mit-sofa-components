library(table1)
library(flextable)
library(tidyverse)

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

# Functions to add commas between 1,000
render.categorical <- function(x, ...) {
  c("", sapply(stats.apply.rounding(stats.default(x)), function(y) with(y,
                            sprintf("%s (%s%%)", prettyNum(FREQ, big.mark=","), PCT))))
}

render.strat <- function (label, n, ...) {
  sprintf("<span class='stratlabel'>%s<br><span class='stratn'>(N=%s)</span></span>", 
          label, prettyNum(n, big.mark=","))
}

run_table1 <- function(cohort) {

  df <- read_csv(paste0("data/cohorts/", cohort, "_24.csv"), show_col_types = FALSE)

  # Factor variables
  #df$d7_nonsurvivors <- 1
  df %>% mutate(d7_nonsurvivors = NA)
  
  df$situation168 <- "Death before d7"

  if (cohort == "eICU") {
    # Used defintion from cohort scripts as accurate to minutes instead of hours
    df$situation168[(df$unitdischargeoffset - df$unitadmitoffset >= 10080) &
                (df$hospitaldischargeoffset - df$unitadmitoffset >= 10080)] <- "Alive at d7"

    df$situation168[(df$unitdischargeoffset - df$unitadmitoffset < 10080) &
                (df$hospitaldischargeoffset - df$unitadmitoffset < 10080) &
                (df$icudeath == 'Survived')] <- "Discharge before d7"

  } else if (cohort == "MIMIC") {
    
    # Used defintion from cohort scripts as accurate to minutes instead of hours for MIMIC

    # alive in ICU at 168hour
    df$situation168[(difftime(df$dischtime, df$icu_intime, units = "secs")>=604800) & (difftime(df$icu_outtime, df$icu_intime, units = "secs") >=604800) & (!is.na(df$deathtime)) & (difftime(df$deathtime, df$icu_intime, units = "secs") >=604800) ]<- "Alive at d7"
    df$situation168[(difftime(df$dischtime, df$icu_intime, units = "secs")>=604800) & (difftime(df$icu_outtime, df$icu_intime, units = "secs") >=604800) & (is.na(df$deathtime)) ] <- "Alive at d7"

    # discharge before 168hour
    df$situation168[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (!is.na(df$deathtime)) & (difftime(df$deathtime,df$icu_outtime,units="secs") >= 259200)] <-"Discharge before d7"
    df$situation168[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (is.na(df$deathtime)) & (difftime(df$dischtime,df$icu_outtime,units="secs") >= 259200)] <-"Discharge before d7"
    df$situation168[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (is.na(df$deathtime)) & (difftime(df$dischtime,df$icu_outtime,units="secs") < 259200) & (!is.na(df$discharge_location)) & (df$discharge_location != "HOSPICE")] <-"Discharge before d7"
    df$situation168[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (is.na(df$deathtime)) & (difftime(df$dischtime,df$icu_outtime,units="secs") < 259200) & (is.na(df$discharge_location)) ] <- "Discharge before d7"
    df$situation168[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (is.na(df$deathtime)) & (difftime(df$dischtime,df$icu_outtime,units="secs") < 259200) & (is.na(df$discharge_location)) ] <- "Discharge before d7"

    df$situation168[(is.na(df$resp_168))] <- "Discharge before d7"

    # Keep only the patients who were alive more than 7 days and have information on ventilation
    # df <- df[(df$situation168=="Alive"),]
    #df <- df[(!is.na(df$resp_168)),]

    }

  df$d7_nonsurvivors <- df$situation168

  # df <- df %>% mutate(d7_nonsurvivors= ifelse(situation168 == "Dead" & icudeath == "Died", 0,
  #                                      ifelse(situation168 == "Alive" & icudeath == "Survived", 1, 2)))

  # df$d7_nonsurvivors <- factor(df$d7_nonsurvivors, levels=c(0, 1, 2), 
  #                                   labels = c('Death before d7', 'Discharge before d7', 'Alive at d7')) 

  df$d7_nonsurvivors <- factor(df$d7_nonsurvivors, levels=c('Death before d7', 'Discharge before d7', 'Alive at d7')) 

  df$gender <- factor(df$gender, level=c('Male',
                                        'Female'))

  df$icudeath <- factor(df$icudeath, levels=c("Survived", "Died"))
  df$sepsis3 <- factor(df$sepsis3, levels=c(0, 1), 
                                    labels = c('Sepsis absent', 'Sepsis present')) 
  df$adm_elective <- factor(df$adm_elective, levels=c(0, 1), 
                                    labels = c('Emergency admission', 'Elective admission'))                                   
  df$cirr_present <- factor(df$cirr_present, levels=c(0, 1), 
                                    labels = c('Cirrhosis absent', 'Cirrhosis present')) 
  df$hypertension_present <- factor(df$hypertension_present, levels=c(0, 1), 
                                    labels = c('Hypertension absent', 'Hypertension present')) 
  df$heart_failure_present <- factor(df$heart_failure_present, levels=c(0, 1), 
                                    labels = c('Congestive heart failure absent', 'Congestive heart failure present')) 
  df$asthma_present <- factor(df$asthma_present, levels=c(0, 1), 
                                    labels = c('Asthma absent', 'Asthma present')) 
  df$copd_present <- factor(df$copd_present, levels=c(0, 1), 
                                    labels = c('COPD absent', 'COPD present')) 
  df <- within(df, ckd_stages <- factor(ckd_stages, levels = c(0, 1, 2, 3, 4, 5)))
  df <- within(df, ckd_stages <- fct_collapse(ckd_stages, Absent=c("0", "1", "2"), Present=c("3", "4", "5")))

  df$ethnicity <- factor(df$ethnicity, levels=c("HISPANIC",
                                                "BLACK",
                                                "WHITE",
                                                "ASIAN",
                                                "OTHER"))

  # Label variables
  label(df$charlson) <- "Charlson comorbidity index"
  label(df$gender) <- "Sex"
  label(df$age) <- "Age"
  label(df$sepsis3) <- "Sepsis admission"
  label(df$adm_elective) <- "Admission type"
  label(df$cirr_present) <- "Cirrhosis"
  label(df$hypertension_present) <- "Hypertension"
  label(df$heart_failure_present) <- "Congestive heart failure"
  label(df$asthma_present) <- "Asthma"
  label(df$copd_present) <- "COPD"
  label(df$ckd_stages) <- "Chronic kidney disease"
  label(df$ethnicity) <- "Ethnicity"

  label(df$resp_24) <- "SOFA - Respiration at 24 hours"
  label(df$cns_24) <- "SOFA - CNS at 24 hours"
  label(df$cv_24) <- "SOFA - Cardiovascular at 24 hours"
  label(df$coag_24) <- "SOFA - Coagulation at 24 hours"
  label(df$renal_24) <- "SOFA - Renal at 24 hours"
  label(df$liver_24) <- "SOFA - Liver at 24 hours"

  t1 <- table1(~ icudeath + gender + age + ethnicity + adm_elective + sepsis3 + charlson + 
                 cirr_present + hypertension_present + heart_failure_present + asthma_present + copd_present + ckd_stages +
                 cns_24 + resp_24 + coag_24 + liver_24 + cv_24  + renal_24 | d7_nonsurvivors,
            data=df,
            overall=T,
            #render.missing=NULL,
            topclass="Rtable1-grid Rtable1-shade Rtable1-times",
            render.categorical=render.categorical, render.strat=render.strat,
            render.continuous=c(.="Mean (SD)", .="Median (Q2, Q3)")
            )

  t1flex(t1) %>% save_as_docx(path=paste0("results/table_d7_nonsurvivors/", cohort, "_24.docx"))

}

cohorts <- c("MIMIC", "eICU")

for (c in cohorts) {
  run_table1(c)
}
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

  if (cohort == "eICU") {
    # Transform icu los from hours to days
    df <- df %>% mutate(los_icu = los_icu/24)
  } 

  df <- df %>% mutate(d7_nonsurvivors= ifelse(los_icu < 7 & icudeath == "Died", 0,
                                       ifelse(los_icu < 7 & icudeath == "Survived", 1, 2)))

  df$d7_nonsurvivors <- factor(df$d7_nonsurvivors, levels=c(0, 1, 2), 
                                    labels = c('Death before d7', 'Discharge before d7', 'Alive at d7')) 

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

  # 24 hours
  df$resp_24  <- factor(df$resp_24,   levels=c("Abnormal","Normal"))
  df$coag_24  <- factor(df$coag_24,   levels=c("Abnormal","Normal"))
  df$cv_24    <- factor(df$cv_24,     levels=c("Abnormal","Normal"))
  df$liver_24 <- factor(df$liver_24,  levels=c("Abnormal","Normal"))
  df$renal_24 <- factor(df$renal_24,  levels=c("Abnormal","Normal"))
  df$cns_24   <- factor(df$cns_24,    levels=c("Abnormal","Normal"))

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
            render.categorical=render.categorical, render.strat=render.strat
            )

  t1flex(t1) %>% save_as_docx(path=paste0("results/table_d7_nonsurvivors/", cohort, "_24.docx"))

}

cohorts <- c("MIMIC", "eICU")

for (c in cohorts) {
  run_table1(c)
}
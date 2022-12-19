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

run_table1 <- function(cohort) {

  df <- read_csv(paste0("data/cohorts/", cohort, "_24.csv"), show_col_types = FALSE)

  df$gender <- factor(df$gender, level=c('Male',
                                        'Female'))

  df$icudeath <- factor(df$icudeath, levels=c("Survived", "Died"))
                                              
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
  df$cns_24   <- factor(df$cns_24,    levels=c("Mechanical Ventilation (MV)","Abnormal","Normal"))

  label(df$gender) <- "Gender"
  label(df$age) <- "Age"
  label(df$ethnicity) <- "Ethnicity"

  label(df$resp_24) <- "SOFA - Respiration at 24 hours"
  label(df$cns_24) <- "SOFA-CNS and MV at 24 hours"
  label(df$cv_24) <- "SOFA - Cardiovascular at 24 hours"
  label(df$coag_24) <- "SOFA - Coagulation at 24 hours"
  label(df$renal_24) <- "SOFA - Renal at 24 hours"
  label(df$liver_24) <- "SOFA - Liver at 24 hours"

  t1 <- table1(~ gender + age + ethnicity + cns_24 + resp_24 + coag_24 + liver_24 + cv_24  + renal_24 | icudeath,
            data=df,
            overall=F,
            extra.col=list(`P-value`=pvalue),
            render.missing=NULL,
            topclass="Rtable1-grid Rtable1-shade Rtable1-times"
            )

  t1flex(t1) %>% save_as_docx(path=paste0("results/table1/", cohort, "_24.docx"))


  df1 <- read_csv(paste0("data/cohorts/", cohort, "_168.csv"), show_col_types = FALSE)

  df1$gender <- factor(df1$gender, level=c('Male',
                                        'Female'))

  df1$icudeath <- factor(df1$icudeath, levels=c("Survived", "Died"))
                                              
  df1$ethnicity <- factor(df1$ethnicity, levels=c("HISPANIC",
                                                  "BLACK",
                                                  "WHITE",
                                                  "ASIAN",
                                                  "OTHER"))

  # 168 hours
  df1$resp_168  <- factor(df1$resp_168,   levels=c("Abnormal", "Normal"))
  df1$coag_168  <- factor(df1$coag_168,   levels=c("Abnormal", "Normal"))
  df1$cv_168    <- factor(df1$cv_168,     levels=c("Abnormal", "Normal"))
  df1$liver_168 <- factor(df1$liver_168,  levels=c("Abnormal", "Normal"))
  df1$renal_168 <- factor(df1$renal_168,  levels=c("Abnormal", "Normal"))
  df1$cns_168   <- factor(df1$cns_168,    levels=c("Mechanical Ventilation (MV)", "Abnormal", "Normal"))
  df1$ethnicity <- factor(df1$ethnicity,  levels=c("HISPANIC", "BLACK", "WHITE", "ASIAN", "OTHER"))

  label(df1$gender) <- "Gender"
  label(df1$age) <- "Age"
  label(df1$ethnicity) <- "Ethnicity"

  label(df1$resp_168) <- "SOFA - Respiration at 168 hours"
  label(df1$cns_168) <- "SOFA-CNS and MV at 168 hours"
  label(df1$cv_168) <- "SOFA - Cardiovascular at 168 hours"
  label(df1$coag_168) <- "SOFA - Coagulation at 168 hours"
  label(df1$renal_168) <- "SOFA - Renal at 168 hours"
  label(df1$liver_168) <- "SOFA - Liver at 168 hours"

  t1 <- table1(~ gender + age + ethnicity + cns_168 + resp_168 + coag_168 + liver_168 + cv_168  + renal_168 | icudeath,
              data=df1,
              overall=F,
              extra.col=list(`P-value`=pvalue),
              render.missing=NULL,
              topclass="Rtable1-grid Rtable1-shade Rtable1-times"
              )

  t1flex(t1) %>% save_as_docx(path=paste0("results/table1/", cohort, "_168.docx"))

}

cohorts <- c("MIMIC", "eICU")

for (c in cohorts) {
  run_table1(c)
}
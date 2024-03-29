library(magrittr) 
library(dplyr)
library(tidyr)
library(gdata)
library(forcats)

encode_data <- function (df, cohort, time) {

    df[,'m_age'] <- NA
    df$m_age <-df$age / 10

    df <- within(df, ethnicity  <- relevel(factor(ethnicity),   ref = "WHITE"))
    df <- within(df, gender     <- relevel(factor(gender),      ref = "Male"))
    df <- within(df, icudeath   <- relevel(factor(icudeath),    ref = "Survived"))
    df <- within(df, ckd_stages <- factor(ckd_stages, levels = c(0, 1, 2, 3, 4, 5)))
    df <- within(df, ckd_stages <- fct_collapse(ckd_stages, Absent=c("0", "1", "2"), Present=c("3", "4", "5")))

    if (time == "24" & cohort == "MIMIC") {
        
        # Restrict cohort to those with los of at least 168 hours
        df <- df[df$los_icu >= 7,] # here in days

        # Apply same filtering as before
        df[,'situation168'] <- "Die"

        # alive in ICU at 168hour
        df$situation168[(difftime(df$dischtime, df$icu_intime, units = "secs")>=604800) & (difftime(df$icu_outtime, df$icu_intime, units = "secs") >=604800) & (!is.na(df$deathtime)) & (difftime(df$deathtime, df$icu_intime, units = "secs") >=604800) ]<- "Alive"
        df$situation168[(difftime(df$dischtime, df$icu_intime, units = "secs")>=604800) & (difftime(df$icu_outtime, df$icu_intime, units = "secs") >=604800) & (is.na(df$deathtime)) ] <- "Alive"

        # discharge before 168hour
        df$situation168[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (!is.na(df$deathtime)) & (difftime(df$deathtime,df$icu_outtime,units="secs") >= 259200)] <-"Discharge"
        df$situation168[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (is.na(df$deathtime)) & (difftime(df$dischtime,df$icu_outtime,units="secs") >= 259200)] <-"Discharge"
        df$situation168[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (is.na(df$deathtime)) & (difftime(df$dischtime,df$icu_outtime,units="secs") < 259200) & (!is.na(df$discharge_location)) & (df$discharge_location != "HOSPICE")] <-"Discharge"
        df$situation168[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (is.na(df$deathtime)) & (difftime(df$dischtime,df$icu_outtime,units="secs") < 259200) & (is.na(df$discharge_location)) ] <-"Discharge"

        # Keep only the patients who were alive more than 7 days and have information on ventilation
        df <- df[(df$situation168=="Alive"),]
        df <- df[(!is.na(df$resp_168)),]

        # df <- within(df, cns_24     <- relevel(factor(cns_24),      ref = "Normal"))
        # df <- within(df, coag_24    <- relevel(factor(coag_24),     ref = "Normal"))
        # df <- within(df, resp_24    <- relevel(factor(resp_24),     ref = "Normal"))
        # df <- within(df, cv_24      <- relevel(factor(cv_24),       ref = "Normal"))
        # df <- within(df, renal_24   <- relevel(factor(renal_24),    ref = "Normal"))
        # df <- within(df, liver_24   <- relevel(factor(liver_24),    ref = "Normal"))

        df <- within(df, cns_24)
        df <- within(df, coag_24)
        df <- within(df, resp_24)
        df <- within(df, cv_24)
        df <- within(df, renal_24)
        df <- within(df, liver_24)

        comps <- c("cns_24", "coag_24", "resp_24", "cv_24", "renal_24", "liver_24")

    } else if (time == "24" & cohort == "eICU") {
        
        # Restrict cohort to those with los of at least 168 hours
        df <- df[df$los_icu >= 168,] # here in hours

        # Assess situation at 168h
        # still alive at 168 hours, 168 hours = 10080 minutes
        df$situation168 <- 0
        df$situation168[(df$unitdischargeoffset - df$unitadmitoffset >= 10080) &
                        (df$hospitaldischargeoffset - df$unitadmitoffset >= 10080)] <- 1

        # remove null sofa scores at 168 hours
        df <- df[df$situation168 == 1,]

        # df <- within(df, cns_24     <- relevel(factor(cns_24),      ref = "Normal"))
        # df <- within(df, coag_24    <- relevel(factor(coag_24),     ref = "Normal"))
        # df <- within(df, resp_24    <- relevel(factor(resp_24),     ref = "Normal"))
        # df <- within(df, cv_24      <- relevel(factor(cv_24),       ref = "Normal"))
        # df <- within(df, renal_24   <- relevel(factor(renal_24),    ref = "Normal"))
        # df <- within(df, liver_24   <- relevel(factor(liver_24),    ref = "Normal"))

        df <- within(df, cns_24)
        df <- within(df, coag_24)
        df <- within(df, resp_24)
        df <- within(df, cv_24)
        df <- within(df, renal_24)
        df <- within(df, liver_24)

        comps <- c("cns_24", "coag_24", "resp_24", "cv_24", "renal_24", "liver_24")

    } else if (time == "168") {

        # df <- within(df, cns_168    <- relevel(factor(cns_168),     ref = "Normal"))
        # df <- within(df, coag_168   <- relevel(factor(coag_168),    ref = "Normal"))
        # df <- within(df, resp_168   <- relevel(factor(resp_168),    ref = "Normal"))
        # df <- within(df, cv_168     <- relevel(factor(cv_168),      ref = "Normal"))
        # df <- within(df, renal_168  <- relevel(factor(renal_168),   ref = "Normal"))
        # df <- within(df, liver_168  <- relevel(factor(liver_168),   ref = "Normal"))

        df <- within(df, cns_168)
        df <- within(df, coag_168)
        df <- within(df, resp_168)
        df <- within(df, cv_168)
        df <- within(df, renal_168)
        df <- within(df, liver_168)

        comps <- c("cns_168", "coag_168", "resp_168", "cv_168", "renal_168", "liver_168")
    }

    ready_df <- df[, append(comps, 
                            c("m_age","gender", "ethnicity", "charlson", "icudeath", 
                            "cirr_present","ckd_stages","heart_failure_present", "asthma_present", "copd_present"))]

    return (ready_df)

}

sens_analysis <- function(df, var) {

    if (var == "ckd") {
        df <- df[df$ckd_stages == "Absent", ]

    } else if (var == "cirrhosis"){
        df <- df[df$cirr_present == 0,]

    } else if (var == "heart_failure") {
        df <- df[df$heart_failure_present == 0,]

    } else if (var == "copd_asthma") {
        df <- df[df$copd_present == 0,]
        df <- df[df$asthma_present == 0,]    
    }

    return(df)
}

run_glm <- function(df, time) {

    if (time == "24") {

        m <- glm(icudeath ~ m_age + gender + ethnicity + charlson + # regular confounders
                            cns_24 + resp_24 + coag_24 + liver_24 + cv_24 + renal_24,  # SOFA components
            data = df, family = "binomial"(link=logit))

    } else if (time == "168") {

        m <- glm(icudeath ~ m_age + gender + ethnicity + charlson + # regular confounders
                            cns_168 + resp_168 + coag_168 + liver_168 + cv_168 + renal_168,  # SOFA components
            data = df, family = "binomial"(link=logit))
    }

    summary(m)
    m_OR <- exp(cbind(OR = coef(m), confint(m), N = log(nobs(m)) ))

    return (m_OR)

}

cohorts <- c("MIMIC","eICU")
times <- c("24","168")
vars_to_remove <- c("all", "ckd", "cirrhosis", "heart_failure", "copd_asthma")

for (c in cohorts) {
    for (t in times) {
        for (v in vars_to_remove) {
            df <- read.csv(paste0("data/cohorts/", c, "_", t, ".csv"))
            df <- encode_data(df, c, t)
            df <- sens_analysis(df, v)
            m_OR <- run_glm(df, t)
            write.csv(m_OR, paste0("results/glm/ITB/", c, "_", t, "_", v, ".csv"))
        }
    }
}
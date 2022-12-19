encode_data <- function (df, cohort, time, sens_anl) {

    df[,'m_age'] <- NA
    df$m_age <-df$age / 10

    df <- within(df, ethnicity  <- relevel(factor(ethnicity),   ref = "WHITE"))
    df <- within(df, gender     <- relevel(factor(gender),      ref = "Male"))
    df <- within(df, icudeath   <- relevel(factor(icudeath),    ref = "Survived"))

    if (time == "24") {

        df <- within(df, cns_24     <- relevel(factor(cns_24),      ref = "Normal"))
        df <- within(df, coag_24    <- relevel(factor(coag_24),     ref = "Normal"))
        df <- within(df, resp_24    <- relevel(factor(resp_24),     ref = "Normal"))
        df <- within(df, cv_24      <- relevel(factor(cv_24),       ref = "Normal"))
        df <- within(df, renal_24   <- relevel(factor(renal_24),    ref = "Normal"))
        df <- within(df, liver_24   <- relevel(factor(liver_24),    ref = "Normal"))

        comps <- c("cns_24", "coag_24", "resp_24", "cv_24", "renal_24", "liver_24")

    } else if (time == "168") {

        df <- within(df, cns_168    <- relevel(factor(cns_168),     ref = "Normal"))
        df <- within(df, coag_168   <- relevel(factor(coag_168),    ref = "Normal"))
        df <- within(df, resp_168   <- relevel(factor(resp_168),    ref = "Normal"))
        df <- within(df, cv_168     <- relevel(factor(cv_168),      ref = "Normal"))
        df <- within(df, renal_168  <- relevel(factor(renal_168),   ref = "Normal"))
        df <- within(df, liver_168  <- relevel(factor(liver_168),   ref = "Normal"))

        comps <- c("cns_168", "coag_168", "resp_168", "cv_168", "renal_168", "liver_168")
    }

    if (s == "no_cirrhosis") {

        df <- df[is.na(df$cirrhosis_id),]

    } else if (s == "no_esrd") {

        df <- df[is.na(df$esrd_id),]

    } # else we just keep df as is

    ready_df <- df[, append(comps, c("m_age","gender", "ethnicity", "icudeath"))]

    write.csv(ready_df, 'data/d.csv')

    return (ready_df)

}

run_glm <- function(df, time) {

    if (time == "24") {

        m <- glm(icudeath ~  m_age + gender + ethnicity + cns_24 + resp_24 + coag_24 + liver_24 + cv_24 + renal_24,
            data = df, family = "binomial"(link=logit))

    } else if (time == "168") {

        m <- glm(icudeath ~  m_age + gender + ethnicity + cns_168 + resp_168 + coag_168 + liver_168 + cv_168 + renal_168,
            data = df, family = "binomial"(link=logit))
    }

    summary(m)
    m_OR <- exp(cbind(OR = coef(m), confint(m)))

    return (m_OR)

}

cohorts <- c("MIMIC", "eICU")
times <- c("24","168")
sens_analys <- c("all", "no_cirrhosis", "no_esrd")

for (c in cohorts) {
    for (t in times) {
        for (s in sens_analys) {

            df <- read.csv(paste0("data/cohorts/", c, "_", t, ".csv"))
            df <- encode_data(df, c, t, s)
            m_OR <- run_glm(df, t)
            write.csv(m_OR, paste0("results/glm/", c, "_", t, "_", s, ".csv"))
        }
    }
}
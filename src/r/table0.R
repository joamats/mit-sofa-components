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

  df <- read_csv(paste0("data/", cohort, "_data.csv"), show_col_types = FALSE)

  if (cohort == "eICU") {
    # Encode ethnicity
    df$ethnicity[df$ethnicity == "African American"] <- "BLACK"
    df$ethnicity[df$ethnicity == "Asian"] <- "ASIAN"
    df$ethnicity[df$ethnicity == "Caucasian"] <- "WHITE"
    df$ethnicity[df$ethnicity == "Hispanic"] <- "HISPANIC"
    df$ethnicity[df$ethnicity == "Native American" | df$ethnicity == "Other/Unknown" | is.na(df$ethnicity)] <- "OTHER"

    # Encode gender
    df$gender[df$gender == 0 ] <- "Female"
    df$gender[df$gender == 1 ] <- "Male"

  } else if (cohort == "MIMIC") {

    df$ethnicity <- df$race
    df$ethnicity[ df$race == 'OTHER' 
                      | df$race == 'UNABLE TO OBTAIN'
                      | df$race == 'UNKNOWN'
                      | df$race == 'MULTIPLE RACE/ETHNICITY'
                      | df$race == 'PATIENT DECLINED TO ANSWER'
                      | df$race == 'AMERICAN INDIAN/ALASKA NATIVE'
                      | df$race == 'NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER'] <- "OTHER" #7

    df$ethnicity[ df$race == 'HISPANIC OR LATINO' 
                      | df$race == 'HISPANIC/LATINO - GUATEMALAN'
                      | df$race == 'HISPANIC/LATINO - PUERTO RICAN'
                      | df$race == 'HISPANIC/LATINO - DOMINICAN'
                      | df$race == 'HISPANIC/LATINO - MEXICAN'
                      | df$race == 'HISPANIC/LATINO - SALVADORAN'
                      | df$race == 'HISPANIC/LATINO - COLUMBIAN'
                      | df$race == 'HISPANIC/LATINO - HONDURAN'
                      | df$race == 'HISPANIC/LATINO - CENTRAL AMERICAN'
                      | df$race == 'HISPANIC/LATINO - CUBAN'
                      | df$race == 'SOUTH AMERICAN'] <- "HISPANIC" #11

    df$ethnicity[ df$race == 'ASIAN' 
                      | df$race == 'ASIAN - KOREAN'
                      | df$race == 'ASIAN - SOUTH EAST ASIAN'
                      | df$race == 'ASIAN - ASIAN INDIAN'
                      | df$race == 'ASIAN - CHINESE'] <- "ASIAN" #4

    df$ethnicity[ df$race == 'BLACK/AFRICAN AMERICAN' 
                      | df$race == 'BLACK/CARIBBEAN ISLAND'
                      | df$race == 'BLACK/AFRICAN'
                      | df$race == 'BLACK/CAPE VERDEAN'] <- "BLACK" #4

    df$ethnicity[ df$race == 'WHITE' 
                      | df$race == 'WHITE - OTHER EUROPEAN'
                      | df$race == 'WHITE - EASTERN EUROPEAN'
                      | df$race == 'WHITE - BRAZILIAN'
                      | df$race == 'WHITE - RUSSIAN'
                      | df$race == 'PORTUGUESE'] <- "WHITE" #6

    df$gender[df$gender == "M"] <- "Male"
    df$gender[df$gender == "F"] <- "Female"
  }

  # Factor variables
  df$gender <- factor(df$gender, level=c('Male',
                                        'Female'))

  df$ethnicity <- factor(df$ethnicity, levels=c("HISPANIC",
                                                "BLACK",
                                                "WHITE",
                                                "ASIAN",
                                                "OTHER"))

  label(df$gender) <- "Sex"
  label(df$ethnicity) <- "Ethnicity"

  t1 <- table1(~ gender + ethnicity,
            data=df,
            overall=T,
            topclass="Rtable1-grid Rtable1-shade Rtable1-times",
            render.categorical=render.categorical, render.strat=render.strat
            )

  t1flex(t1) %>% save_as_docx(path=paste0("results/flow_chart/", cohort, "_original.docx"))

}

cohorts <- c("MIMIC", "eICU")

for (c in cohorts) {
  run_table1(c)
}
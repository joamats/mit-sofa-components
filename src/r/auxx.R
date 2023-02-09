library(dplyr)
library(tidyverse)


df <- read_csv("data/MIMIC_data.csv", show_col_types = FALSE)

## 24hours is 1*24*60*60 = 86400 seconds
df[,'out_in'] <- 5
df$out_in[difftime(df$icu_outtime, df$icu_intime, units = "secs") <  86400] <- 1
df$out_in[difftime(df$icu_outtime, df$icu_intime, units = "secs") >= 86400] <- 0

df[,'death_in'] <- 5
df$death_in[difftime(df$deathtime, df$icu_intime, units = "secs") <=  86400] <- 1
df$death_in[difftime(df$deathtime, df$icu_intime, units = "secs") > 86400] <- 0

df[,'discharge_in'] <- 5
df$discharge_in[difftime(df$dischtime, df$icu_intime, units = "secs") <=  86400] <- 1
df$discharge_in[difftime(df$dischtime, df$icu_intime, units = "secs") > 86400] <- 0

# Transform combination of hosp seq and icu seq into a single seq per patient
df0 <- df %>% arrange(subject_id, hospstay_seq, icustay_seq) %>% group_by(subject_id) %>% mutate(abs_seq = row_number())

df1 <-df0[!(df0$out_in==1),]
df2 <-df1[!(df1$death_in==1),]
df3 <-df2[!(df2$discharge_in==1),]
print(paste0("Patients removed who were not 24h (died or discharged): ", nrow(df) - nrow(df3)))

# Get the new min seq
df4 <- df3 %>% group_by(subject_id) %>% mutate(MinSeq = min(abs_seq, na.rm=T))

# Remove patients who are not on the first stay with 24h
df5 <-df4[(df4$abs_seq==df4$MinSeq),]
print(paste0("Patients removed who were not on the 1st stay with 24h: ", nrow(df4) - nrow(df5)))


library(tidyverse)
library(lubridate)

rawdata<- read_csv("data/MIMIC_data.csv", show_col_types = FALSE)
df1 <-rawdata

print(paste0("Initial Number of Patients: ", nrow(rawdata)))


## 24hours is 1*24*60*60 = 86400 seconds
df1[,'out_in'] <- 5
df1$out_in[difftime(df1$icu_outtime, df1$icu_intime, units = "secs") <  86400] <- 1
df1$out_in[difftime(df1$icu_outtime, df1$icu_intime, units = "secs") >= 86400] <- 0

df1[,'death_in'] <- 5
df1$death_in[difftime(df1$deathtime, df1$icu_intime, units = "secs") <=  86400] <- 1
df1$death_in[difftime(df1$deathtime, df1$icu_intime, units = "secs") > 86400] <- 0

df1[,'discharge_in'] <- 5
df1$discharge_in[difftime(df1$dischtime, df1$icu_intime, units = "secs") <=  86400] <- 1
df1$discharge_in[difftime(df1$dischtime, df1$icu_intime, units = "secs") > 86400] <- 0

df2 <-df1[((df1$first_icu_stay=="TRUE") & (df1$first_hosp_stay=="TRUE")),]
print(paste0("Patients removed who were not on a 1st stay: ", nrow(df1) - nrow(df2)))

df3 <-df2[!(df2$out_in==1),]
df4 <-df3[!(df3$death_in==1),]
df5 <-df4[!(df4$discharge_in==1),]
print(paste0("Patients removed who were not 24h (died or discharged): ", nrow(df2) - nrow(df5)))

df6 <- df5[!(is.na(df5$cns_24)),]
print(paste0("Patients removed without cns info: ", nrow(df5) - nrow(df6)))

final_df <- df6
print(paste0("Final Number of Patients (24h): ", nrow(final_df)))

final_df$t1_ethnicity <- final_df$race
final_df$t1_ethnicity[final_df$race == 'OTHER' 
                      | final_df$race == 'UNABLE TO OBTAIN'
                      | final_df$race == 'UNKNOWN'
                      | final_df$race == 'AMERICAN INDIAN/ALASKA NATIVE'] <- "OTHER"

final_df[,'t1_gender'] <- 0
final_df$t1_gender[final_df$gender == "M"] <- "Male"
final_df$t1_gender[final_df$gender == "F"] <- "Female"

final_df$t1_sepsis3 <- final_df$sepsis3
final_df$t1_sepsis3[final_df$sepsis3 == 'TRUE'] <- "Yes"
final_df$t1_sepsis3[is.na(final_df$sepsis3)] <- "No"

final_df['combinedeathtime'] = final_df$deathtime

final_df[,'t1_icudeath'] <- 0
## 72hours = 72*60*60 = 259200, use seconds for the most accurate calculation
final_df$t1_icudeath[(!is.na(final_df$combinedeathtime)) & ( difftime(final_df$combinedeathtime, final_df$icu_intime, units = "secs") >=0) & (difftime(final_df$icu_outtime, final_df$combinedeathtime, units = "secs") >=0 ) ] <- 1
final_df$t1_icudeath[(!is.na(final_df$combinedeathtime)) & ( difftime(final_df$combinedeathtime, final_df$icu_intime, units = "secs") >=0) & (difftime(final_df$combinedeathtime, final_df$icu_outtime, units = "secs") >0 ) & (difftime(final_df$combinedeathtime, final_df$icu_outtime, units = "secs") <259200 ) ] <- 1

final_df$t1_icudeath[final_df$t1_icudeath == 0] <- "Survived or discharged to other locations within 72 hours of ICU discharge"
final_df$t1_icudeath[final_df$t1_icudeath == 1] <- "ICU death within 72 hours of ICU discharge"

final_df[,'m1_icudeath']<- 0
final_df$m1_icudeath[(!is.na(final_df$combinedeathtime)) & ( difftime(final_df$combinedeathtime, final_df$icu_intime, units = "secs") >=0) & (difftime(final_df$icu_outtime, final_df$combinedeathtime, units = "secs") >=0 ) ] <- 1
final_df$m1_icudeath[(!is.na(final_df$combinedeathtime)) & ( difftime(final_df$combinedeathtime, final_df$icu_intime, units = "secs") >=0) & (difftime(final_df$combinedeathtime, final_df$icu_outtime, units = "secs") >0 ) & (difftime(final_df$combinedeathtime, final_df$icu_outtime, units = "secs") <259200 ) ] <- 1


## for the SOFA_RESP part
final_df[, 'newvent24'] <- 5
final_df$newvent24[final_df$vent_24 == 'InvasiveVent'
                   | final_df$vent_24 == 'HFNC'
                   | final_df$vent_24 == 'Tracheostomy'
                   | final_df$vent_24 == 'NonInvasiveVent'] <- 1
final_df$newvent24[is.na(final_df$vent_24) ]<- 0
final_df$newvent24[final_df$vent_24 == 'SupplementalOxygen'] <- 0


final_df[, 'newvent168'] <- 5
final_df$newvent168[final_df$vent_168 == 'InvasiveVent'
                    | final_df$vent_168 == 'HFNC'
                    | final_df$vent_168 == 'Tracheostomy'
                    | final_df$vent_168 == 'NonInvasiveVent'] <- 1
final_df$newvent168[is.na(final_df$vent_168) ]<- 0
final_df$newvent168[final_df$vent_168 == 'SupplementalOxygen'] <- 0             
table(final_df$newvent168)                     


final_df[, 'newventlast'] <- 5
final_df$newventlast[final_df$vent_last == 'InvasiveVent'
                     | final_df$vent_last == 'HFNC'
                     | final_df$vent_last == 'Tracheostomy'
                     | final_df$vent_last == 'NonInvasiveVent'] <- 1
final_df$newventlast[is.na(final_df$vent_last) ]<- 0
final_df$newventlast[final_df$vent_168 == 'SupplementalOxygen'] <- 0  

final_df$t1_resp24 = final_df$resp_24
final_df$t1_resp24[final_df$newvent24 == 1] <- "Abnormal"
final_df$t1_resp24[final_df$newvent24 == 0 & final_df$resp_24 == 0] <- "Normal"
final_df$t1_resp24[final_df$newvent24 == 0 & final_df$resp_24 >= 1] <- "Abnormal"

final_df$t1_coag24 = final_df$coag_24
final_df$t1_coag24[final_df$coag_24 == 0] <- "Normal"
final_df$t1_coag24[final_df$coag_24 >= 1] <- "Abnormal"

final_df$t1_liver24 = final_df$liver_24
final_df$t1_liver24[final_df$liver_24 == 0] <- "Normal"
final_df$t1_liver24[final_df$liver_24 >= 1] <- "Abnormal"

final_df$t1_cv24 = final_df$cv_24
final_df$t1_cv24[final_df$cv_24 == 0] <- "Normal"
final_df$t1_cv24[final_df$cv_24 >= 1] <- "Abnormal"

final_df$t1_cns24 = final_df$cns_24
final_df$t1_cns24[final_df$vent_24 == "InvasiveVent"] <- "Mechanical Ventilation (MV)"
final_df$t1_cns24[final_df$vent_24 != "InvasiveVent" & final_df$cns_24 == 0] <- "No MV and normal CNS"
final_df$t1_cns24[is.na(final_df$vent_24) & final_df$cns_24 == 0] <- "No MV and normal CNS"
final_df$t1_cns24[final_df$vent_24 != "InvasiveVent" & final_df$cns_24 >= 1] <- "No MV and abnormal CNS"
final_df$t1_cns24[is.na(final_df$vent_24) & final_df$cns_24 >= 1] <- "No MV and abnormal CNS"



final_df$t1_renal24 = final_df$renal_24
final_df$t1_renal24[final_df$renal_24 == 0] <- "Normal"
final_df$t1_renal24[final_df$renal_24 >= 1] <- "Abnormal"

write.csv(df,"data/cohorts/MIMIC_24.csv")


# 168 hours
df <- final_df
df[,'t1_168situation'] <- "Die"

# alive in ICU at 168hour
df$t1_168situation[(difftime(df$dischtime, df$icu_intime, units = "secs")>=604800) & (difftime(df$icu_outtime, df$icu_intime, units = "secs") >=604800) & (!is.na(df$deathtime)) & (difftime(df$deathtime, df$icu_intime, units = "secs") >=604800) ]<- "Alive"
df$t1_168situation[(difftime(df$dischtime, df$icu_intime, units = "secs")>=604800) & (difftime(df$icu_outtime, df$icu_intime, units = "secs") >=604800) & (is.na(df$deathtime)) ] <- "Alive"

# discharge before 168hour
df$t1_168situation[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (!is.na(df$deathtime)) & (difftime(df$deathtime,df$icu_outtime,units="secs") >= 259200)] <-"Discharge"
df$t1_168situation[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (is.na(df$deathtime)) & (difftime(df$dischtime,df$icu_outtime,units="secs") >= 259200)] <-"Discharge"
df$t1_168situation[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (is.na(df$deathtime)) & (difftime(df$dischtime,df$icu_outtime,units="secs") < 259200) & (!is.na(df$discharge_location)) & (df$discharge_location != "HOSPICE")] <-"Discharge"
df$t1_168situation[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (is.na(df$deathtime)) & (difftime(df$dischtime,df$icu_outtime,units="secs") < 259200) & (is.na(df$discharge_location)) ] <-"Discharge"

final_d <- df[(df$t1_168situation=="Alive"),]

print(paste0("Patients removed that did not spend 7 days (died or were discharged): ", nrow(df) - nrow(final_d)))

final_df <- final_d[(!is.na(final_d$resp_168)),]
print(paste0("Patients removed with missing resp info: ", nrow(final_d) - nrow(final_df)))

final_df[,'t1_resp168'] <- 5
final_df$t1_resp168[final_df$newvent168 == 1] <- "Abnormal"
final_df$t1_resp168[final_df$newvent168 == 0 & final_df$resp_168 == 0] <- "Normal"
final_df$t1_resp168[final_df$newvent168 == 0 & final_df$resp_168 >= 1] <- "Abnormal"

final_df[,'t1_coag168'] <- 5
final_df$t1_coag168[ final_df$coag_168 == 0] <- "Normal"
final_df$t1_coag168[ final_df$coag_168 >= 1] <- "Abnormal"

final_df[,'t1_liver168'] <- 5
final_df$t1_liver168[ final_df$liver_168 == 0] <- "Normal"
final_df$t1_liver168[ final_df$liver_168 >= 1] <- "Abnormal"

final_df[,'t1_cv168'] <- 5
final_df$t1_cv168[ final_df$cv_168 == 0] <- "Normal"
final_df$t1_cv168[ final_df$cv_168 >= 1] <- "Abnormal"

final_df[,'t1_cns168'] <- 5
final_df$t1_cns168[final_df$vent_168 == "InvasiveVent"] <- "Mechanical Ventilation (MV)"
final_df$t1_cns168[final_df$vent_168 != "InvasiveVent"  &(!is.na(final_df$cns_168)) & (final_df$cns_168 == 0)] <- "No MV and normal CNS"
final_df$t1_cns168[is.na(final_df$vent_168)  &(!is.na(final_df$cns_168)) & (final_df$cns_168 == 0)] <- "No MV and normal CNS"
final_df$t1_cns168[final_df$vent_168 != "InvasiveVent"  &(!is.na(final_df$cns_168)) & (final_df$cns_168 >= 1)] <- "No MV and abnormal CNS"
final_df$t1_cns168[is.na(final_df$vent_168)  &(!is.na(final_df$cns_168)) & (final_df$cns_168 >= 1)] <- "No MV and abnormal CNS"

final_df[,'t1_renal168'] <- 5
final_df$t1_renal168[ final_df$renal_168 == 0] <- "Normal"
final_df$t1_renal168[ final_df$renal_168 >= 1] <- "Abnormal"

print(paste0("Final Number of Patients (168h): ", nrow(final_df)))

write.csv(df,"data/cohorts/MIMIC_168.csv")

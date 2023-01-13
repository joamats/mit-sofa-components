library(tidyverse)
library(lubridate)

rawdata<- read_csv("data/MIMIC_data.csv", show_col_types = FALSE)
df1 <- rawdata

df1$age <- df1$admission_age
df1$esrd_id <- df1$ESRD_id

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

# Map ethnicity
final_df$ethnicity <- final_df$race
final_df$ethnicity[ final_df$race == 'OTHER' 
                  | final_df$race == 'UNABLE TO OBTAIN'
                  | final_df$race == 'UNKNOWN'
                  | final_df$race == 'MULTIPLE RACE/ETHNICITY'
                  | final_df$race == 'PATIENT DECLINED TO ANSWER'
                  | final_df$race == 'AMERICAN INDIAN/ALASKA NATIVE'
                  | final_df$race == 'NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER'] <- "OTHER" #7

final_df$ethnicity[ final_df$race == 'HISPANIC OR LATINO' 
                  | final_df$race == 'HISPANIC/LATINO - GUATEMALAN'
                  | final_df$race == 'HISPANIC/LATINO - PUERTO RICAN'
                  | final_df$race == 'HISPANIC/LATINO - DOMINICAN'
                  | final_df$race == 'HISPANIC/LATINO - MEXICAN'
                  | final_df$race == 'HISPANIC/LATINO - SALVADORAN'
                  | final_df$race == 'HISPANIC/LATINO - COLUMBIAN'
                  | final_df$race == 'HISPANIC/LATINO - HONDURAN'
                  | final_df$race == 'HISPANIC/LATINO - CENTRAL AMERICAN'
                  | final_df$race == 'HISPANIC/LATINO - CUBAN'
                  | final_df$race == 'SOUTH AMERICAN'] <- "HISPANIC" #11

final_df$ethnicity[ final_df$race == 'ASIAN' 
                  | final_df$race == 'ASIAN - KOREAN'
                  | final_df$race == 'ASIAN - SOUTH EAST ASIAN'
                  | final_df$race == 'ASIAN - ASIAN INDIAN'
                  | final_df$race == 'ASIAN - CHINESE'] <- "ASIAN" #4

final_df$ethnicity[ final_df$race == 'BLACK/AFRICAN AMERICAN' 
                  | final_df$race == 'BLACK/CARIBBEAN ISLAND'
                  | final_df$race == 'BLACK/AFRICAN'
                  | final_df$race == 'BLACK/CAPE VERDEAN'] <- "BLACK" #4

final_df$ethnicity[ final_df$race == 'WHITE' 
                  | final_df$race == 'WHITE - OTHER EUROPEAN'
                  | final_df$race == 'WHITE - EASTERN EUROPEAN'
                  | final_df$race == 'WHITE - BRAZILIAN'
                  | final_df$race == 'WHITE - RUSSIAN'
                  | final_df$race == 'PORTUGUESE'] <- "WHITE" #6


# Map service typ to Medical vs. non-Medical/Surgical
final_df$medical <- final_df$first_service
final_df <- final_df %>% mutate(medical= ifelse(
                    medical == 'CMED' 
                  | medical == 'NMED'
                  | medical == 'MED'
                  | medical == 'OMED', 1, 0))

final_df$gender[final_df$gender == "M"] <- "Male"
final_df$gender[final_df$gender == "F"] <- "Female"

final_df$sepsis3[final_df$sepsis3 == 'TRUE'] <- "Yes"
final_df$sepsis3[is.na(final_df$sepsis3)] <- "No"

# Encode key comorbidities
final_df <- final_df %>% mutate(sepsis3= ifelse(sepsis3=="Yes",1,0))

# Encode cirrhosis
final_df$cirr_present <- final_df$cirrhosis_id 
final_df <- final_df %>% mutate(cirr_present= ifelse(is.na(cirr_present),0,1))

# Encode admission code status
final_df$full_therapy <- final_df$first_code
final_df <- final_df %>% mutate(full_therapy= ifelse(full_therapy=="Full code",1,0))

# Omit discharge code status
final_df$last_code <- NULL

final_df['combinedeathtime'] = final_df$deathtime

final_df[,'icudeath'] <- 0

## 72hours = 72*60*60 = 259200, use seconds for the most accurate calculation
final_df$icudeath[(!is.na(final_df$combinedeathtime)) & (difftime(final_df$combinedeathtime, final_df$icu_intime, units = "secs") >=0) & (difftime(final_df$icu_outtime, final_df$combinedeathtime, units = "secs") >=0 ) ] <- 1
final_df$icudeath[(!is.na(final_df$combinedeathtime)) & (difftime(final_df$combinedeathtime, final_df$icu_intime, units = "secs") >=0) & (difftime(final_df$combinedeathtime, final_df$icu_outtime, units = "secs") >0 ) & (difftime(final_df$combinedeathtime, final_df$icu_outtime, units = "secs") <259200 ) ] <- 1

# "Survived or discharged to other locations within 72 hours of ICU discharge"
# "ICU death within 72 hours of ICU discharge"
final_df$icudeath[final_df$icudeath == 0] <- "Survived"
final_df$icudeath[final_df$icudeath == 1] <- "Died"

# Encode mechanical ventilation
final_df[, 'newvent24'] <- 5
final_df$newvent24[final_df$vent_24 == 'InvasiveVent'
                   | final_df$vent_24 == 'HFNC'
                   | final_df$vent_24 == 'Tracheostomy'
                   | final_df$vent_24 == 'NonInvasiveVent'] <- 1
final_df$newvent24[is.na(final_df$vent_24) ]<- 0
final_df$newvent24[final_df$vent_24 == 'SupplementalOxygen'] <- 0

# Encode SOFA components to normal vs. abnormal 
abnormal = c(3,4)
normal = c(0,1,2)

# First iteration -> following rule was applied
# No MV and abnormal CNS    = Abnormal
# No MV and normal CNS      = Normal
# MV and abnormal CNS       = Abnormal
# MV and normal CNS         = Abnormal

# Rules for Resp
# final_df$resp_24[final_df$newvent24 == 1] <- "Abnormal"
# final_df$resp_24[final_df$newvent24 == 0 & final_df$resp_24 %in% normal] <- "Normal"
# final_df$resp_24[final_df$newvent24 == 0 & final_df$resp_24 %in% abnormal)] <- "Abnormal"

# Rules for CNS
# final_df$cns_24[final_df$vent_24 == 'InvasiveVent'] <- "Mechanical Ventilation (MV)"
# final_df$cns_24[final_df$vent_24 != 'InvasiveVent' & final_df$cns_24 %in% normal] <- "Normal"
# final_df$cns_24[is.na(final_df$vent_24) & final_df$cns_24 %in% normal] <- "Normal"
# final_df$cns_24[final_df$vent_24 != 'InvasiveVent' & final_df$cns_24 %in% abnormal)] <- "Abnormal"
# final_df$cns_24[is.na(final_df$vent_24) & final_df$cns_24 %in% abnormal] <- "Abnormal"

# Second iteration -> no rule -> instead use Ventilation Yes/No as is and implement interaction term in GLM
final_df$resp_24[final_df$resp_24 %in% normal] <- "Normal"
final_df$resp_24[final_df$resp_24 %in% abnormal] <- "Abnormal"

final_df$cns_24[final_df$cns_24 %in% normal] <- "Normal"
final_df$cns_24[final_df$cns_24 %in% abnormal] <- "Abnormal"

final_df$coag_24[final_df$coag_24 %in% normal] <- "Normal"
final_df$coag_24[final_df$coag_24 %in% abnormal] <- "Abnormal"

final_df$liver_24[final_df$liver_24 %in% normal] <- "Normal"
final_df$liver_24[final_df$liver_24 %in% abnormal] <- "Abnormal"

final_df$cv_24[final_df$cv_24 %in% normal] <- "Normal"
final_df$cv_24[final_df$cv_24 %in% abnormal] <- "Abnormal"

final_df$renal_24[final_df$renal_24 %in% normal] <- "Normal"
final_df$renal_24[final_df$renal_24 %in% abnormal] <- "Abnormal"

write.csv(final_df,"data/cohorts/MIMIC_24.csv")

# 168 hours
df <- final_df
df[,'situation168'] <- "Die"

# alive in ICU at 168hour
df$situation168[(difftime(df$dischtime, df$icu_intime, units = "secs")>=604800) & (difftime(df$icu_outtime, df$icu_intime, units = "secs") >=604800) & (!is.na(df$deathtime)) & (difftime(df$deathtime, df$icu_intime, units = "secs") >=604800) ]<- "Alive"
df$situation168[(difftime(df$dischtime, df$icu_intime, units = "secs")>=604800) & (difftime(df$icu_outtime, df$icu_intime, units = "secs") >=604800) & (is.na(df$deathtime)) ] <- "Alive"

# discharge before 168hour
df$situation168[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (!is.na(df$deathtime)) & (difftime(df$deathtime,df$icu_outtime,units="secs") >= 259200)] <-"Discharge"
df$situation168[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (is.na(df$deathtime)) & (difftime(df$dischtime,df$icu_outtime,units="secs") >= 259200)] <-"Discharge"
df$situation168[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (is.na(df$deathtime)) & (difftime(df$dischtime,df$icu_outtime,units="secs") < 259200) & (!is.na(df$discharge_location)) & (df$discharge_location != "HOSPICE")] <-"Discharge"
df$situation168[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (is.na(df$deathtime)) & (difftime(df$dischtime,df$icu_outtime,units="secs") < 259200) & (is.na(df$discharge_location)) ] <-"Discharge"

# Keep only the patients who were alive more than 7 days
final_d <- df[(df$situation168=="Alive"),]

print(paste0("Patients removed that did not spend 7 days (died or were discharged): ", nrow(df) - nrow(final_d)))

final_df <- final_d[(!is.na(final_d$resp_168)),]
print(paste0("Patients removed with missing resp info: ", nrow(final_d) - nrow(final_df)))

final_df[, 'newvent168'] <- 5
final_df$newvent168[final_df$vent_168 == 'InvasiveVent'
                    | final_df$vent_168 == 'HFNC'
                    | final_df$vent_168 == 'Tracheostomy'
                    | final_df$vent_168 == 'NonInvasiveVent'] <- 1
final_df$newvent168[is.na(final_df$vent_168) ] <- 0
final_df$newvent168[final_df$vent_168 == 'SupplementalOxygen'] <- 0    

final_df$resp_168[final_df$resp_168 %in% normal] <- "Normal"
final_df$resp_168[final_df$resp_168 %in% abnormal] <- "Abnormal"

final_df$cns_168[final_df$cns_168 %in% normal] <- "Normal"
final_df$cns_168[final_df$cns_168 %in% abnormal] <- "Abnormal"

final_df$coag_168[ final_df$coag_168 %in% normal] <- "Normal"
final_df$coag_168[ final_df$coag_168 %in% abnormal] <- "Abnormal"

final_df$liver_168[ final_df$liver_168 %in% normal] <- "Normal"
final_df$liver_168[ final_df$liver_168 %in% abnormal] <- "Abnormal"

final_df$cv_168[ final_df$cv_168 %in% normal] <- "Normal"
final_df$cv_168[ final_df$cv_168 %in% abnormal] <- "Abnormal"

final_df$renal_168[ final_df$renal_168 %in% normal] <- "Normal"
final_df$renal_168[ final_df$renal_168 %in% abnormal] <- "Abnormal"

print(paste0("Final Number of Patients (168h): ", nrow(final_df)))

write.csv(final_df,"data/cohorts/MIMIC_168.csv")
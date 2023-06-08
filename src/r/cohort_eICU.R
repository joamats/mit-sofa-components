library(tidyverse)
library(lubridate)

rawdata <- read_csv("data/eICU_data.csv", show_col_types = FALSE)

print(paste0("Initial Number of Patients: ", nrow(rawdata)))

rawdata$age[rawdata$age == "> 89"] <- "90"
rawdata$age[is.na(rawdata$age)] <- "0"
rawdata$age <- as.numeric(rawdata$age)
df1 <- rawdata

## in eICU offset are in minutes, 24*60 = 1440
# Remove patients discharged from ICU in less than 24 hours
df1[,'out_in'] <- 5
df1$out_in[df1$unitdischargeoffset - df1$unitadmitoffset <  1440] <- 1
df1$out_in[df1$unitdischargeoffset - df1$unitadmitoffset >= 1440] <- 0

# Remove patients discharged from hospital in less than 24 hours
df1[,'discharge_in'] <- 5
df1$discharge_in[df1$hospitaldischargeoffset - df1$unitadmitoffset <  1440] <- 1
df1$discharge_in[df1$hospitaldischargeoffset - df1$unitadmitoffset >= 1440] <- 0

# Remove patients who were not 24h in the ICU / hosptial
df2 <-df1[!(df1$out_in==1),]
df3 <-df2[!(df2$discharge_in==1),]
print(paste0("Patients removed who were not 24h: ", nrow(df1) - nrow(df3)))

df4 <- df3 %>% group_by(patienthealthsystemstayid) %>% mutate(MinSeq = min(unitvisitnumber, na.rm=T))

# Remove patients who are not on the first stay with 24h
df5 <-df4[(df4$unitvisitnumber==df4$MinSeq),]
print(paste0("Patients removed who were not on the 1st stay with 24h: ", nrow(df4) - nrow(df5)))

# Remove patients without resp info
df5 <- df4[!(is.na(df4$resp_24)),]
print(paste0("Patients removed without resp info: ", nrow(df4) - nrow(df5)))

# Remove patients under 18
df6 <- df5[(df5$age>=18),]
print(paste0("Patients removed that were less than 18yo: ", nrow(df5) - nrow(df6)))

# Remove patients without gender info
df7 <- df6[!is.na(df6$gender), ]
print(paste0("Patients removed that had no gender info: ", nrow(df6) - nrow(df7)))

# Remove patients with Elective Admission 
df8 <- df7[df7$adm_elective == 0 | is.na(df7$adm_elective),]
print(paste0("Patients removed with Elective Admission: ", nrow(df7) - nrow(df8)))

# Get final cohort
df <- df8

# Encode variables
# Create column for icu_death
df$icudeath <- 0
df$icudeath[df$unitdischargestatus == "Expired"] <- 1
df$icudeath[df$hospitaldischargestatus == "Expired" & (df$hospitaldischargeoffset - df$unitdischargeoffset < 4320)] <-1
df$icudeath[df$hospitaldischargestatus == "Expired" & (df$hospitaldischargeoffset < df$unitdischargeoffset)] <- 1

# 0 = "Survived or discharged to other locations within 72 hours of ICU discharge"
# 1 = "ICU death or Discharged to Hospice within 72 hours of ICU discharge"

df$icudeath[df$icudeath == 0] <- "Survived"
df$icudeath[df$icudeath == 1] <- "Died"

# Encode Sepsis 3 diagnosis
df$sepsis3[df$sepsis3 == 'TRUE'] <- "Yes"
df$sepsis3[is.na(df$sepsis3)] <- "No"

# Encode ethnicity
df$ethnicity[df$ethnicity == "African American"] <- "BLACK"
df$ethnicity[df$ethnicity == "Asian"] <- "ASIAN"
df$ethnicity[df$ethnicity == "Caucasian"] <- "WHITE"
df$ethnicity[df$ethnicity == "Hispanic"] <- "HISPANIC"
df$ethnicity[df$ethnicity == "Native American" | df$ethnicity == "Other/Unknown" | is.na(df$ethnicity)] <- "OTHER"

# Map service typ to Medical vs. non-Medical/Surgical
#df$medical <- df$specialty
#df$medical[is.na(df$medical)] <- "other"
#df <- df %>% mutate(medical= ifelse(
#  medical == 'urology' 
#  | medical == 'unknown'
#  | medical == 'radiology'
#  | medical == 'obstetrics/gynecology'
#  | grepl('surgery', df$medical, ignore.case = TRUE) == 1, 0, 1))

# Encode key comorbidities
df <- df %>% mutate(ckd_stages= ifelse(is.na(ckd_stages),0,ckd_stages))
df <- df %>% mutate(sepsis3= ifelse(sepsis3=="Yes",1,0))

# Encode cirrhosis
df$cirr_present <- df$cirrhosis_id 
df <- df %>% mutate(cirr_present= ifelse(is.na(cirr_present),0,1))

# Encode admission code status
df$full_therapy <- df$first_code
df <- df %>% mutate(full_therapy= ifelse(full_therapy=="Full therapy",1,0))

# Omit discharge code status
df$last_code <- NULL

# Encode gender
df$gender[df$gender == 0 ] <- "Female"
df$gender[df$gender == 1 ] <- "Male"

# Encode age
df$age[df$age == "> 89"] <- 90

# Encode mechanical ventilation at 24h
df[, 'newvent24'] <- NA
df$newvent24[!is.na(df$mv24_id)] <- 1
df$newvent24[is.na(df$mv24_id) ]<- 0

# Encode SOFA components
# abnormal = c(3,4)
# normal = c(0,1,2)

# First iteration -> following rule was applied -> see cohort_MIMIC.r for details
# No MV and abnormal CNS    = Abnormal
# No MV and normal CNS      = Normal
# MV and abnormal CNS       = Abnormal
# MV and normal CNS         = Abnormal

# df$coag_24[df$coag_24 %in% normal] <- "Normal"
# df$coag_24[df$coag_24 %in% abnormal] <- "Abnormal"

# df$liver_24[df$liver_24 %in% normal] <- "Normal"
# df$liver_24[df$liver_24 %in% abnormal] <- "Abnormal"

# df$cv_24[df$cv_24 %in% normal] <- "Normal"
# df$cv_24[df$cv_24 %in% abnormal] <- "Abnormal"

# df$renal_24[df$renal_24 %in% normal] <- "Normal"
# df$renal_24[df$renal_24 %in% abnormal] <- "Abnormal"

# df$resp_24[df$resp_24 %in% normal] <- "Normal"
# df$resp_24[df$resp_24 %in% abnormal] <- "Abnormal"

# df$cns_24[df$cns_24 %in% normal] <- "Normal"
# df$cns_24[df$cns_24 %in% abnormal] <- "Abnormal"

print(paste0("Final Number of Patients (24h): ", nrow(df)))

write.csv(df, "data/cohorts/eICU_24.csv")

# Assess situation at 168h
# still alive at 168 hours, 168 hours = 10080 minutes
df$situation168 <- 0
df$situation168[(df$unitdischargeoffset - df$unitadmitoffset >= 10080) &
                (df$hospitaldischargeoffset - df$unitadmitoffset >= 10080)] <- 1

# remove null sofa scores at 168 hours
df1 <- df[df$situation168 == 1,]

print(paste0("Patients removed whose stay was not longer than 7 days: ", nrow(df) - nrow(df1)))

# Encode mechanical ventilation at 168h
df1[, 'newvent168'] <- NA
df1$newvent168[!is.na(df1$mv168_id)] <- 1
df1$newvent168[is.na(df1$mv168_id) ]<- 0

df1 <- df1[!is.na(df1$s168_id),]

# df1$coag_168[df1$coag_168 %in% normal] <- "Normal"
# df1$coag_168[df1$coag_168 %in% abnormal] <- "Abnormal"

# df1$liver_168[df1$liver_168 %in% normal] <- "Normal"
# df1$liver_168[df1$liver_168 %in% abnormal] <- "Abnormal"

# df1$cv_168[df1$cv_168 %in% normal] <- "Normal"
# df1$cv_168[df1$cv_168 %in% abnormal] <- "Abnormal"

# df1$renal_168[df1$renal_168 %in% normal] <- "Normal"
# df1$renal_168[df1$renal_168 %in% abnormal] <- "Abnormal"

# df1$resp_168[df1$resp_168 %in% normal] <- "Normal"
# df1$resp_168[df1$resp_168 %in% abnormal] <- "Abnormal"

# df1$cns_168[df1$cns_168 %in% normal] <- "Normal"
# df1$cns_168[df1$cns_168 %in% abnormal] <- "Abnormal"

print(paste0("Final Number of Patients (168h): ", nrow(df1)))

write.csv(df1, "data/cohorts/eICU_168.csv")
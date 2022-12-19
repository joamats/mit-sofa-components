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

# Remove patients who are not on a first stay
df2 <-df1[(df1$unitvisitnumber==1),]
print(paste0("Patients removed who were not on a 1st stay: ", nrow(df1) - nrow(df2)))

# Remove patients who were not 24h in the ICU / hosptial
df3 <-df2[!(df2$out_in==1),]
df4 <-df3[!(df3$discharge_in==1),]
print(paste0("Patients removed who were not 24h: ", nrow(df2) - nrow(df4)))

# Remove patients without resp info
df5 <- df4[!(is.na(df4$resp_24)),]
print(paste0("Patients removed without resp info: ", nrow(df4) - nrow(df5)))

# Remove patients under 16
df6 <- df5[(df5$age>=16),]
print(paste0("Patients removed that were less than 16yo: ", nrow(df5) - nrow(df6)))

# Remove patients without gender info
df7 <- df6[!is.na(df6$gender), ]
print(paste0("Patients removed that had no gender info: ", nrow(df6) - nrow(df7)))

# Remove patients with CABG 
df8 <- df7[is.na(df7$cabg_id),]
print(paste0("Patients removed with CABG: ", nrow(df7) - nrow(df8)))

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

# Encode ethnicity
df$ethnicity[df$ethnicity == "African American"] <- "BLACK"
df$ethnicity[df$ethnicity == "Asian"] <- "ASIAN"
df$ethnicity[df$ethnicity == "Caucasian"] <- "WHITE"
df$ethnicity[df$ethnicity == "Hispanic"] <- "HISPANIC"
df$ethnicity[df$ethnicity == "Native American" | df$ethnicity == "Other/Unknown" | is.na(df$ethnicity)] <- "OTHER"

# Encode gender
df$gender[df$gender == 0 ] <- "Female"
df$gender[df$gender == 1 ] <- "Male"

# Encode age
df$age[df$age == ">89"] <- 90

# Encode SOFA components
abnormalvalue = c(1,2,3,4)

df$coag_24[df$coag_24 == 0] <- "Normal"
df$coag_24[df$coag_24 %in% abnormalvalue] <- "Abnormal"

df$liver_24[df$liver_24 == 0] <- "Normal"
df$liver_24[df$liver_24 %in% abnormalvalue] <- "Abnormal"

df$cv_24[df$cv_24 == 0] <- "Normal"
df$cv_24[df$cv_24 %in% abnormalvalue] <- "Abnormal"

df$renal_24[df$renal_24 == 0] <- "Normal"
df$renal_24[df$renal_24 %in% abnormalvalue] <- "Abnormal"

df$resp_24[df$resp_24 == 0] <- "Normal"
df$resp_24[df$resp_24 %in% abnormalvalue] <- "Abnormal"

df$cns_24[df$cns_24 == 0] <- "Normal"
df$cns_24[df$cns_24 %in% abnormalvalue] <- "Abnormal"
df$cns_24[!is.na(df$mv24_id)] <- "Mechanical Ventilation (MV)"

print(paste0("Final Number of Patients (24h): ", nrow(df)))

write.csv(df, "data/cohorts/eICU_24.csv")

# Assess situation at 168h
# still alive at 168 hours, 168 hours = 10080 minutes
df$situation168 <- 0
df$situation168[(df$unitdischargeoffset - df$unitadmitoffset >= 10080) &
                (df$hospitaldischargeoffset - df$unitadmitoffset >= 10080)] <- 1

# remove null sofa scores at 168 hours
df1 <- df[df$situation168 == 1,]

print(paste0("Patients removed whose stay was not longer than 7days: ", nrow(df) - nrow(df1)))

df1 <- df1[!is.na(df1$s168_id),]

df1$coag_168[df1$coag_168 == 0] <- "Normal"
df1$coag_168[df1$coag_168 %in% abnormalvalue] <- "Abnormal"

df1$liver_168[df1$liver_168 == 0] <- "Normal"
df1$liver_168[df1$liver_168 %in% abnormalvalue] <- "Abnormal"

df1$cv_168[df1$cv_168 == 0] <- "Normal"
df1$cv_168[df1$cv_168 %in% abnormalvalue] <- "Abnormal"

df1$renal_168[df1$renal_168 == 0] <- "Normal"
df1$renal_168[df1$renal_168 %in% abnormalvalue] <- "Abnormal"

df1$resp_168[df1$resp_168 == 0] <- "Normal"
df1$resp_168[df1$resp_168 %in% abnormalvalue] <- "Abnormal"

df1$cns_168[df1$cns_168 == 0] <- "Normal"
df1$cns_168[df1$cns_168 %in% abnormalvalue] <- "Abnormal"
df1$cns_168[!is.na(df1$mv168_id)] <- "Mechanical Ventilation (MV)"

print(paste0("Final Number of Patients (168h): ", nrow(df1)))

write.csv(df1, "data/cohorts/eICU_168.csv")
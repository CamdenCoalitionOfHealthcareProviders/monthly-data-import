# Load libraries
library(readxl) # if data sent is an Excel file and not CSV file
library(stringr)
library(data.table)

#########################################################
### Load Data From EPIC and old IDX naming Mechanism ####
########################################################

# Sets working directory, reads file and creates a nickname
setwd("Y:/monthly import/201707/raw")
wd <- getwd()
date <- format(Sys.Date(), "%B%Y")

# Reads in files in format March2015Horizon.csv
# epic <- read.csv(paste(wd,"/3 cooper sh data_04282017", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
epic <- read_excel(paste0(wd,"/Data_for_HIE_34_4347680388582147177.xlsx"))
 
# epic <-   read.csv(file)
colnames(epic)


#########################################################
############# Reformating data for HIE  ################
########################################################

# Renaming columns
hie <- data.frame(epic)
colnames(hie) <- c("Reg.Patient.MRN", "MEMB_ZIP", "MEMB_INSURANCE", "SUBSCRIBER_ID", "SOCIAL_SEC_NO", 
                 "MEMB_NAME", "MEMB_FIRST_NAME", "MEMB_LAST_NAME", "DOB", "GENDER", "MEMB_ADDRESS_LINE_1",
                 "MEMB_CITY", "MEMB_STATE", "CURR_PCP_FULL_NAME", "HOME_PHONE_NUM", "APPT_DT")  

# New columns that are not in Epic
hie$MEMB_ADDRESS_LINE_2 <- NA
hie$VEND_FULL_NAME <- "Cooper_UHI_Nic"
hie$Source <- "UHI_Nic"

# Reformatting data
# Reformat Subscriber ID, FN and LN, DOB
hie$SUBSCRIBER_ID <- paste("NIC",hie$Reg.Patient.MRN, sep="")
hie$MEMB_FIRST_NAME <- toupper(hie$MEMB_FIRST_NAME)
hie$MEMB_LAST_NAME <- toupper(hie$MEMB_LAST_NAME)
hie$DOB <- as.character(hie$DOB)
hie$DOB <- format(as.Date(hie$DOB), "%m/%d/%Y")

# Reformat Gender
table(hie$GENDER)
hie$GENDER <- str_replace(hie$GENDER, "Female", "F")
hie$GENDER <- str_replace(hie$GENDER, "Male", "M")
table(hie$GENDER)

# Reformat MEMB_STATE
table(hie$MEMB_STATE)
hie$MEMB_STATE <- str_replace(hie$MEMB_STATE, "Arizona", "AZ")
hie$MEMB_STATE <- str_replace(hie$MEMB_STATE, "Delaware", "DE")
hie$MEMB_STATE <- str_replace(hie$MEMB_STATE, "New Jersey", "NJ")
hie$MEMB_STATE <- str_replace(hie$MEMB_STATE, "New York", "NY")
hie$MEMB_STATE <- str_replace(hie$MEMB_STATE, "Pennsylvania", "PA") 
hie$MEMB_STATE <- str_replace(hie$MEMB_STATE, "South Carolina", "SC") 

table(hie$MEMB_STATE)

# Keeping the most recent visit data
hie$APPT_DT <- as.Date(hie$APPT_DT, "%m/%d/%Y")
hie_final <- hie[rev(order(hie$Reg.Patient.MRN, hie$APPT_DT)), ] #sort by id and reverse of date
hie_final <- hie_final[!duplicated(hie_final$Reg.Patient.MRN), ] #Removing duplicates and keeping most recent data

df = data.frame(hie_final)
hie_final <- df[ ,c(1:11,17,12:14,18,15,19)]
colnames(hie_final)


#########################################################
############ Create CSV File to Send to HIE Vendor ############
########################################################
savedate <- (strftime(Sys.Date(),format="%Y-%m-%d"))

write.csv(hie_final, file=paste(savedate,"-CooperUHI.csv", sep=""), row.names=FALSE)

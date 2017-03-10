# Attaches packages the code needs to run
suppressMessages(require(reshape))
suppressMessages(require(zipcode))
suppressMessages(require(gtools))

# Sets working directory, reads file and creates a nickname
setwd("Y:/monthly import/201703/raw")
wd <- getwd()
date <- format(Sys.Date(), "%B%Y")

# Reads in file whose file name is formatted January2016CAMCareCapList.csv
camcare  <-  read.csv(paste(wd,"/March2017CAMcare", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
# camcare  <-  read.csv("Y:/monthly import/201609/raw data/September2016CAMCareCapList.csv", sep="", header=TRUE, stringsAsFactors = FALSE)

# Concatenates Provider information
camcare$CURR_PCP_FULL_NAME  <-  paste(camcare$PROV_LNAME, camcare$PROV_FNAME, sep=", ")

# Removes unused fields
camcare$AGE <- NULL
camcare$PANEL_ID <- NULL
camcare$PLAN_DESC <- NULL
camcare$TERM_DATE <- NULL
camcare$PLAN_CODE <- NULL
camcare$PROV_FNAME <- NULL
camcare$PROV_LNAME <- NULL
camcare$PROV_LANG_1 <- NULL
camcare$PROV_LANG_2 <- NULL
camcare$PROV_LANG_3 <- NULL
camcare$PROV_EFF_DATE <- NULL
camcare$EFFECTIVE_DATE <- NULL
camcare$PROV_TERM_DATE <- NULL
camcare$COSMOS_CUST_SEG <- NULL
camcare$LINE_OF_BUSINESS <- NULL
camcare$COSMOS_CUST_SEG_DESC <- NULL

# Renames fields in the camcare
camcare <- reshape::rename(camcare, c(DATE_OF_BIRTH="DOB"))
camcare <- reshape::rename(camcare, c(MEMB_GENDER="GENDER"))
camcare <- reshape::rename(camcare, c(PROVIDER_ID="CURR_PCP_ID"))
camcare <- reshape::rename(camcare, c(PROV_PHONE="PHONE_NUMBER"))
camcare <- reshape::rename(camcare, c(PROV_ADDRESS_LINE_1="CURR_PCP_ADDRESS_LINE_1"))
camcare <- reshape::rename(camcare, c(PROV_ADDRESS_LINE_2="CURR_PCP_ADDRESS_LINE_2"))
camcare <- reshape::rename(camcare, c(PROV_CITY="CURR_PCP_CITY"))
camcare <- reshape::rename(camcare, c(PROV_STATE="CURR_PCP_STATE"))
camcare <- reshape::rename(camcare, c(PROV_ZIP="CURR_PCP_ZIP"))
camcare <- reshape::rename(camcare, c(PAYEE_NAME="VEND_FULL_NAME"))

# Maps languages in the camcare file to the full name of the language
camcare$MEMB_LANGUAGE <- as.character(camcare$MEMB_LANGUAGE)
camcare$MEMB_LANGUAGE[camcare$MEMB_LANGUAGE=="ARA"] <- "Arabic"
camcare$MEMB_LANGUAGE[camcare$MEMB_LANGUAGE=="CHI"] <- "Chinese"
camcare$MEMB_LANGUAGE[camcare$MEMB_LANGUAGE=="ENG"] <- "English"
camcare$MEMB_LANGUAGE[camcare$MEMB_LANGUAGE=="FRE"] <- "French"
camcare$MEMB_LANGUAGE[camcare$MEMB_LANGUAGE=="HEB"] <- "Hebrew"
camcare$MEMB_LANGUAGE[camcare$MEMB_LANGUAGE=="ITA"] <- "Italian"
camcare$MEMB_LANGUAGE[camcare$MEMB_LANGUAGE=="KOR"] <- "Korean"
camcare$MEMB_LANGUAGE[camcare$MEMB_LANGUAGE=="N/A"] <- ""
camcare$MEMB_LANGUAGE[camcare$MEMB_LANGUAGE=="PER"] <- "Persian"
camcare$MEMB_LANGUAGE[camcare$MEMB_LANGUAGE=="POR"] <- "Portuegese"
camcare$MEMB_LANGUAGE[camcare$MEMB_LANGUAGE=="SPA"] <- "Spanish"
camcare$MEMB_LANGUAGE[camcare$MEMB_LANGUAGE=="TUR"] <- "Turkish"
camcare$MEMB_LANGUAGE[camcare$MEMB_LANGUAGE=="UNK"] <- "Unknown"
camcare$MEMB_LANGUAGE[camcare$MEMB_LANGUAGE=="VIE"] <- "Vietnamese"

# Adds text identifiers to Subscriber IDs
camcare$SUBSCRIBER_ID <- paste("U", camcare$SUBSCRIBER_ID, sep="")

# Sets the MEDICAID_NO field as numeric to get rid of scientific notation
options(scipen=999)
camcare$MEDICAID_NO <- as.numeric(as.character(camcare$MEDICAID_NO))

# Cleans the home phone number field of parentheses, spaces and dashes
camcare$HOME_PHONE_NUMBER <- gsub("\\(|\\)|\\-|\\ ", "", camcare$HOME_PHONE_NUMBER)

# Cleans zip codes
camcare$MEMB_ZIP <- clean.zipcodes(camcare$MEMB_ZIP)
camcare$CURR_PCP_ZIP <- clean.zipcodes(camcare$MEMB_ZIP)

# Cleans birth dates
camcare$DOB <- as.Date(camcare$DOB, "%m/%d/%Y")

#Deletes entries with the wrong vendor names#
camcare  <-  subset(camcare, !(VEND_FULL_NAME=="CHILD REGIONAL/CAMDEN"))

# Keeps only where PCP City is Camden or Pennsauken, and keeps all of CamCare
camcare <- subset(camcare, CURR_PCP_CITY=="CAMDEN" | CURR_PCP_CITY=="CADMEN" |CURR_PCP_CITY=="CANDEM" |CURR_PCP_CITY=="PENNSAUKEN" | VEND_FULL_NAME=="CAMCARE HEALTH CORPORATION")

# If the code to rename vendors gives you trouble, modify the below code to fix the errors#
camcare  <-  data.frame(lapply(camcare, as.character), stringsAsFactors=FALSE)

# Adds Identification fields
camcare$PAYER <- "CAMCare"
camcare$Source <- "United"

# Adds in fields to match United, Horizon, and Cooper UHI AllPayers file
camcare$SOCIAL_SEC_NO <- ""
camcare$PRACTICE <- ""
camcare$HIEID <- ""

# Sorts columns in camcare and horizon A-Z
camcare <- camcare[,order(names(camcare))]

# Renames camcare to AllPayers
AllPayers  <-  camcare

AllPayers$CURR_PCP_CITY <- toupper(AllPayers$CURR_PCP_CITY)

# Maps to practice
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME =="CAMCARE HEALTH CORPORATION"] <- "CAMCare"

AllPayers <- as.data.frame(AllPayers)

# Removes fields that don't need to go to CareEvolution from the CareEvolution version of the file
AllPayers$MEMB_LANGUAGE <- NULL
AllPayers$MEMB_ETHNICITY <- NULL

# Adds last capitation 
AllPayers$LastCapitationDate <-  format(Sys.time(), "%m/01/%Y") 

# Remove "U" from string to match TrackVia Subscriber IDs
# AllPayers$SUBSCRIBER_ID <- gsub("U", "", AllPayers$SUBSCRIBER_ID)

# Exports file for CareEvolution
write.csv(AllPayers, (file=paste(format(Sys.Date(), "%Y-%m-%d-"),"UnitedCAMCare",  ".csv", sep="")), row.names=FALSE)

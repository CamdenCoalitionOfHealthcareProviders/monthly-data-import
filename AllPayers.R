# Attaches packages the code needs to run#
suppressMessages(require(reshape))
suppressMessages(require(zipcode))
suppressMessages(require(gtools))

# Sets working directory, reads file and creates a nickname#
wd <- setwd("Y:/Monthly Import/Feb 2016")
date <- format(Sys.Date(), "%B%Y")

# Reads in files in format March2015Horizon.csv
united <- read.csv(paste(wd,"/",date,"UnitedCapList", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
horizon <- read.delim(paste(wd,"/",date,"Horizon",".txt",sep = ""), sep="|", quote = "", stringsAsFactors=FALSE)

#Concatenates Provider information
united$CURR_PCP_FULL_NAME <- paste(united$PROV_LNAME, united$PROV_FNAME, sep=", ")

# Removes unused fields
united$AGE <- NULL
united$PANEL_ID <- NULL
united$PLAN_DESC <- NULL
united$TERM_DATE <- NULL
united$PLAN_CODE <- NULL
united$PROV_FNAME <- NULL
united$PROV_LNAME <- NULL
united$PROV_LANG_1 <- NULL
united$PROV_LANG_2 <- NULL
united$PROV_LANG_3 <- NULL
united$PROV_EFF_DATE <- NULL
united$EFFECTIVE_DATE <- NULL
united$PROV_TERM_DATE <- NULL
united$COSMOS_CUST_SEG <- NULL
united$LINE_OF_BUSINESS <- NULL
united$COSMOS_CUST_SEG_DESC <- NULL

horizon$Member_Months  <-	NULL
horizon$Future_Rx_Costs	<- NULL
horizon$Total_Mem_Months	<- NULL
horizon$Future_Risk_Costs	<- NULL
horizon$Primary_Risk_Factor	<- NULL
horizon$Prior_Rx_Costs_Annualized	<- NULL
horizon$Prior_Total_Costs_Annualized <- NULL

# Renames fields in the united and horizon
united <- reshape::rename(united, c(DATE_OF_BIRTH = "DOB"))
united <- reshape::rename(united, c(MEMB_GENDER = "GENDER"))
united <- reshape::rename(united, c(PROVIDER_ID = "CURR_PCP_ID"))
united <- reshape::rename(united, c(PROV_PHONE = "PHONE_NUMBER"))
united <- reshape::rename(united, c(PROV_ADDRESS_LINE_1 = "CURR_PCP_ADDRESS_LINE_1"))
united <- reshape::rename(united, c(PROV_ADDRESS_LINE_2 = "CURR_PCP_ADDRESS_LINE_2"))
united <- reshape::rename(united, c(PROV_CITY = "CURR_PCP_CITY"))
united <- reshape::rename(united, c(PROV_STATE = "CURR_PCP_STATE"))
united <- reshape::rename(united, c(PROV_ZIP = "CURR_PCP_ZIP"))
united <- reshape::rename(united, c(PAYEE_NAME = "VEND_FULL_NAME"))
horizon <- reshape::rename(horizon, c(Subscriber_ID = "SUBSCRIBER_ID"))
horizon <- reshape::rename(horizon, c(Gender = "GENDER"))
horizon <- reshape::rename(horizon, c(SSN = "SOCIAL_SEC_NO"))

# Adds necessary fields to the horizon file for merging
horizon$MEDICARE_NO	<- ""
horizon$MEMB_ETHNICITY	<- ""
horizon$MEMB_LANGUAGE	<- ""

# Maps languages in the united file to the full name of the language
united$MEMB_LANGUAGE <- as.character(united$MEMB_LANGUAGE)
united$MEMB_LANGUAGE[united$MEMB_LANGUAGE == "ARA"] <- "Arabic"
united$MEMB_LANGUAGE[united$MEMB_LANGUAGE == "CHI"] <- "Chinese"
united$MEMB_LANGUAGE[united$MEMB_LANGUAGE == "ENG"] <- "English"
united$MEMB_LANGUAGE[united$MEMB_LANGUAGE == "FRE"] <- "French"
united$MEMB_LANGUAGE[united$MEMB_LANGUAGE == "HEB"] <- "Hebrew"
united$MEMB_LANGUAGE[united$MEMB_LANGUAGE == "ITA"] <- "Italian"
united$MEMB_LANGUAGE[united$MEMB_LANGUAGE == "KOR"] <- "Korean"
united$MEMB_LANGUAGE[united$MEMB_LANGUAGE == "N/A"] <- ""
united$MEMB_LANGUAGE[united$MEMB_LANGUAGE == "PER"] <- "Persian"
united$MEMB_LANGUAGE[united$MEMB_LANGUAGE == "POR"] <- "Portuegese"
united$MEMB_LANGUAGE[united$MEMB_LANGUAGE == "SPA"] <- "Spanish"
united$MEMB_LANGUAGE[united$MEMB_LANGUAGE == "TUR"] <- "Turkish"
united$MEMB_LANGUAGE[united$MEMB_LANGUAGE == "UNK"] <- "Unknown"
united$MEMB_LANGUAGE[united$MEMB_LANGUAGE == "VIE"] <- "Vietnamese"

# Adds text identifiers to Subscriber IDs
united$SUBSCRIBER_ID <- paste("U", united$SUBSCRIBER_ID, sep="")
horizon$SUBSCRIBER_ID <- paste("H", horizon$SUBSCRIBER_ID, sep="")

# Sets the MEDICAID_NO field as numeric to get rid of scientific notation
options(scipen=999)
united$MEDICAID_NO <- as.numeric(as.character(united$MEDICAID_NO))

# Cleans the home phone number field of parentheses, spaces and dashes
united$HOME_PHONE_NUMBER <- gsub("\\(|\\)|\\-|\\ ", "", united$HOME_PHONE_NUMBER)

# Cleans zip codes
united$MEMB_ZIP <- clean.zipcodes(united$MEMB_ZIP)
united$CURR_PCP_ZIP <- clean.zipcodes(united$MEMB_ZIP)
horizon$MEMB_ZIP <- clean.zipcodes(horizon$MEMB_ZIP)
horizon$CURR_PCP_ZIP <- clean.zipcodes(horizon$CURR_PCP_ZIP)

# Cleans birth dates
united$DOB <- as.Date(united$DOB, "%m/%d/%Y")

#Deletes entries with the wrong vendor names#
united <- subset(united, !(VEND_FULL_NAME=="CHILD REGIONAL/CAMDEN"))

# Keeps only where PCP City is Camden or Pennsauken, and keeps all of CamCare
united <- subset(united, CURR_PCP_CITY=="CAMDEN" | CURR_PCP_CITY=="CADMEN" |CURR_PCP_CITY=="CANDEM" |CURR_PCP_CITY=="PENNSAUKEN" | VEND_FULL_NAME=="CAMCARE HEALTH CORPORATION")

# If the code to rename vendors gives you trouble, modify the below code to fix the errors#
united <- data.frame(lapply(united, as.character), stringsAsFactors=FALSE)

# Adds Identification fields
united$PAYER <- "UNITED"
united$Source <- "United"
horizon$PAYER <- "HORIZON"
horizon$Source <- "Horizon"

# Sorts columns in united and horizon A-Z
united <- united[,order(names(united))]
horizon <- horizon[,order(names(horizon))]

# Merges united and horizon data
AllPayers <- rbind(united,horizon)

AllPayers$CURR_PCP_CITY <- toupper(AllPayers$CURR_PCP_CITY)

#Renames vendors to match Current PCP City#
AllPayers$VEND_FULL_NAME[AllPayers$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES" & AllPayers$CURR_PCP_CITY == "CAMDEN"] <- "LOURDES MEDICAL ASSOCIATES_CAMDEN"
AllPayers$VEND_FULL_NAME[AllPayers$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES PA" & AllPayers$CURR_PCP_CITY == "CAMDEN"] <- "LOURDES MEDICAL ASSOCIATES_CAMDEN"          
AllPayers$VEND_FULL_NAME[AllPayers$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES" & AllPayers$CURR_PCP_CITY == "PENNSAUKEN"] <- "LOURDES MEDICAL ASSOCIATES_PENNSAUKEN"  
AllPayers$VEND_FULL_NAME[AllPayers$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES PA" & AllPayers$CURR_PCP_CITY == "PENNSAUKEN"] <- "LOURDES MEDICAL ASSOCIATES_PENNSAUKEN"  
AllPayers$VEND_FULL_NAME[AllPayers$VEND_FULL_NAME == "OSBORN FAMILY PRACTICE" & AllPayers$CURR_PCP_CITY == "CAMDEN"] <- "OSBORN FAMILY PRACTICE_CAMDEN"
AllPayers$VEND_FULL_NAME[AllPayers$VEND_FULL_NAME == "BROADWAY FAMILY PRACTICE" & AllPayers$CURR_PCP_CITY == "CAMDEN"] <- "RELIANCE BROADWAY_CAMDEN"   
AllPayers$VEND_FULL_NAME[AllPayers$VEND_FULL_NAME == "BROADWAY FAMILY PRACTICE" & AllPayers$CURR_PCP_CITY == "PENNSAUKEN"] <- "RELIANCE BROADWAY_PENNSAUKEN"   

#Maps to practices#
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "ACOSTA RAMON"] <- "Acosta"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "RELIANCE BROADWAY_CAMDEN"] <- "Reliance Broadway"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "RELIANCE BROADWAY_PENNSAUKEN"] <- "Reliance Pennsauken"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "CAMCARE HEALTH CORPORATION"] <- "CAMcare"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "COOPER AMBULATORY PEDIATRICS"] <- "Cooper Pediatrics"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "COOPER FAMILY MEDICINE"] <- "Cooper Family"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "COOPER FAMILY MEDICINE PC"] <- "Cooper Family"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "COOPER PEDIATRICS"] <- "Cooper Pediatrics"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "COOPER PHYSICIANS OFFICES"] <- "Cooper IM"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "COOPER PHYSICIAN OFFICES PA"] <- "Cooper IM"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "Cooper_UHI_Nic"] <- "Cooper IM"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "JEFFREY A KLEEMAN DO"] <- "Fairview"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES_CAMDEN"] <- "Osborn"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "LOURDES MEDICAL ASSOCIATES_PENNSAUKEN"] <- "Lourdes Pediatrics"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "OSBORN FAMILY PRACTICE_CAMDEN"] <- "Osborn"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "PROJECT HOPE"] <- "Project Hope"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "PROJECT HOPE HOMELESS PROGRAM"] <- "Project Hope"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "RIVER PRIMARY CARE CENTER"] <- "Reliance River"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "ST LUKE'S CATHOLIC MED  SVCS"] <- "St. Lukes"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "ST LUKES CATHOLIC MEDICAL SERVICES INC"] <- "St. Lukes"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "VIRTUA FAMILY MEDICINE-COOPER RIVER"] <- "Virtua"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "VIRTUA MEDICAL GROUP"] <- "Virtua"
AllPayers$PRACTICE[AllPayers$VEND_FULL_NAME == "VIRTUA MEDICAL GROUP PA"] <- "Virtua"

AllPayers <- as.data.frame(AllPayers)

# Removes fields that don't need to go to CareEvolution from the CareEvolution version of the file
AllPayers$MEMB_LANGUAGE <- NULL
AllPayers$MEMB_ETHNICITY <- NULL

# Adds last capitation 
AllPayers$LastCapitationDate <- format(Sys.time(), "%m/01/%Y") 

#Remove "U" from string to match TrackVia Subscriber IDs
AllPayers$SUBSCRIBER_ID <- gsub("U", "", AllPayers$SUBSCRIBER_ID)

# Exports file for CareEvolution
write.csv(AllPayers, (file=paste(format(Sys.Date(), "%Y-%m-%d-"),"AllPayers",  ".csv", sep="")), row.names=FALSE)

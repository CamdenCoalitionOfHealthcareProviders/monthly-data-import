suppressMessages(require(reshape))

# Working directory path
path <- "Y:/monthly import/201703/"

# Set working directory where files will be saved
setwd(path)

# Reads in files
allpayers <- read.csv(paste(path, "AllPayerHIEIDs-2017-03-07.csv", sep=""), stringsAsFactors = FALSE)
uhi <- read.csv(paste(path, "UhiHIEIDs-2017-03-07.csv", sep=""), stringsAsFactors = FALSE)

# Subsets out CAMcare data
camcare <- subset(allpayers, allpayers$VEND_FULL_NAME=="CAMCARE HEALTH CORPORATION")
rest <- subset(allpayers, allpayers$VEND_FULL_NAME!="CAMCARE HEALTH CORPORATION")

# Merges back Horizon CAMcare data
camcarehorizon <- subset(camcare, camcare$Source=="Horizon")
allpayers <- rbind(rest, camcarehorizon)

# Renames fields
uhi <- reshape::rename(uhi, c(HOME_PHONE_NUM="HOME_PHONE_NUMBER"))
uhi <- reshape::rename(uhi, c(MEMB_INSURANCE="PAYER"))
uhi <- reshape::rename(uhi, c(ï..Reg.Patient.MRN="Reg.Patient.MRN"))
allpayers <- reshape::rename(allpayers, c(ï..BUS_PHONE_NUMBER="BUS_PHONE_NUMBER"))

# Adds NIC to the uhi Subscriber ID if it's not there 
uhi$SUBSCRIBER_ID <- ifelse(grepl("NIC", uhi$SUBSCRIBER_ID), uhi$SUBSCRIBER_ID, paste("NIC", uhi$SUBSCRIBER_ID, sep=""))

# Deletes unused fields
uhi$Reg.Patient.MRN <- NULL
uhi$MEMB_NAME <- NULL
allpayers$MEMB_NAME <- NULL
allpayers$X.1 <- NULL
allpayers$X <- NULL
uhi$X <- NULL

# Adds capitation date field
uhi$LastCapitationDate <- format(Sys.time(), "%m/01/%Y") 

# Assigns practice to the UHI file
uhi$PRACTICE <- "Cooper IM"

# Creates fields with blank values
uhi$BUS_PHONE_NUMBER  <- "NA"
uhi$CURR_PCP_ADDRESS_LINE_1	<- "NA"
uhi$CURR_PCP_ADDRESS_LINE_2	<- "NA"
uhi$CURR_PCP_CITY	<- "NA"
uhi$CURR_PCP_ID	<- "NA"
uhi$CURR_PCP_STATE	<- "NA"
uhi$CURR_PCP_ZIP	<- "NA"
uhi$IRS_TAX_ID	<- "NA"
uhi$MEDICAID_NO	<- "NA"
uhi$MEDICARE_NO	<- "NA"
uhi$PAYER	<- "NA"
uhi$PHONE_NUMBER	<- "NA"
uhi$VENDOR_ID	<- "NA"

# Sorts columns in both files
allpayers <- allpayers[,order(names(allpayers))]
uhi <- uhi[,order(names(uhi))]

# Binds files horizontally
combined <- rbind(allpayers, uhi)

# Removes spaces from the phone number fields
combined$HOME_PHONE_NUMBER <- gsub(" ", "", combined$HOME_PHONE_NUMBER) 
combined$PHONE_NUMBER <- gsub(" ", "", combined$PHONE_NUMBER)

# Changes case of HIE ID to lowercase
combined$HIEID <- tolower(combined$HIEID)

#Sorts data by HIEID
combined <- combined[order(combined$HIEID),] 

# Identifies the first pair of duplicate values - the one not to be deleted
combined$duplicate2 <- duplicated(combined$HIEID, fromLast=TRUE)

# Identifies second pair of duplicate values - the one to be deleted
combined$duplicate <- duplicated(combined$HIEID) 

# Identifies the urgency with which the duplicate should be cleared (1 is important to resolve)
combined$dupetype <- ifelse(combined$duplicate == TRUE & combined$Source == "United", "dupe1", "dupe3")
combined$dupetype <- ifelse(combined$duplicate == TRUE & combined$Source == "Horizon", "dupe1", combined$dupetype)

# Creates a list of urgent duplicates for reference in script
dupes <- subset(combined, combined$dupetype == "dupe1")

# Marks the urgency of duplicates for the second part of the pair
combined$dupetype <- ifelse(combined$HIEID %in% dupes$HIEID, "dupe1", combined$dupetype)

# Identifies known twins
twins <- subset(combined,
              combined$SUBSCRIBER_ID==102239994 | 
                combined$SUBSCRIBER_ID==102239993 | 
                combined$SUBSCRIBER_ID==101760611 | 
                combined$SUBSCRIBER_ID==101957967 |
                combined$SUBSCRIBER_ID==101758438 | 
                combined$SUBSCRIBER_ID==101854600 |
                combined$SUBSCRIBER_ID==106274833 |
                combined$SUBSCRIBER_ID==106274834 |
                combined$SUBSCRIBER_ID==108034384 |
                combined$SUBSCRIBER_ID==108034385 |
                combined$SUBSCRIBER_ID==110119063 |
                combined$SUBSCRIBER_ID==110119064 |
                combined$SUBSCRIBER_ID=="H71260169" |
                combined$SUBSCRIBER_ID=="H71260168" |
                combined$SUBSCRIBER_ID=="H71514576" |
                combined$SUBSCRIBER_ID=="H71514574" |
                combined$SUBSCRIBER_ID=="H71570565" |
                combined$SUBSCRIBER_ID=="H71570564" |
                combined$SUBSCRIBER_ID=="H71598715" |
                combined$SUBSCRIBER_ID=="H71595647" |
                combined$SUBSCRIBER_ID=="H71579478" |
                combined$SUBSCRIBER_ID=="H71579477" |
                combined$SUBSCRIBER_ID=="H35022345" |
                combined$SUBSCRIBER_ID=="H35022337" |
                combined$SUBSCRIBER_ID=="H474184"   |
                combined$SUBSCRIBER_ID=="H474159"   |
                combined$SUBSCRIBER_ID=="H71650351" |
                combined$SUBSCRIBER_ID=="H71650348" |
                combined$SUBSCRIBER_ID=="H71646507" |
                combined$SUBSCRIBER_ID=="H71646508" |
                combined$SUBSCRIBER_ID=="H71647601" |
                combined$SUBSCRIBER_ID=="H71647600" |
                combined$SUBSCRIBER_ID=="H71637280" |
                combined$SUBSCRIBER_ID=="H71660243"
)

# Subsets values that are TRUE for duplicate and are not a twin
duplicates <- subset(combined, combined$duplicate==TRUE | combined$duplicate2== TRUE & !combined$SUBSCRIBER_ID %in% twins$SUBSCRIBER_ID)

# Removes high confidence duplicates from the file
duplicates <- duplicates[!(duplicates$dupetype=="dupe3" & duplicates$duplicate2==TRUE),]

# Identifies variables to export
duplicates <- duplicates[,c("HIEID",
                          "PRACTICE",
                          "Source",
                          "MEMB_FIRST_NAME", 
                          "MEMB_LAST_NAME",
                          "DOB",
                          "SOCIAL_SEC_NO",
                          "SUBSCRIBER_ID",
                          "VEND_FULL_NAME",
                          "duplicate",
                          "duplicate2",
                          "dupetype")]

# Subsets values that are FALSE for duplicate and are not a twin
uniques <- subset(combined, combined$duplicate==FALSE & !combined$SUBSCRIBER_ID %in% twins$SUBSCRIBER_ID)

# Renames HIEID field
uniques <- rename(uniques, c(HIEID="Patient ID HIE"))

# Removes the field that identifies duplicates
uniques$duplicate <- NULL
uniques$duplicate2 <- NULL
uniques$dupetype <- NULL
twins$duplicate2 <- NULL
twins$duplicate <- NULL
twins$dupetype <- NULL

# Adds fields to identify the data as ones imported in bulk from cap list
twins$MonthlyBulkImport <- "Monthly Import"
uniques$MonthlyBulkImport <- "Monthly Import"

# Exports files
write.csv(twins, paste(Sys.Date(), "-",file="Twins-New-HIE-ID",".csv", sep=""), row.names = FALSE)
write.csv(duplicates, paste(Sys.Date(), "-",file="HIE-Delete",".csv", sep=""), row.names = FALSE)
write.csv(uniques, paste(Sys.Date(),"-",file="TrackVia-Import", ".csv", sep=""), row.names = FALSE)

suppressMessages(require(reshape))

path<-"Y:/Monthly Import/Feb 2016/"
  
# Reads in files
allpayers<-read.csv(paste(path, "AllPayerHIEIDs-2016-02-03.csv", sep=""), stringsAsFactors = FALSE)
uhi<-read.csv(paste(path, "UhiHIEIDs-2016-02-03.csv", sep=""), stringsAsFactors = FALSE)

# Renames fields
uhi<-reshape::rename(uhi, c(HOME_PHONE_NUM="HOME_PHONE_NUMBER"))
uhi<-reshape::rename(uhi, c(MEMB_INSURANCE="PAYER"))
uhi<-reshape::rename(uhi, c(?..Reg.Patient.MRN="Reg.Patient.MRN"))
allpayers<-reshape::rename(allpayers, c(?..BUS_PHONE_NUMBER="BUS_PHONE_NUMBER"))

# ï.. 

# Adds NIC to the uhi Subscriber ID if it's not there 
uhi$SUBSCRIBER_ID<-ifelse(grepl("NIC", uhi$SUBSCRIBER_ID), uhi$SUBSCRIBER_ID, paste("NIC", uhi$SUBSCRIBER_ID, sep=""))

# Deletes unused fields
uhi$Reg.Patient.MRN <- NULL
uhi$MEMB_NAME<- NULL
allpayers$MEMB_NAME <- NULL
allpayers$X.1<-NULL
allpayers$X<-NULL
uhi$X<-NULL

# Adds capitation date field
uhi$LastCapitationDate  <-	format(Sys.time(), "%m/01/%Y") 

# Creates fields with blank values
uhi$BUS_PHONE_NUMBER	<-	"NA"
uhi$CURR_PCP_ADDRESS_LINE_1	<-	"NA"
uhi$CURR_PCP_ADDRESS_LINE_2	<-	"NA"
uhi$CURR_PCP_CITY	<-	"NA"
uhi$CURR_PCP_ID	<-	"NA"
uhi$CURR_PCP_STATE	<-	"NA"
uhi$CURR_PCP_ZIP	<-	"NA"
uhi$IRS_TAX_ID	<-	"NA"
uhi$MEDICAID_NO	<-	"NA"
uhi$MEDICARE_NO	<-	"NA"
uhi$PAYER	<-	"NA"
uhi$PHONE_NUMBER	<-	"NA"
uhi$VENDOR_ID	<-	"NA"
uhi$PRACTICE	<-	"NA"

# Sorts columns in both files
allpayers<-allpayers[,order(names(allpayers))]
uhi<-uhi[,order(names(uhi))]

# Binds files horizontally
combined<-rbind(allpayers, uhi)

# Changes case of HIE ID to lowercase
combined$HIEID<-tolower(combined$HIEID)

# Identifies duplicate values with TRUE or FALSE
combined$duplicate<-duplicated(combined$HIEID)

# Identifies known twins
twins<-subset(combined,
              combined$SUBSCRIBER_ID==102239994 | 
              combined$SUBSCRIBER_ID==102239993 | 
              combined$SUBSCRIBER_ID==101760611 | 
              combined$SUBSCRIBER_ID==101957967 |
              combined$SUBSCRIBER_ID==101758438 | 
              combined$SUBSCRIBER_ID==101854600 |
              combined$SUBSCRIBER_ID==106274833 |
              combined$SUBSCRIBER_ID==106274834 |
              combined$SUBSCRIBER_ID=="H71260169" |
              combined$SUBSCRIBER_ID=="H71260168" |
              combined$SUBSCRIBER_ID=="H71514576" |
              combined$SUBSCRIBER_ID=="H71514574" |
                )

# Subsets values that are TRUE for duplicate and are not a twin
duplicates<-subset(combined, combined$duplicate==TRUE & !combined$SUBSCRIBER_ID %in% twins$SUBSCRIBER_ID)

# Identifies variables to export
duplicates<-duplicates[,c("HIEID",
                    "Source",
                    "SUBSCRIBER_ID",
                    "VEND_FULL_NAME")]

# Subsets values that are FALSE for duplicate and are not a twin
uniques<-subset(combined, combined$duplicate==FALSE& !combined$SUBSCRIBER_ID %in% twins$SUBSCRIBER_ID)

# Removes Practice field from Uniques
uniques$PRACTICE<-NULL

# Renames HIEID field
uniques<-reshape::rename(uniques, c(HIEID="Patient ID HIE"))

# Removes the field that identifies duplicates
uniques$duplicate<-NULL

# Prepare twins file for export
twins$duplicate<-NULL

# Identifies number of columns
c<-ncol(uniques)

# Identifies number of rows
r<-nrow(uniques)

# Identifies row breakpoints to create smaller files to import
r1<-round(r/4)
r2<-round((r/4)*2)
r3<-round((r/4)*3)
r4<-r

# Breaks the one uniques file into 4 pieces to import smaller sizes of data into database 
# Use only if import process freezes with the larger file
uniques1<-uniques[1:r1, 1:30]
uniques2<-uniques[(r1+1):r2, 1:30]
uniques3<-uniques[(r2+1):r3, 1:30]
uniques4<-uniques[(r3+1):r, 1:30]

# Exports files
write.csv(twins, paste(Sys.Date(), "-",file="Twins-New-HIE-ID",".csv", sep=""), row.names = FALSE)
write.csv(duplicates, paste(Sys.Date(), "-",file="HIE-Delete",".csv", sep=""), row.names = FALSE)
write.csv(uniques, paste(Sys.Date(),"-",file="TrackVia-Import", ".csv", sep=""), row.names = FALSE)
#write.csv(uniques1, paste(Sys.Date(),"-",file="TrackVia-Import-Part-1", ".csv", sep=""))
#write.csv(uniques2, paste(Sys.Date(),"-",file="TrackVia-Import-Part-2", ".csv", sep=""))
#write.csv(uniques3, paste(Sys.Date(),"-",file="TrackVia-Import-Part-3", ".csv", sep=""))
#write.csv(uniques4, paste(Sys.Date(),"-",file="TrackVia-Import-Part-4", ".csv", sep=""))

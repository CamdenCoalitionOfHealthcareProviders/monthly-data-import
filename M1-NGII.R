# Read in packages
library("reshape")

# Set working directory 
setwd("Y:/monthly import/201701/")

# Read only the first sheet of the excel file and convert to data frame
data <- read.csv("Active Resident Report January 2017.csv", header=TRUE)

# Sets all blanks cells to NA
data[data==""] <- NA

# Removes rows that are all NA
data2 <- data[apply(data, 1, function(y) !all(is.na(y))),]

# Separates name field
# data2$LastName = as.character(lapply(strsplit(as.character(data2$X.Household.Member.Name), split=", "), "[", 1))
# data2$FirstName = as.character(lapply(strsplit(as.character(data2$X.Household.Member.Name), split=", "), "[", 2))

# Deletes the row with the Super's Unit
data3 <- data2[!(data2$Household.Member.Name=="Super'S, Unit"),]

# Separates the Apt field 
data3$Floor = as.character(lapply(strsplit(as.character(data3$Apt.), split="-"), "[", 1))
data3$AptNum = as.character(lapply(strsplit(as.character(data3$Apt.), split="-"), "[", 2))

# Removes LR or HR from the AptNum field 
data3$AptNum2 <- gsub("LR", "", data3$AptNum)
data3$AptNum3 <- gsub("HR","",  data3$AptNum2)

data3$AptNum <- NULL
data3$AptNum2 <- NULL

# Determines if the apartment is a LR or HR
data3$FN <- ifelse(data3$Floor==15, "HR", "LR")

# Concatenate AptNum3 field and the FN field
data3$Apt.No <- paste0(data3$AptNum3, data3$FN, "")

data4 <- rename(data3, c(Apt="Apt_old"))
data4$Floor <- NULL  
data4$AptNum3 <- NULL
data4$FN <- NULL

# Adds a new field and populates with the current date 
data4$DateLastResidentList <- format(Sys.time(), "%m-01-%Y")

# Removes X. from column names
names(data4) <- sub("X.", "", sub("\\(.*\\)", "", names(data4)))


# Makes sure dates are in date format
# data4$Move.In.Date2 <- as.numeric(data4$Move.In.Date)
# data4$DOB2 <- as.numeric(data4$DOB)
# data4$MoveinDate <- as.Date(data4$Move.In.Date2, origin = "1899-12-30")
# data4$DateofBirth <- as.Date(data4$DOB2, origin = "1899-12-30")

# Removes the intermediary fields we used in making the date
# data4$Move.In.Date2<-NULL
# data4$DOB2<-NULL

# Renames Fields
data4$Apt. <- NULL
data4 <- rename(data4, c(Apt.No="Apt.No."))
data4 <- rename(data4, c(Household.Member.Name="Household Member Name"))
data4 <- rename(data4, c(Phone.Number="Phone Number"))
data4 <- rename(data4, c(Move.In.Date="Move In Date"))
# data4<-reshape::rename(data4, c(DOB="DOB.old"))

# Deletes the 3  totals rows in the bottom of the spreadsheet 
data5 <- head(data4,-3)

# Renames Household Member Name
data5 <- rename(data5, c(Household.Member.Name="Household Member Name"))

# Removes all NA values
data5[is.na(data5)]<-""

# Exports csv file
write.csv(data5,(file=paste ( format(Sys.Date(), "%Y-%m-%d-"),"NGII-TrackVia-Import", ".csv", sep="")), row.names=FALSE)



                          ## De-Duplication Process ##

                                    # GOAL # 

# To keep only one Patient ID HIE per set of duplicates in TrackVia, while making 
# sure we don't lose any data


# load libraries
library(dplyr)

# Set working directory
setwd("Y:/monthly import/201608/de-duplication/")

# Most recent month (CHANGE VALUE EVERY MONTH)
most_recent_month <- "Aug 01 2016"

# Read in Duplicates view from TrackVia ACO Capitation List table
view <- read.csv("duplicates_2016-08-09_2.csv", stringsAsFactors = F)

# Sort by Patient.ID.HIE, LastCapitationDate AND find duplicates
dupe_all <- view %>% arrange(Patient.ID.HIE, desc(LastCapitationDate)) %>% 
  group_by(Patient.ID.HIE) %>% 
  filter(n()>1)

# Find duplicates where both duplicate records UTCount, PRACount, and SpeOpsCount EQUAL 0
dupe_clean_in_r <- filter(dupe_all, UTCount == 0 & PRACount == 0 & SpeOpsCount == 0) %>% 
  group_by(Patient.ID.HIE) %>% 
  filter(n()>1)

# Subset with UTCount, PRACount, or SpeOpsCount GREATER THAN ZERO
dupe_clean_in_trackvia <- filter(dupe_all, UTCount > 0 | PRACount > 0 | SpeOpsCount > 0)

# check to make sure dupe_clean_in_r and dupe_clean_in_trackvia don't have same records
dupe_clean_in_trackvia %>% inner_join(dupe_clean_in_r, by = "Patient.ID.HIE")

# For those records in dupe_clean_in_r (record with ZERO UTCount, PRACount, SpeOpsCount)
  # most_recent_month labeled as "A", other month listed as "B" 
dupe_clean_in_r$aorb <- ifelse(dupe_clean_in_r$LastCapitationDate == most_recent_month, "A", "B")
dupe_a <- filter(dupe_clean_in_r, aorb == "A")
dupe_b <- filter(dupe_clean_in_r, aorb == "B")

# Switch Record Locator from dupe_b to dupe_a
# mark dupe_a$Duplicate "" and keep dupe_b$Duplicate "Yes"
dupe_a_rec <- select(dupe_a, Patient.ID.HIE, Record.Locator)
dupe_b_rec <- select(dupe_b, Patient.ID.HIE, Record.Locator)

dupe_a_new_rec <- left_join(dupe_a, dupe_b_rec, by = "Patient.ID.HIE")
dupe_a_new_rec$Record.Locator.x <- NULL
dupe_a_new_rec$Duplicate <- ""
dupe_a_new_rec_to_bind <- rename(dupe_a_new_rec, "Record Locator" = Record.Locator.y) 

dupe_b_new_rec <- left_join(dupe_b, dupe_a_rec, by = "Patient.ID.HIE")
dupe_b_new_rec$Record.Locator.x <- NULL
dupe_b_new_rec_to_bind <- rename(dupe_b_new_rec, "Record Locator" = Record.Locator.y) 

# Bind dupe_a and dupe_b
dupe_upload_to_trackvia <- rbind(dupe_a_new_rec_to_bind, dupe_b_new_rec_to_bind)

# Clean date fields: LastCapitationDate, DOB, ArchiveCreatedDate
dupe_upload_to_trackvia$LastCapitationDate <- strptime(dupe_upload_to_trackvia$LastCapitationDate, "%b%d%Y")
dupe_upload_to_trackvia$DOB <- strptime(dupe_upload_to_trackvia$DOB, "%b%d%Y")
dupe_upload_to_trackvia$ArchiveCreatedDate <- strptime(dupe_upload_to_trackvia$ArchiveCreatedDate, "%b%d%Y")

# write to CSV (dupe_upload_to_trackvia.csv)
write.csv(dupe_upload_to_trackvia, "dupe_upload_to_trackvia.csv", row.names = F)

# Write dupe_clean_in_trackvia to CSV
## THESE ARE NOT DUPLICATES FOR AUGUST 2016 CAP MONTH  ##
### THESE RECORDS WERE CLEANED MANUALLY 8/9/2016 ###
#### STILL UPLOAD THESE RECORDS TO TRACKVIA WITH DUPLICATES COLUMN BLANK ####

# dupe_clean_in_trackvia$Duplicate <- "" ### Don't use this line next month (SEPTEMBER)
write.csv(dupe_clean_in_trackvia, "dupe_clean_in_trackvia.csv", row.names = F)

# Find records that are not in dupe_all
# These records were in the Duplicates view in TrackVia but might not be actual duplicate records
view_not_dupes <- anti_join(view, dupe_all)
write.csv(view_not_dupes, "view_not_dupes_to_upload.csv", row.names = F)

# To do once you have CSVs:
# Save as Excel files
# Remove "NAs" from fields (don't erase, just remove the "NA")
# Convert scientific notation in Medicaid and Medicare number fields to normal 


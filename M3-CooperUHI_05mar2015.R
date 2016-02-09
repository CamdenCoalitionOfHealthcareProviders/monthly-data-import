date.of.pull <- tolower(strftime(as.POSIXct("2016-02-01"), format = "%m%d%Y"))
data.directory <- paste("Y:/Monthly Import/Feb 2016/")
file.name.stem <- "3 cooper sch data"
date<-format(Sys.Date(), "%B%Y")

IM.Visits <- read.delim(paste(data.directory,file.name.stem,"_",date.of.pull,".txt",sep = ""), dec=",", quote = "")

write.csv(IM.Visits, file="IM Visits Original.csv")

# Needed packages/modules
suppressMessages(require(stringr))
suppressMessages(require(zipcode))
suppressMessages(require(reshape))


CACC.Location <- function(Sch.Location.Name) {
  temp <- rep(FALSE,length(Sch.Location.Name))
  temp[which(Sch.Location.Name == "3 COOPER PLAZA STE 403 CACC")] <- TRUE
  temp[which(Sch.Location.Name == "3 COOPER PLAZA STE 215 CACC")] <- TRUE
  temp[which(Sch.Location.Name == "3 COOPER PLAZA STE 220 CACC")] <- TRUE
  temp[which(Sch.Location.Name == "3 COOPER PLAZA SUITE 215 MEDICINE")] <- TRUE
  temp[which(Sch.Location.Name == "3 COOPER PLAZA STE 403 ORTHO")] <- TRUE
  temp[which(Sch.Location.Name == "3 COOPER PLAZA STE 403 SURG")] <- TRUE
  return(temp)
}

CACC.Predecessor.Location <- function(Sch.Location.Name) {
  temp <- rep(FALSE,length(Sch.Location.Name))
  temp[which(Sch.Location.Name == "3 COOPER PLAZA STE 403 CACC")] <- TRUE
  temp[which(Sch.Location.Name == "3 COOPER PLAZA STE 215 CACC")] <- TRUE
  temp[which(Sch.Location.Name == "3 COOPER PLAZA STE 220 CACC")] <- TRUE
  temp[which(Sch.Location.Name == "3 COOPER PLAZA SUITE 215 MEDICINE")] <- TRUE
  temp[which(Sch.Location.Name == "3 COOPER PLAZA STE 403 ORTHO")] <- TRUE
  temp[which(Sch.Location.Name == "3 COOPER PLAZA STE 403 SURG")] <- TRUE
  temp[which(Sch.Location.Name == "COOPER NEUROLOGICAL INSTITUTE")] <- TRUE
  return(temp)
}

CACC.Predecessor.Specialty <- function(Sch.Dept.Desc) {
  temp <- rep(FALSE,length(Sch.Dept.Desc))
  temp[which(Sch.Dept.Desc == "DEPT OF CPO 215")] <- TRUE
  temp[which(Sch.Dept.Desc == "COOPER NEUROLOGICAL INSTITUTE")] <- TRUE
  temp[which(Sch.Dept.Desc == "DEPARTMENT OF GASTROENTEROLOGY")] <- TRUE
  temp[which(Sch.Dept.Desc == "VASCULAR SURGERY")] <- TRUE
  temp[which(Sch.Dept.Desc == "DEPARTMENT OF DERM COOPER")] <- TRUE
  temp[which(Sch.Dept.Desc == "OTOLARYNGOLOGY")] <- TRUE
  temp[which(Sch.Dept.Desc == "CARDIOTHORACIC SURGERY")] <- TRUE
  temp[which(Sch.Dept.Desc == "DEPARTMENT OF ENDOCRINOLOGY")] <- TRUE
  temp[which(Sch.Dept.Desc == "DEPARTMENT OF NEPHROLOGY")] <- TRUE  
  temp[which(Sch.Dept.Desc == "DEPARTMENT OF NEUROLOGY")] <- TRUE
  temp[which(Sch.Dept.Desc == "DEPARTMENT OF RHEUMATOLOGY")] <- TRUE
  temp[which(Sch.Dept.Desc == "DEPT OF PULMONARY")] <- TRUE
  temp[which(Sch.Dept.Desc == "GENERAL SURGERY")] <- TRUE
  temp[which(Sch.Dept.Desc == "ORTHOPAEDICS")] <- TRUE
  temp[which(Sch.Dept.Desc == "PLASTIC SURGERY")] <- TRUE
  temp[which(Sch.Dept.Desc == "PODIATRY")] <- TRUE
  temp[which(Sch.Dept.Desc == "TRAUMA")] <- TRUE  
  temp[which(Sch.Dept.Desc == "UROLOGY")] <- TRUE
  temp[which(Sch.Dept.Desc == "PHYSICAL MEDICINE & REHAB")] <- TRUE
  temp[which(Sch.Dept.Desc == "NEUROSURGERY")] <- TRUE
  return(temp)
}

CACC.Div <- function(Sch.Dept.Desc) {
  temp <- rep(FALSE,length(Sch.Dept.Desc))
  temp[which(Sch.Dept.Desc == "Int Med")] <- TRUE
  temp[which(Sch.Dept.Desc == "GI")] <- TRUE
  temp[which(Sch.Dept.Desc == "Vascular")] <- TRUE
  temp[which(Sch.Dept.Desc == "Derm")] <- TRUE
  temp[which(Sch.Dept.Desc == "ENT")] <- TRUE
  temp[which(Sch.Dept.Desc == "CT Surg")] <- TRUE
  temp[which(Sch.Dept.Desc == "Endo")] <- TRUE
  temp[which(Sch.Dept.Desc == "Neph")] <- TRUE  
  temp[which(Sch.Dept.Desc == "Neuro")] <- TRUE
  temp[which(Sch.Dept.Desc == "Rheum")] <- TRUE
  temp[which(Sch.Dept.Desc == "Pulm")] <- TRUE
  temp[which(Sch.Dept.Desc == "Gen Surg")] <- TRUE
  temp[which(Sch.Dept.Desc == "Ortho")] <- TRUE
  temp[which(Sch.Dept.Desc == "Plastics")] <- TRUE
  temp[which(Sch.Dept.Desc == "Pod")] <- TRUE
  temp[which(Sch.Dept.Desc == "Trauma")] <- TRUE  
  temp[which(Sch.Dept.Desc == "Urol")] <- TRUE
  temp[which(Sch.Dept.Desc == "PM&R")] <- TRUE
  temp[which(Sch.Dept.Desc == "Neuro Surg")] <- TRUE
  return(temp)
}


CACC.Specialty <- function(Sch.Dept.Desc) {
  temp <- rep(FALSE,length(Sch.Dept.Desc))
  temp[which(Sch.Dept.Desc == "DEPT OF CPO 215")] <- TRUE
  temp[which(Sch.Dept.Desc == "DEPARTMENT OF GASTROENTEROLOGY")] <- TRUE
  temp[which(Sch.Dept.Desc == "VASCULAR SURGERY")] <- TRUE
  temp[which(Sch.Dept.Desc == "DEPARTMENT OF DERM COOPER")] <- TRUE
  temp[which(Sch.Dept.Desc == "OTOLARYNGOLOGY")] <- TRUE
  temp[which(Sch.Dept.Desc == "CARDIOTHORACIC SURGERY")] <- TRUE
  temp[which(Sch.Dept.Desc == "DEPARTMENT OF ENDOCRINOLOGY")] <- TRUE
  temp[which(Sch.Dept.Desc == "DEPARTMENT OF NEPHROLOGY")] <- TRUE  
  temp[which(Sch.Dept.Desc == "DEPARTMENT OF NEUROLOGY")] <- TRUE
  temp[which(Sch.Dept.Desc == "DEPARTMENT OF RHEUMATOLOGY")] <- TRUE
  temp[which(Sch.Dept.Desc == "DEPT OF PULMONARY")] <- TRUE
  temp[which(Sch.Dept.Desc == "GENERAL SURGERY")] <- TRUE
  temp[which(Sch.Dept.Desc == "ORTHOPAEDICS")] <- TRUE
  temp[which(Sch.Dept.Desc == "PLASTIC SURGERY")] <- TRUE
  temp[which(Sch.Dept.Desc == "PODIATRY")] <- TRUE
  temp[which(Sch.Dept.Desc == "TRAUMA")] <- TRUE  
  temp[which(Sch.Dept.Desc == "UROLOGY")] <- TRUE
  temp[which(Sch.Dept.Desc == "PHYSICAL MEDICINE & REHAB")] <- TRUE
  temp[which(Sch.Dept.Desc == "NEUROSURGERY")] <- TRUE
  temp[which(Sch.Dept.Desc == "DEPARTMENT OF INF. DISEASE")] <- TRUE
  temp[which(Sch.Dept.Desc == "BREAST SURGERY")] <- TRUE
  temp[which(Sch.Dept.Desc == "COLORECTAL SURGERY")] <- TRUE
  return(temp)
}

Camden.ZIP <- function(ZIP) {
  temp <- rep(FALSE,length(ZIP))
  temp[which(ZIP == "08101")] <- TRUE
  temp[which(ZIP == "08102")] <- TRUE
  temp[which(ZIP == "08103")] <- TRUE
  temp[which(ZIP == "08104")] <- TRUE
  temp[which(ZIP == "08105")] <- TRUE
  #temp[which(ZIP == "08106")] <- TRUE turned this off on 12jul2013
  return(temp)
}

CACC.Patient <- function(FSC,ZIP) {
  temp <- rep(FALSE,length(FSC))
  temp[which(FSC == "MANAGED MEDICAID")] <- TRUE
  temp[which(FSC == "MEDICAID")] <- TRUE
  temp[which(FSC == "SELF PAY")] <- TRUE
  temp[which(FSC == "MEDICARE" & Camden.ZIP(ZIP))] <- TRUE
  temp[which(FSC == "MANAGED MEDICARE" & Camden.ZIP(ZIP))] <- TRUE
  return(temp)
}



Shorten.Division.Names <- function(Sch.Dept.Desc) {
  Sch.Dept.Desc <- as.character(Sch.Dept.Desc)
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "DEPT OF CPO 215", "Int Med")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "DEPARTMENT OF GASTROENTEROLOGY", "GI")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "DEPARTMENT OF CARDIOLOGY", "Cardio")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "VASCULAR SURGERY", "Vascular")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "DEPARTMENT OF DERM COOPER", "Derm")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "OTOLARYNGOLOGY", "ENT")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "CARDIOTHORACIC SURGERY", "CT Surg")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "DEPARTMENT OF ENDOCRINOLOGY", "Endo")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "DEPARTMENT OF INF. DISEASE", "Inf Dis")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "DEPARTMENT OF NEPHROLOGY", "Neph")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "DEPARTMENT OF NEUROLOGY", "Neuro")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "DEPARTMENT OF RHEUMATOLOGY", "Rheum")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "DEPT OF PULMONARY", "Pulm")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "GENERAL SURGERY", "Gen Surg")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "ORTHOPAEDICS", "Ortho")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "PEDIATRIC SURGERY", "Ped Surg")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "PLASTIC SURGERY", "Plastics")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "PODIATRY", "Pod")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "TRAUMA", "Trauma")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "UROLOGY", "Urol")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "PHYSICAL MEDICINE & REHAB", "PM&R")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "NEUROSURGERY", "Neuro Surg")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "COOPER NEUROLOGICAL INSTITUTE", "CNI")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "COOPER FACULTY OB/GYN", "OB/GYN")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "BMA ENDOCRINOLOGY", "Endo")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "BMA GASTROENTEROLOGY", "GI")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "BMA PULMONARY", "Pulm")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "BREAST SURGERY", "Breast Surg")
  Sch.Dept.Desc <- str_replace(Sch.Dept.Desc, "COLORECTAL SURGERY", "Colorectal Surg")
  return(Sch.Dept.Desc)
}




Shorten.Location.Names <- function(Sch.Location.Name) {
  Sch.Location.Name <- str_replace(Sch.Location.Name, "3 COOPER PLAZA SUITE 215 MEDICINE", "Suite 215")
  Sch.Location.Name <- str_replace(Sch.Location.Name, "3 COOPER PLAZA STE 403 ORTHO", "Suite 403")
  Sch.Location.Name <- str_replace(Sch.Location.Name, "3 COOPER PLAZA STE 403 SURG", "Suite 403")
  Sch.Location.Name <- str_replace(Sch.Location.Name, "3 COOPER PLAZA STE 220 CACC", "Suite 220")
  Sch.Location.Name <- str_replace(Sch.Location.Name, "3 COOPER PLAZA STE 215 CACC", "Suite 215")
  Sch.Location.Name <- str_replace(Sch.Location.Name, "3 COOPER PLAZA STE 403 CACC", "Suite 403")
  Sch.Location.Name <- str_replace(Sch.Location.Name, "3 COOPER PLAZA STE 104", "Suite 104")
  return(Sch.Location.Name)
}

Condense.Visit.Types <- function(Sch.Visit.Type.Name) {
  Sch.Visit.Type.Name <- as.character(Sch.Visit.Type.Name)
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "FELLOW FOLLOW-UP", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "FOLLOW -UP", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "FOLLOW-UP", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "FOLLOW-UP VISIT VISIT", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "FOLLOW UP", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "FOLLOW UP PULMONARY", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "FOLLOW-UP VISIT PULMONARY", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "FOLLOW UP SLEEP", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "FOLLOW-UP VISIT SLEEP", "FOLLOW-UP VISIT") 
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "FOLLOW UP VISIT", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "POST-OP", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "HOSPITAL FOLLOW-UP VISIT", "FOLLOW-UP VISIT")  
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "POST OP-1ST VISIT", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "POST OP VISIT", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "BREAST PATIENT", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "COSMETIC VISIT", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "ENDOCRINE", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "PHYSICAL 30 MINUTES", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "ROWAN MEDICAL STUDENT", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "SICK FOLLOW-UP", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "STUDENTS", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "SUTURE REMOVAL", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "FRACTURE CARE", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "PRE-OP CL", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "PROCEDURE", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "SURGICAL FOLLOW-UP VISIT", "FOLLOW-UP VISIT")  
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "SURGICAL PROCEDURE", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "ACUPUNTURE", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "ACUPUNCTURE", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "FOLLOW-UP PAP", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "MANIPULATION AND OSTEOPATHIC", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "POST OP-2ND VISIT", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "POST OPERATIVE", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "PRE OPERATIVE", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "PROBLEM VISIT", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "POST OP VISIT", "FOLLOW-UP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "FOLLOW-UP VISIT VISIT", "FOLLOW-UP VISIT")
  
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "HOSPITAL RELEASE", "NEW PATIENT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "HOSPITAL RELEASE 1", "NEW PATIENT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "IN PATIENT REFERRAL", "NEW PATIENT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "IN PRACTICE REFERRAL", "NEW PATIENT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "INITIAL OFFICE VISIT", "NEW PATIENT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "NEW HOSPITAL PATIENT", "NEW PATIENT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "NEW PATIENT VISIT", "NEW PATIENT")  
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "NEW PULMONARY", "NEW PATIENT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "NEW SLEEP PT", "NEW PATIENT")  
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "SICK NEW PATIENT", "NEW PATIENT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "HOSPITAL FOLLOW UP", "NEW PATIENT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "INITIAL OFFICE VISIT CONCUSSION", "NEW PATIENT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "NEW PT SEEN/DIV IN COOPER HOSP", "NEW PATIENT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "NEW PATIENT 1", "NEW PATIENT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "NEW PATIENT CONCUSSION", "NEW PATIENT")
  
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "AORTA ULTRASOUND", "OTHER")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "ARTERIAL", "OTHER")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "CAROTID ULTRASOUND", "OTHER")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "PATCH TEST", "OTHER")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "PPD PLACEMENT", "OTHER")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "PPD READ", "OTHER")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "RENAL VISCERAL", "OTHER")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "VEIN MAPPING DIALYSIS", "OTHER")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "VENOUS", "OTHER")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "BOTOX INJECTION", "OTHER")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "COLPOSCOPYL", "OTHER")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GLUCOSE CHECK", "OTHER")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "INJECTION", "OTHER")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "INJECTION ", "OTHER")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "BLOOD PRESSURE CHECK", "OTHER")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "OTHER ", "OTHER")
  
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GROUP VISIT 120 MIN", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP HEADACHES", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP DIABETES POST HOSPITAL", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP DIABETES", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP DIABETIC IOV", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP UHI HEART FAILURE", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP PRIMARY MUSCULOSKELETAL", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP SLEEP APNEA", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP CHRONIC KIDNEY", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP METABOLIC", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP HAND UPPER EXTREMITY", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP ASTHMA COPD", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP HEPATITIS", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP COUMADIN", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP BENIGN PROSTATE HYPRTROPHY", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP DIABETIC LOWER EXT SCR NEW", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP DIABETES FUV 1 HR ENGLISH", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP DIABETES FUV 1 HR SPANISH", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP DIABETES NEW 1 HR SPANISH", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP DIABETES NEW 1 HR ENGLISH", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP HAND SURGERY FUV", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP HAND SURGERY NEW", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP KNEE PAIN NEW", "GROUP VISIT")
  Sch.Visit.Type.Name <- str_replace(Sch.Visit.Type.Name, "GRP KNEE PAIN NEW SPANISH", "GROUP VISIT")
  Sch.Visit.Type.Name <- factor(Sch.Visit.Type.Name)
  return(Sch.Visit.Type.Name)
}




Visit.Length.Adjustment <- function(df, GME.Visit.Length.Adj = FALSE) {
  df$Adj.Util.Minutes <- df$Sch.Scheduled.Minutes
  
  #Group Visit Corrections
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GROUP VISIT 120 MIN")] <- 15
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP HEADACHES")] <- 15
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP DIABETES POST HOSPITAL")] <- 15
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP DIABETES")] <- 15
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP DIABETIC IOV")] <- 45
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP UHI HEART FAILURE")] <- 20
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP PRIMARY MUSCULOSKELETAL")] <- 15
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP SLEEP APNEA")] <- 30
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP CHRONIC KIDNEY")] <- 20
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP METABOLIC")] <- 15
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP HAND UPPER EXTREMITY")] <- 15
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP ASTHMA COPD")] <- 30
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP HEPATITIS")] <- 15
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP COUMADIN")] <- 15
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP BENIGN PROSTATE HYPRTROPHY")] <- 15
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP DIABETES NEW 1 HR ENGLISH")] <- 45
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP DIABETES NEW 1 HR SPANISH")] <- 45
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP DIABETES NEW 1 HR ENGLISH")] <- 15
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP DIABETES NEW 1 HR SPANISH")] <- 45
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP HAND SURGERY FUV")] <- 15
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP HAND SURVERY NEW")] <- 15
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP KNEE PAIN NEW")] <- 15
  df$Adj.Util.Minutes[which(as.character(df$Sch.Visit.Type.Name) == "GRP KNEE PAIN NEW SPANISH")] <- 15
  
  
  #GME.Visit Correction
  if (GME.Visit.Length.Adj) {
    df$Adj.Util.Minutes[which(as.character(df$Sch.VisitCatName) == "ESTABLISHED PATIENT" &
                                as.character(df$GME.Visit) == "Int Med - GME"  )] <- (16/28) * df$Adj.Util.Minutes[which(as.character(df$Sch.VisitCatName) == "ESTABLISHED PATIENT" &
                                                                                                                           as.character(df$GME.Visit) == "Int Med - GME"  )]
    
    df$Adj.Util.Minutes[which(as.character(df$Sch.VisitCatName) == "ESTABLISHED PATIENT" &
                                as.character(df$GME.Visit) == "Neph - Fellows"  )] <- (21/28) * df$Adj.Util.Minutes[which(as.character(df$Sch.VisitCatName) == "ESTABLISHED PATIENT" &
                                                                                                                            as.character(df$GME.Visit) == "Neph - Fellows"  )]
    
    df$Adj.Util.Minutes[which(as.character(df$Sch.VisitCatName) == "PROCEDURES" &
                                as.character(df$GME.Visit) == "Plastics - GME"  )] <- (15/25) * df$Adj.Util.Minutes[which(as.character(df$Sch.VisitCatName) == "PROCEDURES" &
                                                                                                                            as.character(df$GME.Visit) == "Plastics - GME"  )]
    
  }
  
  return(df)
}



Distance.From.3Cooper <- function(lat,long) {
  r <- 6371*0.621371 # radius of the Earth in miles
  ThreeCooperLong <- as.numeric(-75.11513)
  ThreeCooperLat <- as.numeric(39.93618)
  # Convert to radians and make two vectors of point A and point B
  long <- long * pi/180
  lat <- lat * pi/180
  long2 <- ThreeCooperLong * pi/180
  lat2 <- ThreeCooperLat * pi/180
  return(acos(sin(lat)*sin(lat2) + cos(lat)*cos(lat2) * cos(long2-long)) * r)
}

IDX.User <- function(df) {
  Users <- read.delim("W:/Call Center/IDX User List.txt")
  df <- merge(df,Users, by.x = "Sch.Appt.Username", by.y = "USERNAME", all.x = TRUE)
  df$FULL.NAME <- as.character(df$FULL.NAME)
  df$FULL.NAME[which(is.na(df$FULL.NAME))] <- as.character(df$Sch.Appt.Username[which(is.na(df$FULL.NAME))])
  return(df)
}


FUV.Lag <- function(tempVisits, Division, tempAppts) {
  temp <- data.frame(Same.Day.Scheduled.Lag=as.numeric(NA),
                     Percent.Scheduled.Same.Day=as.numeric(NA),
                     Any.Day.Scheduled.Lag=as.numeric(NA),
                     Percent.Scheduled.Any.Day=as.numeric(NA))
  tempAppts <- tempAppts[which(as.character(tempAppts$Sch.Dept.Desc) == Division),]
  tempAppts$Visit.Date <- as.POSIXlt(NA)
  if (nrow(tempAppts) == 0) return(temp)
  if (nrow(tempVisits) == 0) return(temp)
  if(is.null(tempVisits)) return(temp)
  for (i in 1:nrow(tempAppts)) {
    test <- (tempVisits$Reg.Patient.MRN == tempAppts$Reg.Patient.MRN[i] &
               as.numeric(difftime(tempVisits$Txn.Service.Date, tempAppts$Sch.Appt.Date[i],  units = "days")) < 2)
    if(sum(test) == 0) next
    tempdate <- tempVisits$Txn.Service.Date[which(test)]
    tempAppts$Visit.Date[i] <- max(tempdate, na.rm = TRUE)
  }
  
  rm(test,  i)
  
  tempAppts <- tempAppts[-which(is.na(tempAppts$Visit.Date)),]
  
  if (nrow(tempAppts) == 0) return(temp)
  tempAppts$Sch.Same.Day <- (abs(difftime(tempAppts$Sch.Schedule.Date, tempAppts$Visit.Date, units = "days")) < 1)
  temp$Same.Day.Scheduled.Lag <- ifelse(sum(tempAppts$Sch.Same.Day) > 0, 
                                        mean(difftime(tempAppts$Sch.Appt.Date[which(tempAppts$Sch.Same.Day)], tempAppts$Visit.Date[which(tempAppts$Sch.Same.Day)], units = "days")),
                                        0)
  temp$Percent.Scheduled.Same.Day <- sum(tempAppts$Sch.Same.Day) / nrow(tempVisits)
  temp$Any.Day.Scheduled.Lag <- as.numeric(mean(difftime(tempAppts$Sch.Appt.Date, tempAppts$Visit.Date, units = "days")))
  temp$Percent.Scheduled.Any.Day <- nrow(tempAppts) / nrow(tempVisits)
  return(temp)
}


IDX.file.prep <- function(df, IDX = TRUE, GME.Visit.Length.Adj = FALSE, CACC_only = FALSE, time.period = "Week") { 
  
  if(class(df$Sch.Schedule.Date)[1] != "POSIXct") {df$Sch.Schedule.Date <- strptime(df$Sch.Schedule.Date,format="%m/%d/%Y")}
  if(class(df$Sch.Appt.Date)[1] != "POSIXct") {df$Sch.Appt.Date <- strptime(df$Sch.Appt.Date,format="%m/%d/%Y")}
  df$Sch.Appt.Date <- as.Date(df$Sch.Appt.Date)
  df$Appt.Day <- factor(strftime(df$Sch.Appt.Date,format="%a"), levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), ordered = TRUE) 
  df$Appt.Year <- strftime(df$Sch.Appt.Date,format="%Y") 
  df$Sch.Schedule.Date <- as.Date(df$Sch.Schedule.Date)
  df$Sch.Cancel.Bump.Date <- strptime(df$Sch.Cancel.Bump.Date,format="%m/%d/%Y")
  df$Cancel.Bump.Notice <- Cancel.Bump.Notice(df$Sch.Cancel.Bump.Date, df$Sch.Appt.Date)
  if ("SchRescheduleDtid.ActDate" %in% colnames(df)) df$SchRescheduleDtid.ActDate <- strptime(df$SchRescheduleDtid.ActDate,format="%m/%d/%Y")
  if ("SchRescheduleApptDtId.ActDate" %in% colnames(df)) df$SchRescheduleApptDtId.ActDate <- strptime(df$SchRescheduleApptDtId.ActDate,format="%m/%d/%Y")
  if ("SchRescheduleApptDtId.ActDate" %in% colnames(df)) df$Cancel.Bump.Lag <- Cancel.Bump.Notice( df$Sch.Cancel.Bump.Date, df$SchRescheduleApptDtId.ActDate)
  
  if(time.period == "Week") {
    df$Appt.Week <- strftime(df$Sch.Appt.Date,format="%Y.W%W") 
    df$Appt.Week <- str_replace(replacement = "2013.W52", string = df$Appt.Week, pattern = "2014.W00")
    df$Sch.Week <- strftime(df$Sch.Schedule.Date,format="%Y.W%W")
    df$Sch.Week <- str_replace(replacement = "2013.W52",string = df$Sch.Week, pattern = "2014.W00")
    df$Cancel.Bump.Week <- strftime(df$Sch.Cancel.Bump.Date,format="%Y.W%W")
    df$Cancel.Bump.Week <- str_replace(replacement = "2013.W52",string = df$Cancel.Bump.Week, pattern = "2014.W00")
  }
  if(time.period == "Month") {
    df$Appt.Month <- strftime(df$Sch.Appt.Date,format="%Y.M%m") 
    df$Sch.Month <- strftime(df$Sch.Schedule.Date,format="%Y.M%m")
    df$Cancel.Bump.Month <- strftime(df$Sch.Cancel.Bump.Date,format="%Y.M%m")
  }
  
  df$Binned.Lag <- cut(as.numeric(difftime(df$Sch.Appt.Date, df$Sch.Schedule.Date, units="days")), 
                       breaks = c(-1,0,1,2,3,7,14,21,30,60,90,180,50000),
                       labels = c("Same Day", "1 Day", "2 Days", "3 Days", "4-5 Days", "1-2 Weeks", "2-3 Weeks", "3 Weeks - 1 Month", "1-2 Months",
                                  "2-3 Months", "3-6 Months", "6+ Months"))
  
  
  
  if (CACC_only) {
    df$Reg.Patient.Zip <- clean.zipcodes(df$Reg.Patient.Zip)
    df$Condensed.Visit.Types <- Condense.Visit.Types(df$Sch.Visit.Type.Name)
    df$CACC.Location <- CACC.Location(df$Sch.Location.Name)
    df$CACC.Specialty <- CACC.Specialty(df$Sch.Dept.Desc)
    df$CACC.Predecessor.Specialty <- CACC.Predecessor.Specialty(df$Sch.Dept.Desc)
    df$CACC.Patient <- CACC.Patient(df$Reg.Fsc.1.Rpt.Cat.3.Name, df$Reg.Patient.Zip)
    df$Reg.Patient.Birth.Date <- strptime(df$Reg.Patient.Birth.Date,format="%m/%d/%Y")
  }
  if ("Sch.Appointment.Time" %in% colnames(df)) {
    df$Sch.Appointment.Time <- gsub(pattern = " N", replacement = "PM", df$Sch.Appointment.Time)
    df$Sch.Appointment.Time <- gsub(pattern = " M", replacement = "AM", df$Sch.Appointment.Time)
    df$Sch.Appt.Time <- strptime(df$Sch.Appointment.Time, format = "%I:%M:%S%p")
  }
  else {
    df$Sch.Appt.Time <- strptime(df$Sch.Appointment.Date.Time, format = "%m/%d/%Y %l:%M:%S %p")
  }
  
  df$Sch.Appt.Time.Minutes <- strftime(df$Sch.Appt.Time, format = "%M")
  df$Sch.Appt.Time <- strftime(df$Sch.Appt.Time, format = "%H")
  df$Start.Time <- as.integer(df$Sch.Appt.Time) * 60 + as.integer(df$Sch.Appt.Time.Minutes)
  df$End.Time <- as.integer(df$Sch.Appt.Time) * 60 + as.integer(df$Sch.Appt.Time.Minutes) + as.integer(df$Sch.Scheduled.Minutes)
  df$Sch.Appt.Session <- ifelse(df$Sch.Appt.Time < 12, "AM", ifelse(df$Sch.Appt.Time < 17, "PM", "EVE"))
  df$Sch.Appt.Session <- factor(df$Sch.Appt.Session, levels = c("AM", "PM", "EVE"), ordered = TRUE)
  
  df$adj.Status <- ifelse(as.character(df$Sch.Status.Description) == "Cancelled" &
                            df$Cancel.Bump.Notice < 2,"No_Notice_Cancel", as.character(df$Sch.Status.Description))
  df$adj.Status <- gsub("No Show", "No_Show", df$adj.Status)
  df$Yield <- NA
  df$Yield <- ifelse(as.character(df$adj.Status) == "No_Notice_Cancel" | as.character(df$adj.Status) == "No_Show",
                     0,NA)
  df$Yield <- ifelse(as.character(df$adj.Status) == "Arrived" & is.na(df$Yield),
                     1,df$Yield)
  
  if (CACC_only) df$CACC.Predecessor.Location <- CACC.Predecessor.Location(df$Sch.Location.Name)
  if (CACC_only) df$Condensed.Visit.Types <- Condense.Visit.Types(df$Sch.Visit.Type.Name) 
  if (CACC_only) df$Sch.Location.Name <- Shorten.Location.Names(df$Sch.Location.Name)
  if (CACC_only) df$Sch.Dept.Desc <- Shorten.Division.Names(df$Sch.Dept.Desc)
  if (CACC_only) df$Non.Prov.Visit <- Non.Prov.Visit(df$Sch.Scheduled.Provider.Name)
  if (IDX) df <- IDX.User(df)
  if (IDX) df$IDX.User <- df$FULL.NAME
  if (IDX) df <- subset(df, select = -c(FULL.NAME))
  if (CACC_only) df <- GME.Visit(df)
  df <- Visit.Length.Adjustment(df, GME.Visit.Length.Adj = GME.Visit.Length.Adj)
  if (CACC_only) df <- Exception.Label(df)
  if (CACC_only) df <- GV.Opp.Label(df)
  if (CACC_only)  df$Patient.Binned.Age <- cut(as.numeric(difftime(df$Sch.Appt.Date, as.Date(df$Reg.Patient.Birth.Date), units="days")/365.25), 
                                               breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80, 150))
  
  df$Sch.Dept.Desc <- as.factor(df$Sch.Dept.Desc)
  df$Location <- as.factor(df$Sch.Location.Name)
  df$Location <- substr(as.character(df$Location), start = 1, stop = 31)
  df$Provider <- as.factor(df$Sch.Scheduled.Provider.Name)
  df$Division <- as.factor(df$Sch.Dept.Desc)
  df$Session.Key <- paste(df$Sch.Scheduled.Provider.Name, df$Sch.Dept.Desc, df$Location, df$Sch.Appt.Date, df$Sch.Appt.Session, sep = " ; ")
  #put this back on on Monday August 5  df$Reg.Patient.MRN <- as.character(df$Reg.Patient.MRN)
  return(df)
}

Cancel.Bump.Notice <- function(Sch.Cancel.Bump.Date, Sch.Appt.Date) {
  temp <- rep(NA, length = length(Sch.Cancel.Bump.Date))
  temp <- as.integer(round(difftime(Sch.Appt.Date, Sch.Cancel.Bump.Date, units="days")))
  #The code below converts the difference from calendar days to working days (no holiday correction)
  remainder <- temp %% 7
  remainder <- remainder + (-2)*(as.integer(strftime(Sch.Cancel.Bump.Date, format = "%w")) > 
                                   as.integer(strftime(Sch.Appt.Date, format = "%w")))
  temp <- 5 * (temp %/% 7) + remainder
  return(temp)
}



GME.Visit <- function(df) {
  temp <- rep.int("Attending",nrow(df))
  temp[which(as.character(df$Sch.Scheduled.Provider.Name) == "GENERAL MD,CLINIC")] <- "GME" #Gen Surg GME Clinic
  temp[which(as.character(df$Sch.Scheduled.Provider.Name) == "BREAST CLINIC SURGERY")] <- "GME" #Gen Surg GME Clinic
  temp[which(as.character(df$Sch.Dept.Desc) == "Derm")] <- "GME" #All Derm visits are GME
  temp[which(as.character(df$Sch.Dept.Desc) == "Neuro" &
               as.character(df$Sch.Location.Name) == "Suite 403" &
               as.character(strftime(strptime(df$Sch.Appt.Date, format = "%Y-%m-%d"), format = "%a")) == "Tue")] <- "GME" #Neuro Tuesday GME 
  temp[which(as.character(df$Sch.Dept.Desc) == "Neuro" &
               as.character(df$Sch.Location.Name) == "Suite 403" &
               as.character(strftime(strptime(df$Sch.Appt.Date, format = "%Y-%m-%d"), format = "%a")) == "Thu")] <- "GME" #Neuro Thursday GME  
  temp[which(as.character(df$Sch.Dept.Desc) == "Rheum" &
               as.character(df$Sch.Location.Name) == "Suite 403" &
               as.character(df$Sch.Scheduled.Provider.Name) != "HAN MD,KWANG HOON" &
               as.character(strftime(strptime(df$Sch.Appt.Date, format = "%Y-%m-%d"), format = "%a")) == "Wed")] <- "Fellows" #Rheum Fellows
  temp[which(as.character(df$Sch.Dept.Desc) == "Plastics" &
               as.character(df$Sch.Location.Name) == "Suite 403" &
               as.character(strftime(strptime(df$Sch.Appt.Date, format = "%Y-%m-%d"), format = "%a")) == "Thu")] <- "GME" #Plastics Thursday GME 
  temp[which(as.character(df$Sch.Scheduled.Provider.Name) == "NEUROSURGERY RESIDENT,CLINIC")] <- "GME" #Neuro Surg GME Clinic
  temp[which(as.character(df$Sch.Dept.Desc) == "Int Med" &
               (as.character(df$Sch.Scheduled.Provider.Name) != "KIM DO,NAMI" &
                  as.character(df$Sch.Scheduled.Provider.Name) != "MARTINEZ MD,MIGUEL E" &
                  as.character(df$Sch.Scheduled.Provider.Name) != "MELLI MD,JENNY" &
                  as.character(df$Sch.Scheduled.Provider.Name) != "NEWELL MD,GLENN C"))] <- "GME" #Int Med GME sessions 
  temp[which(as.character(df$Sch.Dept.Desc) == "Int Med" &
               (as.character(df$Sch.Scheduled.Provider.Name) == "CCCP,CLINIC1" |
                  as.character(df$Sch.Scheduled.Provider.Name) == "CCCP,CLINIC2"))] <- "UME" #Int Med GME sessions 
  temp[which(as.character(df$Sch.Dept.Desc) == "GI" &
               as.character(df$Sch.Location.Name) == "Suite 215" &
               as.character(df$Sch.Scheduled.Provider.Name) != "CAPANESCU MD,CRISTINA")] <- "Fellows" #GI Fellows  
  temp[which(as.character(df$Sch.Dept.Desc) == "Pulm" &
               (as.character(df$Sch.Scheduled.Provider.Name) != "AKERS MD,STEPHEN M" &
                  as.character(df$Sch.Scheduled.Provider.Name) != "HAQUE MD,ANWAR M"))] <- "Fellows" #Pulm Fellows, gives Akers credit for GV 
  temp[which(as.character(df$Sch.Dept.Desc) == "Neph" &
               as.character(df$Sch.Location.Name) == "Suite 215" &
               as.character(df$Sch.Scheduled.Provider.Name) != "SIKAND MD,SEEMA" &
               as.character(strftime(strptime(df$Sch.Appt.Date, format = "%Y-%m-%d"), format = "%a")) == "Wed")] <- "Fellows" #Neph Fellows
  df$Division.Edu <- paste(df$Sch.Dept.Desc, temp, sep = " - ")
  return(df)
  
}





GV.Opp.Label <- function(df) {
  df$GV.Opp <- character(length = nrow(df))
  reasons <- paste(df$Sch.Reason.for.Visit, df$Sch.Reason.For.Visit.Continued)
  knee.list <- c("knee")
  hand.list <- c("carpel", "carpul", "carpal", "wrist", "thumb", "tumb", "finger", "hand", "fing")
  headache.list <- c("Headache", "Migraine", "Headahce", "h/a", "h/ a", "346")
  DM.list <- c("diab", "diiab", "Metabolic", "High Sugar", "DM", "Insulin", "D/M",
               "uncontrol", "Type I", "Type 1", "Type 2")
  sleep.apnea.list <- c("sleep apnea", "apnea", "OSA ", "OSA.", "CPAP", "sleep ")
  COPD.list <- c("COPD", "emphysema")
  asthma.list <- c("asthma", "xolair","xoalir", "reactive airway")
  for (i in 1:length(knee.list)) {
    df$GV.Opp[which((as.character(df$Sch.VisitCatName) == "NEW PATIENT")  & 
                      as.character(df$Sch.Dept.Desc) == "Ortho" &
                      grepl(knee.list[i], reasons, ignore.case = TRUE) 
                    & df$CACC.Patient)] <- "Knee"
  }
  for (i in 1:length(hand.list)) {
    df$GV.Opp[which((as.character(df$Sch.VisitCatName) == "ESTABLISHED PATIENT"|
                       as.character(df$Sch.VisitCatName) == "NEW PATIENT")  & 
                      as.character(df$Sch.Dept.Desc) == "Ortho" &
                      grepl(hand.list[i], reasons, ignore.case = TRUE) 
                    & df$CACC.Patient)] <- "Hand"
  }
  for (i in 1:length(headache.list)) {
    df$GV.Opp[which((as.character(df$Sch.VisitCatName) == "ESTABLISHED PATIENT"|
                       as.character(df$Sch.VisitCatName) == "NEW PATIENT")  & 
                      as.character(df$Sch.Dept.Desc) == "Neuro" &
                      grepl(headache.list[i], reasons, ignore.case = TRUE) 
                    & df$CACC.Patient)] <- "Headache"
  }
  for (i in 1:length(DM.list)) {
    df$GV.Opp[which((as.character(df$Sch.VisitCatName) == "ESTABLISHED PATIENT"|
                       as.character(df$Sch.VisitCatName) == "NEW PATIENT")  & 
                      as.character(df$Sch.Dept.Desc) == "Endo" &
                      grepl(DM.list[i], reasons, ignore.case = TRUE) 
                    & df$CACC.Patient)] <- "Diabetes"
  }  
  for (i in 1:length(DM.list)) {
    df$GV.Opp[which((as.character(df$Sch.VisitCatName) == "ESTABLISHED PATIENT"|
                       as.character(df$Sch.VisitCatName) == "NEW PATIENT")  & 
                      as.character(df$Sch.Dept.Desc) == "Pod" &
                      grepl(DM.list[i], reasons, ignore.case = TRUE) 
                    & df$CACC.Patient)] <- "Diabetic Foot Care"
  }  
  for (i in 1:length(sleep.apnea.list)) {
    df$GV.Opp[which((as.character(df$Sch.VisitCatName) == "ESTABLISHED PATIENT"|
                       as.character(df$Sch.VisitCatName) == "NEW PATIENT")  & 
                      as.character(df$Sch.Dept.Desc) == "Pulm" &
                      grepl(sleep.apnea.list[i], reasons, ignore.case = TRUE) 
                    & df$CACC.Patient)] <- "Sleep Apnea"
  }  
  for (i in 1:length(COPD.list)) {
    df$GV.Opp[which((as.character(df$Sch.VisitCatName) == "ESTABLISHED PATIENT"|
                       as.character(df$Sch.VisitCatName) == "NEW PATIENT")  & 
                      as.character(df$Sch.Dept.Desc) == "Pulm" &
                      grepl(COPD.list[i], reasons, ignore.case = TRUE) 
                    & df$CACC.Patient)] <- "COPD"
  }  
  for (i in 1:length(asthma.list)) {
    df$GV.Opp[which((as.character(df$Sch.VisitCatName) == "ESTABLISHED PATIENT"|
                       as.character(df$Sch.VisitCatName) == "NEW PATIENT")  & 
                      as.character(df$Sch.Dept.Desc) == "Pulm" &
                      grepl(asthma.list[i], reasons, ignore.case = TRUE) 
                    & df$CACC.Patient)] <- "Asthma"
  }  
  return(df)
}

Exception.Label <- function(df) {
  df$Exception <- character(length = nrow(df))
  
  df$Exception[which(as.character(df$Sch.Dept.Desc) == "Gen Surg" &
                       df$CACC.Patient &
                       (as.character(df$Sch.Scheduled.Provider.Name) == "PATEL MD,ROHIT A" |
                          as.character(df$Sch.Scheduled.Provider.Name) == "O'CONNELL MD,BRENDAN G"))] <- "Bariatric" #This removes bariatric errors
  
  df$Exception[which(as.character(df$Sch.Dept.Desc) == "Neuro Surg" &
                       df$CACC.Patient &
                       (as.character(df$Sch.Scheduled.Provider.Name) == "TURTZ MD,ALAN R"))] <- "Brain Surg" #This removes brain errors
  df$Exception[which(as.character(df$Sch.Dept.Desc) == "Neuro Surg" &
                       df$CACC.Patient &
                       (as.character(df$Sch.Scheduled.Provider.Name) == "GOLDMAN MD,HOWARD W"))] <- "Brain Surg" #This removes brain errors
  
  df$Exception[which(as.character(df$Sch.Dept.Desc) == "Neuro" &
                       (as.character(df$Sch.Visit.Type.Name) == "NEUROPSYCH EVALUATION") &
                       ((df$CACC.Patient & !df$CACC.Location) |
                          (!df$CACC.Patient & df$CACC.Location)))] <- "Neuro Psych" #This removes Neuro Psych errors
  
  df$Exception[which(as.character(df$Sch.Dept.Desc) == "Neuro" &
                       grepl("INJECTION", as.character(df$Sch.Visit.Type.Name)) &
                       df$CACC.Patient &
                       (as.character(df$Sch.Scheduled.Provider.Name) == "COLCHER MD,AMY"))] <- "COLCHER - BOTOX" #This removes Colcher's Botox visits
  
  df$Exception[which((grepl("EMG", as.character(df$Sch.Visit.Type.Name)) |
                        grepl("INJECTION", as.character(df$Sch.Visit.Type.Name))) &
                       df$CACC.Patient &
                       (as.character(df$Sch.Scheduled.Provider.Name) == "BODOFSKY MD,ELLIOT B"))] <- "BODOFSKY - BOTOX" #This removes Bodofsky's Botox visits
  
  
  
  
  df$Exception[which(grepl("AUTO", as.character(df$Sch.Comment)) & df$CACC.Patient)] <- "Auto"
  
  df$Exception[which(grepl("WORKER", as.character(df$Sch.Comment)) & df$CACC.Patient)] <- "Worker's Comp"
  df$Exception[which(grepl("WORKMAN", as.character(df$Sch.Comment)) & df$CACC.Patient)] <- "Worker's Comp"
  df$Exception[which(grepl("WC", as.character(df$Sch.Comment)) & df$CACC.Patient)] <- "Worker's Comp"
  
  
  df$Exception[which((as.character(df$Sch.Visit.Type.Name) == "ELECTROMYOGRAPHY" |
                        as.character(df$Sch.Visit.Type.Name) == "EMG2") & 
                       df$CACC.Patient)] <- "EMG" #This removes EMG errors
  
  
  df$Exception[which(as.character(df$Sch.Dept.Desc) == "Derm" &
                       df$CACC.Location & 
                       !df$CACC.Patient)] <- "Derm Comm'l in CACC" #This removes Derm comm'l pts seen upstairs ONLY
  
  
  df$Exception[which(as.character(df$Sch.Dept.Desc) == "Int Med" &
                       df$CACC.Patient &
                       (as.character(df$Sch.Scheduled.Provider.Name) == "NEWELL MD,GLENN C"))] <- "Dr. Newell" #This removes Newell's pts
  
  df$Exception[which(!df$CACC.Patient & 
                       df$CACC.Location &
                       as.character(df$GME.Visit) == "Int Med - GME")] <- "Int Med - GME" #This removes all GME visits from exception lists
  
  df$Exception[which(as.character(df$Sch.Dept.Desc) == "Ortho" &
                       !df$CACC.Patient &
                       df$CACC.Location & 
                       (as.character(df$Sch.Scheduled.Provider.Name) == "FULLER MD,DAVID A"))] <- "Neuro-Muscular Clinic" #This removes Fuller errors
  
  return(df)
}






CACC.date.diff <- function(date1, date2) {
  if (str_sub(date1, start = 6L, end = 6L) != str_sub(date2, start = 6L, end = 6L)) {
    stop("Dates are for different types of periods")
  }
  if (grepl(".W", date1)) {
    year1 <- as.integer(str_split_fixed(date1, ".W", 2)[,1])
    week1 <- as.integer(str_split_fixed(date1, ".W", 2)[,2])
    year2 <- as.integer(str_split_fixed(date2, ".W", 2)[,1])
    week2 <- as.integer(str_split_fixed(date2, ".W", 2)[,2])
    return(52*(year1 - year2) + (week1 - week2))
  }
  if (grepl(".M", date1)) {
    year1 <- as.integer(str_split_fixed(date1, ".W", 2)[,1])
    month1 <- as.integer(str_split_fixed(date1, ".W", 2)[,2])
    year2 <- as.integer(str_split_fixed(date2, ".W", 2)[,1])
    month2 <- as.integer(str_split_fixed(date2, ".W", 2)[,2])
    return(12*(year1 - year2) + (month1 - month2))
  }
}

CACC.date.shift <- function(date1, shift = 0) {
  if (grepl(".W", date1)) {
    year1 <- as.integer(str_split_fixed(date1, ".W", 2)[,1])
    week1 <- as.integer(str_split_fixed(date1, ".W", 2)[,2])
    year2 <- year1 + (week1 + shift) %/% 52
    week2 <- (week1 + shift) %% 52
    week2 <- sprintf("%02.0f",week2)
    return(paste(year2,".W",week2, sep = ""))
  }
  if (grepl(".M", date1)) {
    year1 <- as.integer(str_split_fixed(date1, ".M", 2)[,1])
    month1 <- as.integer(str_split_fixed(date1, ".M", 2)[,2])
    year2 <- year1 + (month1 + shift) %/% 12
    month2 <- (month1 + shift) %% 12
    if (month2 == 0L) {
      year2 = year2 - 1L
      month2 = 12L
    }
    month2 <- sprintf("%02.0f",month2)
    return(paste(year2,".M",month2, sep = ""))
  }
}


Non.Prov.Visit <- function(Sch.Scheduled.Provider.Name) {
  non.prov.list <- c("NURSE,CAMDEN", "VASCULAR LAB,CACC", "PRO FIT,PROVIDER", "VASCULAR LAB,CAMDEN",
                     "AUDIO VOORHEES", "AUDIOLOGY WT", "DIETITIAN,BMA", "NEURODIAGNOSTIC,LAB",
                     "NURSE,CDHI", "NURSE,ENDOCRINOLOGY", "ORTHO,RADIOLOGY TECH", "PFT LAB,1 COOPER",
                     "PFT LAB,BMA", "PFT LAB,VOORHEES", "SLEEP LAB,VOORHEES", "VASCULAR LAB,MARLTON",
                     "VASCULAR LAB,VOORHEES", "VASCULAR LAB,WASHINGTON TWP", "VASCULAR LAB,WILLINGBORO",
                     "CCCP,CLINIC1", "CCCP,CLINIC2", "NURSE 2,CAMDEN", "CYBER KNIFE,UNIT", "INFINITY,CAMDEN",
                     "TWENTY ONE EX,VOORHEES", "V2300CD,CAMDEN", "CHEMO,NURSE", "NURSE,PAT", "NURSE PAT,PRACTITIONER",
                     "PHLEBOTOMY", "LPN,CAMDEN", "EXAM,ROOM B", "RESEARCH", "EEG,TECHNICIAN")
  Sch.Scheduled.Provider.Name <- as.character(Sch.Scheduled.Provider.Name)
  temp <- rep(FALSE, length = length(Sch.Scheduled.Provider.Name))
  temp <- Sch.Scheduled.Provider.Name %in% non.prov.list
  return(temp)
}


IM.Visits <- IM.Visits[which(as.character(IM.Visits$Reg.Patient.Deceased) == "N"),] #Remove deceased patients
#IM.Visits <- IM.Visits[which(is.na(IM.Visits$Reg.Patient.Deactive.Date)),] #Remove patients who have been deactivated
#IM.Visits <- IM.Visits[which(as.character(IM.Visits$Sch.Scheduled.Provider.Name) != "NURSE,CAMDEN"),] #Removes Nurse Visits
#IM.Visits <- IM.Visits[which(Camden.ZIP(clean.zipcodes(IM.Visits$Reg.Patient.Zip))),] #Remove non-Camden patients

IM.Visits <- IDX.file.prep(IM.Visits, CACC_only = TRUE, IDX=FALSE)
IM.Visits <- IM.Visits[which(IM.Visits$CACC.Patient),] #Removes non-CACC patients
IM.Visits <- subset(IM.Visits, Sch.Scheduled.Provider.Name != "CCCP,CLINIC1")
IM.Visits <- subset(IM.Visits, Sch.Scheduled.Provider.Name != "CCCP,CLINIC2")

IM.Visits <- IM.Visits[order(IM.Visits$Sch.Appt.Date, decreasing = TRUE),]


IM.Visits <- subset(IM.Visits, select = c(Reg.Patient.MRN, Reg.Patient.Zip, Reg.Fsc.1.Name, Reg.Patient.HQ.CPI.Number, Reg.Patient.MRN, Reg.Patient.SSN, Reg.Patient.Full.Name, Reg.Patient.First.Name,
                                          Reg.Patient.Last.Name, Reg.Patient.Birth.Date, Reg.Patient.Sex,
                                          Reg.Patient.Addr.1, Reg.Patient.Addr.2, Reg.Patient.City, Reg.Patient.State, 
                                          Sch.Scheduled.Provider.Name, Reg.Patient.Phone, Reg.Patient.Phone.Area))

IM.Visits <- IM.Visits[-which(duplicated(IM.Visits$Reg.Patient.MRN)),]

IM.Visits$Last.IM.Provider.Seen <- IM.Visits$Sch.Scheduled.Provider.Name
IM.Visits <- subset(IM.Visits, select = c(Reg.Patient.MRN, Reg.Patient.Zip, Reg.Fsc.1.Name, Reg.Patient.HQ.CPI.Number, Reg.Patient.SSN, Reg.Patient.Full.Name, Reg.Patient.First.Name,
                                          Reg.Patient.Last.Name, Reg.Patient.Birth.Date, Reg.Patient.Sex,
                                          Reg.Patient.Addr.1, Reg.Patient.Addr.2, Reg.Patient.City, Reg.Patient.State, 
                                          Last.IM.Provider.Seen, Reg.Patient.Phone, Reg.Patient.Phone.Area))


#Removes United Healthcare Insurance patients to avoid duplicate entries when binded with United dataset#
# IM.Visits <- subset(IM.Visits, !(Reg.Fsc.1.Name=="UNITED HEALTHCARE"))
# IM.Visits <- subset(IM.Visits, !(Reg.Fsc.1.Name=="UHC COMMUNITY PLAN"))
# IM.Visits <- subset(IM.Visits, !(Reg.Fsc.1.Name=="UHC COMMUNITY PLAN MEDICAID (C)"))
# IM.Visits <- subset(IM.Visits, !(Reg.Fsc.1.Name=="UHC COMMUNITY PLAN MEDICAID (NC"))


#Adds a vendor column and prepopulates it#
IM.Visits$VEND_FULL_NAME <- "Cooper_UHI_Nic"

#Matches the variable values in the gender column to the United list for consistency#
IM.Visits$Reg.Patient.Sex<- gsub("female", "F", IM.Visits$Reg.Patient.Sex)
IM.Visits$Reg.Patient.Sex<- gsub("male", "M", IM.Visits$Reg.Patient.Sex)


#Renames fields in the uhi dataset#
IM.Visits <- rename(IM.Visits, c(Reg.Patient.HQ.CPI.Number="SUBSCRIBER_ID"))
IM.Visits <- rename(IM.Visits, c(Reg.Patient.SSN="SOCIAL_SEC_NO"))
IM.Visits <- rename(IM.Visits, c(Reg.Patient.First.Name="MEMB_FIRST_NAME"))
IM.Visits <- rename(IM.Visits, c(Reg.Patient.Last.Name="MEMB_LAST_NAME"))
IM.Visits <- rename(IM.Visits, c(Reg.Patient.Birth.Date="DOB"))
IM.Visits <- rename(IM.Visits, c(Reg.Patient.Sex="GENDER"))
IM.Visits <- rename(IM.Visits, c(Reg.Patient.Addr.1="MEMB_ADDRESS_LINE_1"))
IM.Visits <- rename(IM.Visits, c(Reg.Patient.Addr.2="MEMB_ADDRESS_LINE_2"))
IM.Visits <- rename(IM.Visits, c(Reg.Patient.City="MEMB_CITY"))
IM.Visits <- rename(IM.Visits, c(Reg.Patient.State="MEMB_STATE"))
IM.Visits <- rename(IM.Visits, c(Last.IM.Provider.Seen="CURR_PCP_FULL_NAME"))
IM.Visits <- rename(IM.Visits, c(Reg.Patient.Zip="MEMB_ZIP"))
IM.Visits <- rename(IM.Visits, c(Reg.Patient.Full.Name="MEMB_NAME"))
IM.Visits <- rename(IM.Visits, c(Reg.Fsc.1.Name="MEMB_INSURANCE"))
IM.Visits <- rename(IM.Visits, c(Reg.Patient.Phone="PHONE"))
IM.Visits <- rename(IM.Visits, c(Reg.Patient.Phone.Area="AREA"))

#Concatenates the area and the phone number fields to create one field for home phone number#
IM.Visits$HOME_PHONE_NUM<-paste(IM.Visits$AREA, IM.Visits$PHONE, sep = "", collapse = NULL)

#Deletes the old fields for phone number to avoid confusion#
IM.Visits$AREA <-NULL
IM.Visits$PHONE<- NULL

#Removes NA values from the home phone number field#
IM.Visits$HOME_PHONE_NUM[IM.Visits$HOME_PHONE_NUM == "NANA"] <- ""

#Adds Source field
IM.Visits$Source<-"UHI_Nic"

#Adds NIC to begingin of ID
IM.Visits$SUBSCRIBER_ID<-paste("NIC", IM.Visits$SUBSCRIBER_ID, sep="")

#Esports final file#
write.csv(IM.Visits, file = paste(data.directory, Sys.Date(), "-","CooperUHI", ".csv", sep = ""), row.names = FALSE)

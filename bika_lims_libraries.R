 #              [!IMPORTANT]
## Before making changes here take a look at "bika_lims_names_filter.R"

## Enabling readability and ease of use 
## Changing the current Variable names to Easily Readable Names

LimsEasyRead <- function(make_readable){
	readable_data <- rename(
		make_readable, 
		c("BirthDate" = "Date of Birth",
		"ConsentSMS" = "Consent to SMS",
		"Analyses_0_Result" = "Results",
		"Analyses_0_ResultCaptureDate" = "Date Result Captured",
		"Analyses_0_DateAnalysisPublished" = "Date Analysis Published",
		"Analyses_0_DateReceived" = "Date Analysis Received",
		"Analyses_0_review_state" = "Analysis State",
		"Analyses_0_Unit" = "Unit",
		"Analyses_0_creation_date" = "Date Analysis Creation",
		"ReasonForVLtest" = "Reason for VL",
		"VLBreastFeeding" = "Breast Feeding",
		"VLPoorAdherence" = "Poor Adherence",
		"VLPregnant" = "Pregnant",
		"VLTreatmentHistory_0_Drug" = "Drug 1",
		"VLTreatmentHistory_0_Treatment" = "Treament",
		"VLTreatmentHistory_1_Drug" ="Drug 2",
		"VLTreatmentHistory_2_Drug" ="Drug 3",
		"Client.x" ="Client"
			)
		)
	return(readable_data)
}

## Easy to use date time formats
## NB : Only works for readable data that has been passed thru the above function
LimsEasyDates <- function(uneasy_date){
  uneasy_date$'Date of Birth' <- as.Date(uneasy_date$'Date of Birth')
  uneasy_date$'Date Result Captured' <- as.Date(uneasy_date$'Date Result Captured')
  uneasy_date$'Date Analysis Published' <- as.Date(uneasy_date$'Date Analysis Published')
  uneasy_date$'Date Analysis Received' <- as.Date(uneasy_date$'Date Analysis Received')
  uneasy_date$'Date Analysis Creation' <- as.Date(uneasy_date$'Date Analysis Creation')
  return(uneasy_date)
}

# Set some variables as Factor to save memory and ease of use
LimsSetFactors <- function(unfactored){
  unfactored$Client <- as.factor(unfactored$Client)
  unfactored$`Consent to SMS` <- as.factor(unfactored$`Consent to SMS`)
  unfactored$Gender <- as.factor(unfactored$Gender)
  unfactored$Unit <- as.factor(unfactored$Unit)
  unfactored$Gender <- as.factor(unfactored$Gender)
  unfactored$`Analysis State` <- as.factor(unfactored$`Analysis State`)
  unfactored$`Breast Feeding` <- as.factor(unfactored$`Breast Feeding`)
  unfactored$`Poor Adherence` <- as.factor(unfactored$`Poor Adherence`)
  unfactored$Pregnant <- as.factor(unfactored$Pregnant)
  return(unfactored)
}

# Change  Results "Failed" = "0",
#                 "Target Not Detected" = "999999999999",
#                 "Invalid" = "999999999009"
#                  etc
# Then converting Results from Char to Num
LimsSetNumeric <- function(to_numeric){
	to_numeric <- within(to_numeric,
	                            {
		                        Results[Results == "Failed"] <- "0"
		                        Results[Results == "Target Not Detected"] <- "999999999999"
		                        Results[Results == "Invalid"] <- "999999999009"
	                          })
	to_numeric$Results <- as.numeric(to_numeric$Results)
	return(to_numeric)
}

## The function fetches the 3 bika csv data files and combines then by PatientUID
LimsCombineCSV <- function(){
	# Load the data
	patients <- read.csv("./bika_csv_files/patients.csv", header = TRUE, stringsAsFactors = FALSE)
	analysis <- read.csv("./bika_csv_files/analysis.csv", header = TRUE, stringsAsFactors = FALSE)
	cases <- read.csv("./bika_csv_files/cases.csv", header = TRUE, stringsAsFactors = FALSE)

	# Renaming to obtain uniqueness using PatientUID
	library(reshape)
	patients <- rename(patients, c("UID" = "PatientUID")) # change UID in patients to PatientUID
	cases <- rename(cases, c("Patient_uid" = "PatientUID")) # change Patient_uid in cases to PatientUID

	# merging the 3 csv files to one
	patients_combined <- merge(merge(patients, analysis, by = "PatientUID"), cases, by = "PatientUID", all.x = TRUE)

	# Remove dublicates if any
	patients_combined <- patients_combined[, !duplicated(colnames(patients_combined))]

	# save combined data to csv as patients_"combined.csv"
	write.csv(patients_combined, file = "./bika_csv_files/Patents_Combined.csv")

	# Remove Columns with All NA's and write to csv calles "Patents_Combined_no_NA.csv"
	patients_combined_no_NAs <- patients_combined[ , !apply( patients_combined, 2, function(x) all(is.na(x)))]
	write.csv(patients_combined_no_NAs, file = "./bika_csv_files/Patents_Combined_no_NA.csv")

	return(patients_combined_no_NAs)
}

## The functions takes combined data returned by LimsCombineCSV and applies the other functions 
LimsModifier <- function(to_modify){
	source("./builds/bika_lims_filter.R")
	# Filter and Make Readable and make dates usable : modified_data
	modified_data <- LimsSetNumeric(
					    to_numeric = LimsSetFactors(
					    	unfactored = LimsEasyDates(
					    		uneasy_date = LimsEasyRead(
					    			make_readable = LimsFilter(
					    				data_to_filter = to_modify
					    				)
					    			)
					    		)
					    	)
					    )

	# Calculate Age of Patients and TAT etc
	modified_data <- within(modified_data, {
          `Outside Tat`                                 <- ""
          `VL below 1000`                               <- ""
          `Age range`                                   <- ""          
          Age                                           <- year(Sys.Date()) - year(modified_data$`Date of Birth`)
          `Age range`[`Age` <  14]                      <- "< 14 Yrs"
          `Age range`[`Age` >= 14]                      <- ">= 14 Yrs"     
          `Turn Around Time`                            <- day(modified_data$`Date Analysis Published`) - day(modified_data$`Date Analysis Received`)
          `Outside Tat`[`Turn Around Time` <= 3]        <- "<= 3 days"
          `Outside Tat`[`Turn Around Time` >  3]        <- "> 3 days"
          `VL below 1000`[`Results` <  1000 & `Results` != 0]            <- "< 1000"
          `VL below 1000`[`Results` >= 1000 & `Results` < 999999999009]  <- ">= 1000"
          `VL below 1000`[`Results` ==  999999999999]                     <- "TND"          
          `VL below 1000`[`Results` ==  0]                                <- "Failed"     
          `VL below 1000`[`Results` ==  999999999009]                     <- "Invalid"
        })

	# save filtered data to csv
	write.csv(modified_data, file = "./bika_csv_files/Patents_filtered.csv")
	return(modified_data)
}


## This is te statistics Calculator.
## Nothing Fancy, just some simple frequencies or cross tabulations
## It can be improved to plot bar graphs,predictive anlysics etc
LimsAphlStats  <- function(finalised){
  tat <- table(finalised$`Outside Tat`)
  cat("\n ********************************************************* \n", 
      "Overal TAT Turn Around Time <= 3 days / more", "\n",
      "________________________________________________________ \n")
  print(tat)
  
  vl <- table(finalised$`VL below 1000`)
  cat("\n *********************************************************\n", 
      "Overal VL , either < 1000 / >= 1000 copies/ml", "\n",
      "________________________________________________________ \n")
  print(vl)
  
  client_tat <- table(finalised$Client, finalised$`Outside Tat`)
  cat("\n ********************************************************* \n", 
      "Turn Around Time <= 3 days / more by Client", "\n",
      "________________________________________________________ \n")
  print(client_tat)
  
  client_vl <- table(finalised$Client, finalised$`VL below 1000`)
  cat("\n ********************************************************* \n", 
      "VL Results < 1000 / >= 1000 copies/ml by Client", "\n",
      "________________________________________________________ \n")
  print(client_vl)
  
  age_group_vl <- table(finalised$`Age range`, finalised$`VL below 1000`)
  cat("\n ********************************************************* \n", 
      "VL Results < 1000 / >= 1000 copies/ml by Age", "\n",
      "________________________________________________________ \n")
  print(age_group_vl)
  
  gender_vl <- table(finalised$Gender, finalised$`VL below 1000`)
  cat("\n ********************************************************* \n", 
      "VL Results < 1000 / >= 1000 copies/ml : Gender Based", "\n",
      "________________________________________________________ \n")
  print(gender_vl)
  
  pregnancy_vl <- table(finalised$Pregnant, finalised$`VL below 1000`)
  cat("\n ********************************************************* \n", 
      "VL Results < 1000 / >= 1000 copies/ml in Pregnancy", "\n",
      "________________________________________________________ \n")
  print(pregnancy_vl)
  
  breast_feeding_vl <- table(finalised$`Breast Feeding`, finalised$`VL below 1000`)
  cat("\n ********************************************************* \n", 
      "VL Results < 1000 / >= 1000 copies/ml in Breast Feeding", "\n",
      "________________________________________________________ \n")
  print(breast_feeding_vl)

    cat("\n ********************************************************* \n
            ********************************************************** \n", 
      "By : Aurthur Musendame \n Mobile : +263776406399 \n Email : aurthurmusendame@gmail.com", "\n",
      "________________________________________________________ \n")

}

## Aurthur Musendame :  Proudly Zimbabwean
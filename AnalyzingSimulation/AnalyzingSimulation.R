#### Script for analyzing DashboardTester simulation ####



#### SETUP ####

# Setting working directory to source file location
#setwd("SOURCE-FILE-LOCATION-DIRECTORY")

# loading necessary packages
library(cyphr)
library(data.table)




#### IMPORTING DECRYPTION KEYS ####

# In a first step, you need to place the keypair for decrypting
# the files encrypted by your ChatDashboard instance in the "DecryptionKeypair"
# folder in this directory

# creating keypair for decrypting chatlogs
researcher_key_pair <- cyphr::keypair_openssl(pub = "./DecryptionKeypair", key = "./DecryptionKeypair", envelope = TRUE)




#### IMPORTING SIMULATED DATA DONATIONS ####

# First of all, you need to download the simulated data donation from your server and place them
# in the "SimulatedDonations" folder in this directory

# extracting file paths of all simulated donations in "SimulatedDonations" folder
FileList <- list.files("./SimulatedDonations")
FilePaths <- sapply(FileList,function(x){return(paste("./SimulatedDonations/",x,sep = ""))})
names(FilePaths) <- NULL

# importing encrypted simulated donations
EncryptedList <- lapply(FilePaths, readRDS)

# decrypting simulated donations
# If you are getting an error here, you are likely not using the correct keypair for decryption
DecryptedList <- lapply(EncryptedList, decrypt_object, researcher_key_pair)




#### IMPORTING SIMULATION LOGS ####

# Running the simulation should have automatically placed a file
# starting with "SimulationLog_" in the "SimulationLogs" folder of this
# directory.

# this is automatically taking the first listed file, we recommend only keeping
# one file in this folder at a time
SimulationLog <- readRDS(list.files(path="./SimulationLogs",pattern = "rds",full.names=TRUE)[1])



#### EXCLUDING SIMULATION RUNS WITHOUT DATA DONATIONS FROM LOG ####

# creating a list of all actions from all simulated participants
# to identify simulated participants who did not click on consent
# for data donation and no corresponeding simulated donated data file is
# on the server
# NOTE: This only happens when the RSelenium session encounters an unexpected error.
AllUserActions <- sapply(SimulationLog,`[[`,2)
DonationConsent <- sapply(AllUserActions, function(x){print(x[length(x)] == "Gave Donation consent")})

# checking if there are any simulated participants where data donation
# failed
which(DonationConsent == TRUE)
length(which(DonationConsent == TRUE)) == length(DecryptedList)

# spacing out he simulated donations to keep the order right and
# ensure that every simulated participant log corresponds to the correct data
# set on the server
DecryptedList2 <- as.list(rep(NA,length(DonationConsent)))
DecryptedList2[which(DonationConsent == TRUE)] <- DecryptedList

# Inspecting simulation logs for simulated participants
# where the simulation errored out (no donated data)
AllUserActions[which(is.na(DonationConsent))]

# Simulation logs for simulated participants
# where we do have donated data
AllUserActions[which(DonationConsent)]
AllUserActions[c(which(DonationConsent == FALSE),which(is.na(DonationConsent)))]









############### Comparing actions of simulated participants to simulated data donations ####################

# The code below is comparing the names of the
# columns randomly selected by simulated participants
# in the data donation step with the columns
# present in the simulated data donations.
# Testing must ensure that:
# a) Only columns containing non-PII information are contained
# b) Only columns manually selected for donation are contained


#### CHECKING DATA ####

# Checking how many simulated participants de/selected columns at all
for (i in 1:length(SimulationLog)) {print(length(SimulationLog[[i]]$SelectedColumns) != 1)}

# Function for comparing actions of simulated participants to simulated data donation
ColSummary <- function(SimulationLog,donation) {
  
                # Listing all potentially existing columns names
                # Must be the same names as in DF saved by ChatDashboard instance on the server
                ExistingColumns <- c("Timestamp",
                                     "Sender",
                                     "Sender_anonimized",
                                     "Message",
                                     "Message_simplified",
                                     "Message_words",
                                     "Links",
                                     "Links_anonimized",
                                     "Media",
                                     "Media_anonimized",
                                     "Locations",
                                     "Locations_anonimized",
                                     "Emoji",
                                     "Emoji_description",
                                     "Smilies",
                                     "System_messages",
                                     "Word_count",
                                     "Time_order",
                                     "Display_order")
                
                # defining preselected columns. These are the columns that are displayed to
                # (simulated) ChatDashboard participants by default
                PreselectedColumns <- c("Timestamp",
                                        "Sender_anonimized",
                                        "Links_anonimized",
                                        "Media_anonimized",
                                        "Locations_anonimized",
                                        "Emoji",
                                        "Emoji_description",
                                        "Smilies",
                                        "Word_count",
                                        "Time_order",
                                        "Display_order")
                
                # non-selected columns These are the columns that are NOT displayed to
                # (simulated) ChatDashboard participants by default
                NonselectedColumns <- c("Sender",
                                        "Message",
                                        "Message_simplified",
                                        "Message_words",
                                        "Links",
                                        "Media",
                                        "Locations",
                                        "System_messages")
                  
                
                # Defining PI columns. These are the names of columns that could
                # potentially contain Personal Identifiable Information (PII)
                PI_columns <- c("Sender",
                                "Message",
                                "Message_simplified",
                                "Message_words",
                                "Links",
                                "Media",
                                "Locations",
                                "System_messages")
                
                # Checking if donation object is a valid data.frame
                # This is important for NAs if the simulation errored out
                # for some simulated participants
                if (!is.data.frame(donation)) {
                  
                  print("Processing Error - no data donated")
                  return("Processing Error - no data donated")
                  
                
                # Running analysis if there is a valid data.frame
                } else {
                 
                  # Analysis for cases where simulated participants did NOT de/select any colums
                  # in the data selection step
                  if (length(SimulationLog$SelectedColumns) == 1 & is.na(SimulationLog$SelectedColumns)[1]) {
                    
                    # printing progress info
                    print("No column selection")
                    
                    # initializing values for selected and contained columns
                    SelectedColumns <- ExistingColumns
                    ContainedColumns <- colnames(donation)
                    
                    # Computing which columns were selected, contained, PI
                    Selected <- c(ExistingColumns %in% SelectedColumns)
                    Contained <- c(ExistingColumns %in% ContainedColumns)
                    PI <- c(ExistingColumns %in% PI_columns)
                    
                    # putting everything into one dataframe
                    ColAn <- cbind.data.frame(ExistingColumns,Selected,Contained,PI)
                    
                    # computing individual labels for each column
                    #library(data.table)
                    ColAn$Selected <- fifelse(ColAn$Selected == TRUE, "selected", "not selected", NA)
                    ColAn$Contained <- fifelse(ColAn$Contained == TRUE, "contained", "not contained", NA)
                    ColAn$PI <- fifelse(ColAn$PI == TRUE, "PI", "not PI", NA)
                    
                    # from lables, compute overall summary and combine in dataframe
                    output <- list(Data = ColAn,
                                   Selected = ColAn$ExistingColumns[ColAn$Selected == "selected"],
                                   Excluded = ColAn$ExistingColumns[ColAn$Selected == "not selected"],
                                   Contained = ColAn$ExistingColumns[ColAn$Contained == "contained"],
                                   ExcludedPI = ColAn$ExistingColumns[ColAn$Selected == "not selected" & ColAn$PI == "PI"],
                                   ExcludedNonPI = ColAn$ExistingColumns[ColAn$Selected == "not selected" & ColAn$PI == "not PI"],
                                   NotSelectedButIncludedNotPI = ColAn$ExistingColumns[ColAn$Selected == "not selected" & ColAn$Contained == "contained" & ColAn$PI == "not PI"],
                                   NotSelectedButIncludedPI = ColAn$ExistingColumns[ColAn$Selected == "not selected" & ColAn$Contained == "contained" & ColAn$PI == "PI"],
                                   SelectedButNotIncludedNotPI = ColAn$ExistingColumns[ColAn$Selected == "selected" & ColAn$Contained == "not contained" & ColAn$PI == "not PI"],
                                   SelectedButNotIncludedPI = ColAn$ExistingColumns[ColAn$Selected == "selected" & ColAn$Contained == "not contained" & ColAn$PI == "PI"],
                                   ContainedPISelected = ColAn$ExistingColumns[ColAn$Selected == "selected" & ColAn$Contained == "contained" & ColAn$PI == "PI"],
                                   ContainedPINotSelected = ColAn$ExistingColumns[ColAn$Selected == "not selected" & ColAn$Contained == "contained" & ColAn$PI == "PI"])
                    
                    # give indicitave names to dataframe columns
                    names(output) <- c("No column selection",
                                       "Selected",
                                       "Excluded",
                                       "Contained",
                                       "ExcludedPI",
                                       "ExcludedNonPI",
                                       "NotSelectedButIncludedNotPI",
                                       "NotSelectedButIncludedPI",
                                       "SelectedButNotIncludedNotPI",
                                       "SelectedButNotIncludedPI",
                                       "ContainedPISelected",
                                       "ContainedPINotSelected")
                    
                    # returning output
                    return(output)
                    
                  }
                  
                  # Analysis for cases where simulated participants did NOT de/select any colums
                  # in the data selection step
                  else {
                   
                    # printing progress info
                    print("Columns selected, data donated")
                    
                    # Selected Columns that simulated participants clicked on
                    ClickedOnColumns <- SimulationLog$SelectedColumns[(tail(which(is.na(SimulationLog$SelectedColumns)),n = 2)[1] + 1):(tail(which(is.na(SimulationLog$SelectedColumns)),n = 2)[2] - 1)]
            
                    # Columns that are actually contained in the simulated donation
                    ContainedColumns <- colnames(donation)
                    
                    # Columns selected for donation
                    Selected <- c(ExistingColumns %in% ClickedOnColumns)
                    

                    #### Summing up info in dataframe
                    
                    # Computing which columns were selected, contained, PI
                    Contained <- c(ExistingColumns %in% ContainedColumns)
                    PI <- c(ExistingColumns %in% PI_columns)
                    ColAn <- cbind.data.frame(ExistingColumns,Selected,Contained,PI)
                    
                    # computing individual labels for each column
                    #library(data.table)
                    ColAn$Selected <- fifelse(ColAn$Selected == TRUE, "selected", "not selected", NA)
                    ColAn$Contained <- fifelse(ColAn$Contained == TRUE, "contained", "not contained", NA)
                    ColAn$PI <- fifelse(ColAn$PI == TRUE, "PI", "not PI", NA)
                    
                    # from lables, compute overall summary and combine in dataframe
                    output <- list(Data = ColAn,
                                Selected = ColAn$ExistingColumns[ColAn$Selected == "selected"],
                                Excluded = ColAn$ExistingColumns[ColAn$Selected == "not selected"],
                                Contained = ColAn$ExistingColumns[ColAn$Contained == "contained"],
                                ExcludedPI = ColAn$ExistingColumns[ColAn$Selected == "not selected" & ColAn$PI == "PI"],
                                ExcludedNonPI = ColAn$ExistingColumns[ColAn$Selected == "not selected" & ColAn$PI == "not PI"],
                                NotSelectedButIncludedNotPI = ColAn$ExistingColumns[ColAn$Selected == "not selected" & ColAn$Contained == "contained" & ColAn$PI == "not PI"],
                                NotSelectedButIncludedPI = ColAn$ExistingColumns[ColAn$Selected == "not selected" & ColAn$Contained == "contained" & ColAn$PI == "PI"],
                                SelectedButNotIncludedNotPI = ColAn$ExistingColumns[ColAn$Selected == "selected" & ColAn$Contained == "not contained" & ColAn$PI == "not PI"],
                                SelectedButNotIncludedPI = ColAn$ExistingColumns[ColAn$Selected == "selected" & ColAn$Contained == "not contained" & ColAn$PI == "PI"],
                                ContainedPISelected = ColAn$ExistingColumns[ColAn$Selected == "selected" & ColAn$Contained == "contained" & ColAn$PI == "PI"],
                                ContainedPINotSelected = ColAn$ExistingColumns[ColAn$Selected == "not selected" & ColAn$Contained == "contained" & ColAn$PI == "PI"])
                    
                    # give indicitave names to dataframe columns
                    names(output) <- c("Columns selected",
                                       "Selected",
                                       "Excluded",
                                       "Contained",
                                       "ExcludedPI",
                                       "ExcludedNonPI",
                                       "NotSelectedButIncludedNotPI",
                                       "NotSelectedButIncludedPI",
                                       "SelectedButNotIncludedNotPI",
                                       "SelectedButNotIncludedPI",
                                       "ContainedPISelected",
                                       "ContainedPINotSelected")
                    
                    # returning output
                    return(output)
                    
                  
                  # end of else statement
                  }
                  
                # end of else statement 
                }
  
# end of function 
}




#### APPLYING ANALYSIS FUNCTION TO DATA ####


# Computing results of simulated participants actions vs. data contained in simulated donations
ColResults <- list()
for (i in seq_along(SimulationLog)) {
  
  ColResults[[i]] <- ColSummary(SimulationLog[[i]],DecryptedList2[[i]])

}


# How many errors, column selection or no-selections are there? [There should only be few errors, if any]

ColType <- rep(NA,length(ColResults))
  
for (i in seq_along(ColResults)){
  
  if (is.null(names(ColResults[[i]][1]))) {
    
    ColType[i] <- NA
    
  } else {
   
    ColType[i] <- names(ColResults[[i]][1]) 
    
  }
  
  
};ColType

# Checking NotSelectedButIncludedNotPI [THESE SHOULD ALL EMPTY, IF NOT, THEN THERE IS NON-PI INFORMATION INCLUDED THAT WAS NOT SELECTED BY THE SIMULATED PARTICIPANT]
Bug_NotSelectedButIncludedNotPI <- list()

for (i in seq_along(ColResults)){
  
  if (is.null(names(ColResults[[i]][1]))) {
    
    Bug_NotSelectedButIncludedNotPI[[i]] <- "No Data donated"
    
  } else {
    
    Bug_NotSelectedButIncludedNotPI[[i]] <- ColResults[[i]]$NotSelectedButIncludedNotPI
    
  }
  
  
};Bug_NotSelectedButIncludedNotPI





# Checking NotSelectedButIncludedPI [THESE SHOULD BE ALL EMPTY, IF NOT THEN THERE IS PI INFORMATION INCLUDED THAT WAS NOT SELECTED BY THE SIMULATED PARTICIPANT]
Bug_NotSelectedButIncludedPI <- list()

for (i in seq_along(ColResults)){
  
  if (is.null(names(ColResults[[i]][1]))) {
    
    Bug_NotSelectedButIncludedPI[[i]] <- "No Data donated"
    
  } else {
    
    Bug_NotSelectedButIncludedPI[[i]] <- ColResults[[i]]$NotSelectedButIncludedPI
    
  }
  
  
};Bug_NotSelectedButIncludedPI



# Checking SelectedButNotIncludedNotPI [THESE SHOULD BE ALL EMPTY, IF NOT THEN THERE IS NON-PI INFORMATION THAT WAS SELECTED BY THE SIMULATED PARTICIPANT NOT INCLUDED IN THE DATA]
Bug_SelectedButNotIncludedNotPI<- list()

for (i in seq_along(ColResults)){
  
  if (is.null(names(ColResults[[i]][1]))) {
    
    Bug_SelectedButNotIncludedNotPI[[i]] <- "No Data donated"
    
  } else {
    
    Bug_SelectedButNotIncludedNotPI[[i]] <- ColResults[[i]]$SelectedButNotIncludedNotPI
    
  }
  
  
};Bug_SelectedButNotIncludedNotPI



# Checking SelectedButNotIncludedNotPI [THESE CAN CONTAIN VALUES, THESE PI COLUMNS WERE SELECTED BY SIMULATED PARTICIPANTS BUT AUTOMATICALLY REMOVED]
Bug_SelectedButNotIncludedPI<- list()

for (i in seq_along(ColResults)){
  
  if (is.null(names(ColResults[[i]][1]))) {
    
    Bug_SelectedButNotIncludedPI[[i]] <- "No Data donated"
    
  } else {
    
    Bug_SelectedButNotIncludedPI[[i]] <- ColResults[[i]]$SelectedButNotIncludedPI
    
  }
  
  
};Bug_SelectedButNotIncludedPI




##### CLOSER LOOK AT BAD CASES; IF ANY ARE PRESENT  ####

#Bug_NotSelectedButIncludedNotPI [THESE MUST BE ALL 'FALSE' BEFORE YOU LET REAL PEOPLE ONTO YOUR CHATDASHBOARD INSTANCE]
problematics1<- lapply(Bug_NotSelectedButIncludedNotPI,function(x){print(if(length(x) == 0){return(FALSE)} else if (length(x) == 1 & x[1] == "No Data donated"){return(FALSE)} else {return(TRUE)})})
problematic_indices1 <- which(problematics1 == TRUE)

# checking logs and data [THESE MUST ALL BE EMPTY]
AllUserActions[problematic_indices1]
DecryptedList2[problematic_indices1]
ColResults[problematic_indices1]
SimulationLog[problematic_indices1]


#Bug_NotSelectedButIncludedPI [THESE MUST BE ALL 'FALSE' BEFORE YOU LET REAL PEOPLE ONTO YOUR CHATDASHBOARD INSTANCE]
problematics2<- lapply(Bug_NotSelectedButIncludedPI,function(x){print(if(length(x) == 0){return(FALSE)} else if (length(x) == 1 & x[1] == "No Data donated"){return(FALSE)} else {return(TRUE)})})
problematic_indices2 <- which(problematics2 == TRUE)

# checking logs and data [THESE MUST ALL BE EMPTY]
AllUserActions[problematic_indices2]
DecryptedList2[problematic_indices2]
ColResults[problematic_indices2]
SimulationLog[problematic_indices2]



#Bug_SelectedButNotIncludedNotPI [THESE SHOULD BE ALL 'FALSE', IF THEY ARE NOT, NON-PI DATA THAT WAS SELECTED FOR DONATION IS MISSING IN THE SIMULATED DONATIONS]
problematics3<- lapply(Bug_SelectedButNotIncludedNotPI,function(x){print(if(length(x) == 0){return(FALSE)} else if (length(x) == 1 & x[1] == "No Data donated"){return(FALSE)} else {return(TRUE)})})
problematic_indices3 <- which(problematics3 == TRUE)

# checking logs and data [THESE SHOULD ALL BE EMPTY]
AllUserActions[problematic_indices3]
DecryptedList2[problematic_indices3]
ColResults[problematic_indices3]
SimulationLog[problematic_indices3]



# Bug_SelectedButNotIncludedPI [THESE ARE ALLOWED TO CONTAIN VALUES, THESE PI COLUMNS WERE SELECTED BY PARTICIPANTS BUT AUTOMATICALLY REMOVED]
problematics4<- lapply(Bug_SelectedButNotIncludedPI,function(x){print(if(length(x) == 0){return(FALSE)} else if (length(x) == 1 & x[1] == "No Data donated"){return(FALSE)} else {return(TRUE)})})
problematic_indices4 <- which(problematics4 == TRUE)

# checking logs and data [THESE ARE ALLOWED TO CONTAIN VALUES]
AllUserActions[problematic_indices4]
DecryptedList2[problematic_indices4]
ColResults[problematic_indices4]
SimulationLog[problematic_indices4]




########### FINAL CHECKS ##############

# Final Check
table(unlist(problematics1)) # [THESE SHOULD BE ALL 'FALSE', IF NOT, YOU HAVE NON-SELECTED COLUMNS WITH NON-PII IN YOUR DONATIONS]
table(unlist(problematics2)) # [THESE SHOULD BE ALL 'FALSE', IF NOT, YOU HAVE NON-SELECTED COLUMNS WITH POTENTIAL PII IN YOUR DONATIONS]
table(unlist(problematics3)) # [THESE SHOULD BE ALL 'FALSE', IF NOT, YOU ARE MISSING SELECTED COLUMNS WITH NON-PII IN YOUR DONATIONS]


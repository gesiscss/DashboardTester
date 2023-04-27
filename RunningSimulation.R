#### Script for running a simulation of participants on a ChatDashboard instance 

# Tutorial for using RSelenium:
# https://www.youtube.com/watch?v=WRjKyCZsbE4

##### setup

# garbage collector to close potentially remaining RSelenium connections before initiating a new one
gc()

# Set this to the source file directory
#setwd("SOURCE FILE DIRECTORY")

# number of users to simulate (simulating 1 user takes several minutes)
simulate_n_users = 15

# loading necessary packages (install first if not available)
require(WhatsR)
require(RSelenium)
require(tidyverse)
require(rvest)

# sourcing simulation function
source("./SimulationFunction.R")

# initiating empty list for storing simulation results
SimulationResults <- list()

# looping through all participants to be simulated and storing results
# TODO: adapt parameters for SimulateChatDashboardParticipant() function before running the loop
for (i in 1:simulate_n_users) {
  
  # simulation loop
  SimulationResults[[i]]  <- SimulateChatDashboardParticipant(url = "https://shiny.molekulare-psychologie.de/jkohne/ChatDashboardShowcase/?id=", # url of the ChatDashboard instance you want to test,
                                                              # e.g. https://shiny.molekulare-psychologie.de/jkohne/ChatDashboardShowcase/?id=
                                                              id = "SimulatedParticipant", # Name for the simulated participant, will be appended to the /?id= part of the url and used as a username
                                                              pw = "password", # Password for accessing the ChatDashboard instance
                                                              browser = "chrome", # Webbrowser to use for simulating participants
                                                              version = "112.0.5615.49", # version of webdriver to use, this needs to match a browser version currently installed on your system
                                                              port = 4567L, # port to use for RSelenium session
                                                              filePath = "/home/juko/Desktop/GoogleDrive/Dissertation/Infrastruktur_Studie_1/Sharing/DashboardTester/UploadData" # path to folder containing the files a simulated participant might upload. Can be generated
                                                              # using the create_chatlog() function in the WhatsR package.
                                                              )
  
  # print info
  print(paste0("finsihed simulating user ",i))
  
}

# save simulation results as data frame in folder for further analysis. 
# WARNING: This will overwrite existing files with the same name!
saveRDS(SimulationResults,paste("./AnalyzingSimulation/SimulationLogs/","SimulationLog_",gsub(" ","_",Sys.time()),".rds",sep = ""))


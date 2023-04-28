#### Script for running a simulation of participants on a ChatDashboard instance 

##### setup

# garbage collector to close potentially remaining RSelenium connections before initiating a new one
gc()

# Set this to the source file directory
#setwd("SOURCE FILE DIRECTORY")

# number of users to simulate (simulating 1 user takes several minutes)
simulate_n_users = 1

# loading necessary packages (install first if not available)
require(WhatsR)
require(RSelenium)
require(tidyverse)
require(rvest)


##### setting simulation parameters (passed to SimulateChatDashboardParticipant())

# set the url where your ChatDashboard instance is running (including the forwarding parameter)
url_set = "https://shiny.molekulare-psychologie.de/jkohne/ChatDashboardShowcase/?id="

# set the id that the simulated participant should use to log in to the ChatDashboard
id_set = "SimulatedParticipant"

# set the password the simulated participant should use to log in to the ChatDashboard
pw_set = "password"

# set the web driver used by R Selenium to simulate the participant
browser_set = "chrome"

# set the chromeversion used to simulate the participant, passed to chreomver paramter in RSelenium::rsDriver()
version_set = "112.0.5615.49"

# set thew port used to simulate the participant, must be valid and available
port_set = 4567L

# set the file path for the chat logs to be uploaded by simulated participants
filePath_set = "./UploadData"













##### running simulation

# sourcing simulation function
source("./SimulateChatDashboardParticipant.R")

# initiating empty list for storing simulation results
SimulationResults <- list()

# looping through all participants to be simulated and storing results
# TODO: adapt parameters for SimulateChatDashboardParticipant() function before running the loop
for (i in 1:simulate_n_users) {
  
  # simulation loop
  SimulationResults[[i]]  <- SimulateChatDashboardParticipant(url = url_set,
                                                              id = id_set,
                                                              pw = pw_set,
                                                              browser = browser_set,
                                                              version = version_set,
                                                              port = port_set,
                                                              filePath = filePath_set,
                                                              )
  
  # print info
  print(paste0("finsihed simulating user ",i))
  
}

# save simulation results as data frame in folder for further analysis. 
# WARNING: This will overwrite existing files with the same name!
saveRDS(SimulationResults,paste("./AnalyzingSimulation/SimulationLogs/","SimulationLog_",gsub(" ","_",Sys.time()),".rds",sep = ""))


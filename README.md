# DashboardTester

DashboardTester is an RSelenium script for simulating users on the [ChatDashboard](https://github.com/gesiscss/ChatDashboard) webapps. It can use self-exported and simulated chat logs for testing purposes and is written for the structure of the [default configuration of ChatDashboard](https://shiny.molekulare-psychologie.de/jkohne/ChatDashboardShowcase/?id=ShowCaseUser). Should you want to test an instance of ChatDashboard that you adapted, you might need to adapt DashboardTester accordingly. The scripts simulates research participants uploading, subsetting, and donating data, and enables to test if
 - a) All Personal Identifiable Information (PII) is reliably removed from donated datasets
 - b) Data not manually selected for data donation is not contained in the donations.

## Scientifc Use
If you are using DashboardTester in your research, please cite it accordingly:
```R
@software{julian_kohne_2023_7875924,
  author       = {Julian Kohne},
  title        = {{DashboardTester: Scripts for simulating and 
                   analyzing Participants on ChatDashboard}},
  month        = apr,
  year         = 2023,
  publisher    = {Zenodo},
  version      = {v1.0.0},
  doi          = {10.5281/zenodo.7875924},
  url          = {https://doi.org/10.5281/zenodo.7875924}
}
```

## Setup

#### ChatDashboard Settings
Before using DashboardTester, please ensure that the following requirements are met:

 - You have an instance of ChatDashboard running on an online server
 - The ChatDashboard instance is configured with `save_to_server = TRUE` and `use_forwarding = TRUE`
 - You have access to the `UserData` folder on the server where this instance is running
 - You have access to the RSA key pair necessary for decrypting data donations made through this instance


#### DashboardTester Dependencies
To setup the DashboardTester, you first need to download this repository to your local PC and install all necessary R-packages. If you already
have installed `WhatsR` and the dependencies for `ChatDashboard`, you should already have all necessary packages available, except for RSelenium which can be installed using `install.packages("RSelenium")`. If there are any issues, you can manually install all necessary packages for `DashboardTester`

```R
# Installing DashboardTester dependencies
# code form: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
DashboardTester_dep <- c("cyphr",
                         "data.table",
                         "WhatsR",
                         "RSelenium",
                         "tidyverse",
                         "rvest")
                       
DashboardTester_new <- DashbaordTester_dep[!(DashbaordTester_dep %in% installed.packages()[,"Package"])]
if(length(DashboardTester_new)) install.packages(DashboardTester_new)

```

#### DashboardTester Settings
Before running the script, you should also make the following adaptations to it:

```R
# Setting the working directory to the source file location
setwd("SOURCE FILE DIRECTORY")

# set numbers of participants to simulate (start with a few to ensure everything is working)
simulate_n_users = 5

# set the url where your ChatDashboard instance is running (including the forwarding parameter)
url_set = "INSERT-URL-HERE"

# set the id that the simulated participant should use to log in to the ChatDashboard
id_set = "SimulatedParticipant"

# set the password the simulated participant should use to log in to the ChatDashboard
pw_set = "password"

# set the web driver used by R Selenium to simulate the participant
browser_set = "chrome"

# set the chromeversion used to simulate the participant, passed to chromever paramter in RSelenium::rsDriver()
version_set = "112.0.5615.49"

# set thew port used to simulate the participant, must be valid and available
port_set = 4567L

# set the file path for the chat logs to be uploaded by simulated participants
filePath_set = "./UploadData"

```

#### Place necessary Data

To run DashboardTester, the script relies on some data that needs to be manually placed by you:
 
  - place the .txt files containing chats that simulated participants should upload into the `UploadData` folder in this directory. These .txt files can either be simulated using `WhatsR::create_chatlog()` or be manually exported between two consenting researchers for testing purposes.
  - place the RSA keypair for decrypting data donations into the `DecryptionKeypair` folder in the `AnalyzingSimulation/DecryptionKeypair/` directory

## Running DashboardTester

To run the simulation, simply highlight everything in the `RunningSimulation.R` script and execute in RStudio. You will see the progress in the RStudio console and will see the behavior of the simulated participants in a browser window.
**IMPORTANT:** Do not manually interact with the browser window as to not interfere with the simulated participants behavior!

## Analyzing Results

After the simulation is finished, you can analyze the results to ensure that PII is reliably excluded and only data manually selected for donation is contained in the donations. To do so, download the encrypted data donations from the server where your ChatDashboard instance is running and place them in the `./AnalyzingSimulation/SimulatedDonatios/` directory of the DashboardTester folder. Afterwards, open the `AnalyzingSimulation.R` script and run it line by line to compare the log of simulated participants behavior to the simulated, donated datasets.

## Troubleshooting
Here are some issues that might pop up and their most likely causes.

 - **Simulation does not start or returns 'ERROR':** Please make sure that you indicated an available web driver and specified a version that corresponds to the one installed on your system. For Google Chrome, you can check this [list](https://chromedriver.chromium.org/downloads). Should the issue persist, you can try to install the most up to date versions of the `wdman` and `RSelenium` packages from github using `devtools::install_github()`
 - **I get errors that specific files and folders do not exist:** Please check that you set the working directory to the source file location in `RunningSimulation.R`. Also ensure that you placed the .txt files to be uploaded by simulated participants in the `UploadData` folder before running the simulation, and that your RSA keys and the encrypted data donations are in the correct folders before running the analysis script. In addition, check if all file paths in the scripts correspond to the correct structure for file paths on your operating system.
 - **I can't decrypt the encrypted data donations:** Please ensure that you are using the correct RSA keypair in the `DecrpytionKeypair` folder, that corresponds to the one used on the ChatDashboard instance you are testing.
 - **I get errors in the analysis script:** Please make sure that the column names used in the analysis script correspond to the column names of your data donations.
 - **There are no files on the server after the simulation**: Please check that you are looking in the correct folder (`UserData`) on the server and that your ChatDashboard instance (the `app.R` file on the server) is configured with `save_to_server = TRUE`.

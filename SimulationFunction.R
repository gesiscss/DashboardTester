# Function for simulating research participants on an instance of ChatDashboard
SimulateChatDashboardParticipant <- function(url = "URL-TO-YOUR-SHINY-APP", # url of the ChatDashboard instance you want to test,
                                             # e.g. https://shiny.molekulare-psychologie.de/jkohne/ChatDashboardShowcase/?id=
                                             id = "SimulatedParticipant", # Name for the simulated participant, will be appended to the /?id= part of the url and used as a username
                                             pw = "password", # Password for accessing the ChatDashboard instance
                                             browser = "chrome", # web driver for simulating participants
                                             version = "112.0.5615.49", # version of web driver to use, needs to match a browser version currently installed on your system
                                             port = 4567L, # port to use for RSelenium session
                                             filePath = "PATH-TO-YOUR-TEST-FILES" # path to folder containing the files a simulated participant should upload. Can be generated
                                             # using the create_chatlog() function in the WhatsR package.
){
  
  # using trycatch to not break loop should an error occur
  out <- tryCatch({
    
    ### listing test files and selecting one file at random
    Testfiles <- list.files(filePath)[which(sapply(strsplit(list.files(filePath),".",fixed = TRUE),`[[`,2) == "txt")]
    filePath <- paste0(filePath,"/",sample(Testfiles,1))
    
    # Initializing output list (settings and simulated participant behavior)
    Output <- list(Setup = c(url,id,pw,browser, version, port, filePath,time_start = Sys.time(),time_finish = NA),
                   UserActions = NA,
                   SelectedColumns = NA,
                   ExcludedRows = NA,
                   ServersideMessages = NA)
    
    
    #### Setting up web driver ####
    
    # creating driver object
    rs_driver_object <- rsDriver(browser = browser,
                                 chromever = version,
                                 verbose = F,
                                 port = port)
    
    # creating client from driver object
    remDr <- rs_driver_object$client
    
    
    #### Simulate Participant Behavior ####
    
    # navigating to website
    Sys.sleep(3)
    url <- paste0(url,id)
    remDr$navigate(url)
    Sys.sleep(3)
    
    # entering username
    login <- remDr$findElement(using = "xpath",value = '//*[@id="auth-user_id"]')
    login$click()
    login$sendKeysToElement(list(id))
    
    # entering password 
    pwd <- remDr$findElement(using = "xpath",value = '//*[@id="auth-user_pwd"]')
    pwd$click()
    pwd$sendKeysToElement(list(pw))
    
    # moving mouse around and clicking
    remDr$mouseMoveToLocation(100, 50)
    remDr$click(buttonId = 1)
    
    # clicking login button
    Sys.sleep(3)
    remDr$findElement(using = "xpath", '//*[@id="auth-go_auth"]')$clickElement()
    Output[[2]] <- "Log in complete"
    
    # moving mouse around and clicking
    remDr$mouseMoveToLocation(100, 50)
    remDr$click(buttonId = 1)
    
    ### clicking intro check button
    Sys.sleep(3)
    remDr$findElement(using = "xpath", '//*[@id="IntroCheck"]')$clickElement()
    Output[[2]] <- c(Output[[2]],"Intro confirmed")
    
    # sending file path to upload button
    Sys.sleep(3)
    remDr$findElement("id", 'file')$sendKeysToElement(list(filePath))
    Output[[2]] <- c(Output[[2]],"File uploaded")
    
    # click processing button
    Sys.sleep(30)
    remDr$findElement(using = "xpath", '//*[@id="submit"]')$clickElement()
    Output[[2]] <- c(Output[[2]],"File processing button clicked")
    Sys.sleep(60)
    
    # Creating ColClicks object
    ColClicks <- 0
    
    ##### SELECTING COLUMNS FUNCTION ####
    
    Column_select <- function(){
      
      # click dropdown menu
      remDr$findElement(using = "css selector", ".btn.dropdown-toggle.btn-default")$clickElement()
      
      # defining Xpaths for column selector elements
      ColSelectorVec <- NULL
      Sys.sleep(3)
      for (i in 0:15) {ColSelectorVec[i] <- paste0('//*[@id="bs-select-3-',i,'"]/span[2]')}
      
      # random selection of xpaths
      RandCols <- NULL
      RandCols <- sample(ColSelectorVec,size = sample(1:50,1),replace = TRUE)
      
      # clicking on random selection of columns
      Sys.sleep(1)
      sapply(RandCols,function(x){remDr$findElement(using = "xpath", x)$clickElement();Sys.sleep(1)})
      
      # updating colClicks object
      ColClicks <<- ColClicks + length(RandCols)
      
      # unclicking dropdown menu
      Sys.sleep(3)
      remDr$findElement(using = "css selector", ".btn.dropdown-toggle.btn-default")$clickElement()
      
      #### Saving Data Selection in Variables ####
      
      # User decision
      Output[[2]] <<- c(Output[[2]],"Selected Columns")
      
      # selected columns
      page_colsel_xpath <- '//*[@id="bs-select-3"]/ul'
      doc_pages_colsel <- read_html(remDr$getPageSource()[[1]]) # we already scraped this
      children_colsel_pages <- doc_pages_colsel %>% html_node(xpath = page_colsel_xpath) %>% html_children()
      Selected_columns <- html_text(children_colsel_pages)[as.character(html_attrs(children_colsel_pages)) == "selected"]
      
      # saving to output
      Output[[3]] <<- c(Output[[3]],Selected_columns,NA)
      
    }
    
    
    ##### SELECTING ROWS FUNCTION ####
    
    Row_select <- function(){
      
      # creating empty vector/emptying it
      RowSelectorVec <- NULL
      
      # getting number of existing table pages
      Sys.sleep(3)
      pages_xpath <- paste0('//*[@id="DataTables_Table_',ColClicks,'_paginate"]/span')
      doc_pages <- read_html(remDr$getPageSource()[[1]])
      children_pages <- doc_pages %>% html_node(xpath = pages_xpath) %>% html_children()
      
      # removing ellipsis if present
      if (html_text(children_pages[c(6)][[1]]) == "…") { children_pages <- children_pages[-c(6)]}
      if (html_text(children_pages[c(2)][[1]]) == "…") { children_pages <- children_pages[-c(2)]}
      
      # removing current table sheet to reduce the number of possible selections to 5
      children_pages <- children_pages[-c(1)]
      
      # chose random pages to click on
      page_nums <- sample(1:length(children_pages),sample(1:length(children_pages),1), replace = TRUE)
      
      # remove zeros from page_nums
      page_nums <- page_nums[page_nums != 0]
      
      # creating Xpaths of table pages to click on
      pages_xpaths <- sapply(page_nums, function(x){paste0(pages_xpath,'/a[',x,']')})
      
      # for each selected table page, we select rows to exclude at random
      for (i in pages_xpaths) {
        
        # going to table page
        Sys.sleep(3)
        remDr$findElement(using = "xpath", i)$clickElement()
        Sys.sleep(3)
        
        # counting number of rows and creating rows_Xpaths accordingly
        rows_xpaths <- paste0('//*[@id="DataTables_Table_',ColClicks,'"]/tbody')
        doc_rows <- read_html(remDr$getPageSource()[[1]])
        children_rows <- doc_rows %>% html_node(xpath = rows_xpaths) %>% html_children()
        length(children_rows)
        
        # creating empty vector/emptying it
        RowSelectorVec <- NULL
        j <- NULL
        
        # creating vector of xpaths for rows to be clicked on
        for (j in seq_along(children_rows)) {
          
          RowSelectorVec[j] <- paste0('//*[@id="DataTables_Table_',ColClicks,'"]/tbody/tr[',j,']')
          
        }
        
        # random selection of Xpaths for rows to click on
        RandRows <- sample(RowSelectorVec,size = sample(1:50,1),replace = TRUE)
        
        # clicking on random selection of rows
        sapply(RandRows,function(x){remDr$findElement(using = "xpath", x)$clickElement()})
        
        # Saving excluded rows
        Output[[4]] <<- c(Output[[4]],i,RandRows,NA)
        
      }
      
      # clicking on exclude rows for all selected rows
      remDr$findElement(using = "xpath", '//*[@id="excludeRows"]')$clickElement()
      
      # updating Colclicks to get correct DataTables_Table_XX number
      ColClicks <<- ColClicks + 1
      
      # saving to output
      Output[[2]] <<- c(Output[[2]],"Selected Rows")
      
    }
    
    
    ##### RESTORING ROWS FUNCTION ####
    
    # function for restoring rows and updating ColClicks
    Row_restore <- function(){
      
      # clicking on restore_rows button
      remDr$findElement(using = "xpath", '//*[@id="RestoreRows"]')$clickElement()
      
      # updating ColClicks to get correct DataTables_Table_XX number
      ColClicks <<- ColClicks + 1
      
      # saving to output
      Output[[2]] <<- c(Output[[2]],"Restored Rows")
      
    }
    
    
    #### RANDOM ORDER OF SELECTING COLUMNS,ROWS, AND RESTORING THEM. ####
    
    # initial random value to determine next step of participant
    RandValStart = sample(100:200,1)
    
    # loop for random order and times for removing rows, columns, and restoring rows
    DataSelection <- function(RandVal = RandValStart) {
      
      while (RandVal >= 100) {
        
        if (RandVal > 166) {
          
          Column_select()
          RandVal <- sample(0:200,1)
          
        }
        
        if (RandVal > 133 && RandVal <= 166) {
          
          Row_select()
          RandVal <- sample(0:200,1)
          
        }
        
        if (RandVal <= 133 && RandVal >= 100) {
          
          Row_restore()
          RandVal <- sample(0:200,1)
          
        }
        
      }
      
    }
    
    # Running function for random user behavior: selecting columns, rows & restoring excluded rows/columns
    Sys.sleep(3)
    DataSelection()
    
    
    ##### DATA DONATION #####
    
    ### clicking on data donation button
    Sys.sleep(3)
    remDr$findElement(using = "xpath", '//*[@id="donation"]')$clickElement()
    Output[[2]] <- c(Output[[2]],"Data Donation button clicked")
    Sys.sleep(3)
    
    ## initial value for random choice for data donation
    Value_randStart = sample(1:200,1)
    
    # function for random behavior of clicking on donation button and either accepting or going back and restart data selection
    DataDonation <- function(Value_rand = Value_randStart) {
      
      while (Value_rand <= 100) {
        
        ### going back to regular page
        Sys.sleep(3)
        remDr$findElement(using = "xpath", '/html/body/div[9]/div[7]/button[1]')$clickElement()
        Output[[2]] <<- c(Output[[2]],"Clicking no on data donation popup")
        
        # UserDecision
        Output[[2]] <<- c(Output[[2]],"Went back from donation consent")
        
        ### Call Data Selection again
        DataSelection()
        
        ### clicking on data donation button
        remDr$findElement(using = "xpath", '//*[@id="donation"]')$clickElement()
        Output[[2]] <<- c(Output[[2]],"Data Donation button clicked")
        
        # renewing random variable
        Value_rand = sample(1:200,1)
        
      }
      
      # clicking on okay button for data donation
      Sys.sleep(3)
      remDr$findElement(using = "xpath", '/html/body/div[9]/div[7]/button[2]')$clickElement()
      Output[[2]] <<- c(Output[[2]],"Gave Donation consent")
      
    }
    
    #### Running Data Donation function ####
    DataDonation()
    Output$Setup["time_finish"] <- Sys.time()
    
    ### clicking "ok" on auto-removal message
    Sys.sleep(5)
    remDr$findElement(using = "xpath", '/html/body/div[9]/div[7]/button[2]')$clickElement()
    Sys.sleep(3)
    
    
    #### CLOSING PROCESS #####
    
    #### Logging every server-side message
    Output[[5]] <- remDr$log("browser")
    names(Output) <- c("Setup","UserActions","SelectedColumns","ExcludedRows","ServersideMessages")
    
    ### closing web driver after finishing
    remDr$close()
    rs_driver_object$server$stop()
    
    ### returning log
    return(Output)
    
    # closing down webdriver and logging error message should the iteration fail
  },
  
  error = function(cond) {
    
    # closing driver
    print("ERROR")
    Sys.sleep(5)
    remDr$close()
    rs_driver_object$server$stop()
    
    ### returning log
    return(Output)
    
  }
  
  # end of tryCatch
  )
  
  ### returning log
  return(Output)
  
}
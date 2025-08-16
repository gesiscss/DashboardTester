# Function for simulating research participants on an instance of ChatDashboard

# Tutorial for using RSelenium:
# https://www.youtube.com/watch?v=WRjKyCZsbE4

# when running the function outside of a scriopt, you need to load the following packages:
# library(RSelenium)
# library(rvest)
# library(xml2)
# library(magrittr)


SimulateChatDashboardParticipant <- function(url = "URL-TO-YOUR-SHINY-APP", # url of the ChatDashboard instance you want to test,
                                             # e.g. https://l.linklyhq.com/l/1kUiI/?id=
                                             id = "SimulatedParticipant", # Name for the simulated participant, will be appended to the /?id= part of the url and used as a username
                                             pw = "password", # Password for accessing the ChatDashboard instance
                                             browser = "firefox", # web driver for simulating participants
                                             version = "latest", # version of web driver to use, needs to match a browser version currently installed on your system
                                             port = 4567L, # port to use for RSelenium session
                                             filePath = "PATH-TO-YOUR-TEST-FILES" # path to folder containing the files a simulated participant should upload. Can be generated
                                             # using the create_chatlog() function in the WhatsR package.
){
  
  # using trycatch to not break loop should an error occur
  out <- tryCatch({
    
    ### listing test files and selecting one file at random
    Testfiles <- list.files(filePath)[which(sapply(strsplit(list.files(filePath),".",fixed = TRUE),`[[`,2) == "txt")]
    filePath <- paste0(filePath,"/",sample(Testfiles,1))
    filePath <- normalizePath(filePath, winslash = "/", mustWork = TRUE)
    
    # Initializing output list (settings and simulated participant behavior)
    Output <- list(
      Setup = c(url,id,pw,browser,version,port,filePath,time_start = Sys.time(), time_finish = NA),
      UserActions = character(),
      SelectedColumns = NA,
      ExcludedRows = NA,          # numeric bucket
      ServersideMessages = character()
    )
    
    
    #### Setting up web driver quietly ####
    
    # quiet helper (one-liner)
    quiet <- function(x) suppressWarnings(suppressMessages(x))
    
    # creating driver object
    rs_driver_object <- quiet(rsDriver(browser = browser,
                                 geckover = version,
                                 verbose = FALSE,
                                 check = FALSE,
                                 port = port,
                                 phantomver = NULL))
    
    # creating client from driver object
    remDr <- rs_driver_object$client
    
    # Define retry function to deal with stale elements
    retry <- function(expr, tries = 5, pause = 0.3) {
      for (i in seq_len(tries)) {
        ok <- try(force(expr), silent = TRUE)
        if (!inherits(ok, "try-error")) return(invisible(TRUE))
        Sys.sleep(pause)
      }
      stop("retry failed")
    }
    
    # waiting function
    wait_for <- function(using, value, timeout = 20) {
      t0 <- Sys.time()
      repeat {
        if (length(remDr$findElements(using, value))) return(invisible(TRUE))
        if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) stop("timeout: ", value)
        Sys.sleep(0.25)
      }
    }
    
    
    #### Simulate Participant Behavior ####
    
    # navigating to website
    remDr$navigate(paste0(url, id))
    wait_for("id", "auth-user_id")
    remDr$findElement("id", "auth-user_id")$sendKeysToElement(list(id))
    remDr$findElement("id", "auth-user_pwd")$sendKeysToElement(list(pw))
    retry({ remDr$findElement("id", "auth-go_auth")$clickElement() })
    Sys.sleep(3)
    
    # clicking intro check button
    wait_for("id", "IntroCheck")
    retry({ remDr$findElement("id", "IntroCheck")$clickElement() })
    Sys.sleep(3)
    
    # moving mouse around and clicking
    remDr$mouseMoveToLocation(100, 50)
    remDr$click(buttonId = 1)
    
    # sending file path to upload button
    wait_for("id", "file")
    retry({ remDr$findElement("id", "file")$sendKeysToElement(list(filePath)) })
    Sys.sleep(3)
    
    # click processing button
    retry({ remDr$findElement("id", "submit")$clickElement() })
    Output[[2]] <- c(Output[[2]],"File processing button clicked")
    Sys.sleep(15)
    
    # Select random chat donor
    Sys.sleep(5)
    retry({ remDr$findElement("id", "person_select-selectized")$clickElement() })
    retry({
      opts <- remDr$findElements("css selector", ".selectize-dropdown-content .option[data-selectable]")
      opts[[ sample.int(length(opts), 1) ]]$clickElement()
    })
    Output[[2]] <- c(Output[[2]], "Random person selected from dropdown")
    Sys.sleep(3)
    
    #click "continue" button
    Sys.sleep(3)
    retry({ remDr$findElement("id", "person_submit")$clickElement() })
    wait_for("id", "donation")
    Sys.sleep(3)

    # Creating ColClicks object
    ColClicks <- 0
    
    ##### SELECTING COLUMNS FUNCTION ####
    
    Column_select <- function(){
      # open dropdown
      retry({ remDr$findElement("css selector", ".btn.dropdown-toggle.btn-default")$clickElement() })
      
      # dynamic menu id
      menu    <- remDr$findElement("css selector", "div[id^='bs-select-'][role='listbox']")
      menu_id <- menu$getElementAttribute("id")[[1]]
      
      # parse current selection
      src <- remDr$getPageSource()[[1]]
      doc <- xml2::read_html(src)
      ul  <- rvest::html_node(doc, xpath = sprintf('//*[@id="%s"]/ul', menu_id))
      lis <- rvest::html_children(ul)                             # <li> nodes
      is_sel <- as.character(rvest::html_attrs(lis)) == "selected"
      sel_idx <- which(is_sel); n_sel <- length(sel_idx)
      
      # plan minimal changes: may deselect but never drop below 2 selected
      max_desel <- max(0, n_sel - 2)
      n_desel   <- if (max_desel > 0) sample(0:max_desel, 1) else 0
      desel_idx <- if (n_desel > 0) sample(sel_idx, n_desel) else integer(0)
      
      candidates <- which(!is_sel)
      n_add_max  <- min(length(candidates), 50)
      n_add      <- if (n_add_max > 0) sample(0:n_add_max, 1) else 0
      add_idx    <- if (n_add > 0) sample(candidates, n_add, replace = FALSE) else integer(0)
      
      picks <- c(desel_idx, add_idx)
      if (!length(picks)) {
        retry({ remDr$findElement("css selector", ".btn.dropdown-toggle.btn-default")$clickElement() })
        # log current selection and exit
        Selected_columns <- rvest::html_text(lis)[is_sel]
        Output[[2]] <<- c(Output[[2]], "Selected Columns")
        Output[[3]] <<- c(Output[[3]], Selected_columns, NA)
        return(invisible(NULL))
      }
      
      # click each option (toggle) – click the <a> under each <li>
      opt_xpath <- sprintf('//*[@id="%s"]//li/a', menu_id)
      for (j in picks) {
        retry({ remDr$findElements("xpath", opt_xpath)[[j]]$clickElement() })
        Sys.sleep(0.2)
      }
      
      ColClicks <<- ColClicks + length(picks)
      
      # close dropdown
      retry({ remDr$findElement("css selector", ".btn.dropdown-toggle.btn-default")$clickElement() })
      
      # log final selected labels
      src2 <- remDr$getPageSource()[[1]]
      doc2 <- xml2::read_html(src2)
      ul2  <- rvest::html_node(doc2, xpath = sprintf('//*[@id="%s"]/ul', menu_id))
      lis2 <- rvest::html_children(ul2)
      is_sel2 <- as.character(rvest::html_attrs(lis2)) == "selected"
      labels <- rvest::html_text(lis2)[is_sel2]
      
      Output[[2]] <<- c(Output[[2]], "Selected Columns")
      Output[[3]] <<- c(Output[[3]], labels, NA)
    }
    
    
    
    
    ##### SELECTING ROWS FUNCTION ####
    
    # Function for selecting rows and excluding them
    Row_select <- function(){
      pags <- remDr$findElements(
        "css selector",
        ".dataTables_paginate a.paginate_button:not(.previous):not(.next):not(.current)"
      )
      if (length(pags) == 0L) return(invisible(NULL))
      to_click <- sample(seq_along(pags), size = min(3, length(pags)))
      
      cells_css <- "#frame tbody tr:not(.dataTables_empty) td:first-child"
      rows_css  <- "#frame tbody tr:not(.dataTables_empty)"
      excluded_abs <- integer(0)
      
      click_rows_and_remove <- function(){
        for (t in 1:40) {
          if (length(remDr$findElements("css selector", cells_css)) > 0) break
          Sys.sleep(0.1)
        }
        cells <- remDr$findElements("css selector", cells_css)
        n <- length(cells); if (n == 0L) return(invisible(NULL))
        k <- max(1, min(n, floor(n/3)))
        idx <- sample.int(n, k)
        
        for (j in idx) {
          retry({
            el <- remDr$findElements("css selector", cells_css)[[j]]
            remDr$executeScript("arguments[0].scrollIntoView({block:'center'});", list(el))
            el$clickElement()
          })
          Sys.sleep(0.1)
        }
        if (length(remDr$findElements("css selector", "#frame tbody tr.selected")) == 0) {
          for (j in idx) {
            retry({
              el <- remDr$findElements("css selector", rows_css)[[j]]
              remDr$executeScript("arguments[0].scrollIntoView({block:'center'});", list(el))
              el$clickElement()
            })
            Sys.sleep(0.1)
          }
        }
        
        # read ABSOLUTE numbers shown in the first column of selected rows
        src <- remDr$getPageSource()[[1]]
        doc <- xml2::read_html(src)
        # td or th as first child
        num_txt <- rvest::html_text(rvest::html_nodes(doc, css = "#frame tbody tr.selected > *:first-child"))
        nums <- suppressWarnings(as.integer(gsub("[^0-9-]", "", num_txt)))
        nums <- nums[!is.na(nums)]
        if (length(nums)) excluded_abs <<- c(excluded_abs, nums)
        
        retry({ remDr$findElement("id", "excludeRows")$clickElement() })
        for (t in 1:40) {
          if (length(remDr$findElements("css selector", "#frame tbody tr.selected")) == 0) break
          Sys.sleep(0.1)
        }
      }
      
      click_rows_and_remove()
      
      for (i in to_click) {
        retry({
          remDr$findElements(
            "css selector",
            ".dataTables_paginate a.paginate_button:not(.previous):not(.next):not(.current)"
          )[[i]]$clickElement()
        })
        for (t in 1:40) {
          if (length(remDr$findElements("css selector", cells_css)) > 0) break
          Sys.sleep(0.1)
        }
        click_rows_and_remove()
      }
      
      ColClicks <<- ColClicks + 1
      if (length(excluded_abs)) Output[[4]] <<- c(Output[[4]], excluded_abs, NA)  # one NA separator per call
    }
    
    
    
  
    
    ##### RESTORING ROWS FUNCTION ####
    
    # function for restoring rows and updating ColClicks
    Row_restore <- function(){
      retry({ remDr$findElement("id", "RestoreRows")$clickElement() })
      
      # wait for table to repopulate after restore
      cells_css <- 'table.dataTable tbody tr:not(.dataTables_empty) td:nth-child(1)'
      for (t in 1:40) {
        if (length(remDr$findElements("css selector", cells_css)) > 0) break
        Sys.sleep(0.1)
      }
      
      ColClicks <<- ColClicks + 1
      Output[[2]] <<- c(Output[[2]], "Restored Rows")
      Sys.sleep(2)
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
    # clicking on data donation button
    retry({ remDr$findElement("id", "donation")$clickElement() })
    Output[[2]] <- c(Output[[2]], "Data Donation button clicked")
    
    # wait for shinyalert modal to appear
    Sys.sleep(3)
    for (i in 1:80) {
      if (length(remDr$findElements("css selector", "button.confirm, button.cancel")) > 0) break
      Sys.sleep(0.25)
    }
    
    ## initial value for random choice for data donation
    Value_randStart = sample(1:200,1)
    
    # function for random behavior of clicking on donation button and either accepting or going back and restart data selection
    DataDonation <- function(Value_rand = Value_randStart) {
      
      # assume donation modal is already open
      while (Value_rand <= 100) {
        
        # click "Zurück zur Datenauswahl"
        retry({ remDr$findElement("css selector", "button.cancel")$clickElement() })
        # wait modal closed
        for (i in 1:20) {
          if (length(remDr$findElements("css selector", "button.confirm, button.cancel")) == 0) break
          Sys.sleep(0.25)
        }
        
        # redo data selection
        DataSelection()
        
        # open donation modal again
        Sys.sleep(3)
        retry({ remDr$findElement("id", "donation")$clickElement() })
        # wait modal open
        #for (i in 1:20) {
        #  if (length(remDr$findElements("css selector", "button.confirm, button.cancel")) > 0) break
        #  Sys.sleep(0.25)
        #}
        
        Value_rand <- sample(1:200, 1)
      }
      
      # confirm donation
      Sys.sleep(3)
      retry({ remDr$findElement("css selector", "button.confirm")$clickElement() })
      # wait modal closed
      #for (i in 1:20) {
      #  if (length(remDr$findElements("css selector", "button.confirm, button.cancel")) == 0) break
      #  Sys.sleep(0.25)
      #}
      Output[[2]] <<- c(Output[[2]], "Gave Donation consent")
    }
    
    
    #### Running Data Donation function ####
    DataDonation()
    Output$Setup["time_finish"] <- Sys.time()
    
    ### clicking "ok" on auto-removal message
    Sys.sleep(5)
    remDr$findElement("css selector", "button.confirm")$clickElement()
    Sys.sleep(3)
    
    #### CLOSING PROCESS #####
    
    #### Logging every server-side message
    #Output[[5]] <- tryCatch(
    #  remDr$log("browser"),
    #  error = function(e) list(message = paste("Browser logs not available:", e$message))
    #)
    names(Output) <- c("Setup","UserActions","SelectedColumns","ExcludedRows","ServersideMessages")
    
    ### closing web driver after finishing
    remDr$close()
    rs_driver_object$server$stop()
    
    ### returning log
    return(Output)
    
    # closing down webdriver and logging error message should the iteration fail
  },
  
  error = function(cond) {
    # message("ERROR")  # or remove entirely if you want no output
    try(remDr$close(), silent = TRUE)
    try(rs_driver_object$server$stop(), silent = TRUE)
    return(Output)
  }
  
  # end of tryCatch
  )
  
  ### returning log
  return(Output)
  
}
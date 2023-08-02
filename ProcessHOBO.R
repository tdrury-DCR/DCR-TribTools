###############################  HEADER  ######################################
#  TITLE: ProcessHOBO.R
#  DESCRIPTION: Process raw HOBO txt files and import data to the database
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: 2020-12-30
#  GIT REPO: 
#  R version 3.5.3 (2019-03-11)  x86_64
##############################################################################.

# HOBO Workflow
# 1. Collect HOBO data and relaunch in field (ensure logger names are correct, make sure stage values at time collected are noted)
      # Settings: Computer Timezone should be set to GMT (UTC), units should be in ft/psi
# 2. Back at the office, transfer all downloaded .hobo files to the unprocessed folder specified in config file
# 3. Open up the plots for each file (no need to process, or do baro correction) and export each dataset to a txt file output in the unprocessed folder
# 4. View each file and clean up any stage values or temperatures at the beginning of each file that were messed up due to handling
# 5. Open the Ratings-HOBO shiny App 
# 6. You will be forced to process-import any Barometric files before moving on to water level files
#
# What the processing function does:
#
# Goes to the unprocessed folder and finds the file selected
# For Barometric files the only real task is to convert temperature to C and format the data for import
# For water level files, depending on what station is selected, it brings in the appropriate barometric data from DB
# Performs a left join with the barometric data by date
# Calculates the raw head (sensor water - sensor baro)/0.4335
# Subtract the stage value (arg) from the last calculated head (this is the offset)
# Calculate water level for remaining raw head measurements using offset
# Converts all temperatures to C
# Formats data for import

# ### Load libraries Needed
# library(tidyverse)
# library(lubridate)
# library(scales)
# library(odbc)
# library(RODBC)
# library(DBI)
# library(readxl)
# library(glue)

# #### Config file
# LOAD THIS FROM LAUNCH SCRIPT

#### HOBO TOOL Funtion Args
# hobo_path <- config[["HOBO_Imported"]]
# updir <- config[["HOBO_Staging"]] ### Set directory to where exported .hobo and .txt are staged
# hobo_db <- db ### Same as rating info - all in Hydro DB
# baro_table <- config[["HOBO_BARO"]]
# hobo_tbl <- config[["HOBO"]]
# ImportFlagTable <- config[["HydroFlagIndex"]]

### NOTE: After processing, raw data (txt) and .hobo files will get moved to the appropriate location file

# #Set user info
# user <-  Sys.getenv("USERNAME")
# userdata <- readxl::read_xlsx(path = config[["Users"]])
# username <- paste(userdata$FirstName[userdata$Username %in% user],userdata$LastName[userdata$Username %in% user],sep = " ")

###
### _____________________________________________________________________________________
###

### PROCESS_BARO function will import Barometric HOBO data to the Hydro Database

### List barometric txt files for HOBO downloads to be processed (pick mannually here, comment out if using in shiny app)
# baro_files <- list.files(updir, recursive = T, full.names = F, include.dirs = T, pattern = "^[^~$]+(_BARO_).*\\.txt$")
# baro_files ### Show the files
# baro_file <- baro_files[1] ### Pick a file

### Utility to find gaps in barometric data ####

# dfb <- dbReadTable(con, baro_table)
# dfb <- dfb[order(dfb$DateTimeUTC),]
# dfb$diff <- dfb$DateTimeUTC - lag(dfb$DateTimeUTC, n = 1L)
# 
# which(dfb$diff != 900)

PROCESS_BARO <- function(baro_file, userlocation){
  print(paste0("Barometric HOBO Data started processing at ", Sys.time()))
  ### Extract the location information from the Plot Title listed in the file
  file <- paste0(updir,"/", baro_file)
  
  ### Get Location info from file 
  loc <- str_split_fixed(baro_file, "_", n = 2) 
  loc <- loc[,1]
  
  ### Read the raw data file (tab delimited text file)
  baro <- readr::read_tsv(file, col_names = F, skip = 2) %>% 
    select(1:3) %>%
    drop_na() %>%
    mutate("Location" = loc, "ID" = NA)
  
  ### Rename columns
  names(baro) <- c("DateTimeUTC","Logger_psi", "Logger_temp_f","Location", "ID")
  ### Convert F to C
  baro$Logger_temp_c <- (baro$Logger_temp_f - 32) * 0.5556
  ### Round to 2 decimals
  baro[,c(2,3,6)] <- round(baro[,c(2,3,6)], digits = 2)
  
  if(userlocation == "Quabbin") {
    baro <- baro %>% 
      mutate(ImportDate = as_date(today()))
  }
  
  ### TIMEZONE TROUBLESHOOTING - RESOLVED BY USING UTC DATA OUTPUT AND WRITING DATA TO DB WITH UTC TIMEZONE
  ### Force the Data time zone (Outputs in local time)
  # str(baro)
  # 
  # baro$DateTimeUTC <- as.character(baro$DateTimeUTC) 
  # baro$DateTimeUTC <- as.POSIXct(baro$DateTimeUTC, tz = "UTC")
  # 
  # LocTZ <- "America/New_York"
  # UTC <- "UTC"
  # baro$DateTimeUTC <- force_tz(baro$DateTimeUTC, LocTZ) %>% as.POSIXct()
  # 
  # ### Convert to UTC to eliminate gaps and duplicates due to DST
  # baro$DateTimeUTC <- with_tz(baro$DateTimeUTC, UTC)
  
  ### Connect to db #1
  schema <- userlocation
  dsn <- 'DCR_DWSP_App_R'
  database <- "DCR_DWSP"
  tz <- 'UTC'
  con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)
  
  ### A function to fetch record IDs from the database table and assign record IDs to the new data
  setIDs <- function(){
    qry <- dbGetQuery(con, glue("SELECT max(ID) FROM [{schema}].[{baro_table}]"))
    ### Get current max ID
    if(is.na(qry)) {
      qry <- 0
    } else {
      qry <- qry
    }
    ID_max <- as.numeric(unlist(qry))
    rm(qry)
    
    ### Set IDs
    baro$ID <- seq.int(nrow(baro)) + ID_max
  }
  baro$ID <- setIDs()
  
  ### Reorder columns to match db
  col_order <- c(dbListFields(con, schema_name = schema, name = baro_table), "Logger_temp_f")
  baro <-  baro[col_order]
  
  ### Check for duplicate data in the database
  baro_existing <- dbGetQuery(con, glue("SELECT * FROM [{schema}].[{baro_table}] WHERE 
                                  [Location] = '{loc}'"))

  duplicates <- semi_join(baro, baro_existing, by="DateTimeUTC")
  
  if (nrow(duplicates) > 0){
    stop(paste0("This file duplicates ",nrow(duplicates)," existing ",loc," BARO records in the database."))
  }
  
  ### Get appropriate barometric file based on location

  if (userlocation == "Wachusett") {
    if(loc %in% c("FHLN", "FPRN", "HLNW", "PRNW")){
      baro_grp <- "FPRN"
    } else {
      baro_grp <- c("MD02", "MD83")
    }
  } else { ### Quabbin
    baro_grp <- "213"
  }  
  
  
  ### Grab last 1 days records to plot with new data to check for missed data corrections
  t <- min(baro$DateTimeUTC)
  baro_tbl <- tbl(con, Id(schema = schema, table = baro_table))
  baro_prior <- baro_tbl %>% 
    filter(Location %in% baro_grp, DateTimeUTC >= (t - 86400), DateTimeUTC < t) %>% 
    collect()
  
  ### Convert C to F
  baro_prior$Logger_temp_f <- baro_prior$Logger_temp_c * 1.8 + 32 
  ### R assumes that timezone is in local timezone (since access does not store tz info - it converts to what it thinks is UTC automatically
  ### So change it back to America/New_York, which reverses this change
  # baro_prior$DateTimeUTC <- with_tz(baro_prior$DateTimeUTC, tzone = "America/New_York") %>% as.POSIXct()
  ### Then force the actually timezone to be UTC *** NOTE - this may be unreliable if records cross the daylight savings time boundary
  # baro_prior$DateTimeUTC <- force_tz(baro_prior$DateTimeUTC, tzone = "UTC")
  
  ### Print to compare tail and head of consecutive records
  # print(tail(baro_prior, n = 10))
  # print(head(baro, n=10))
  baro <- select(baro, -Logger_temp_f)
  
  if(nrow(baro_prior) == 0) {
    baro_prior <-  NULL
  }
  ### Disconnect from db and remove connection obj
  dbDisconnect(con) #1
  rm(con)
  
  print(paste0("Barometric HOBO Data finished processing at ", Sys.time()))
  
  dfs <- list(
    "df" = baro,
    "df_flag" = NULL,
    "df_prior" = baro_prior)
  
  return(dfs)
} # End PROCESS BARO
# Comment out if running in shiny

# dfs <- PROCESS_BARO(baro_file)

###
### _____________________________________________________________________________________
###

### This function plots up the barometric data to preview before its imported

# pd <- dfs[[1]] # baro
# var2 <- NULL
# df_prior <- baro_prior
# df_baro <- dfs[[1]]

PREVIEW_BARO <- function(df_baro, df_prior = NULL, var2 = NULL){
  
  pd <- df_baro
  loc <- unlist(pd[1, "Location"])
  var2 <- "Logger_temp_c"
  cols <- c("Air Temperature (C)" = "turquoise3",
            "Air Temperature (C) - prior" = "yellowgreen",
            "Air Pressure (psi)" = "sienna2", 
            "Air Pressure (psi) - prior" = "tomato4"
  )
  
  if(is.null(df_prior)){
    prior <-  FALSE
  } else {
    prior <-  TRUE
  }
  
    title <- paste0("Raw Air Pressure and Air Temperature from Barometric HOBO\n At Location ", loc)
    y1lab <- "Air Pressure (psi)"
    y1lim <- max(pd$Logger_psi)
    y1data <- pd$Logger_psi
    y1prior <- df_prior$Logger_psi
    y1color <- "Air Pressure (psi)"
    y1prior_col <- "Air Pressure (psi) - prior"
    y2col <- "Air Temperature (C)"
    y2prior_col <- "Air Temperature (C) - prior"

  
  y2lim <- max(pd$Logger_temp_c)
  y2temp <- 
    mult <- y1lim / abs(y2lim)
  
  plot  <- ggplot() +
    geom_line(data = pd, aes(x = DateTimeUTC, y = Logger_temp_c * mult, color = y2col), size = 1) +
    # text = paste("Date-Time: ", pd$DateTimeUTC, "<br>", "Temperature (C): ", pd$Logger_temp_c)), size = 0.5) +
    geom_line(data = pd, aes(x = DateTimeUTC, y = y1data, color = y1color), size = 1) 
  # text = paste("Date-Time: ", pd$DateTimeUTC, "<br>", "Air Pressure (psi): ", pd$Logger_psi)), size = 1) 
  
  # Check for prior data to plot 
  if(isTRUE(prior)){
    plot <- plot +
      geom_line(data = df_prior, aes(x = DateTimeUTC, y = Logger_temp_c * mult, color = y2prior_col), size = 1) +
      # text = paste("Date-Time: ", df_prior$DateTimeUTC, "<br>", "Temperature (C): ", df_prior$Logger_temp_c))) +#, size = 0.5) +
      
      geom_line(data = df_prior, aes(x = DateTimeUTC, y = y1prior, color = y1prior_col), size = 1) +
      # text = paste("Date-Time: ", df_prior$DateTimeUTC, "<br>", "Air Pressure (psi): ", df_prior$Logger_psi))) + #, size = 1) +
      geom_vline(xintercept = min(pd$DateTimeUTC), color = "gray10", linetype = 2, size = 1.5, alpha = 0.8)
  }

    plot <- plot +
      scale_y_continuous(breaks = pretty_breaks(),limits = c(NA, 1.2 * y1lim),
                         sec.axis = sec_axis(~./mult, breaks = pretty_breaks(), name = "Air Temperature (C)"))

  plot <- plot +
    scale_x_datetime(breaks = pretty_breaks(n=12)) + 
    scale_colour_manual(values = cols) +
    labs(y = y1lab,
         x = "Date",
         color = "") +
    ggtitle(title) +
    theme_linedraw() +
    theme(plot.title = element_text(color= "black", face="bold", size=14, vjust = 1, hjust = 0.5),
          legend.position = "bottom",
          legend.text = element_text(margin = margin(r=0.8, unit="cm")),
          axis.title.x = element_text(angle = 0, face = "bold", color = "black"),
          axis.title.y = element_text(angle = 90, face = "bold", color = "black"))
  # plot <- plotly::ggplotly(plot, tooltip = c("text"))
  return(plot)
}
# 
# plot <- PREVIEW_BARO(df_baro = dfs[[1]], df_prior = dfs[[3]], var2 = NULL)
# plot
# #
# df_baro <- dfs[[1]]
# df_prior <-  dfs[[3]]

###
### _____________________________________________________________________________________
###

IMPORT_BARO <- function(df_baro, baro_file, userlocation){
  print(paste0("Barometric HOBO Data started importing at ", Sys.time()))
  loc <- str_split_fixed(baro_file, "_", n = 2) 
  loc <- loc[,1]
  file <- paste0(updir,"/", baro_file)
  hobo_file <- str_replace(file, "txt", "hobo")
  hobo_name <- str_replace(baro_file, "txt", "hobo")
  ### Import the data to the database - Need to use RODBC methods here.
  
  ### CONNECT TO A FRONT-END DATABASE ####
    ### Set DB
    schema <- userlocation
    dsn <- 'DCR_DWSP_App_R'
    database <- "DCR_DWSP"
    tz <- 'UTC'
    con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)

  odbc::dbWriteTable(con, DBI::SQL(glue("{database}.{schema}.{baro_table}")), value = df_baro, append = TRUE)
  ### Disconnect from db and remove connection obj
  dbDisconnect(con)
  rm(con)
  
  ### Move the processed raw data file and hobo file to the appropriate processed folder
  dir_num <- as.numeric(which(!is.na(str_match(list.dirs(hobo_path, recursive = T, full.names = T), loc))))
  subdir <- list.dirs(hobo_path, recursive = T, full.names = T)[dir_num]
  ### Move the processed raw data file and hobo file to the appropriate processed folder
 
  if(!dir.exists(subdir)) {
    dir.create(subdir)
  }
  
  file.rename(file, paste0(subdir, "/", baro_file))
  file.rename(hobo_file, paste0(subdir, "/", hobo_name))
  
  SendEmail(df=df_baro, table=baro_table, file=baro_file, emaillist=emaillist, username=username, userlocation=userlocation)
  
  return(paste0("Barometric HOBO Data finished importing at ", Sys.time()))
} ### End function

# IMPORT_BARO(df_baro, baro_file)

###
### _____________________________________________________________________________________
###

# ### List txt files for HOBO downloads to be processed
# hobo_txt_files <- list.files(updir, recursive = T, full.names = F, include.dirs = T, pattern = ".txt")
# hobo_txt_files ### Show the files
# hobo_txt_file <- hobo_txt_files[1] ### Pick a file
# username <- "Dan Crocker"
# stage <- 1.28 ### Enter stage at time of data download (Numeric entry in Shiny App

PROCESS_HOBO <- function(hobo_txt_file, stage, username, userlocation){
  print(paste0("HOBO Data started processing at ", Sys.time()))
  
  ### Extract the location information from the Plot Title listed in the file
  loc <- str_split_fixed(hobo_txt_file, "_", n = 2) 
  loc <- loc[,1]
  file <- paste0(updir,"/", hobo_txt_file)
  hobo_file <- str_replace(hobo_txt_file, "txt", "hobo")
  
  ### Read the raw data file (tab delimited text file)
  df <- readr::read_tsv(file, skip = 2, col_names = F, col_types = cols_only(X1 = "T", X2 = "d", X3 = "d")) %>% 
    select(1:3) %>%
    drop_na() %>%
    mutate("Location" = loc, "ID" = NA)
  
  ### Rename columns and fix time zone designation
  names(df) <- c("DateTimeUTC","Logger_psi", "Logger_temp_f","Location", "ID")
  
  ### Round to 2 decimals
  df[,2] <- round(df[,2], digits = 4)
  df[,3] <- round(df[,3], digits = 2)
  
  ### TIMEZONE TROUBLESHOOTING - RESOLVED BY USING UTC DATA OUTPUT AND WRITING DATA TO DB WITH UTC TIMEZONE
  # ### Force the Data time zone (Outputs in local time)
  # df$DateTimeUTC <-  force_tz(df$DateTimeUTC, tzone = "America/New_York")
  # ### Convert to UTC to eliminate gaps and duplicates due to DST
  # df$DateTimeUTC <- with_tz(df$DateTimeUTC, "UTC")
  ### Get appropriate barometric file based on location
  
  if (userlocation == "Wachusett") {
    if(loc %in% c("FHLN", "FPRN", "HLNW", "PRNW")){
      baro <- "FPRN"
    } else {
      baro <- c("MD02", "MD83")
    }
  } else { ### Quabbin
    baro <- "213"
  }  
  
  ### Connect to db # 2  ## IMPORTANT - timezone set as UTC
  ### CONNECT TO A FRONT-END DATABASE ####
  ### Set DB
  schema <- userlocation
  dsn <- 'DCR_DWSP_App_R'
  database <- "DCR_DWSP"
  tz <- 'UTC'
  con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)
  
  baro_tbl <- tbl(con, Id(schema = schema, table = baro_table))
  

  ### Filter barometric records to the appropriate barometer and time period needed
  df_baro <- baro_tbl %>%
    filter(Location %in% baro,
           DateTimeUTC >= df$DateTimeUTC) %>%
    drop_na(Logger_psi) %>% 
    collect()
  
  ### Fix time zone designation
  # df_baro$DateTimeUTC <-  force_tz(df_baro$DateTimeUTC, tzone = "UTC")
  # df_baro$DateTimeUTC <- with_tz(df_baro$DateTimeUTC, tzone = "America/New_York")
  
  ### Get existing data to check for duplicates
  if(loc == "SYW177"){
    hobo_tbl <- "tbl_HOBO_WELLS"
  }
  
  hobo_existing <- dbGetQuery(con, glue("SELECT * FROM [{schema}].[{hobo_tbl}] WHERE 
                                  [Location] = '{loc}'"))


  ### Check for duplicate existing data in database
  duplicates <- semi_join(df, hobo_existing, by="DateTimeUTC")
  
  if (nrow(duplicates) > 0){
    stop(paste0("This file duplicates ",nrow(duplicates)," existing ",loc," HOBO records in the database."))
  }
  
  ### Join the barometric data for each record
  df2 <- left_join(df,df_baro[,3:4], by = "DateTimeUTC") %>%
    dplyr::rename("Logger_psi_baro" = Logger_psi.y,"Logger_psi" = Logger_psi.x)
  ### Find the last time-stamp
  end_time <- max(df2$DateTimeUTC)
  
  missing_baro_times <- df2$DateTimeUTC[which(is.na(df2$Logger_psi_baro))]
  print(glue("The following HOBO times (UTC) have no matching barometer data for compensation: {missing_baro_times}"))
  
  if(any(is.na(df2$Logger_psi_baro))){
    stop("It looks like there is missing barometric compensation data! This needs to be provided or else the discharge calculations will fail.\nSee log file for missing times")
  }
  ### Calculate raw stage
  df2$raw_stage <- NA ### create empty column
  df2$raw_stage <- (df2$Logger_psi - df2$Logger_psi_baro) / 0.43352750192825
  
  ### get the last raw stage value using the end time
  last_stage <- df2$raw_stage[df2$DateTimeUTC == end_time]
  
  ### Calculate the stage offset to be applied to each raw stage (stage is a function argument)
  if(loc == "SYW177"){
    offset <-  26.90625 - stage -  last_stage
  } else {
    offset <- stage - last_stage
  }
  ### Calculate the final stage using the offset
  df2$Stage_ft <- round(df2$raw_stage + offset, digits = 3)
  
  ### Source the function to calculate discharges
  # source("HOBO_calcQ.R")
  if(loc == "SYW177"){
    ### Well depth from casing top to bottom = 26.90625, stick-up height (2.20 + WLM calibration adjustment) = 2.00 ft 
    df_HOBO <- df2 %>% 
      mutate("RatingFlag" = NA,
             "Discharge_cfs" = NA,
             "Water_Level_ft" = round(26.90625 - 2.20 - Stage_ft, 2)) # Water level below ground surface
    print(head(hobo_tbl))
  } else {
    ### Calcualte all discharges and save df2 to a new df with discharge info
    df_HOBO <- HOBOcalcQ(schema = schema, loc = loc, df_HOBO = df2)
  }

  ### Set record IDs
  setIDs <- function(){
    qry <- dbGetQuery(con, glue("SELECT max(ID) FROM [{schema}].[{hobo_tbl}]"))
    ### Get current max ID
    if(is.na(qry)) {
      qry <- 0
    } else {
      qry <- qry
    }
    ID_max <- as.numeric(unlist(qry))
    rm(qry)
    
    ### Set IDs
    df_HOBO$ID <- seq.int(nrow(df_HOBO)) + ID_max
  }
  df_HOBO$ID <- setIDs()
  
  ### Make a flag df if there are any discharge related flags (only above/below rating curve can be automatically calculated)
  setFlagIDs <- function(){
    if(all(is.na(df_HOBO$RatingFlag)) == FALSE){ # Condition returns FALSE if there is at least 1 non-NA value, if so proceed
      ### Split the flags into a separate df and assign new ID
      df_flags <- df_HOBO[,c("ID","RatingFlag")] %>%
        rename("SampleID" = ID, "FlagCode" = RatingFlag) %>%
        drop_na()
    } else {
      df_flags <- NA
    }
    
    if(!is.na(df_flags)){
      query.flags <- dbGetQuery(con, glue("SELECT max(ID) FROM [{schema}].[{ImportFlagTable}]"))
      # Get current max ID
      if(is.na(query.flags)) {
        query.flags <- 0
      } else {
        query.flags <- query.flags
      }
      ID.max.flags <- as.numeric(unlist(query.flags))
      rm(query.flags)
      
      ### ID flags
      df_flags$ID <- seq.int(nrow(df_flags)) + ID.max.flags
      df_flags$DataTableName <- hobo_tbl
      df_flags$DateFlagged <-  Sys.Date()
      df_flags$ImportStaff <-  username
      df_flags$Comment <- "Flags generated during data import"
      
      # Reorder df_flags columns to match the database table exactly # Add code to Skip if no df_flags
      df_flags <- df_flags[,c(3,4,1,2,5,6,7)]
    } else { # Condition TRUE - All FlagCodes are NA, thus no df_flags needed, assign NA
      df_flags <- NA
    } # End flags processing chunk
  } # End set flags function
  df_flags <- setFlagIDs()
  
  ### Reorder and select columns to match db
  col_order <- c(dbListFields(con, schema_name = schema, name = hobo_tbl), "Logger_temp_f")
  
  ### Grab last 1 days records to plot with new data to check for missed data corrections
  t <- min(df_HOBO$DateTimeUTC)
  
  hobo_tbl <- tbl(con, Id(schema = schema, table = hobo_tbl))

  # hobo_prior$DateTimeUTC <-  force_tz(hobo_prior$DateTimeUTC, tzone = "UTC") 
  hobo_prior <- hobo_tbl %>% 
    filter(Location == loc,
           DateTimeUTC >= (t - 86400), DateTimeUTC < t) %>% 
    arrange(DateTimeUTC) %>% 
    collect()
  
  if(nrow(hobo_prior) > 0){
    ### Convert C to F
    hobo_prior$Logger_temp_f <- hobo_prior$Logger_temp_c * 1.8 + 32 
  } else {
    hobo_prior <- NULL
  }
  # hobo_prior$DateTimeUTC <- with_tz(hobo_prior$DateTimeUTC, tzone = "America/New_York")
  
  ### Disconnect from db and remove connection obj
  dbDisconnect(con) #3
  rm(con)
  
  ### Convert F to C
  df_HOBO$Logger_temp_c <- round((df_HOBO$Logger_temp_f - 32) * 0.5556, digits = 2)
  
  if (userlocation == "Quabbin") { ### Add extra fields
    df_HOBO <- df_HOBO %>% 
      mutate(Weir_ft = NA_real_, Baro_psi = NA_real_, ImportDate = today())
  }
  
  df_HOBO <-  df_HOBO[, col_order]
  
  # print(tail(hobo_prior, n = 10))
  
  # print(glue("Structure: {str(df_HOBO)}"))
  # print(glue("Class: {class(df_HOBO)}"))
  # print(glue("Number of Rows: {nrow(df_HOBO)}"))
  # 
  # print(head(df_HOBO, n = 10))
  # print(head(df_flags, n = 10))
  
  df_HOBO <- dplyr::select(df_HOBO, -Logger_temp_f)
  
  if (userlocation == "Wachusett") { ### Quabbin has no manual stage measurements to get 
  ### Connect to db  #4 ## IMPORTANT - timezone set as UTC

    ### Set DB Connection
    schema <- userlocation
    dsn <- 'DCR_DWSP_App_R'
    database <- "DCR_DWSP"
    tz <- 'America/New_York'
    con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)

  ### Bring in stage and temperature manual measurements and filter to the date range of the HOBO data being imported
    df_stage  <- dbGetQuery(con, glue("SELECT [Location], [DateTimeET], [Parameter], [FinalResult] 
                                  FROM [{schema}].[tblTribFieldParameters] WHERE [Parameter] = 'Staff Gauge Height'
                                  AND [Location] = '{loc}'"))
      
    df_stage <- df_stage %>% 
      filter(DateTimeET >= min(df_HOBO$DateTimeUTC), ### Note - tzs are comparable since stage data is converted to UTC when read into R
             DateTimeET <= max(df_HOBO$DateTimeUTC)
             )
    
    
    df_temp  <- dbGetQuery(con, glue("SELECT [Location], [DateTimeET], [Parameter], [FinalResult] 
                                  FROM [{schema}].[tblTribFieldParameters] WHERE [Parameter] = 'Water Temperature'
                                  AND [Location] = '{loc}'"))
    
    df_temp <- df_temp %>% 
      filter(DateTimeET >= min(df_HOBO$DateTimeUTC), ### Note - tzs are comparable since stage data is converted to UTC when read into R
             DateTimeET <= max(df_HOBO$DateTimeUTC)
      )
    
    
  } else {
    df_stage <-  NULL ### When Quabbin enters manual stage readings, the table name needs to replace NULL
    df_temp <- NULL ### When Quabbin enters manual temperature readings, the table name needs to replace NULL
    df_conductivity <- NULL 
  }
  
  if (userlocation == "Wachusett") {
  
  dbDisconnect(con) #4
  rm(con)
  
  dfs <- list(
    "df" = df_HOBO,
    "df_flag" = df_flags,
    "df_prior" = hobo_prior,
    "df_stage" = df_stage,
    "df_temp" = df_temp,
    "df_conductivity" = NULL)
  
  } else {
    dfs <- list(
      "df" = df_HOBO,
      "df_flag" = df_flags,
      "df_prior" = hobo_prior,
      "df_stage" = NULL,
      "df_temp" = NULL,
      "df_conductivity" = NULL)
  }
  
  print(paste0("HOBO Data finished processing at ", Sys.time()))
  
  return(dfs)
} ### End function

# df_hobo <- df_HOBO
# var2 = "Discharge"
# dfs <- PROCESS_HOBO(hobo_txt_file, stage, username,userlocation="Wachusett")
# df_hobo <- dfs[[1]]
# df_flags <- dfs[[2]]
# df_prior <- dfs[[3]]
# df_stage <- dfs[[4]]
# df_temp <- dfs[[5]]
# ###
### _____________________________________________________________________________________
###

PREVIEW_HOBO <- function(df_hobo, df_prior = NULL, df_stage = NULL, df_temp = NULL, var2 = NULL){
  
  pd <- df_hobo
  loc <- unlist(pd[1, "Location"])
  ### cols is used as the ordering of data in the plot legend
  cols <- c("Stage (ft)" = "darkgreen", #cols[1]
            "Stage (ft) - prior" = "darkseagreen4", #cols[2]
            "Stage (ft) - manual" = "darkorange2", #cols[3]
            "Water Temperature (C)" = "purple4", #cols[4]
            "Water Temperature (C) - prior" = "orchid4", #cols[5]
            "Water Temperature (C) - manual" = "magenta", #cols[6]
            "Discharge (cfs)" = "blue4",  #cols[7]
            "Discharge (cfs) - prior" = "steelblue", #cols[8]
            "Groundwater level (ft below ground surface)" = "blue3", #cols[9]
            "Groundwater level (ft below ground surface) - prior" = "blue4" #cols[10]
  )
  ### Create empty vectors that will be filled based on the parameters on each plot
  cols_legend <- NULL
  linetype_legend <- NULL
  shape_legend <- NULL
  
  if(is.null(df_prior)){
    prior <-  FALSE
  } else {
    prior <-  TRUE
  }
  
  if(loc == "SYW177"){
    title <- paste0("Groundwater Level and Temperature from HOBO\n At Location ", loc)
    y1lab <- "Groundwater Level (ft below ground surface)"
    y1lim <- max(pd$Water_Level_ft)
    y1data <- pd$Water_Level_ft
    y1prior <- df_prior$Water_Level_ft
    y1color <- "Groundwater level (ft below ground surface)"
    y1prior_col <- "Groundwater level (ft below ground surface) - prior"
    y2data <- pd$Logger_temp_c
    y2col <- "Water Temperature (C)"
    y2prior <- df_prior$Logger_temp_c
    y2prior_col <- "Water Temperature (C) - prior"
    y2lim <- max(pd$Logger_temp_c)
    y2lab <- "Water Temperature (C)"
    ### Add legend items with colors for data being added to plot in this step
    cols_legend <- append(cols_legend,c(cols[9],cols[4]))
    ### Add the linetype data being added to plot in this step (solid for line, NA for points)
    linetype_legend <- append(linetype_legend,c("Groundwater level (ft below ground surface)" = "solid",
                                                "Water Temperature (C)" = "solid"))
    ### Add the shape of the point being added to the plot in this step (NA for lines, 19 for points)
    shape_legend <- append(shape_legend,c("Groundwater level (ft below ground surface)" = NA,
                                          "Water Temperature (C)" = NA))
    
  } else {
    y1lab <- "Stage (ft)"
    if(nrow(df_stage) > 0){
                 y1lim <- max(c(pd$Stage_ft,df_stage$FinalResult))
                    } else {y1lim <- max(pd$Stage_ft)}
    y1data <- pd$Stage_ft
    y1prior <- df_prior$Stage_ft
    y1prior_col <- "Stage (ft) - prior"
    y1color <- "Stage (ft)"
    if(var2 == "Temperature"){
      title <- paste0("Stage and Water Temperature at Location ", loc)
      y2data <- pd$Logger_temp_c
      y2prior <- df_prior$Logger_temp_c
      y2col <- "Water Temperature (C)"
      if(!is.null(df_temp) && nrow(df_temp) > 0){
              y2lim <- max(c(pd$Logger_temp_c,df_temp$FinalResult)) 
                    } else {y2lim <- max(pd$Logger_temp_c)}
      y2prior_col <- "Water Temperature (C) - prior"
      y2lab <- "Water Temperature (C)"
      ### Add legend items with colors for data being added to plot in this step
      cols_legend <- append(cols_legend,c(cols[1],cols[4]))
      ### Add the linetype data being added to plot in this step (solid for line, NA for points)
      linetype_legend <- append(linetype_legend,c("Stage (ft)" = "solid",
                                                  "Water Temperature (C)" = "solid"))
      ### Add the shape of the point being added to the plot in this step (NA for lines, 19 for points)
      shape_legend <- append(shape_legend,c("Stage (ft)" = NA,
                                            "Water Temperature (C)" = NA))  
      
    } else { # Var2 is discharge
      title <- paste0("Stage and Discharge at Location ", loc)
      y2data <- pd$Discharge_cfs
      y2prior <- df_prior$Discharge_cfs
      y2col <-  "Discharge (cfs)"
      y2lim <- max(pd$Discharge_cfs)
      y2prior_col <- "Discharge (cfs) - prior"
      y2lab <- "Discharge (cfs)"
      ### Add legend items with colors for data being added to plot in this step
      cols_legend <- append(cols_legend,c(cols[1],cols[7]))
      ### Add the linetype data being added to plot in this step (solid for line, NA for points)
      linetype_legend <- append(linetype_legend,c("Stage (ft)" = "solid",
                                                  "Discharge (cfs)" = "solid"))
      ### Add the shape of the point being added to the plot in this step (NA for lines, 19 for points)
      shape_legend <- append(shape_legend,c("Stage (ft)" = NA,
                                            "Discharge (cfs)" = NA)) 
    }
  }
  
  mult <- y1lim / abs(y2lim)
  
  plot  <- ggplot(pd, aes(x = DateTimeUTC)) +
    geom_line(aes(y = y1data, color = y1color), size = 1)  +
    geom_line(aes(y = y2data * mult, color = y2col), size = 1)
  
  # Check for prior data to plot 
  if(isTRUE(prior)){
    plot <- plot +
      geom_line(data = df_prior, aes(x = df_prior$DateTimeUTC, y = y1prior, color = y1prior_col), size = 1) +
      geom_line(data = df_prior, aes(x = df_prior$DateTimeUTC, y = y2prior * mult, color = y2prior_col), size = 1) +
      geom_vline(xintercept = min(pd$DateTimeUTC), color = "gray10", linetype = 2, size = 1.5, alpha = 0.8)

    ### Add legend items with colors for data being added to plot in this step
    if(loc == "SYW177"){
      ### Add legend items with colors for data being added to plot in this step
      cols_legend <- append(cols_legend,c(cols[10],cols[5]))
      ### Add the linetype data being added to plot in this step (solid for line, NA for points)
      linetype_legend <- append(linetype_legend,c("Groundwater level (ft below ground surface) - prior" = "solid",
                                                  "Water Temperature (C) - prior" = "solid"))
      ### Add the shape of the point being added to the plot in this step (NA for lines, 19 for points)
      shape_legend <- append(shape_legend,c("Groundwater level (ft below ground surface) - prior" = NA,
                                            "Water Temperature (C) - prior" = NA))

    } else {
      ### Add legend items with colors for data being added to plot in this step
      cols_legend <- append(cols_legend,c(cols[2]))
      ### Add the linetype data being added to plot in this step (solid for line, NA for points)
      linetype_legend <- append(linetype_legend,c("Stage (ft) - prior" = "solid"))
      ### Add the shape of the point being added to the plot in this step (NA for lines, 19 for points)
      shape_legend <- append(shape_legend,c("Stage (ft) - prior" = NA))

    

    cols_legend <- append(cols_legend,
                          switch(var2,
                                 "Temperature" = c(cols[5]),
                                 "Discharge" = c(cols[8])))
    ### Add the linetype data being added to plot in this step (solid for line, NA for points)
    linetype_legend <- append(linetype_legend,
                              switch(var2,
                                     "Temperature" = c("Water Temperature (C) - prior" = "solid"),
                                     "Discharge" = c("Discharge (cfs) - prior" = "solid")))
    ### Add the shape of the point being added to the plot in this step (NA for lines, 19 for points)
    shape_legend <- append(shape_legend,
                           switch(var2,
                                  "Temperature" = c("Water Temperature (C) - prior" = NA),
                                  "Discharge" = c("Discharge (cfs) - prior" = NA)))
}
}
  
  if(!is.null(df_stage) && nrow(df_stage) > 0) {
    plot <- plot +
      geom_point(data = df_stage, aes(x = DateTimeET, y = FinalResult, color = "Stage (ft) - manual"), size = 2)

    ### Add legend items with colors for data being added to plot in this step
    cols_legend <- append(cols_legend,c(cols[3]))
    ### Add the linetype data being added to plot in this step (solid for line, NA for points)
    linetype_legend <- append(linetype_legend,c("Stage (ft) - manual" = "blank"))
    ### Add the shape of the point being added to the plot in this step (NA for lines, 19 for points)
    shape_legend <- append(shape_legend,c("Stage (ft) - manual" = 19))
  }
  
  if(!is.null(df_temp) && nrow(df_temp) > 0 && var2=="Temperature") {
    plot <- plot +
      geom_point(data = df_temp, aes(x = DateTimeET, y = FinalResult*mult, color = "Water Temperature (C) - manual"), size = 2)
    
    ### Add legend items with colors for data being added to plot in this step
    cols_legend <- append(cols_legend,c(cols[6]))
    ### Add the linetype data being added to plot in this step (solid for line, NA for points)
    linetype_legend <- append(linetype_legend,c("Water Temperature (C) - manual" = "blank"))
    ### Add the shape of the point being added to the plot in this step (NA for lines, 19 for points)
    shape_legend <- append(shape_legend,c("Water Temperature (C) - manual" = 19))
    }

  if(loc == "SYW177"){
    plot <- plot +
      scale_y_continuous(breaks = pretty_breaks(), limits = c(1.2 * y1lim, NA), trans = scales::reverse_trans(),
                         sec.axis = sec_axis(trans = ~./mult,breaks = pretty_breaks(), name = y2lab))
  } else {
    plot <- plot +
      scale_y_continuous(breaks = pretty_breaks(), limits = c(NA, 1.2 * y1lim),
                         sec.axis = sec_axis(trans = ~./mult, breaks = pretty_breaks(), name = y2lab)) 
  }
  
  plot <- plot + 
    scale_x_datetime(breaks = pretty_breaks(n=12)) + 
    scale_colour_manual(values = cols_legend[order(factor(names(cols_legend),levels = names(cols)))], #orders legend items and colors based on cols order
                        guide = guide_legend(override.aes = list(
                          linetype = linetype_legend[order(factor(names(linetype_legend),levels = names(cols)))], #orders legend linetypes based on cols order
                          shape = shape_legend[order(factor(names(shape_legend),levels = names(cols)))]))) + #orders legend point shapes based on cols order
    labs(y = y1lab,
         x = "Date",
         colour = "") +
    ggtitle(title) +
    theme_linedraw() +
    theme(plot.title = element_text(color= "black", face="bold", size=14, vjust = 1, hjust = 0.5),
          legend.position = "bottom",
          legend.text = element_text(margin = margin(r=0.8, unit="cm")),
          axis.title.x = element_text(angle = 0, face = "bold", color = "black"),
          axis.title.y = element_text(angle = 90, face = "bold", color = "black"))
  
  # plot
  return(plot)

}
# plot <- PREVIEW_HOBO(df_hobo = df_hobo, df_prior = NULL, var2 = "Discharge")
# plot
# Comment out if running in shiny
# df_HOBO <- PROCESS_HOBO(hobo_txt_file = hobo_file, stage = stage)

###
### _____________________________________________________________________________________
###

IMPORT_HOBO <- function(df_hobo, df_flags, hobo_txt_file, userlocation){
  print(paste0("HOBO Data started importing at ", Sys.time()))
  loc <- str_split_fixed(hobo_txt_file, "_", n = 2) 
  loc <- loc[,1]
  file <- paste0(updir,"/", hobo_txt_file)
  hobo_file <- str_replace(hobo_txt_file, "txt", "hobo")
  if(loc == "SYW177"){
    hobo_tbl <- "tbl_HOBO_WELLS"
  }
  ### Import the data to the database
  schema <- "Wachusett"
  dsn <- 'DCR_DWSP_App_R'
  database <- "DCR_DWSP"
  tz <- 'UTC'
  con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)
  ### Write data
  odbc::dbWriteTable(con, DBI::SQL(glue("{database}.{schema}.{hobo_tbl}")), value = df_hobo, append = TRUE)

  # Flag data
  if ("data.frame" %in% class(df_flags)){ # Check and make sure there is flag data to import
    print("Importing flags...")
    odbc::dbWriteTable(con, DBI::SQL(glue("{database}.{schema}.{ImportFlagTable}")), value = df_flags, append = TRUE)
  }
  # Disconnect from db and remove connection obj
  dbDisconnect(con)
  rm(con)
  
  ### Move the processed raw data file and hobo file to the appropriate processed folder
  dir_num <- as.numeric(which(!is.na(str_match(list.dirs(hobo_path, recursive = T, full.names = T), loc))))
  subdir <- list.dirs(hobo_path, recursive = T, full.names = T)[dir_num]
  
  file.rename(file, paste0(subdir, "/", hobo_txt_file))
  file.rename(paste0(updir,"/", hobo_file), paste0(subdir, "/", hobo_file))
  
  SendEmail(df=df_hobo, table=hobo_tbl, file=hobo_file, emaillist=emaillist, username=username, userlocation=userlocation)
  
  print(paste0("HOBO Data finished importing at ", Sys.time()))
  return("Import Successful")
}




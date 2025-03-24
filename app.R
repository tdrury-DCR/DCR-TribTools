###############################  HEADER  ######################################
#  TITLE: app.r
#  DESCRIPTION: Shiny web app to generate rating curves
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: 2020-12-30
#  GIT REPO: 
#  R version 3.5.3 (2019-03-11)  x86_64
#  File Dependencies: Hobo_Rating_Configs.csv, LaunchTribTOOLS.R
##############################################################################.

### edit r environmental variables to avoid hardcoding UN and PW
# Uncomment the line below to add ARCGIS_USER and ARCGIS_PASSWORD to your system variable list
# usethis::edit_r_environ()
# Use this format: Variable = Value
# Note: The above text do not have quotes, however when you use them below in the token, you do use quotes.

### LOAD CONFIGS FOR WAVE-WIT APPS ####
username <- Sys.getenv('USERNAME')
eq_team_root <- paste0('C:/Users/',username,'/Commonwealth of Massachusetts/DCR-TEAMS-DWSPEQ - Documents/')

# Function to load packages, and install if missing
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages(lib.loc = r_lib)[, "Package"])]
  if (length(new.pkg))
      install.packages(new.pkg, lib = r_lib, dependencies = TRUE, repos = "https://cloud.r-project.org")
  si <- sessionInfo()
  loaded_pkgs <- names(si[["otherPkgs"]])
  pkg <- pkg[!pkg %in% loaded_pkgs]  
  sapply(pkg, require, character.only = TRUE)
}
### Package List ####
### NOTE - Shiny must be installed and loaded in the LaunchAppGitHub.R script - any other packages requred should be listed below

packages <- c("DBI", "odbc","shiny","shinyjs", "tidyverse", "lubridate", "DT", "naniar", "shinyWidgets", "magrittr", "xts", "shinyTime",
              "plotly",  "scales", "stringr", "shinythemes", "nlstools", "readxl", "shinycssloaders", "glue", "RDCOMClient", "dygraphs", 
              "pool", "logging")
ipak(packages) 

substrRight <<- function(x, n){
  substr(x, nchar(x) - n + 1, nchar(x))
}

# basicConfig()

# options(shiny.error = function() {
#   logging::logerror(sys.calls() %>% as.character %>% paste(collapse = ", ")) })
  
# options(shiny.error = browser)
# options(shiny.error = recover)

### Set environment timezone
# Sys.setenv(TZ='UTC')
### Set Location Dependent Variables - datatsets and distro

if (userlocation == "Wachusett") {
  rootdir <- wach_team_root
} else {
  rootdir <- quab_team_root
}

#Set user info
user <-  Sys.getenv("USERNAME") %>% toupper()
userdata <- readxl::read_xlsx(path = paste0(eq_team_root, config[["Users"]]))
userdata <- userdata[toupper(userdata$Username) %in% user,]
username <<- paste(userdata[2], userdata[1], sep = " ")
userlocation <<- paste0(userdata[6])

if (userlocation == "Wachusett") { ### WACHUSETT ####
  schema <- "Wachusett"
  ### Connect to the DWSP database in SQL Server

  dsn <- 'DCR_DWSP_App_R'
  database <- "DCR_DWSP"
  tz <- 'America/New_York'
  tz_out <- tz
  con <<- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz, timezone_out =  tz_out)

    ### DISCHARGE CALCULATOR Args
  location_table <- config[["LocationsTable"]]
  df_locs <- dbReadTable(con, Id(schema = schema, table = location_table))
  
  ### RATING TOOL Function Args
  measurement_data <- config[["DischargeTable"]] ### Set the table name with discharges
  rating_data <- config[["RatingsTable"]] ### Get the rating information
  df_discharges <- dbReadTable(con, Id(schema = schema, table = measurement_data))
  df_ratings <- dbReadTable(con, Id(schema = schema, table = rating_data))

  df_trib_monitoring <- tbl(con, Id(schema = schema, table = "tblTributaryFieldNotes"))
  df_trib_monitoring <- df_trib_monitoring %>%
    select(-"Edit_timestamp") %>%
    collect() 
  
  df_trib_monitoring <- df_trib_monitoring %>%
    dplyr::arrange(desc(FieldObsDate))

  db_fp <- tbl(con, Id(schema = schema, table = "tblTribFieldParameters"))
  df_fp <- db_fp %>%
    filter(DateTimeET > as_date("2024-01-01"),
           Parameter %in% c("Staff Gauge Height", "Specific Conductance")) %>%
    select(3:7) %>%
    collect()

  dbDisconnect(con)
  rm(con)

  # Then convert time tz and the format into UTC
  df_fp <- df_fp |> 
    mutate("DateTimeUTC" = with_tz(df_fp$DateTimeET, tz = "UTC"))
  
  ### HOBO TOOL Function Args
  hobo_path <<- paste0(rootdir, config[["HOBO_Imported"]])
  updir <<- paste0(rootdir, config[["HOBO_Staging"]])
  hobo_db <<- "DCR_DWSP"
  baro_table <<- config[["HOBO_BARO"]]
  hobo_table <<- config[["HOBO"]]
  ImportFlagTable <<- config[["HydroFlagIndex"]]
  wave_db <<- config[["DB_Access"]]
  mayfly_data_dir <<- paste0(rootdir, config[["Mayfly_Staging"]])
  mayfly_data_processed <<- paste0(rootdir, config[["Mayfly_Imported"]])
  mayfly_table <<- config[["Mayfly Table"]]
  emaillist <<- config[["Email_List"]]
  
  ### Source Modules and functions
  source("fun_mod_hobos.R")
  source("mod_ratings.R")
  source("Ratings.R")
  source("mod_hobos.R")
  source("ProcessHOBO.R")
  source("ProcessMayflyData.R")
  source("outlook_email.R")
  source("mod_mayfly_correct.R")
  source("fun_mayfly_correct.R")
  source("HOBO_calcQ.R")
  source("mod_dischargecalc.R")
  ### UI  ####
  ### font-family: 'Lobster', cursive;
  # shinythemes::themeSelector(),
  ui <-  navbarPage(
    "DCR-DWSP TRIB TOOLS",
    tabPanel("HOBO/MAYFLY",
             fluidPage(theme = shinytheme("united"),
                       h1("Tributary Sensor Data Tool"),
                       HOBO_UI("mod_hobos"))
    ),    tabPanel("RATINGS",
             fluidPage(theme = shinytheme("united"),
                       h1("Tributary Rating Tool"),
                       RATINGS_UI("mod_ratings"))
    ),
    tabPanel("MAYFLY DATA CORRECTION",
             fluidPage(theme = shinytheme("united"),
                       h1("Mayfly Data Correction Tools"),
                       actionButton("refresh", "REFRESH"),
                       br(),
                       MF_CORRECT_UI("mod_mayfly_correct"))
    ),
    tabPanel("DISCHARGE CALCULATOR",
             fluidPage(theme = shinytheme("united"),
                       h1("Manual Discharge Calculator"),
                       DISCHARGECALC_UI("mod_dischargecalc"))
    )
    
  ) ### END UI ####
  
  ### SERVER  ####
  server <- function(input, output, session) {
    ### Setup parameters
    options(scipen = 999)
    callModule(RATINGS, "mod_ratings", df_discharges = df_discharges, df_ratings = df_ratings)
    callModule(HOBO, "mod_hobos", hobo_path = hobo_path, updir = updir, hobo_db = hobo_db, df_trib_monitoring = df_trib_monitoring,
               baro_table = baro_table, hobo_table = hobo_table, mayfly_data_dir = mayfly_data_dir,
               mayfly_data_processed = mayfly_data_processed, ImportFlagTable = ImportFlagTable, username = username, userlocation = userlocation)
    callModule(MF_CORRECT, "mod_mayfly_correct", df_fp = df_fp, 
               df_trib_monitoring = df_trib_monitoring, username, userlocation)
    callModule(DISCHARGE, "mod_dischargecalc", df_ratings = df_ratings, df_locs = df_locs, userlocation)
    # Stop app when browser session window closes
    session$onSessionEnded(function() {
      stopApp()
    })    
    
  } # End Server 
  
} else { ### QUABBIN ####
 
  ### Connect to the DWSP database in SQL Server 
  database <- config[["HOBO_DB"]]
  ### Connect to Database #1
  ### Connect to the DWSP database in SQL Server
  schema <- "Quabbin"
  dsn <- 'DCR_DWSP_App_R'
  database <- "DCR_DWSP"
  tz <- 'UTC'
  con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)
  
  ### DISCHARGE CALCULATOR Args
  location_table <- config[["LocationsTable"]]
  df_locs <- dbReadTable(con, Id(schema = schema, table = location_table))
  
  ### RATING TOOL Function Args
  measurement_data <- config[["DischargeTable"]] ### Set the table name with discharges
  rating_data <- config[["RatingsTable"]] ### Get the rating information
  df_discharges <- dbReadTable(con, Id(schema = schema, table = measurement_data))
  df_ratings <- dbReadTable(con, Id(schema = schema, table = rating_data))
  db_fp <- tbl(con, Id(schema = schema, table = "tblTribFieldParameters"))
  df_fp <- db_fp %>%
    filter(Parameter %in% c("Staff Gauge Height", "Specific Conductance")) %>%
    select(2:6) %>%
    rename(Location = Site) %>%
    collect()

  df_trib_monitoring <- tbl(con, Id(schema = schema, table = "tblMayflyMaintenance"))
  df_trib_monitoring <- df_trib_monitoring %>%
    select(-"Edit_timestamp") %>%
    collect() 
  
  df_trib_monitoring <- df_trib_monitoring %>%
    rename(DateTimeUTC = DateTimeET) %>%
    mutate(DateTimeUTC = with_tz(force_tz(DateTimeUTC, tzone="America/New_York"), tzone="UTC"),
           Mayfly_DownloadTimeUTC = DateTimeUTC) %>%
    dplyr::arrange(desc(DateTimeUTC))
  
  dbDisconnect(con)
  rm(con)
  
  # First force the tz attribute to reflect the timezone that the data appears in
  df_fp$DateTimeET <- force_tz(df_fp$DateTimeET, tz = "America/New_York")
  # Then convert time tz and the format into UTC
  df_fp$DateTimeET <- with_tz(df_fp$DateTimeET, tz = "UTC")
  df_fp <- df_fp %>%
    dplyr::rename(DateTimeUTC = DateTimeET)
  
  ### HOBO TOOL Function Args
  hobo_path <<- paste0(rootdir, config[["HOBO_Imported"]])
  updir <<- paste0(rootdir, config[["HOBO_Staging"]])
  hobo_db <<- "DCR_DWSP"
  baro_table <<- config[["HOBO_BARO"]]
  hobo_table <<- config[["HOBO"]]
  ImportFlagTable <<- config[["HydroFlagIndex"]]
  wave_db <<- config[["DB_Access"]]
  mayfly_data_dir <<- paste0(rootdir, config[["Mayfly_Staging"]])
  mayfly_data_processed <<- paste0(rootdir, config[["Mayfly_Imported"]])
  mayfly_table <<- config[["Mayfly Table"]]
  emaillist <<- config[["Email_List"]]
  
  ### Source Modules and functions
  source("mod_ratings.R")
  source("Ratings.R")
  source("mod_hobos.R")
  source("ProcessHOBO.R")
  source("outlook_email.R")
  source("HOBO_calcQ.R")
  source("mod_dischargecalc.R")
  source("fun_mod_hobos.R")
  source("ProcessMayflyData.R")
  source("mod_mayfly_correct.R")
  source("fun_mayfly_correct.R")
  source("mod_dischargecalc.R")
  # source("ProcessMayflyData.R")
  
  ui <-  navbarPage(
    "DCR-DWSP TRIB TOOLS",
    tabPanel("MAYFLY/HOBO",
             fluidPage(theme = shinytheme("united"),
                       h1("Tributary Sensor Data Tool"),
                       HOBO_UI("mod_hobos"))
    ),    tabPanel("RATINGS",
                   fluidPage(theme = shinytheme("united"),
                             h1("Tributary Rating Tool"),
                             RATINGS_UI("mod_ratings"))
    ),
    tabPanel("MAYFLY DATA CORRECTION",
             fluidPage(theme = shinytheme("united"),
                       h1("Mayfly Data Correction Tools"),
                       actionButton("refresh", "REFRESH"),
                       br(),
                       MF_CORRECT_UI("mod_mayfly_correct"))
    ),
    tabPanel("DISCHARGE CALCULATOR",
             fluidPage(theme = shinytheme("united"),
                       h1("Manual Discharge Calculator"),
                       DISCHARGECALC_UI("mod_dischargecalc"))
    )
    
    
  ) ### END UI ####
  
  ### SERVER  ####
  server <- function(input, output, session) {
    ### Setup parameters
    options(scipen = 999)
    callModule(RATINGS, "mod_ratings", df_discharges = df_discharges, df_ratings = df_ratings)
    callModule(HOBO, "mod_hobos", hobo_path = hobo_path, updir = updir, hobo_db = hobo_db, df_trib_monitoring = df_trib_monitoring,
               baro_table = baro_table, hobo_table = hobo_table, mayfly_data_dir = mayfly_data_dir,
               mayfly_data_processed = mayfly_data_processed, ImportFlagTable = ImportFlagTable, username = username, userlocation = userlocation)
    callModule(MF_CORRECT, "mod_mayfly_correct", df_fp = df_fp, 
               df_trib_monitoring = df_trib_monitoring, username, userlocation)
    callModule(DISCHARGE, "mod_dischargecalc", df_ratings = df_ratings, df_locs = df_locs, userlocation)
    # Stop app when browser session window closes
    session$onSessionEnded(function() {
      stopApp()
    })    
    
  } # End Server 
  
}

# combines the user interface and server
shinyApp(ui = ui, server = server)

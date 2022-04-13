###############################  HEADER  ######################################
#  TITLE: app.r
#  DESCRIPTION: Shiny web app to generate rating curves
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: 2020-12-30
#  GIT REPO: 
#  R version 3.5.3 (2019-03-11)  x86_64
#  File Dependencies: Hobo_Rating_Configs.csv, LaunchTribTOOLS.R
##############################################################################.

### UI #### 

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos="https://cloud.r-project.org", quiet = T, verbose = F)
  sapply(pkg, require, character.only = TRUE)
}
### Package List ####
### NOTE - Shiny must be installed and loaded in the LaunchAppGitHub.R script - any other packages requred should be listed below

packages <- c("DBI", "odbc","shiny","shinyjs", "tidyverse", "lubridate", "DT", "naniar",
              "plotly",  "scales", "stringr", "shinythemes", "nlstools", "readxl", "shinycssloaders", "glue", "RDCOMClient")
ipak(packages) 

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
userdata <- readxl::read_xlsx(path = paste0(user_root, config[["Users"]]))
userdata <- userdata[toupper(userdata$Username) %in% user,]
username <<- paste(userdata[2], userdata[1], sep = " ")
userlocation <<- paste0(userdata[6])

if (userlocation == "Wachusett") { ### WACHUSETT ####
schema <- "Wachusett"
### Connect to the DWSP database in SQL Server

dsn <- 'DCR_DWSP_App_R'
database <- "DCR_DWSP"
tz <- 'UTC'
con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)

  ### RATING TOOL Function Args
  measurement_data <- config[["DischargeTable"]] ### Set the table name with discharges
  rating_data <- config[["RatingsTable"]] ### Get the rating information
  df_discharges <<- dbReadTable(con, Id(schema = schema, table = measurement_data))
  df_ratings <<- dbReadTable(con, Id(schema = schema, table = rating_data))  
  df_trib_monitoring <<- dbReadTable(con, Id(schema = schema, table = "tblTributaryFieldNotes")) 
  
  dbDisconnect(con)
  rm(con)
  
  ### HOBO TOOL Function Args
  hobo_path <<- paste0(rootdir, config[["HOBO_Imported"]])
  updir <<- paste0(rootdir, config[["HOBO_Staging"]])
  hobo_db <<- "DCR_DWSP"
  baro_tbl <<- config[["HOBO_BARO"]]
  hobo_tbl <<- config[["HOBO"]]
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
  source("ProcessMayflyData.R")
  source("outlook_email.R")

### UI  ####
### font-family: 'Lobster', cursive;

    # shinythemes::themeSelector(),
  ui <-  navbarPage(
    "DCR-DWSP TRIB TOOLS",
    tabPanel("HOBO/MAYFLY",
             fluidPage(theme = shinytheme("united"),
                       h1("Tributary Sensor Data Tool"),
                       HOBO_UI("mod_hobos"))
    ),
    tabPanel("RATINGS",
             fluidPage(theme = shinytheme("united"),
                       h1("Tributary Rating Tool"),
                       RATINGS_UI("mod_ratings"))
    )
  ) ### END UI ####
  
  ### SERVER  ####
  server <- function(input, output, session) {
    callModule(RATINGS, "mod_ratings", df_discharges = df_discharges, df_ratings = df_ratings)
    callModule(HOBO, "mod_hobos", hobo_path = hobo_path, updir = updir, hobo_db = hobo_db,
               baro_tbl = baro_tbl, hobo_tbl = hobo_tbl, mayfly_data_dir = mayfly_data_dir,
               mayfly_data_processed = mayfly_data_processed, ImportFlagTable = ImportFlagTable, username = username, userlocation = userlocation)
    
    # Stop app when browser session window closes
    session$onSessionEnded(function() {
      stopApp()
    })    
    
  } # End Server 
  
} else { ### QUABBIN ####
 
  ### Connect to the DWSP database in SQL Server 
  # schema <- "Quabbin"
  # database <- "DCR_DWSP" 
  # con <- dbConnect(odbc::odbc(), database, timezone = 'UTC')
  
  database <- config[["HOBO_DB"]]
  ### Connect to Database #1
  ### Connect to the DWSP database in SQL Server
  schema <- "Quabbin"
  dsn <- 'DCR_DWSP_App_R'
  database <- "DCR_DWSP"
  tz <- 'UTC'
  con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)
  
  ### RATING TOOL Function Args
  measurement_data <- config[["DischargeTable"]] ### Set the table name with discharges
  rating_data <- config[["RatingsTable"]] ### Get the rating information
  df_discharges <- dbReadTable(con, Id(schema = schema, table = measurement_data))
  df_ratings <- dbReadTable(con, Id(schema = schema, table = rating_data))

  
  dbDisconnect(con)
  rm(con)
  
  ### HOBO TOOL Function Args
  hobo_path <<- paste0(rootdir, config[["HOBO_Imported"]])
  updir <<- paste0(rootdir, config[["HOBO_Staging"]])
  hobo_db <<- database # Same as rating info - all in Hydro DB
  baro_tbl <<- config[["HOBO_BARO"]]
  hobo_tbl <<- config[["HOBO"]]
  ImportFlagTable <<- config[["HydroFlagIndex"]]
  mayfly_data_dir <<- paste0(rootdir, config[["Mayfly_Staging"]])
  mayfly_data_processed <<- paste0(rootdir, config[["Mayfly_Imported"]])
  mayfly_table <<- config[["Mayfly Table"]]
  wave_db <<- config[["DB_Access"]]
  emaillist <<- NA # Edit after updating config file
  
  ### Source Modules and functions
  source("mod_ratings_q.R")
  source("Ratings.R")
  source("mod_hobos_q.R")
  source("ProcessHOBO.R")
  source("outlook_email.R")
  # source("ProcessMayflyData.R")
  
  ui <-  navbarPage(
    "DCR-DWSP TRIB TOOLS",
    tabPanel("HOBO",
             fluidPage(theme = shinytheme("united"),
                       h1("Tributary Sensor Data Tool"),
                       HOBO_UI("mod_hobos_q"))
    ),
    tabPanel("RATINGS",
             fluidPage(theme = shinytheme("united"),
                       h1("Tributary Rating Tool"),
                       RATINGS_UI("mod_ratings_q"))
    )
  ) ### END UI ####  
  
  ### SERVER  ####
  server <- function(input, output, session) {
    callModule(RATINGS, "mod_ratings_q", df_discharges = df_discharges, df_ratings = df_ratings)
    callModule(HOBO, "mod_hobos_q", hobo_path = hobo_path, updir = updir, hobo_db = hobo_db, 
               baro_tbl = baro_tbl, hobo_tbl = hobo_tbl, ImportFlagTable = ImportFlagTable, username = username, userlocation = userlocation)
    
    # Stop app when browser session window closes
    session$onSessionEnded(function() {
      stopApp()
    })    
  } # End Server 
  
}

# combines the user interface and server
shinyApp(ui = ui, server = server)


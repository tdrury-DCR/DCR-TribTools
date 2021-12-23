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
    install.packages(new.pkg, dependencies = TRUE, repos="http://cran.rstudio.com/", quiet = T, verbose = F)
  sapply(pkg, require, character.only = TRUE)
}
### Package List ####
### NOTE - Shiny must be installed and loaded in the LaunchAppGitHub.R script - any other packages requred should be listed below

packages <- c("DBI", "odbc","shiny","shinyjs", "tidyverse", "lubridate", "DT", "naniar",
              "plotly",  "scales", "stringr", "shinythemes", "nlstools", "readxl", "shinycssloaders", "glue", "RDCOMClient")
ipak(packages) 

### Set environment timezone
# Sys.setenv(TZ='UTC')

#Set user info
user <-  Sys.getenv("USERNAME") %>% toupper()
userdata <- readxl::read_xlsx(path = config[10])
userdata <- userdata[toupper(userdata$Username) %in% user,]
username <- paste(userdata[2], userdata[1], sep = " ")
userlocation <<- paste0(userdata[6])

if (userlocation == "Wachusett") { ### WACHUSETT ####
schema <- "Wachusett"
### Connect to the DWSP database in SQL Server

dsn <- 'DCR_DWSP_App_R'
database <- "DCR_DWSP"
tz <- 'UTC'
con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[18], timezone = tz)

  ### RATING TOOL Function Args
  measurement_data <- config[9] ### Set the table name with discharges
  rating_data <- config[8] ### Get the rating information
  df_discharges <<- dbReadTable(con, Id(schema = schema, table = measurement_data))
  df_ratings <<- dbReadTable(con, Id(schema = schema, table = rating_data))  
  
  dbDisconnect(con)
  rm(con)
  
  ### HOBO TOOL Function Args
  hobo_path <<- config[1]
  updir <<- config[2]
  hobo_db <<- "DCR_DWSP"
  baro_tbl <<- config[4]
  hobo_tbl <<- config[5]
  ImportFlagTable <<- config[6]
  wave_db <<- config[7]
  mayfly_data_dir <<- config[16]
  mayfly_data_processed <<- config[17]
  mayfly_table <<- "tblMayfly"
  emaillist <<- config[19]
  
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
  
  database <- config[3]
  ### Connect to Database #1
  ### Connect to the DWSP database in SQL Server
  schema <- "Quabbin"
  dsn <- 'DCR_DWSP_App_R'
  database <- "DCR_DWSP"
  tz <- 'UTC'
  con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[18], timezone = tz)
  
  ### RATING TOOL Function Args
  measurement_data <- config[9] ### Set the table name with discharges
  rating_data <- config[8] ### Get the rating information
  df_discharges <- dbReadTable(con, Id(schema = schema, table = measurement_data))
  df_ratings <- dbReadTable(con, Id(schema = schema, table = rating_data))

  
  dbDisconnect(con)
  rm(con)
  
  ### HOBO TOOL Function Args
  hobo_path <<- config[1]
  updir <<- config[2]
  hobo_db <<- database # Same as rating info - all in Hydro DB
  baro_tbl <<- config[4]
  hobo_tbl <<- config[5]
  ImportFlagTable <<- config[6]
  mayfly_data_dir <<- NULL
  mayfly_data_processed <<- config[17]
  mayfly_table <<- "tblMayfly"
  wave_db <<- config[7]
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


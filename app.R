###________________________________________________________________________________
#     Title: app.r
#     Description: Shiny web app to generate rating curves
#     Written by: Dan Crocker
#     Last Updated: October 30, 2018
#     File Dependencies: Hobo_Rating_Configs.csv
#
###________________________________________________________________________________

### UI #### 

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos="http://cran.rstudio.com/", quiet = T, verbose = F)
  sapply(pkg, require, character.only = TRUE)
}
### Package List ####
### NOTE - Shiny must be installed and loaded in the LaunchAppGitHub.R script - any other packages requred should be listed below

packages <- c("RODBC", "DBI", "odbc","shiny","shinyjs", "tidyverse", "lubridate", "DT",
              "plotly",  "scales", "stringr", "shinythemes", "nlstools", "readxl")
ipak(packages) 

config <- read_excel(paste0(getwd(),"/TribToolsConfig.xlsx"))
config <- as.character(config$ConfigValue)
### Set db with Discharge and Rating Data ####
db <- config[3]
### Connect to Database
con <- dbConnect(odbc::odbc(),
                 .connection_string = paste("driver={Microsoft Access Driver (*.mdb)}",
                                            paste0("DBQ=", db), "Uid=Admin;Pwd=;", sep = ";"),
                 timezone = "America/New_York")

### RATING TOOL Function Args
measurement_data <- config[9] ### Set the table name with discharges
rating_data <- config[8] ### Get the rating information
df_discharges <- dbReadTable(con, measurement_data)
df_ratings <- dbReadTable(con, rating_data)

dbDisconnect(con)
rm(con)

### HOBO TOOL Funtion Args
hobo_path <- config[1]
updir <- config[2]
hobo_db <- db # Same as rating info - all in Hydro DB
baro_tbl <- config[4]
hobo_tbl <- config[5]
ImportFlagTable <- config[6]

#Set user info
user <-  Sys.getenv("USERNAME")
userdata <- readxl::read_xlsx(path = config[10])
username <- paste(userdata$FirstName[userdata$Username %in% user],userdata$LastName[userdata$Username %in% user],sep = " ")

### Source Modules and functions
source("mod_ratings.R")
source("Ratings.R")
source("mod_hobos.R")
source("ProcessHOBO.R")

### UI  ####
### font-family: 'Lobster', cursive;

    # shinythemes::themeSelector(),
 ui <-  navbarPage(
   "DCR-DWSP RATING-HOBO TOOLS",
      tabPanel("HOBO",
        fluidPage(theme = shinytheme("united"),
          h1("Tributary HOBO Data Tool"),
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
               baro_tbl = baro_tbl, hobo_tbl = hobo_tbl, ImportFlagTable = ImportFlagTable, username = username)
    
# Stop app when browser session window closes
 session$onSessionEnded(function() {
  stopApp()
  })    
    
} # End Server    

# combines the user interface and server
shinyApp(ui = ui, server = server)


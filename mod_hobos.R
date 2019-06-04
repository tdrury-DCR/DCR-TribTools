###________________________________________________________________________________
#     Title: mod_hobos
#     Description: Shiny web app module to review and import hobo data to database
#     Written by: Dan Crocker
#     Last Updated: October 30, 2018
#
###________________________________________________________________________________

### UI ###

HOBO_UI <- function(id) {
  ns <- NS(id) # see General Note 1
  tagList(
    useShinyjs(),
      div(id = "hobos", ### Set div() for Refresh Button ####
        title = "Tributary HOBO Data Tool",
        fluidRow(
          column(12,
                 actionButton(ns("refresh"), "REFRESH", align = "left"),  ### Refresh button ####
                 br(),
                 textOutput(ns("intro")), ### Intro Text ####
                 br()
          )
        ),
        fluidRow(
          column(4,
                 strong(h4("1. SELECT A HOBO FILE:")),
                 uiOutput(ns("file.UI")), ### File UI ####
                 verbatimTextOutput(ns("file_selection")), ### File selected text ####
                 uiOutput(ns("stage.UI")) ### Stage UI ####
          ),
          column(4,
                 strong(h4("2. PROCESS/REVIEW HOBO DATA:")),
                 br(),
                 uiOutput(ns("process.UI")), ### Process Button ####
                 uiOutput(ns("text_process_status")) %>% withSpinner()
          ),
          column(4,
                 strong(h4("3. IMPORT HOBO DATA:")),
                 br(),
                 uiOutput(ns("import.UI")), ### Import Button ####
                 uiOutput(ns("text_import_status"))
          )
        ),
        fluidRow(### DATA TABSET PANELS
          column(12,
            tabsetPanel(
              tabPanel("DATA PLOT",
                       plotOutput(ns("plot"), width = "100%", height = "500px") %>% withSpinner(), 
                       uiOutput(ns("var2.UI"))### PLOT PAGE ####
              ),
              tabPanel("TABULAR HOBO DATA PREVIEW",  ### DATA PREVEIW PAGE ####
                       dataTableOutput(ns("table_data_preview"))
              ),
              tabPanel("TABULAR HOBO FLAG DATA PREVIEW", ### FLAG DATA PREVIEW PAGE ####
                       dataTableOutput(ns("table_flag_preview"))
              )
            )# End Tabset Panel
          ) # End Col
        ) # End FR
    ) # End Div
  ) # End Taglist
} # End UI  ####
            
################################.
###         SERVER          ####
################################.
        
HOBO <- function(input, output, session, hobo_path, updir, hobo_db, baro_tbl, hobo_tbl, ImportFlagTable, username){  # Same as rating info - all in Hydro DB

### Source the HOBO functions ####
  source("ProcessHOBO.R", local = T) ### source Script ####
    
  ns <- session$ns 
  
  ImportStatus <- reactiveVal("")
  
### INTRO TEXT #### 
  output$intro <- renderText({paste0("The HOBO DATA TOOL is designed to facilitate the processing and importing of HOBO Logger Data.\n 
                                       All raw text files exported from HOBO Ware should be checked over, corrections made with annotations prior to using this tool. \n
                                       Barometric files associated with water level files need to be imported first so that the compensation data is available to correct \n
                                       the water level data. All files successfully imported will be moved to an archive folder.")
  })

#### FILE SELECTION ####

# filter files to show only barometric files until there are none, and then show the other files     
# Make the File List
files <- reactive({
  all_files  <- list.files(updir, recursive = T, full.names = F, include.dirs = T, pattern = "^[^~$]+.txt$")
  baro_files <- list.files(updir, recursive = T, full.names = F, include.dirs = T, pattern = "^[^~$]+(_BARO_).*\\.txt$")
  
  if(length(baro_files) > 0){
      files <- baro_files
    } else {
      show('stage')
      files <- all_files 
    }

files <- files
})

var2 <- reactive({
  input$var2
})
  
### SET FILE TYPE ####
file_type <- reactive({
  baro_files <- list.files(updir, recursive = T, full.names = F, include.dirs = T, pattern = "(_BARO_).*\\.txt$")
  if(length(baro_files) > 0){
    file_type <- "baro"
  } else {
    file_type <- "wl"
  file_type <- file_type
  }
})

### Files UI ####  
output$file.UI <- renderUI({
  selectInput(inputId = ns("file"),
              label = "1. Choose file to upload:",
              choices = files(),
              selected = 1)
})


### 
# Update Select Input when a file is imported (actually when the import button is pressed (successful or not))
  observeEvent(input$import, {
    updateSelectInput(session = session,
                      inputId = ns("file"),
                      label = "1. Choose file to upload:",
                      choices = files(),
                      selected = "")
  })   
well_file <-   reactive({
  length(grep(input$file, pattern = "(SYW177_).*\\.txt$")) #### LEFT OFF HERE 
})
   
### Stage UI ####
output$stage.UI <- renderUI({
 req(file_type() == "wl")
 numericInput(inputId = ns("stage"),
              label = "Provide stage (ft) at time of download:", value = 0, min = 0, max = 30, step = 0.01, width = "100%")
              
})
### Var2 UI ####
output$var2.UI <- renderUI({
  req(file_type() == "wl", well_file() == 0)
  radioButtons(inputId = ns("var2"),
               label = "Select the secondary Y axis value to plot:", 
               choices = c("Discharge", "Temperature"),
               selected = "Discharge", 
               inline = T,
               width = "100%")
})
  
  ### PROCESS UI ####

output$process.UI <- renderUI({
  req(input$file)
  actionButton(inputId = ns("process"),
               label = paste0('Process "', input$file, '" Data'),
               width = '90%')
})     
  
  # ### PLOT UI #### 
  # output$plot_ui <- renderUI({
  #   req(dfs)
  #   plotOutput(ns("plot"), width = "100%", height = "500px")
  # })  
  

  ### RUN PREVIEW FUNCTION ####
  plot <- eventReactive(input$process,{
    req(dfs)
    if(file_type() == "baro"){
      PREVIEW_BARO(df(), df_prior(), var2 = NULL)
    } else {
      PREVIEW_HOBO(df(), df_prior(), df_stage(), var2 = var2())
    }
  }
  )  
  #### PLOT OUTPUT ####
  output$plot <- renderPlot({
    req(try(df()))
    if(file_type() == "baro" | well_file() == 1){
      PREVIEW_BARO(df(), df_prior(), var2 = NULL)
    } else {
      PREVIEW_HOBO(df(), df_prior(), df_stage(), var2 = var2())
    }
  })
  
### RUN PROCESS FUNCTION ####
# Run the function to process the data and return 2 dataframes and path as list
dfs <- eventReactive(input$process,{
  if(file_type() == "baro"){
    PROCESS_BARO(baro_file = input$file)
    } else {
    PROCESS_HOBO(wl_file = input$file, stage = input$stage, username = username)
  }
})
  
  ### Show Import Button ####                 
  observeEvent(input$process, {
    ImportStatus("")
    show(ns('import'))
    show(ns('table_data_preview'))
    show(ns('table_flag_preview'))
  })  
  
### FILE SELECTED ####
file_selected <- eventReactive(input$file, {
  input$file
})
# ### FILE PROCESSED ####
file_processed <- eventReactive(input$process, {
  input$file
})

### PROCESS STATUS ####
process_status <- reactive({
  if(input$file != file_processed()){
    " "
  }else if(inherits(try(dfs()), "try-error")){
    geterrmessage()
  }else{
    paste0('The file "', input$file, '" was successfully processed')
  }
})


### IMPORT STATUS ####
import_status <- reactive({
  ImportStatus()
})

### PROCESS OUTPUT DFS ####    
# Extract each dataframe
df <- reactive({
  dfs()[[1]]
})
df_flags  <- reactive({
  dfs()[[2]]
})
df_prior <- reactive({
  dfs()[[3]]
})
df_stage <- reactive({
  dfs()[[4]]
})


### Import UI ####
# Import Action Button - Will only be shown when a file is processed successfully
output$import.UI <- renderUI({
  req(try(dfs()))
  actionButton(inputId = ns("import"),
               label = paste("Import", file_processed(), "Data"),
               width = '90%')
})

# Import Data - Run import_data function ####
observeEvent(input$import, {
  out <- tryCatch(
    if(file_type() == "baro"){
      IMPORT_BARO(df_baro = df(), baro_file = input$file)
    } else {
      IMPORT_HOBO(df_hobo = df(), df_flags = df_flags(), wl_file = input$file)
    },
    error = function(e) e)
  
  ImportFailed <- any(class(out) == "error")
  
  if (ImportFailed == TRUE){
    print(paste0("Import Failed at ", Sys.time() ,". There was an error: "))
    print(out)
    ImportStatus(paste0("Import Failed at ", Sys.time() ,". There was an error:\n", out))
  } else {
    print(paste0("Successful import of", nrow(df()), " records in '", input$file, "' to Database at ", Sys.time()))
    ImportStatus(paste0("Successful import of ", nrow(df()), " records from file: '", input$file, "' to Database at ", Sys.time()))
  }
  return(ImportFailed)
  
})
# Add text everytime successful import
# observeEvent(input$import, {
#   insertUI(
#     selector = ns("#import"),
#     where = "afterEnd",
#     ui = h4(paste("Successful import of", nrow(df()), "record(s) in '", input$file, "' to Database"))
#   )
# })

# Hide import button and tables when import button is pressed (So one cannot double import same file)
observeEvent(input$import, {
  hide(ns('import'))
  hide(ns('table_data_preview'))
  hide(ns('table_flag_preview'))
})


### Text Outputs ####
output$file_selection <- renderText({file_selected()})
output$text_process_status <- renderText({process_status()}) 
output$text_import_status <- renderText({
  req(try(import_status()))
  import_status()
  })

observeEvent(input$refresh, {
  shinyjs::reset("hobos")
})

#### TABLE OUTPUTS ####
# Sys.setenv(TZ='UTC')
# Sys.getenv()
### Processed data Table - Only make table if processing is successful
output$table_data_preview <- renderDataTable({
  req(try(df()))
  dt <- df()
  dt$DateTimeUTC <- as.character(format(dt$DateTimeUTC, format = "%Y-%m-%d %H:%M"))
  datatable(dt, 
            colnames = c("ID", "Location", "Date-Time (UTC)", "Logger PSI", "Logger Temp (C)", "Stage (ft)", "Discharge (cfs)"),
            options = list(pageLength = 50)) #%>% 
    # formatDate(
    #   columns = "DateTimeUTC",
    #   method = 'toLocaleString')
      # method = 'toISOString')
    #   params = list("se", list(timeZone = "UTC", hour12 = FALSE))
    # )
})

### Processed Flag Table - Only make table if processing is successful
output$table_flag_preview <- renderDataTable({
  req(try(df_flags()))
  datatable(df_flags(), options = list(pageLength = 25))
})

} # end server function

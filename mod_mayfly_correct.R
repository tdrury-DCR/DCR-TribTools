###############################################################################.
#  TITLE: mod_mayfly_correct.R
#  DESCRIPTION: Shiny Module to correct stage and conductivity data from Hydros21 Mayfly loggers
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: 2022-06-01
#  GIT REPO: TribTools
#  R version 4.1.2 (2021-11-01)  x86_64
##############################################################################.

### UI ###
MF_CORRECT_UI <- function(id) {
  ns <- NS(id) 
  tagList(
    useShinyjs(),
    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    div(id = "mf", ### Set div() for Refresh Button ####
        # title = "MAYFLY DATA CORRECTION",
        fluidRow(### PARAMETER AND DATE SELECTION
          column(6,
                 strong(h4("1. SELECT PARAMETER FOR CORRECTION:")),
                 radioGroupButtons(
                   inputId = ns("par_select"), label = NULL, 
                   selected = character(0),
                   choices = c("Stage", "Conductivity"), 
                   justified = TRUE, 
                   status = "primary",
                   individual = TRUE,
                   checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                 ),
                 strong(h4("UNCORRECTED DATA SUMMARY:")),
                 tableOutput(ns("uncorrected_data")),
                 strong(h4("2. Select Location for Correction:")),
                 uiOutput(ns("loc.UI"))
          ),
          column(6,
                 strong(h4("3. PREVIEW DATA:")),
                 br(),
                 actionButton(inputId = ns("make_preview_plot"),
                              label = paste0('Generate preview plot for selected location'),
                              width = '90%'), ### Process Button ####
                 uiOutput(ns("preview_plot.UI"))
          )
        ), # End Fluid Row
        div(id = ns("data_correction_well_panel"),
            uiOutput(ns("CORRECTION.UI")),### DATA CORRECTION UI ####
        ), # End Div 
        fluidRow(### DATA TABSET PANELS
          column(12,
                 tabsetPanel(
                   tabPanel("MAYFLY DATA PREVIEW",  ### DATA PREVEIW PAGE ####
                            dataTableOutput(ns("original_data_preview"))
                   ),
                   tabPanel("TRIBUTARY MONITORING NOTES",
                            dataTableOutput(ns("trib_monitoring_notes"))
                   ),
                   tabPanel("DISCRETE DATA PREVIEW", ### FLAG DATA PREVIEW PAGE ####
                            dataTableOutput(ns("manual_data_preview"))
                   ),
                   tabPanel("CORRECTED DATA PREVIEW",
                            dataTableOutput(ns("corrected_data_preview"))
                   )
                 )# End Tabset Panel
          ) # End Col
        ) # End FR# End FR
    ) # #End Div
  ) # End Taglist
} # End UI
        
        
################################.
###         SERVER          ####
################################.

MF_CORRECT <- function(input, output, session, df_fp, df_trib_monitoring, username, userlocation) {  # Same as rating info - all in Hydro DB
########################################################################.
###               STAGE AND CONDUCTIVITY COMPONENTS                ####
########################################################################.
    
  source("fun_mayfly_correct.R", local = TRUE)
  ns <- session$ns 
  shinyjs::disable(ns("make_preview_plot"))
  # shinyjs::hide(ns("data_correction_well_panel"))
  
  par <- reactive({
    req(input$par_select)
    switch(input$par_select,
           "Stage" = "Stage_ft",
           "Conductivity" = "Conductivity_uScm"
    )           
  })
  
  ### Reactive Values container ####
  rxvals <- reactiveValues()
  
  # summary_title <- glue("Summary of uncorrected Mayfly records for parameter '{par()}'")
  # uncorrected_summary <- reactive({
  #   req(par())
  #   data_correct_summary(parameter = par())
  #   })
  
  observeEvent(input$par_select, {
    rxvals$summary <- data_correct_summary(parameter = par())
  })
  
  output$uncorrected_data <- renderTable({
    req(rxvals$summary)
    rxvals$summary
  })
  
  dsn <- 'DCR_DWSP_App_R'
  database <- "DCR_DWSP"
  tz <- 'UTC'
  tz_out <- tz
  con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz, timezone_out = tz_out)
  
  mayfly_tbl <- tbl(con, Id(schema = userlocation, table =  "tblMayfly"))
  hobo_tbl <- tbl(con, Id(schema = userlocation , table =  "tbl_HOBO"))
  
  # dbDisconnect(con)
  # rm(con)
  
  # short_locs is hard coded to avoid expensive database querying... need to add M110 once online
  if(userlocation == "Wachusett"){
  short_locs <-  c("MD01", "MD02", "MD03", "MD05", "MD06", "MD83", "M110")
  full_locs <- df_trib_monitoring$Location %>% unique() %>% sort() 
  loc_choices <- full_locs[match(short_locs, substrRight(full_locs, 4))]
  }else{
    short_locs <- c("211","GATE")
    loc_choices <- short_locs
  }
  # observeEvent(input$par_select, {
  #   print(loc_choices)
  # print(loc_choices)
  # })
  
  ### Files UI ####  
  output$loc.UI <- renderUI({
    req(par() %in% c("Stage_ft", "Conductivity_uScm"))
    selectizeInput(ns("loc_select"), 
                label = NULL, 
                choices = loc_choices,
                multiple = FALSE,
                options = list(
                  placeholder = 'Please select a location below',
                  onInitialize = I('function() { this.setValue(""); }')
                )
    )
  })

  loc_selected <- reactive({
    req(input$loc_select)
    substrRight(input$loc_select, 4)
  })
  
  observe({
    req(input$loc_select)
    if (input$loc_select %in% loc_choices) {
      shinyjs::enable(ns("make_preview_plot"))
      # shinyjs::show(ns("data_correction_well_panel"))
    } else {
      shinyjs::disable(ns("make_preview_plot"))
      # shinyjs::hide(ns("data_correction_well_panel"))
    }
  })

  output$preview_plot.UI <- renderUI({
    req(plot1())
    dygraphOutput(ns("output_plot1"), width = "100%", height = "500px") %>% withSpinner()
  })
  
  plot1 <- eventReactive(input$make_preview_plot, {  
    plot1 <- preview_plot(loc = loc_selected(), 
                 par = par(),
                 sum_loc = rxvals$summary %>% filter(Location == loc_selected()),
                 df_fp = df_fp, 
                 df_trib_monitoring = df_trib_monitoring)
    plot1
  })
  
  #### PLOT1 OUTPUT ####
  output$output_plot1 <- renderDygraph({
    req(plot1())
    plot1()
  })
  
  
########################################################################.
###                    DATA CORRECTION UI                           ####
########################################################################.  
    output$CORRECTION.UI <- renderUI({
      req(input$par_select)
      if(input$par_select == "Stage") { ### STAGE UI ####
          # div(id = "stage_correction_well_panel",
        fluidRow(
          strong(h4("4. CORRECT STAGE DATA:")),
          br(),
          column(6, 
                 wellPanel(
                   strong(h4("MAYFLY STAGE CORRECTION INSTRUCTIONS:")),
                   em("Select the time range for the data correction... Adjust the correction equation coefficients to provide temperature compensation for the raw stage measurements, while maintaining agreement with the manual stage values"),
                   br(),
                   hr(),
                   br(),
                   fluidRow(
                     column(width = 5,
                            uiOutput(ns("start_time.UI")),
                            uiOutput(ns("end_time.UI")),
                            em("Correction Start:"),
                            verbatimTextOutput(ns("model_start")),
                            em("Correction End:"),
                            verbatimTextOutput(ns("model_end"))
                     ),
                     column(width = 3,
                            dateInput(ns("date_start_manual"), "Start date override:"),
                            dateInput(ns("date_end_manual"), "End date override:"),
                            materialSwitch(inputId = ns("dt_start_override"),
                                           label = "Use override start DT", value = FALSE, status = "info")
                     ),
                     column(width = 4,
                            timeInput(ns("start_time_manual"), "Start time override (HH:MM UTC):", seconds = FALSE),
                            timeInput(ns("end_time_manual"), "End time override (HH:MM UTC):", seconds = FALSE),
                            materialSwitch(inputId = ns("dt_end_override"),
                                           label = "Use override end DT", value = FALSE, status = "info")
                     )
                   ), # End FR
                   br(),
                   hr(),
                   fluidRow(em("The temperature equation is a function of temperature, as follows:")),
                   fluidRow(em("Stage_ft = RawStage_ft + ((30 - Logger_temp_c) * coeff_a)^pow * mult * -1")),
                   fluidRow(em("Placeholder for tips on behavior of each variable.... ")),
                   fluidRow(
                     br(),
                     column(width = 6,
                            sliderInput(inputId = ns("main_mult"), label = "Main multiplier variable:" , min = 0.01 , max = 0.5, value = 0.10 , step = 0.01),
                            sliderInput(inputId = ns("power"), label = "Power variable:" , min = 0.03 , max = 1, value = 0.3 , step = 0.01),
                            sliderInput(inputId = ns("tune_mult"), label = "Fine tune multiplier variable:" , min = -2 , max = 2.0, value = 1 , step = 0.05)
                     ),
                     column(width = 6,
                            numericInput(inputId = ns("set_stage_target"), label = "Set Stage Target (ft):", value = 0, min = 0, max = 10, step = 0.01),
                            uiOutput(ns("drift.UI")),
                            numericInput(inputId = ns("final_offset"), label = "Final Stage Offset (ft):", value = 0, min = 0, max = 0.5, step = 0.01),
                            actionButton(inputId = ns("make_correction_plot"),
                                         label = paste0('Generate plot with data correction'),
                                         width = '90%')
                     )
                   )
                 )# End Well Panel 
          ), # End Col
          column(6, # Correction Plot
                 em("Data Correction Plot"),
                 uiOutput(ns("data_correction_plot.UI")),
                 br(),
                 em("Inspect the corrected output data and once the correction is acceptable, click the 'Import' button to add the final stage and discharge records to the database table."),
                 strong(h4("5. PROCESS CORRECTED STAGE DATA:")),
                 br(),
                 uiOutput(ns("process.UI")), ### Process Button ####
                 uiOutput(ns("text_process_status")) %>% withSpinner(),
                 br(),
                 strong(h4("6. IMPORT CORRECTED STAGE DATA:")),
                 br(),
                 uiOutput(ns("import.UI")), ### Import Button ####
                 uiOutput(ns("text_import_status")) %>% withSpinner()
          )# End Col
        ) # End FR
        # ) # End Div
      } else {  # CONDUCTIVTY UI ####
        # div(id = "conductivity",
        fluidRow(
          strong(h4("4. CORRECT CONDUCTIVITY DATA:")),
          br(),
          column(6,
                 wellPanel(
                   strong(h4("MAYFLY CONDUCTIVITY CORRECTION INSTRUCTIONS:")),
                   em("Select the time range for the data correction...Adjust the correction equation coefficients to provide fouling drift compensation and calibration offset, while maintaining agreement with the YSI sensor results"),
                   br(),
                   hr(),
                   br(),
                   fluidRow(
                     column(width = 5,
                            uiOutput(ns("start_time.UI")),
                            uiOutput(ns("end_time.UI")),
                            em("Correction Start:"),
                            verbatimTextOutput(ns("model_start")),
                            em("Correction End:"),
                            verbatimTextOutput(ns("model_end"))
                     ),
                     column(width = 3,
                            dateInput(ns("date_start_manual"), "Start date override:"),
                            dateInput(ns("date_end_manual"), "End date override:"),
                            materialSwitch(inputId = ns("dt_start_override"),
                                           label = "Use override start DT", value = FALSE, status = "info")
                     ),
                     column(width = 4,
                            timeInput(ns("start_time_manual"), "Start time override (HH:MM UTC):", seconds = FALSE),
                            timeInput(ns("end_time_manual"), "End time override (HH:MM UTC):", seconds = FALSE),
                            materialSwitch(inputId = ns("dt_end_override"),
                                           label = "Use override end DT", value = FALSE, status = "info")
                     )
                   ), # End FR
                   br(),
                   hr(),
                   fluidRow(em("The fouling (drift) equation is a cummulative linear time-step correction: # time increments/ total drift, as follows:")),
                   fluidRow(em("Correction multiplier = ((sensor_2 - sensor_1) - (meter_post - meter_pre)) / sensor_1")),
                   br(),
                   fluidRow(
                     column(width = 6,
                            uiOutput(ns("drift.UI")),
                            em("Mayfly Raw Concuctivity, prior to cleaning (\u03BCS/cm):"),
                            verbatimTextOutput(ns("pre_clean_sc")),
                            em("Mayfly Raw Concuctivity, after cleaning (\u03BCS/cm):"),
                            verbatimTextOutput(ns("post_clean_sc"))
                     ), 
                     column(width = 6,
                            numericInput(ns("meter_pre"), label = "YSI Conductivity Pre-cleaning (\u03BCS/cm) (Optional):", value = 0, min = 0, max = 25000, step = 1),
                            numericInput(ns("meter_post"), label = "YSI Conductivity Post-cleaning (\u03BCS/cm) (Optional):", value = 0, min = 0, max = 25000, step = 1),
                            numericInput(ns("final_offset"), label = "Calibration Offset (\u03BCS/cm):", value = 0, min = 0, max = 200, step = 1),
                            actionButton(ns("make_correction_plot"),
                                         label = paste0('Generate plot with data correction'),
                                         width = '90%')
                     ) # End Col
                   ) # End FR
                 )# End Well Panel
          ), # End Col
          column(6, # Correction Plot
                 em("Data Correction Plot"),
                 uiOutput(ns("data_correction_plot.UI")),
                 # box(textOutput(ns("legendDivID")), title = "Legend", collapsible = TRUE, width = 5),
                 br(),
                 em("Inspect the corrected output data and once the correction is acceptable, click the 'Import' button to add the final stage and discharge records to the database table."),
                 strong(h4("5. PROCESS CORRECTED DATA:")),
                 br(),
                 uiOutput(ns("process.UI")), ### Process Button ####
                 uiOutput(ns("text_process_status")) %>% withSpinner(),
                 br(),
                 strong(h4("6. IMPORT CORRECTED DATA:")),
                 br(),
                 uiOutput(ns("import.UI")), ### Import Button ####
                 uiOutput(ns("text_import_status")) %>% withSpinner()
          )# End Col
        ) # End FR
        # ) # End Div
      } # End else
}) # END CORRECTION UI
  
  
  ### DATA CORRECTION SERVER ####
  
  ### TIME RANGE CONTROLS ####
  ### Filter Correction summary to location selected

  sum_loc <- reactive({
    req(par(), loc_selected())
    rxvals$summary %>% 
      filter(Location == loc_selected())
  })
  
  min_dt <- reactive({
    req(par())
    req(sum_loc())
    as.POSIXct(sum_loc()$MinDateTimeUTC, tz = "UTC")
  })
  
  max_dt<- reactive({
    req(par())
    req(sum_loc())
    as.POSIXct(sum_loc()$MaxDateTimeUTC, tz = "UTC")  
  })
  
  manual_stage_times <- reactive({
    req(par() == "Stage_ft")
    req(min_dt(), max_dt(), loc_selected())
    df_fp %>%
      filter(Location == loc_selected(),
             Parameter == "Staff Gauge Height",
             between(DateTimeUTC, min_dt() - hours(3), max_dt() + hours(3))) %>% 
      use_series(DateTimeUTC) %>% 
      unique() %>% 
      sort()
  })
  
  ### Get the cleaning dates
  
  if(userlocation == "Wachusett"){
  mf_cleanings <- reactive({
    df_trib_monitoring[which(df_trib_monitoring$Mayfly_Cleaned),]
  })
  
  cleanings <- reactive({
    req(loc_selected())
    mf_cleanings() %>%
      dplyr::filter(substrRight(Location, 4) == loc_selected()) %>% 
      rowwise() %>% 
      mutate(DateTimeUTC = with_tz(Mayfly_DownloadDateTimeET, tzone = "UTC")) %>%
      arrange(DateTimeUTC)
  })
  }else{
    mf_cleanings <- reactive({
      df_trib_monitoring[which(df_trib_monitoring$Mayfly_Cleaned),]
    })
    
    cleanings <- reactive({
      req(loc_selected())
      mf_cleanings() %>%
        dplyr::filter(substrRight(Location, 4) == loc_selected()) %>% 
        rowwise() %>% 
        arrange(DateTimeUTC)
    })
  }
  ### Start Time ####
  # Default value should be the first uncorrected Mayfly stage value
  # Choices should be the manual times + 15 minutes 
  start_time_choices <- reactive({
    req(min_dt())
    if(par() == "Stage_ft") {
      req(manual_stage_times())
      c(min_dt(), manual_stage_times() + minutes(15))
    } else { # Conductivity start time should be just after a cleaning
      req(cleanings())
      c(min_dt(), cleanings()$DateTimeUTC[cleanings()$DateTimeUTC] > min_dt()) 
    }
  })
  # If there needs to be a different time correction user should use the timeInput widgets
  
  ### End Time ####
  # Default value should be the third manual stage point (this should span ~1 month)
  # Choices should be the manual times)
  end_time_choices <- reactive({
    if(par() == "Stage_ft") {
      req(manual_stage_times(), model_start_time())
      manual_stage_times()[manual_stage_times() > input$start_time_select]
    } else { # Conductivity end time should be just prior to a cleaning
      req(cleanings(), model_start_time())
      c(cleanings()$DateTimeUTC[cleanings()$DateTimeUTC > input$start_time_select], max_dt())
    }
  })
  
  # If there needs to be a different time correction user should use the timeInput widgets
  model_start_time <- reactive({
    req(input$start_time_select)
    if(input$dt_start_override) {
      as.POSIXct(paste0(strftime(input$date_start_manual, "%Y-%m-%d"), " ", strftime(input$start_time_manual, "%H:%M")), tz = "UTC")
    } else { ### For conductivity, find the first time aft
      as.POSIXct(input$start_time_select, tz = "UTC")
    }
  })
  
  model_end_time <- reactive({
    req(model_start_time())
    if(input$dt_end_override) {
      as.POSIXct(paste0(strftime(input$date_end_manual, "%Y-%m-%d"), " ", strftime(input$end_time_manual, "%H:%M")), tz = "UTC")
    } else {
      as.POSIXct(input$end_time_select, tz = "UTC")
    }
  })
  
  output$model_start <- renderPrint({
    req(model_start_time())
    model_start_time() 
  })
  output$model_end <- renderPrint({ 
    req(model_end_time())
    model_end_time() 
  })
  
  output$pre_clean_sc <- renderPrint({
    req(pre_clean_spcd())
    pre_clean_spcd() 
  })
  output$post_clean_sc <- renderPrint({ 
    req(post_clean_spcd())
    post_clean_spcd() 
  })
  
  output$original_data_preview <- renderDataTable({
    req(df_model())
    datatable(df_model()) 
  })
  
  output$trib_monitoring_notes <- renderDataTable({
    req(df_trib_monitoring)
    datatable(df_trib_monitoring) 
  })
  
  output$manual_data_preview <- renderDataTable({
    req(df_fp_model())
    datatable(df_fp_model()) 
  })
  
  output$corrected_data_preview <- renderDataTable({
    req(corrected_output())
    datatable(corrected_output()[[1]])
  })
  ########################################################################.
  ###                             PREP DATA                           ####
  ########################################################################.
  
  ### Need to trim the date ranges here since we need to do a full join later on and the times may not line up

  df_fp_model <- reactive({
    req(model_start_time(), model_end_time())
    if(par() == "Stage_ft") { 
      df_fp %>% 
        filter(Location == loc_selected(),
               Parameter == "Staff Gauge Height",
               between(DateTimeUTC, model_start_time() - hours(1), model_end_time() + hours(1))) 
    } else {
      df_fp %>% 
        filter(Location == loc_selected(),
               Parameter == "Specific Conductance",
               between(DateTimeUTC, model_start_time() - hours(1), model_end_time() + hours(1))) 
    }
  })
  
  df_model <- reactive({
    loc <- loc_selected()
    met <- model_end_time()
    mst <- model_start_time()
    req(df_fp_model())
    if(par() == "Stage_ft") { 
      mayfly_tbl %>%
        select(c(2:6)) %>% 
        filter(Location == loc,
               between(DateTimeUTC, mst, met)) %>% 
        collect()
    } else {
      mayfly_tbl %>%
        select(c(2,3,8)) %>% 
        filter(Location == loc,
               between(DateTimeUTC, mst, met)) %>% 
        collect()
    }
  })
  
  ### DYNAMIC UI COMPONENTS ####
  
  output$start_time.UI <- renderUI({
    req(start_time_choices())
    selectInput(ns("start_time_select"), 
                label = "Correction Start Times:", 
                choices = start_time_choices(),
                multiple = FALSE
    )
  })
    
  output$end_time.UI <- renderUI({
    req(end_time_choices())
    selectInput(ns("end_time_select"), 
                label = "Correction end times:", 
                choices = end_time_choices(),
                multiple = FALSE,
                selected = end_time_choices()[1])
  })  
  
  
  ### CORRECTION PLOT ####
  output$data_correction_plot.UI <- renderUI({
    req(corrected_output())
    dygraphOutput(ns("output_plot2"), width = "100%", height = "500px") %>% withSpinner()
  })
  
  # Get Mayfly conductivity values just prior to and after cleaning
  pre_clean_spcd <- reactive({
    req(par() == "Conductivity_uScm")
    req(model_end_time())
    ### Get the last raw conductivity before the model-end-time
    loc <- loc_selected()
    met <- model_end_time()
    mf_temp <- mayfly_tbl %>% 
      arrange(DateTimeUTC) %>% 
      filter(Location == loc, 
             is.na(Conductivity_uScm),
             DateTimeUTC <= met) %>% 
      collect()
    
    mf_temp %>% 
      slice(n()) %>% 
      pull(RawConductivity_uScm)
  })
  
  post_clean_spcd  <- reactive({
    req(par() == "Conductivity_uScm")
    req(model_end_time())
    ### Get the first raw conductivity after the model-end-time
    loc <- loc_selected()
    met <- model_end_time()
    d <- mayfly_tbl %>% 
      arrange(DateTimeUTC) %>% 
      filter(Location == loc, 
             is.na(Conductivity_uScm),
             DateTimeUTC > met) %>% 
      collect()
    
    d2 <- d %>% 
      slice(1) %>% 
      pull(RawConductivity_uScm)
    
    if(nrow(d) == 0) {
      d <- mayfly_tbl %>% 
        arrange(DateTimeUTC) %>% 
        filter(Location == loc, 
               is.na(Conductivity_uScm),
               DateTimeUTC == met) %>% 
        collect()
      
      d2 <- d %>%   
        slice(1) %>% 
        pull(RawConductivity_uScm)
    }
    d2
  })
  
  # sensor_drift <- reactive({
  #   req(par() == "Conductivity_uScm")
  #   req(pre_clean_spcd(), post_clean_spcd())
  #   post_clean_spcd() - pre_clean_spcd() # Post should usually be higher than Pre
  # })

  output$drift.UI <- renderUI({
    if(par() == "Stage_ft") {
      sliderInput(inputId = ns("drift"), label = "Drift Correction (ft):", 
                  min = -0.3 , max = 0.3, value = 0 , step = 0.01)
    } else {
      sliderInput(inputId = ns("drift"), label = "Manual Fouling (Drift) Correction (\u03BCS/cm):",
                  min = 0 , max = 200, value = 0, step = 1)
    }
  })
  
  corrected_output <- eventReactive(input$make_correction_plot, {
    loc <- loc_selected()
    met <- model_end_time()
    mst <- model_start_time()
    if(par() == "Stage_ft") {
      out <- MF_STAGE_CORRECT(df = df_model(), 
                              df_hobo = hobo_tbl %>% 
                                filter(Location == loc,
                                       between(DateTimeUTC, mst, met)) %>% 
                                select(3,6) %>% 
                                collect(),
                              df_fp =  df_fp_model(),
                              coeff_a = input$main_mult, 
                              mult = input$tune_mult, 
                              pow = input$power, 
                              stage_target = input$set_stage_target,
                              drift = input$drift,
                              final_offset = input$final_offset)
      
    } else {
      out <- MF_COND_CORRECT(df = df_model(),
                             df_fp =  df_fp_model(),
                             df_trib_monitoring = df_trib_monitoring,
                             drift = input$drift,
                             start = pre_clean_spcd(), #Mayfly
                             end = post_clean_spcd(), #Mayfly
                             meter_pre = input$meter_pre, #YSI
                             meter_post = input$meter_post, #YSI
                             final_offset = input$final_offset)
    }
    return(out)
  })
  
  #### PLOT2 OUTPUT ####
  output$output_plot2 <- renderDygraph({
    req(corrected_output())
    corrected_output()[[2]] 
  })  
  
### PROCESS / IMPORT CORRECTED DATA ####
  
  output$process.UI <- renderUI({
    req(corrected_output())
    actionButton(inputId = ns("process"),
                 label = paste0('Prepare corrected data for import'),
                 width = '90%')
  })     
  
  ### RUN PROCESS FUNCTION ####
  # Run the function to process the data and return 2 dataframes and path as list
  dfs <- eventReactive(input$process,{
    loc <- loc_selected()
    met <- model_end_time()
    mst <- model_start_time()
    
    if(par() == "Stage_ft") {

      PROCESS_CORRECTED_STAGE(df_mayfly = mayfly_tbl %>%
                                filter(Location == loc,
                                       between(DateTimeUTC, mst, met)) %>%
                              collect(), 
                              df_corrected = corrected_output()[[1]], 
                              username = username, 
                              userlocation = userlocation)
    } else {
      PROCESS_CORRECTED_COND(df_mayfly = mayfly_tbl %>%
                               filter(Location == loc,
                                      between(DateTimeUTC, mst, met)) %>% 
                               collect(), 
                             df_corrected = corrected_output()[[1]], 
                             username = username, 
                             userlocation = userlocation)
    }
  })
  
 ### PROCESS OUTPUTS 
  df <- reactive({
    req(dfs())
    dfs()[[1]]
  })
  
  df_flags  <- reactive({
    req(dfs())
    dfs()[[2]]
  })  
  
  ### PROCESS STATUS ####
  process_status <- reactive({
    req(dfs())
    if(!isTruthy(dfs())){
      " "
    }else if(inherits(try(dfs()), "try-error")){
      geterrmessage()
    }else{
      paste0('The data was successfully processed')
    }
  })
  
  ### Import UI ####
  # Import Action Button - Will only be shown when a file is processed successfully
  output$import.UI <- renderUI({
    req(corrected_output())
    req(try(dfs()))
    actionButton(inputId = ns("import"),
                 label = "Import corrected data to database",
                 width = '90%')
  })
  
  # Import Data - Run import_data function ####
  observeEvent(input$import, {
    out <- tryCatch(
      IMPORT_CORRECTED_MAYFLY(df_mayfly = df(), df_flags = df_flags(), userlocation = userlocation),
      error = function(e) e)
    
    ImportFailed <- any(class(out) == "error")
    
    if (ImportFailed == TRUE){
      print(paste0("Update Failed at ", Sys.time() ,". There was an error: "))
      print(out)
      rxvals$import_status <- paste0("Update Failed at ", Sys.time() ,". There was an error:\n", out)
    } else {
      print(paste0("Successful update of ", nrow(df()), " records to the database at ", Sys.time()))
      rxvals$summary <- data_correct_summary(parameter = par())
      rxvals$import_status <- paste0("Successful update of ", nrow(df()), " records to the database at ", Sys.time())
    }
  })

  # Hide import button and tables when import button is pressed (So one cannot double import same file)
  observeEvent(input$import, {
    hide('import')
    # hide(ns('table_data_preview'))
    # hide(ns('table_flag_preview'))
  })
  
  ### Text Outputs ####
  output$text_process_status <- renderText({
    req(try(process_status()))
    process_status()
    }) 
  
  output$text_import_status <- renderText({
    req(try(rxvals$import_status))
    rxvals$import_status
  })
} # End Server Function
  
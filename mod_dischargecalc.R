###________________________________________________________________________________
#     Title: mod_dischargecalc
#     Description: Shiny web app module to manually calculate discharges
#     Written by: Travis Drury
#     Last Updated: November 1, 2023
###________________________________________________________________________________

### UI #### 

DISCHARGECALC_UI <- function(id) {
  ns <- NS(id) # see General Note 1
  
  tagList(
    useShinyjs(),
    # shinythemes::themeSelector(),
    div(id = "dischargeoutput", ### Set div() for Refresh Button ####
        title = "Tributary Discharge Calculator",
        fluidRow(
          column(12,
                 br(),
                 textOutput(ns("intro")),
                 br(),
                 h3("Inputs:"),
                 br(),
          )
        ),
        fluidRow( 
          column(4, 
                 uiOutput(ns("site_ui")),   
          ),
          column(width = 2,
                 numericInput(ns("stage_input"),"Stage (ft)", min = 0, max = 50, step = 0.01, value = 0),
                 
          ),       
          column(width = 3,
                 dateInput(ns("date_manual"), "Date:"),

          ),
          column(width = 3,
                 timeInput(ns("time_manual"), "Time (HH:MM UTC):", seconds = FALSE),
        )),
        hr(),
        fluidRow(
          column(
          width=12,
          textOutput(ns("instructions")),
          br()
        )),
        fluidRow(
        column(3,
               offset=2,
               uiOutput(ns("calc_discharge.UI"),style="padding-top:21px"),
        ),
        column(3,
               offset=1,
               strong(textOutput(ns("calculatedtext"))),
               verbatimTextOutput(ns("dischargeoutput")),
      )),
      
      fluidRow(
        column(6,
               offset=6,
               strong(textOutput(ns("flagwarning"))))
      )
        

      
          ) # End Tabset Panel
        ) # End fluid row
} ### End UI ####





### SERVER ####

DISCHARGE <- function(input, output, session, df_ratings, df_locs, userlocation){
  # df <- df_wach_flow
  
  ns <- session$ns 
  
  
  ### Add function to pull out a certain amount of letters on the right of a string of characters
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }

  ### Identify schema for HOBOcalcQ 
  schema <- userlocation
  
  ### Get list of tributaries with active rating curves, but exclude old forestry and USGS sites
  rating_locs <- df_ratings %>% 
    filter(IsCurrent==TRUE,
           !MWRA_Loc %in% c("FPRN", "FHLN","MD04","MD69","MD07") ) %>% 
    rename(LocationMWRA = MWRA_Loc)
  
  ### Add full names of locations
  locs <- left_join(rating_locs,df_locs, by="LocationMWRA",)
  
  
  ### Sort locations alphabetically
  locations <- sort(unique(locs$LocationLabel)) 
  
  ### Site Selection
  site_choices <- reactive({
    c(locations) %>% sort() %>% paste()
  })


  # Site UI
  output$site_ui <- renderUI({
    selectInput(ns("site"), "Location:",
                choices = c(site_choices(),"Choose Location"),
                selected = "Choose Location",
                multiple = FALSE,
                width = "400px")
  })
  
  

  ### Intro text
  output$intro <- renderText({paste0("This module utilizes the rating curves of Wachusett tributaries to calculate a discharge on a given date and time at a given stage. Tributaries with USGS flow data are not available in this calculator.")
  })
  
  
  #### Instruction text for below inputs and above CALCULATE DISCHARGE button
  output$instructions <- renderText({paste0("Enter the input parameters above then click the Calculate Discharge button to calculate the discharge. When the inputs are changed, the discharge value will be removed until the Calculate Discharge button is clicked again.")})
  
  
  ### Create CALCULATE DISCHARGE button
  output$calc_discharge.UI <- renderUI({
    actionButton(inputId = ns("calc_discharge"),
                 label = "CALCULATE DISCHARGE",
                 width = '200px')
  })
  

  ### Source for HOBOcalcQ function
  source("HOBO_calcQ.R")

  
  ### create df_HOBO dataframe in format required for HOBOcalcQ function
  df_HOBO <- reactive({
    data.frame(DateTimeUTC=as.POSIXct(paste0(strftime(input$date_manual, "%Y-%m-%d"), " ", strftime(input$time_manual, "%H:%M")), tz = "UTC"), Stage_ft=input$stage_input)
  })
  
  ### Run HOBOcalcQ and output to table
  calctable <- eventReactive(input$calc_discharge,{
    
    req(input$site != "Choose Location")
    
    HOBOcalcQ(schema = schema, loc = substrRight(input$site,4), df_HOBO = df_HOBO())
    
  })
  
  ### Save copy of df_HOBO that was used for most recent press of CALCULATE DISCHARGE button
  df_calculated <- eventReactive(input$calc_discharge,{
    df_HOBO()})
  
  ### Save copy of location that was used for most recent press of CALCULATE DISCHARGE button
  loc_calculated <- eventReactive(input$calc_discharge,{
    input$site})
  
  ### Create and show calculated discharge value only if data hasn't changed in inputs
  output$dischargeoutput <- reactive({
    
    if(all(df_HOBO() == df_calculated()) & loc_calculated()[1] == input$site){
      calctable()[1,5]
  
    } else {
      NA
    }
  })
  
  ### Create and show any relevant flag warnings only if data hasn't changed in inputs
  output$flagwarning <- reactive({
   
   if(all(df_HOBO() == df_calculated()) & loc_calculated()[1] == input$site){
     
      case_when(calctable()[1,6] == 113 ~ paste0("Warning: Input stage is below the rating curve."),
                calctable()[1,6] == 111 ~ paste0("Warning: Input stage is above the rating curve."),
                TRUE ~ NA)
     }else{
   NA
 }
   })
 
  ### Create and show "Calculated Discharge (cfs)" label for output only if data hasn't changed in inputs
   output$calculatedtext <- reactive({ 
   if(all(df_HOBO() == df_calculated()) & loc_calculated()[1] == input$site){
     
    paste0("Calculated Discharge (cfs):")
   
     }else{
      NA
   }
 }
 )
 

} # end Server Function




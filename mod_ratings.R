###________________________________________________________________________________
#     Title: mod_ratings
#     Description: Shiny web app module to view and create rating curves
#     Written by: Dan Crocker
#     Last Updated: October 30, 2018
#
# Next steps: For existing rating curve - add a table below the plot that shows the equation
#  Also - show the coefficient stats using the same measurements as the new rating (so cull out
#  any poor measurements in addition to drop_meas(). Then compare the standard error of the old rating
#  to the new rating ...  if the new rating is better then use it, if not, keep the existing rating. 
###________________________________________________________________________________

### UI #### 

RATINGS_UI <- function(id) {
  ns <- NS(id) # see General Note 1
  tagList(
    useShinyjs(),
    # shinythemes::themeSelector(),
    div(id = "ratings", ### Set div() for Refresh Button ####
        title = "Tributary Rating Tool",
    fluidRow(
      column(12,
      div(actionButton(ns("refresh"), "REFRESH"), align = "left"),
      # theme = "united",  # <--- To use a theme, uncomment this
        br(),
        textOutput(ns("intro")),
      br()
      )
    ),
    fluidRow( 
      column(4, 
        uiOutput(ns("site_ui")),   
        numericInput(ns("rating1"),"Select/Create Rating #:", min = 1.01, step = 0.01, value = 0, width = "200px"),
        textOutput(ns("pzf_current"))
      ),
      column(4,
        numericInput(ns("offset"),"Enter offset (ft) (Point of Zero Flow [PZF]", min = 0, max = 15, step = 0.01, value = 0, width = "400px"),
        numericInput(ns("break1"),"Enter first stage breakpoint (for 2-3 part rating)", min = 0, max = 15, step = 0.01, value = 0, width = "400px"),
        numericInput(ns("break2"),"Enter second stage breakpoint (for 3 part rating only)", min = 0, max = 15, step = 0.01, value = 0, width = "400px")
      ),
      column(3,
        uiOutput(ns("calc_rating.UI"))     
      )
    ),
    fluidRow(
      column(6,
        wellPanel(
          textInput(ns("drop_measurement"), "Remove Measurement(s) (Separate multiple with commas):"),
          br(),
          uiOutput(ns("plot1_ui")),
          uiOutput(ns("axis_ui")),
          br(),
          em("NLS OUTPUT:"),
          uiOutput(ns("datatext_ui"))
        ) # End Well Panel
      ),
      column(6,
        wellPanel(
          uiOutput(ns("rating2_ui")),
          br(),
          uiOutput(ns("plot2_ui")),
          br()
          # uiOutput(ns("table3_ui"))
        ) # End Well Panel
      ) # End Column
    ), # End Fluid Row
    fluidRow(
        tabsetPanel( ### Model Data Tabs ####
          tabPanel("Discharge Measurements",
            dataTableOutput(ns("discharges"))
          ),
          tabPanel("Ratings",
            dataTableOutput(ns("ratings"))
          ) # End Tab Panel
        ) # End Tabset Panel
    ) # End fluid row
  )  ### End Div() for refresh ####
  ) # End Taglist
} ### End UI ####

### SERVER ####

RATINGS <- function(input, output, session, df_ratings, df_discharges){
  # df <- df_wach_flow
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  df_discharges[,c(4:5)] <- with_tz(df_discharges[,c(4:5)], tzone = "America/New_York")
  df_ratings[,c(9:10)] <- with_tz(df_ratings[,c(9:10)] , tzone = "America/New_York")
  
  source("Ratings.R", local = T) #### Source the rating functions

  # Main Selection
  # df_ratings <- tbl_ratings
  # df_discharges <- tbl_discharges
  
  ns <- session$ns 
  # Change case of DATE column for Date Select, pull into long format for filtering

  output$intro <- renderText({paste0("This module utilizes the nls package to determine the nonlinear (weighted) least-squares estimates of the parameters of a nonlinear model.  Once a location is selected, choose which rating to create. An existing rating can be recalculated or a new rating can be created.\n
                                     Discharge measurements that correspond to the rating being generated will be selected.\n
                                     Measurements are quality weighted by the model (relatively) as follows:\n
                                    'Excellent' = 100, 'Good' = 85, 'Fair' = 70, 'Poor' = 0 (No quality provided = Fair(70)\n
                                     click the 'Calculate Rating' button to inspect the curve fit and statistics. Hover on any of the measurements to see the \n
                                     measurement number and other metadata for each point. If certain points are far from the curve, follow-up measurements should be taken and points should be evaluated for removal based on age/quality.\n 
                                     To exclude measurements and rerun the rating model, add the measurement numbers to the box above the plot.\n
                                     Once a rating is acceptable add the coefficients, and any removed measurements to the rating table in the database. Decide if it is warranted to recalculate any historical discharges with the new rating. \n
                                     Lastly, update rating start and end dates and make sure to set the new rating to 'current' so it will be applied to new data going into the database.")
    })
  
  ### Site Selection
  site_choices <- reactive({
    c("HOLDEN FORESTRY WEIR - HLNW",  df_discharges %>% .$Location) %>% sort() %>% paste()
    })
  
  # Site UI
  output$site_ui <- renderUI({
      selectInput(ns("site"), "Select Location:",
                  choices = c(site_choices(),"Choose Location"),
                  selected = "Choose Location",
                  multiple = FALSE,
                  width = "400px")
  })      
  
  output$pzf_current <- renderText({
    req(input$site != "Choose Location")
    paste0("The PZF for the current rating at the selected site (", substrRight(input$site, 4), ") is: ", df_ratings$a1[df_ratings$MWRA_Loc == substrRight(input$site, 4) & df_ratings$Current == TRUE], " ft")
  })
  
  output$discharges <- renderDataTable({
    datatable(df_discharges) %>%
      formatDate(columns = c("DateTimeStartET","DateTimeEndET"), method = 'toLocaleString')
  })
    
  output$ratings <- renderDataTable({
    datatable(df_ratings) %>%
      formatDate(columns = c("DateTimeStartET","DateTimeEndET"), method = 'toLocaleString')
  })

  ### Remove Rating measurements ####
  drop_meas <- reactive({
    if(!is.null(input$drop_measurement)){
      x <- str_split(input$drop_measurement,",") %>%
        lapply(function(x) as.numeric(x))
      as.vector(as.integer(unlist(x)))
      drop_meas <- unlist(x)
    }
    else{
      drop_meas <- NULL
    }
  })
  
  offset_ft <- reactive({
    if(isTruthy(input$offset)){
    input$offset
    }
    else{
      NULL
    }
  })
  
  break1 <- reactive({
    if(isTruthy(input$break1)){
      input$break1
    }
    else{
      NULL
    }
  })
  
  break2 <- reactive({
    if(isTruthy(input$break2)){
      input$break2
    }
    else{
      NULL
    }
  })
  
  
  axes <- reactive({
    input$axis
  })
  
  ### Make Rating ####
  # Run Model Button - Will only be shown when a data is prepared successfully
  output$calc_rating.UI <- renderUI({
    actionButton(inputId = ns("calc_rating"),
                 label = "CALCULATE RATING",
                 width = '200px')
  })
  

   
  plot1_value <- reactiveValues(type =  "a")

  ### Run the rating function #### This generates a plot [1] and a table [2]
  dfs <- eventReactive(input$calc_rating,{
    if(!is.null(offset_ft())){
      MAKE_RATING(tbl_discharges = df_discharges,
                  tbl_ratings = df_ratings, 
                  loc = input$site,
                  axes = axes(),
                  offset1 = offset_ft(), 
                  offset2 = offset_ft(), 
                  offset3 = offset_ft(),
                  break1 = break1(), 
                  break2 = break2(), 
                  drop_meas = drop_meas())
      } else {
      stop("An offset must be provided to calculate rating!")
    }
  })
  
# Axis UI - only show options for 1 Parameter plot
  output$axis_ui <- renderUI({
      checkboxInput(ns("axis"), "Log Scale Axes", value = F)
  })  
  
observeEvent(input$calc_rating,{
  plot1_value$type <- "b"
  })

observeEvent(input$site,{
  plot1_value$type <- "a"
}, ignoreInit = T)

  p_rating <- reactive({dfs()$plot}) ### This is the plot 
  data <- reactive({dfs()$data}) ### This is the rating data
  # t_conf <- reactive({dfs()[[3]]}) ### This is the table with coefficients

  output$plot1_ui <- renderUI({
    req(input$site != "Choose Location")
      if(as.character(plot1_value$type) == "a"){
      plotlyOutput(ns("plot1a"), width = "100%", height = "500px") %>% withSpinner()
      } else {
      plotlyOutput(ns("plot1b"), width = "100%", height = "500px") %>% withSpinner()
      }
  })
  
  # Plot 1a shows first - just the measurements
  output$plot1a <- renderPlotly({plot_measurements()})
  # Plot 1b replaces plot 1a when a rating is made
  output$plot1b <- renderPlotly({p_rating()}) 

  output$datatext_ui <- renderUI({
    req(try(dfs()))
    verbatimTextOutput(ns("datatext"))
  })
  
  parts <- reactive({
    if(input$break2 > 0){
      3
    } else if(input$break1 > 0){
      2
    } else {
      1
    }
  })

  datatext <- reactive({
    if(parts() == 1){
    l <- list(`RATING EQUATION:` =  data()$eq1,
           `COEFFICIENT STATS:` = data()$t_sum1,
           `COEFFICIENT 95% CONFIDENCE INTERVALS:` = data()$conf_int1,
           `DROPPED MEASUREMENTS` = drop_meas()
      )
        print(l, quote = FALSE)
    } else if(parts() == 2){
     l <- list(`RATING EQUATION (Part 1):`= data()$eq1,
          `COEFFICIENT STATS:` = data()$t_sum1,
          `COEFFICIENT 95% CONFIDENCE INTERVALS:` = data()$conf_int1,
          `RATING EQUATION (Part 2):` = data()$eq2,
          `COEFFICIENT STATS:` = data()$t_sum2,
          `COEFFICIENT 95% CONFIDENCE INTERVALS:` = data()$conf_int2,
          `DROPPED MEASUREMENTS` = drop_meas()
      )
     print(l, quote = FALSE)
    } else {
      l <- list(`RATING EQUATION (Part 1):`= data()$eq1,
                `COEFFICIENT STATS:` = data()$t_sum1,
                `COEFFICIENT 95% CONFIDENCE INTERVALS:` = data()$conf_int1,
                `RATING EQUATION (Part 2)` = data()$eq2,
                `COEFFICIENT STATS:` = data()$t_sum2,
                `COEFFICIENT 95% CONFIDENCE INTERVALS:` = data()$conf_int2,
                `RATING EQUATION (Part 3):` = data()$eq3,
                `COEFFICIENT STATS:` = data()$t_sum3,
                `COEFFICIENT 95% CONFIDENCE INTERVALS:` = data()$conf_int3,
                `DROPPED MEASUREMENTS` = drop_meas()
      )
      print(l, quote = FALSE)
    }
  })
  
  output$datatext <- renderPrint(datatext(), quoted = F)
  
  ### Plot 2 ####
#   eventReactive(input$site,{ 
#     plot1_value$type <- "a"
# }, ignoreInit = T)
  
plot_measurements <- eventReactive(input$site,{ 
     PLOT_MEASUREMENTS(df_discharges, df_ratings, input$site)
  })
  
plot2 <- eventReactive(c(input$site,input$rating2),{ 
  req(input$rating2)
     PLOT_RATING(df_discharges, df_ratings, loc = input$site, ratingNo = input$rating2)
  }, ignoreNULL = T)
  
  site_ratings <- reactive({
    req(input$site != "Choose Location")
    df_ratings$RatingNum[df_ratings$MWRA_Loc == substrRight(input$site, 4)]
  })
  current_rating <- reactive({
    req(site_ratings())
    df_ratings$RatingNum[df_ratings$MWRA_Loc == substrRight(input$site, 4) & df_ratings$Current == TRUE]
  })  
  
  # Site UI
  output$rating2_ui <- renderUI({
    req(current_rating())
    selectInput(ns("rating2"), "Compare to Existing Rating:",
                choices = site_ratings(),
                selected = current_rating(),
                width = "100%",
                multiple = FALSE)
  }) 
  
  # rating2 <- reactive({input$rating2})
  
  output$plot2_ui <- renderUI({
    req(site_ratings())
    plotlyOutput(ns("plot2"), width = "100%", height = "500px")
  })
  output$plot2 <- renderPlotly({plot2()})
               
  ### Refresh form
  observeEvent(input$refresh, {
    shinyjs::reset("ratings")
  })   

} # end Server Function




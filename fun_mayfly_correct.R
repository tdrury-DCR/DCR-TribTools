###############################################################################.
#  TITLE: fun_mayfly_correct.R
#  DESCRIPTION: Functions for Mayfly data corrections
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: 2022-06-01
#  GIT REPO: TribTools
#  R version 4.1.2 (2021-11-01)  x86_64
##############################################################################.

########################################################################.
###                              SUMMARY                            ####
########################################################################.

### With Mayfly table group by Location summarize where is.null Stage_ft or is.null Conductivity
### Get min/max dates
### Data corrections limited to 2022 and forward to avoid all the historicasl data from MD83 showing up

data_correct_summary <- function(parameter) {
  dsn <- 'DCR_DWSP_App_R'
  database <- "DCR_DWSP"
  tz <- "UTC"
  tz_out <- "UTC"
  con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz, timezone_out = tz_out)
  
  db_mayfly <- tbl(con, Id(schema = userlocation, table =  "tblMayfly"))
  # Records where discharge is estimated for data gaps may not have Stage, so discharge must also be empty 
  # Only pull data for years that have not been fully corrected already (typically year after most recent annual report)
  if(parameter == "Stage_ft") { 
    df <- db_mayfly %>% 
      filter(is.na(Stage_ft), is.na(Discharge_cfs), DateTimeUTC >= as_datetime("2024-01-01")) %>%
      group_by(Location) %>%
      summarize("MinDateTimeUTC" = min(DateTimeUTC, na.rm = TRUE),
                "MaxDateTimeUTC" = max(DateTimeUTC, na.rm = TRUE)) %>% 
      collect()
  } else {
    df <- db_mayfly %>% 
      filter(is.na(Conductivity_uScm), !is.na(RawConductivity_uScm), DateTimeUTC >= as_datetime("2024-01-01")) %>%
      group_by(Location) %>%
      summarize("MinDateTimeUTC" = min(DateTimeUTC, na.rm = TRUE),
                "MaxDateTimeUTC" = max(DateTimeUTC, na.rm = TRUE)) %>% 
      collect()
  }
  
  summary <- df %>% dplyr::arrange(Location)
  
  summary$MinDateTimeUTC <- format(as.POSIXct(summary$MinDateTimeUTC, tz = "UTC"), format = '%F %R %Z')
  summary$MaxDateTimeUTC <- format(as.POSIXct(summary$MaxDateTimeUTC, tz = "UTC"), format = '%F %R %Z')
  
  dbDisconnect(con)
  rm(con)
  return(summary)
}
        #* TESTING DATA_CORRECT_SUMMARY ----
        # parameter <- "Stage_ft"
        # parameter <- "Conductivity_uScm"
        # data_correct_summary(parameter)
        ### END TESTING - COMMENT OUT ABOVE WHEN DONE 
        
########################################################################.
###                          PREVIEW PLOT                          ####
########################################################################.

preview_plot <- function(loc, par, sum_loc, df_fp, df_trib_monitoring) {
  
  dsn <- 'DCR_DWSP_App_R'
  database <- "DCR_DWSP"
  tz <- "UTC"
  tz_out <- "UTC"
  con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz, timezone_out = tz_out)
  
  mayfly_tbl <- tbl(con, Id(schema = userlocation, table =  "tblMayfly"))
  hobo_tbl <- tbl(con, Id(schema = userlocation, table =  "tbl_HOBO"))
 
   ###  FILTER DATA  ####
  
  mayfly_cols <- switch(par,
                        "Stage_ft" = c(1:7), 
                        "Conductivity_uScm" = c(1:3,8,9))
  
  min_dt <- as.POSIXct(sum_loc$MinDateTimeUTC, tz = "UTC")
  max_dt<- as.POSIXct(sum_loc$MaxDateTimeUTC, tz = "UTC")  
  
  if(userlocation == "Wachusett"){
  df_hobo <- hobo_tbl %>% 
    filter(between(DateTimeUTC, min_dt, max_dt),
           Location == loc) %>% 
    select(2,3,6,7) %>% 
    collect()
  } else {
    df_hobo <- hobo_tbl %>% 
      filter(between(DateTimeUTC, min_dt, max_dt),
             Location == loc) %>% 
      select(2,3,4,6) %>% 
      collect()
  }
  
  df_mayfly <- mayfly_tbl %>% 
    filter(Location == loc, between(DateTimeUTC, min_dt, max_dt)) %>% 
    select(all_of(mayfly_cols)) %>% 
    collect()
  
  df_fp2 <- df_fp %>% 
    filter(Location == loc,
           Parameter == ifelse(par == "Stage_ft", "Staff Gauge Height", "Specific Conductance"),
           between(DateTimeUTC, min_dt - hours(3), max_dt + hours(3)))

  manual_stage_times <- df_fp2 %>% 
    pluck("DateTimeUTC")
  
  # manual_stage_times
  
  ### Generate Mayfly cleaning times for vertical lines on the plot (For conductivity correction)  
  # mayfly_cleanings_dt_UTC <- df_trib_mon %>% 
  #   filter(Location == loc, Mayfly_Cleaned == TRUE) %>% 
  #   mutate("cleaningDateTimeUTC" = as_datetime(glue("{FieldObsDate} {Mayfly_DownloadTimeUTC}"))) %>% 
  #   pull(cleaningDateTimeUTC)
  
  ########################################################################.
  ###           INITIAL EXPLORATORY PLOT FOR STAGE CORRECTION         ####
  ########################################################################.
  # dygraph showing full range of Raw and Corrected data 
  
  max_dt_UTC <- max(df_mayfly$DateTimeUTC, na.rm = TRUE)

  if(par == "Stage_ft") {
    y_label <- "Stage (ft)"
    
  dg <- left_join(df_mayfly[ , c("DateTimeUTC", "RawStage_ft")], df_hobo[ , c("DateTimeUTC", "Stage_ft")]) %>%
    full_join(df_fp2[ , c("DateTimeUTC", "FinalResult")]) %>% 
    distinct()
  
  names(dg) <- c("DateTimeUTC", "Raw_Mayfly", "HOBO", "Manual")
  
  ### Join in the flag data so estimated values can be a different series
  p  <- xts(dg, order.by = dg$DateTimeUTC, tzone = "UTC")
  # x_label <- strftime(p$DateTimeUTC,format = "%B-%m \n%Y")
  
  plot <- dygraph(p[,-1], main = glue("Mayfly {par} (raw) at {loc}")) %>%
    dyOptions(useDataTimezone = TRUE, axisLineWidth = 1.5, fillGraph = FALSE, pointSize = 3, colors = c("blue", "green", "orange" ,"red")) %>%
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 1, hideOnMouseOut = FALSE)  %>%
    dyAxis(name = "y", label = y_label, valueRange = c(0, 1.2 * ceiling(max(dg$HOBO, dg$Manual, dg$Raw_Mayfly, na.rm = TRUE)))) %>%
    dyRangeSelector(dateWindow = c(max_dt_UTC - months(1), max_dt_UTC + hours(5)), strokeColor = '') %>%
    dyCrosshair(direction = "vertical") %>% 
    dyLegend(width = "100%", hideOnMouseOut = FALSE)
  } else {
    y_label <- "Specific Conductance (\u03BCS/cm)"
    dg <- full_join(df_mayfly[ , c("DateTimeUTC", "RawConductivity_uScm")], df_fp2[ , c("DateTimeUTC", "FinalResult")]) %>% 
      distinct()
    
    names(dg) <- c("DateTimeUTC", "Raw_Conductivity", "YSI_Conductivity")
    
    ### Get the cleaning dates
    if(userlocation == "Wachusett"){
    cleanings <- df_trib_monitoring[which(df_trib_monitoring$Mayfly_Cleaned),]
    
    cleanings <- cleanings %>% 
      dplyr::filter(substrRight(Location, 4) == loc) %>% 
      rowwise() %>% 
      mutate(DateTimeUTC = as_datetime(if(is.na(Mayfly_DownloadTimeUTC)) paste0(FieldObsDate, " ", HOBO_DownloadTimeUTC) else paste0(FieldObsDate, " ", Mayfly_DownloadTimeUTC)))# %>%
      # select(c(3,10,28)) %>% 
      # arrange(DateTimeUTC)
    } else {
      cleanings <- df_trib_monitoring[which(df_trib_monitoring$Mayfly_Cleaned),]
      
      cleanings <- cleanings %>%
        dplyr::filter(substrRight(Location, 4) == loc) %>% 
          rowwise()
    }
    
    ### Join in the flag data so estimated values can be a different series
    p  <- xts(dg, order.by = dg$DateTimeUTC, tzone = "UTC")
    # x_label <- strftime(p$DateTimeUTC,format = "%B-%m \n%Y")
    
    plot <- dygraph(p[,-1], main = glue("Mayfly {par} (raw) at {loc}")) %>%
      dyOptions(useDataTimezone = TRUE, axisLineWidth = 1.5, fillGraph = FALSE, pointSize = 3, colors = c("blue", "red", "orange" ,"purple4")) %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 1, hideOnMouseOut = FALSE)  %>%
      dyAxis(name = "y", label = y_label, valueRange = c(0, 1.2 * ceiling(max(dg$YSI_Conductivity, dg$Raw_Conductivity, na.rm = TRUE)))) %>%
      dyEvent(c(cleanings$DateTimeUTC, rep("cleaning",nrow(cleanings))), labelLoc = "bottom", color = "purple") %>% 
      dyRangeSelector(dateWindow = c(max_dt_UTC - months(1), max_dt_UTC + hours(5)), strokeColor = '') %>%
      dyCrosshair(direction = "vertical") %>% 
      dyLegend(width = "100%", hideOnMouseOut = FALSE)
  }
  
  return(plot)
}

        #* TESTING PREVIEW PLOT ----
        # loc <- "MD05"
        # sum_loc <- data_correct_summary(parameter = par) %>% filter(Location == substrRight(loc,4))
        # preview_plot(loc = loc, par = par, sum_loc, df_fp = df_fp, df_trib_monitoring = df_trib_monitoring)
        ###

########################################################################.
###                          STAGE CORRECTION                       ####
########################################################################.
        
        #* TESTING MF_STAGE_CORRECT ----
        # dsn <- 'DCR_DWSP_App_R'
        # database <- "DCR_DWSP"
        # tz <- "UTC"
        # tz_out <- "UTC"
        # con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz, timezone_out = tz_out)
        # 
        # db_hobo <- tbl(con, Id(schema = schema, table = "tbl_HOBO"))
        # db_mayfly <- tbl(con, Id(schema = schema, table =  "tblMayfly"))
        # par <-  "Stage_ft"
        # # par <- "Conductivity_uScm"
        #  
        # if(par == "Stage_ft") {
        #   parameter <- "Staff Gauge Height"
        # } else {
        #   parameter <- "Specific Conductance"
        # }
        # model_start_time <- as_datetime("2024-12-10 15:30:00")
        # model_end_time <-   as_datetime("2024-12-17 14:45:00")
        # 
        # df <- db_mayfly %>%
        #   select(c(2:7)) %>%
        #   filter(Location == loc,
        #          between(DateTimeUTC, model_start_time, model_end_time)) |> 
        #   collect()
        # 
        # df_hobo = db_hobo %>%
        #   filter(Location == loc,
        #          between(DateTimeUTC, model_start_time, model_end_time)) %>%
        #   select(3,7) |> 
        #   collect()
        # 
        # df_fp <- df_fp # Loaded in app.r
        # coeff_a <- 0.11
        # mult <- 0.33
        # pow <- 1.12
        # stage_target <- 0.41
        # drift <- 0
        # final_offset <-  0
        # dfs <- MF_STAGE_CORRECT(df, df_hobo, df_fp, coeff_a, mult, pow, stage_target, drift, final_offset)
        # ### Table
        # dfs[[1]]
        # ### plot
        # dfs[[2]]
        ### END TESTING - COMMENT OUT ABOVE WHEN DONE 
        
MF_STAGE_CORRECT <- function(df, df_hobo, df_fp, coeff_a, mult, pow, stage_target, drift, final_offset) {

  ### Get loc
  loc <- df$Location[1]  
    
  ### Apply temp correction
  df_mayfly_corrected <- df %>%
    mutate(Stage_ft = RawStage_ft - ((35 - Logger_temp_c) * coeff_a)^pow * mult)
  
  offset <- df_mayfly_corrected$Stage_ft[nrow(df_mayfly_corrected)] - stage_target
  
  ### Apply a fouling offset, where correction is pro-rated over time so that correction matches first record to stage at download
  
  drift_corr <- seq((drift/nrow(df_mayfly_corrected)), drift, by = (drift/nrow(df_mayfly_corrected)))
  df_mayfly_corrected$Stage_ft <- df_mayfly_corrected$Stage_ft - drift_corr
  
  names(df_hobo) <- c("DateTimeUTC", "HOBO")
  ### Apply offset to match manual times
  df_mayfly_corrected$Stage_ft <- round(df_mayfly_corrected$Stage_ft - offset - final_offset, 2)

  time_start <- min(df_mayfly_corrected$DateTimeUTC)
  time_end <- max(df_mayfly_corrected$DateTimeUTC)
  
  dg2 <- left_join(df_mayfly_corrected[ , c("DateTimeUTC", "Logger_temp_c", "RawStage_ft", "Stage_ft")], df_hobo, by = "DateTimeUTC") %>%
    full_join(df_fp[ , c("DateTimeUTC", "FinalResult")]) %>% distinct()
  
  names(dg2) <- c("DateTimeUTC", "Mayfly_temp_C", "Raw_Mayfly","Corrected_Mayfly", "HOBO", "Manual")
  
  ### Reorder colums so corrected result on top
  dg2 <- dg2[,c(1,2,3,5,4,6)]
  
  ### Join in the flag data so estimated values can be a different series
  p  <- xts(dg2, order.by = dg2$DateTimeUTC, tzone = "UTC")
  # x_label <- strftime(p$DateTimeUTC,format = "%B-%m \n%Y")
  
  plot <- dygraph(p[,-1], main = glue("Mayfly Stage Correction at {loc}")) %>%
    dyOptions(useDataTimezone = TRUE, axisLineWidth = 1.5, fillGraph = FALSE, pointSize = 3, colors = c("blue", "purple", "green", "orange" ,"red")) %>%
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 1, hideOnMouseOut = FALSE)  %>%
    dyAxis(name = "y", label = "Stage (ft)", valueRange = c(0, 1.2 * max(dg2$Corrected_Mayfly, na.rm = TRUE)), independentTicks = TRUE) %>%
    dyAxis(name = "y2", label = "Water Temperature (C)", valueRange = c(min(0, min(dg2$Mayfly_temp_C, na.rm = TRUE)), max(dg2$Mayfly_temp_C, na.rm = TRUE) +5), independentTicks = TRUE) %>%
    dySeries("Mayfly_temp_C", axis=('y2')) %>% 
    dyRangeSelector(dateWindow = c(time_start - hours(1), time_end + hours(1)), strokeColor = '') %>%
    dyCrosshair(direction = "vertical") %>% 
    dyLegend(width = "100%", hideOnMouseOut = FALSE)
  # plot2
  dfs <- list(
    "df" = df_mayfly_corrected,
    "plot_corrected" = plot
  )
  return(dfs)
}

########################################################################.
###                       PROCESS CORRECTED DATA                    ####
########################################################################.

# df_mayfly <- db_mayfly %>%
#   filter(Location == loc,
#          between(DateTimeUTC, model_start_time, model_end_time))
# df_corrected <- dfs[[1]]
# username <- username
# userlocation <- userlocation

PROCESS_CORRECTED_STAGE <- function(df_mayfly, df_corrected, username, userlocation) {
  
  dsn <- 'DCR_DWSP_App_R'
  database <- "DCR_DWSP"
  tz <- 'UTC'
  con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)
  
  loc <- df_mayfly$Location[1]
  schema <- userlocation
  source("HOBO_calcQ.R")
  ### Calcualte all discharges and save df
  df <- HOBOcalcQ(schema = schema, loc = loc, df_HOBO = df_corrected)
  
  df <- df %>% 
    left_join(df_mayfly[, c(1,3)])  
  
  ### Make a flag df if there are any discharge related flags (only above/below rating curve can be automatically calculated)
  setFlagIDs <- function(){
    if(all(is.na(df$RatingFlag)) == FALSE){ # Condition returns FALSE if there is at least 1 non-NA value, if so proceed
      ### Split the flags into a separate df and assign new ID
      df_flags <- df[,c("ID","RatingFlag")] %>%
        rename("SampleID" = ID, "FlagCode" = RatingFlag) %>%
        drop_na()
      
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
      df_flags$DataTableName <- "tblMayfly"
      df_flags$DateFlagged <-  Sys.Date()
      df_flags$ImportStaff <-  username
      df_flags$Comment <- "Flags generated during data import"
      
      # Reorder df_flags columns to match the database table exactly # Add code to Skip if no df_flags
      df_flags <- df_flags[,c(3,4,1,2,5,6,7)]
      
    } else {
      df_flags <- NA
    }
  } # End set flags function
  df_flags <- setFlagIDs()
  
  dfs <- list(
    "df" = df,
    "df_flag" = df_flags)
  
  # Disconnect from db and remove connection obj
  dbDisconnect(con)
  rm(con)
  
  print(paste0("Mayfly Data finished processing at ", Sys.time()))

return(dfs)
}

# dfs2 <- PROCESS_CORRECTED_MAYFLY(df_mayfly, df_corrected, username, userlocation)

########################################################################.
###                 SPECIFIC CONDUCTANCE CORRECTION                 ####
########################################################################.

          ### Manual Testing - DOES NOT PLOT
          
          # loc <-  "MD01"
          # model_start_time <- as_datetime("2021-12-15 19:30:00")
          # model_end_time <-   as_datetime("2022-01-06 20:20:00")
          # df <- db_mayfly %>%
          #   select(c(2,3,8)) %>%
          #   filter(Location == loc,
          #          between(DateTimeUTC, model_start_time, model_end_time))
          # 
          # df_fp_model <- df_fp %>%
          #   filter(Location == loc,
          #          Parameter == "Specific Conductance",
          #          between(DateTimeUTC, model_start_time, model_end_time))
          # 
          # drift <- 0
          # start <- 234.3
          # end <-  228.7
          # meter_pre <- 0
          # meter_post <- 0
          # final_offset <- 0
          
          # 
          # MF_COND_CORRECT(df = df, df_fp = df_fp_model , df_trib_monitoring = df_trib_monitoring,
          #                   drift = drift, start = start, end = end, meter_pre = meter_pre, meter_post = meter_post, final_offset = final_offset)
          ### END TESTING - COMMENT OUT ABOVE WHEN DONE 


MF_COND_CORRECT <- function(df, df_fp_model, df_trib_monitoring, drift, start, end, meter_pre, meter_post, final_offset) {
  # Notes: 
  # Drift arg allows for a manual overide of the correction
  ### Get loc
  loc <- df$Location[1]  
  
  ### Inputs to formula
  if (is.null(end) || is.na(end) || end == "") {
    end <- df %>% 
      slice(n()) %>% 
      pull(RawConductivity_uScm)
  }
  # Monitor reading before sensor is cleaned  
  sensor_1 <- start + final_offset #
  print(paste0("sensor_1 (before cleaning: ", start))
  # Monitor reading after sensor is cleaned
  sensor_2 <-  end + final_offset # adding final offset here so drift increment is adjusted by final offset too
  print(paste0("sensor_2 (after cleaning: ", end))
  
  ## Fouling correction formula
  f_corr <- if(drift > 0) {
    drift - (meter_post - meter_pre)
  } else {
    ((sensor_2 - sensor_1) - (meter_post - meter_pre)) #/ sensor_1
  }
  ## Correction
  
  ## Add correction to data
  f_corr <- seq((f_corr/nrow(df)),f_corr, by=(f_corr/nrow(df)))
  
  df_mayfly_corrected <- df %>% 
    mutate(Conductivity_uScm = RawConductivity_uScm + f_corr %>% round(1))
  
  df_mayfly_corrected$Conductivity_uScm <- df_mayfly_corrected$Conductivity_uScm + final_offset
  
  time_start <- min(df_mayfly_corrected$DateTimeUTC)
  time_end <- max(df_mayfly_corrected$DateTimeUTC)
  
  if(nrow(df_fp_model) > 0) {
    dg2 <- full_join(df_mayfly_corrected[ , c("DateTimeUTC", "RawConductivity_uScm", "Conductivity_uScm")], df_fp_model[ , c("DateTimeUTC", "FinalResult")]) %>% distinct()
    names(dg2) <- c("DateTimeUTC", "Raw_Conductivity", "Corrected_Conductivity", "YSI_Conductivity")
    aes_colors <- c("blue", "green","red")
    } else {
    dg2 <- df_mayfly_corrected[ , c("DateTimeUTC", "RawConductivity_uScm")]
    names(dg2) <- c("DateTimeUTC", "Raw_Conductivity", "Corrected_Conductivity")
    aes_colors <- c("blue", "green")
  }
  
  p  <- xts(dg2, order.by = dg2$DateTimeUTC, tzone = "UTC")
  # x_label <- strftime(p$DateTimeUTC,format = "%B-%m \n%Y")
  
  ### Get the cleaning dates
  cleanings <- df_trib_monitoring[which(df_trib_monitoring$Mayfly_Cleaned),]
  
  if(userlocation == "Wachusett"){
  cleanings <- cleanings %>% 
    dplyr::filter(substrRight(Location, 4) == loc) %>% 
    rowwise() %>% 
    mutate(DateTimeUTC = as_datetime(if(is.na(Mayfly_DownloadTimeUTC)) paste0(FieldObsDate, " ", HOBO_DownloadTimeUTC) else paste0(FieldObsDate, " ", Mayfly_DownloadTimeUTC)))# %>%
    # select(c(3,10,28)) %>% 
    # arrange(DateTimeUTC)
  }else{
    cleanings <- cleanings %>% 
      dplyr::filter(substrRight(Location, 4) == loc) %>% 
      rowwise()
  }
  
  plot <- dygraph(p[,-1], main = glue("Mayfly Corrected Specific Conductance at {loc}")) %>%
    dyOptions(useDataTimezone = TRUE, axisLineWidth = 1.5, fillGraph = FALSE, pointSize = 3, colors = aes_colors) %>%
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 1, hideOnMouseOut = FALSE)  %>%
    dyAxis(name = "y", label = "Conductivity (\u03BCS/cm)", valueRange = c(0, 1.2 * ceiling(max(dg2$YSI_Conductivity, dg2$Raw_Conductivity, dg2$Corrected_Conductivity, na.rm = TRUE)))) %>%
    dyEvent(c(cleanings$DateTimeUTC, rep("cleaning",nrow(cleanings))), labelLoc = "bottom") %>% 
    dyRangeSelector(dateWindow = c(time_end - months(1), time_end + hours(5)), strokeColor = '') %>%
    dyCrosshair(direction = "vertical") %>% 
    dyLegend(width = "100%", hideOnMouseOut = FALSE)
  
  dfs <- list(
    "df" = df_mayfly_corrected,
    "plot_corrected" = plot
  )
  return(dfs)
}

PROCESS_CORRECTED_COND <- function(df_mayfly, df_corrected, username, userlocation) {
  
  dsn <- 'DCR_DWSP_App_R'
  database <- "DCR_DWSP"
  tz <- 'UTC'
  con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)
  
  loc <- df_mayfly$Location[1]
  schema <- userlocation
  ### Calcualte all discharges and save df
  
  df <- df_corrected %>% 
    left_join(df_mayfly[, c(1,3)])

  dfs <- list(
    "df" = df,
    "df_flag" = NA)
  
  # Disconnect from db and remove connection obj
  dbDisconnect(con)
  rm(con)
  
  print(paste0("Mayfly Data finished processing at ", Sys.time()))
  
  return(dfs)
}

########################################################################.
###                       IMPORT CORRECTED DATA                     ####
########################################################################.

### Using db_mayfly, mutate stage from the model output (corrected_output[[2]])
### Then run the calc_q function to get discharge
### Then send a delete statement to delete out the old records and append to insert the updated records 
### - this will be faster than doing an update statement

IMPORT_CORRECTED_MAYFLY <- function(df_mayfly, df_flags, userlocation){
  print(paste0("Mayfly Data started importing at ", Sys.time()))

  mayfly_tbl <- "tblMayfly"
  dsn <- 'DCR_DWSP_App_R'
  database <- "DCR_DWSP"
  tz <- 'UTC'
  con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)
  schema <- userlocation
  
  ### Import/Update options 
  # - 1. Delete records and re-import (would require brining in all columns)
  # - 2. Run Update query for Stage_ft and Discharge_cfs and only update those records
  
  # Option 1 - Delete/Append - NOT BEST PRACTICES, DO NOT USE
  
  # odbc::dbWriteTable(con, DBI::SQL(glue("{database}.{schema}.{mayfly_tbl}")), value = df_mayfly, append = TRUE)
  
  
  # Option 2 - Updates ### THIS IS REALLY SLOW, deprecated in favor of join table update
  
  # update_ids <- df_mayfly$ID
  # stages <- df_mayfly$Stage_ft
  # discharges <- df_mayfly$Discharge_cfs
  # 
  # qry_part1 <- glue("UPDATE [{schema}].[{mayfly_tbl}] SET [Stage_ft] = {stages}, [Discharge_cfs] = {discharges}")
  # # qry_part1
  # qry_part2 <-  glue(" WHERE [ID] = {update_ids}")
  # # qry_part2
  # # Add index on Location and include in update statement 
  # # Make temp table and join 
  # 
  # qry_update <- str_c(qry_part1, qry_part2)
  # # qry_update
  # # Step 3 - Run the update query - will run each updated record individually
  # 
  # for(i in qry_update){
  #   # print(i)
  #   odbc::dbGetQuery(con, i)
  # }
  
  
  # Option #3
  # Create temp table with data to be updated - ID, Stage_ft, Discharge_cfs cols
### EXAMPLE ####
  # UPDATE A
  # SET A.field1 = temp.field1
  # From permanentTableName A
  # Join temptablename temp on A.field2 = temp.field2 and A.field3 = temp.field3
  # 
  # Where field1 is the value you want to update and field2 and field3 are just what maps the two tables together. 
  # If you only need one or need more fields to map them just use whatever you need. 
  # And this will only update records that match in the join, so it won’t update any records in your permanent table unless they join to records in the temp table
  
  if("Stage_ft" %in% names(df_mayfly)) {
    
    df_temp <- df_mayfly %>% select(c("ID", "Stage_ft", "Discharge_cfs"))
    odbc::dbWriteTable(con, Id(table="#tempMayfly"), value = df_temp, append = FALSE)
    # returnTemp <- dbReadTable(con, Id(schema = schema, table = '#tempMayfly'))
    # Join 
    qry_join <- glue("UPDATE A SET A.[Stage_ft] = temp.[Stage_ft], 
                   A.[Discharge_cfs] = temp.[Discharge_cfs] FROM [{schema}].[{mayfly_tbl}] A
                    JOIN [#tempMayfly] temp ON A.ID = temp.ID")
    
    odbc::dbGetQuery(con, qry_join)
    
    # Flag data
    if ("data.frame" %in% class(df_flags)){ # Check and make sure there is flag data to import
      print("Importing flags...")
      odbc::dbWriteTable(con, Id(schema=schema, table=ImportFlagTable), value = df_flags, append = TRUE)
    } else {
      print("No flags to import")
    }
    
    # Disconnect from db and remove connection obj
    dbDisconnect(con)
    rm(con)
    
    SendEmail(df = df_mayfly, 
              table = mayfly_tbl, 
              file = "No file...Corrected Stage and Discharge data added to the table.", 
              emaillist = emaillist, 
              username = username, 
              userlocation = userlocation)
    
  } else {
    
    df_temp <- df_mayfly %>% select(c("ID", "Conductivity_uScm"))
    odbc::dbWriteTable(con, Id(table="#tempMayfly"), value = df_temp, append = FALSE)
    # returnTemp <- dbReadTable(con, Id(schema = schema, table = '#tempMayfly'))
    # Join 
    qry_join <- glue("UPDATE A SET A.[Conductivity_uScm] = temp.[Conductivity_uScm] FROM [{schema}].[{mayfly_tbl}] A
                    JOIN [#tempMayfly] temp ON A.ID = temp.ID")
    
    odbc::dbGetQuery(con, qry_join)
    
    # Disconnect from db and remove connection obj
    dbDisconnect(con)
    rm(con)
    
    SendEmail(df = df_mayfly, 
              table = mayfly_tbl, 
              file = "No file...Corrected conductivity data added to the table.", 
              emaillist = emaillist, 
              username = username, 
              userlocation = userlocation)
    
  }
  
  print(paste0("Mayfly Data finished importing at ", Sys.time()))
  return("Import Successful")
}

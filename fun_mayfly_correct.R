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

data_correct_summary <- function(parameter) {
  dsn <- 'DCR_DWSP_App_R'
  database <- "DCR_DWSP"
  tz <- 'UTC'
  con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)
  
  db_mayfly <- tbl(con, Id(schema = "Wachusett", table =  "tblMayfly"))
  
  if(parameter == "Stage_ft") {
    df <- db_mayfly %>% 
      filter(is.na(Stage_ft)) %>%
      group_by(Location) %>%
      summarize("MinDateTimeUTC" = min(DateTimeUTC),
                "MaxDateTimeUTC" = max(DateTimeUTC)) %>% 
      collect()
  } else {
    df <- db_mayfly %>% 
      filter(is.na("Conductivity_uScm")) %>%
      group_by(Location) %>%
      summarize("MinDateTimeUTC" = min(DateTimeUTC),
                "MaxDateTimeUTC" = max(DateTimeUTC)) %>% 
      collect()
  }
  
  summary <- df %>% dplyr::arrange(Location)
  
  summary$MinDateTimeUTC <- format(as.POSIXct(summary$MinDateTimeUTC, tz = "UTC"), format = '%F %R %Z')
  summary$MaxDateTimeUTC <- format(as.POSIXct(summary$MaxDateTimeUTC, tz = "UTC"), format = '%F %R %Z')
  
  dbDisconnect(con)
  rm(con)
  return(summary)
}
# parameter <- "Stage_ft"
# data_correct_summary(parameter)
########################################################################.
###                          PREVIEW PLOT                          ####
########################################################################.

preview_plot <- function(loc, par, sum_loc, df_mayfly, df_hobo, df_fp, df_trib_mon) {
  
  ###  FILTER DATA  ####
  
  mayfly_cols <- switch(par,
                        "Stage_ft" = c(1:7), 
                        "Conductivity_uScm" = c(1:3,8,9))
  
  min_dt <- as.POSIXct(sum_loc$MinDateTimeUTC, tz = "UTC")
  max_dt<- as.POSIXct(sum_loc$MaxDateTimeUTC, tz = "UTC")  
  
  short_loc <- substrRight(loc, 4)  
  
  df_hobo <- df_hobo %>% 
    filter(between(DateTimeUTC, min_dt, max_dt))
  
  df_mayfly <- df_mayfly %>% 
    filter(Location == short_loc, between(DateTimeUTC, min_dt, max_dt)) %>% 
    select(all_of(mayfly_cols))
  
  df_fp <- df_fp %>% 
    filter(Location == short_loc,
           Parameter == ifelse(par == "Stage_ft", "Staff Gauge Height", "Specific Conductance"),
           between(DateTimeUTC, min_dt - hours(3), max_dt + hours(3)))
  
  manual_stage_times <- df_fp %>% 
    use_series(DateTimeUTC)
  
  # manual_stage_times
  
  ### Generate Mayfly cleaning times for vertical lines on the plot (For conductivity correction)  
  # mayfly_cleanings_dt_UTC <- df_trib_mon %>% 
  #   filter(Location == loc, Mayfly_Cleaned == TRUE) %>% 
  #   mutate("cleaningDateTimeUTC" = as_datetime(glue("{FieldObsDate} {Mayfly_DownloadTimeUTC}"))) %>% 
  #   pull(cleaningDateTimeUTC)
  
  ########################################################################.
  ###           INITIAL EXPLORATORY PLOT FOR STAGE CORRECTION         ####
  ########################################################################.
  # dygraph showing full range of Raw and Corrected data compared to HOBO
  
  max_dt_UTC <- max(df_mayfly$DateTimeUTC, na.rm = TRUE)
  
  y_label <- ifelse(par == "Stage_ft", "Stage (ft)", "Conductivity (\u03BCS/cm)")
  
  # print(names(df_mayfly))
  # print(names(df_hobo))
  # print(names(df_fp))
  
  dg <- left_join(df_mayfly[ , c("DateTimeUTC", "RawStage_ft")], df_hobo[ , c("DateTimeUTC", "Stage_ft")]) %>%
    full_join(df_fp[ , c("DateTimeUTC", "FinalResult")]) %>% 
    distinct()
  
  names(dg) <- c("DateTimeUTC", "Raw_Mayfly", "HOBO", "Manual")
  
  ### Join in the flag data so estimated values can be a different series
  p  <- xts(dg, order.by = dg$DateTimeUTC, tzone = "UTC")
  # x_label <- strftime(p$DateTimeUTC,format = "%B-%m \n%Y")
  
  plot <- dygraph(p[,-1], main = glue("Mayfly {par} (raw) at {loc}")) %>%
    dyOptions(useDataTimezone = TRUE, axisLineWidth = 1.5, fillGraph = FALSE, pointSize = 3, colors = c("blue", "green", "orange" ,"red")) %>%
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 1, hideOnMouseOut = FALSE)  %>%
    dyAxis(name = "y", label = y_label, valueRange = c(0, ceiling(max(dg$HOBO, dg$Manual, dg$Raw_Mayfly, na.rm = TRUE)))) %>%
    dyRangeSelector(dateWindow = c(max_dt_UTC - months(1), max_dt_UTC + hours(5)), strokeColor = '') %>%
    dyCrosshair(direction = "vertical")
  
  return(plot)
}

### Manual Testing - DOES NOT PLOT
# loc <-  "MD01"
# par <-  "Stage_ft"
# model_start_time <- as_datetime("2021-11-05 18:30:00")
# model_end_time <-   as_datetime("2021-11-15 18:45:00")
# 
# df_hobo = db_hobo %>%
#   filter(Location == loc) %>%
#   select(3,6)
# 
# df <- db_mayfly %>%
#   select(c(2:6)) %>%
#   filter(Location == loc,
#          between(DateTimeUTC, model_start_time, model_end_time))
# coeff_a <- 0.11
# mult <- 0.33
# pow <- 1.12
# stage_target <- 1.8
# final_offset <-  0
# df_fp <- df_fp %>% 
#   filter(Location == loc,
#          Parameter == "Staff Gauge Height",
#          between(DateTimeUTC, model_start_time, model_end_time))
         
# sum_loc <- data_correct_summary(df = db_mayfly, parameter = par) %>% filter(Location == substrRight(loc,4))
# preview_plot(loc = loc, par = par, sum_loc, df_mayfly = db_mayfly, df_hobo = db_hobo, df_fp, df_trib_mon = df_trib_monitoring)


########################################################################.
###                          TEMP CORRECTION                         ####
########################################################################.

MF_TEMP_CORRECT <- function(df, df_hobo, df_fp, coeff_a, mult, pow, stage_target, drift, final_offset) {

  ### Get loc
  loc <- df$Location[1]  
    
  ### Apply temp correction
  df_mayfly_corrected <- df %>%
    mutate(Stage_ft = RawStage_ft - ((30 - Logger_temp_c) * coeff_a)^pow * mult)
  
  offset <- df_mayfly_corrected$Stage_ft[nrow(df_mayfly_corrected)] - stage_target
  
  ### Apply a fouling offset, where correction is pro-rated over time so that correction matches first record to stage at download
  
  drift_corr <- seq((drift/nrow(df_mayfly_corrected)), drift, by=(drift/nrow(df_mayfly_corrected)))
  df_mayfly_corrected$Stage_ft <- df_mayfly_corrected$Stage_ft - drift_corr
  
  names(df_hobo) <- c("DateTimeUTC", "HOBO")
  ### Apply offset to match manual times
  df_mayfly_corrected$Stage_ft <- round(df_mayfly_corrected$Stage_ft - offset - final_offset, 2)

  time_start <- min(df_mayfly_corrected$DateTimeUTC)
  time_end <- max(df_mayfly_corrected$DateTimeUTC)
  
  dg2 <- left_join(df_mayfly_corrected[ , c("DateTimeUTC", "RawStage_ft", "Stage_ft")], df_hobo, by ="DateTimeUTC") %>%
    full_join(df_fp[ , c("DateTimeUTC", "FinalResult")]) %>% distinct()
  
  names(dg2) <- c("DateTimeUTC", "Raw_Mayfly","Corrected_Mayfly", "HOBO", "Manual")
  
  ### Join in the flag data so estimated values can be a different series
  p  <- xts(dg2, order.by = dg2$DateTimeUTC, tzone = "UTC")
  # x_label <- strftime(p$DateTimeUTC,format = "%B-%m \n%Y")
  
  plot <- dygraph(p[,-1], main = glue("Mayfly Stage Correction at {loc}")) %>%
    dyOptions(useDataTimezone = TRUE, axisLineWidth = 1.5, fillGraph = FALSE, pointSize = 3, colors = c("blue", "green", "orange" ,"red")) %>%
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 1, hideOnMouseOut = FALSE)  %>%
    dyAxis(name = "y", label = "Stage (ft)", valueRange = c(0, max(dg2$Corrected_Mayfly, na.rm = TRUE) + 0.2)) %>%
    dyRangeSelector(dateWindow = c(time_start - hours(1), time_end + hours(1)), strokeColor = '') %>%
    dyCrosshair(direction = "vertical")
  # plot2
  dfs <- list(
    "df" = df_mayfly_corrected,
    "plot_corrected" = plot
  )
  return(dfs)
}

### Run temp correction function
# dfs <- MF_TEMP_CORRECT(df = df, 
#                        df_hobo = df_hobo, 
#                        df_fp = df_fp , 
#                        coeff_a = coeff_a, 
#                        pow = pow, 
#                        mult = mult, 
#                        stage_target = stage_target, 
#                        final_offset = final_offset)

########################################################################.
###                       PROCESS CORRECTED DATA                    ####
########################################################################.

# df_mayfly <- db_mayfly %>%
#   filter(Location == loc,
#          between(DateTimeUTC, model_start_time, model_end_time))
# df_corrected <- dfs[[1]]
# username <- username
# userlocation <- userlocation

PROCESS_CORRECTED_MAYFLY <- function(df_mayfly, df_corrected, username, userlocation) {
  
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
  
  # Option 1 - Delete/Append
  
  # odbc::dbWriteTable(con, DBI::SQL(glue("{database}.{schema}.{mayfly_tbl}")), value = df_mayfly, append = TRUE)
  
  
  # Option 2 - Updates
  
  update_ids <- df_mayfly$ID
  stages <- df_mayfly$Stage_ft
  discharges <- df_mayfly$Discharge_cfs
  
  qry_part1 <- glue("UPDATE [{schema}].[{mayfly_tbl}] SET [Stage_ft] = {stages}, [Discharge_cfs] = {discharges}")
  # qry_part1
  qry_part2 <-  glue(" WHERE [ID] = {update_ids}")
  # qry_part2
  # Add index on Location and include in update statement 
  # Make temp table and join 
  
  qry_update <- str_c(qry_part1, qry_part2)
  # qry_update
  # Step 3 - Run the update query - will run each updated record individually
  
  for(i in qry_update){
    # print(i)
    odbc::dbGetQuery(con, i)
  }
  
  # Flag data
  if ("data.frame" %in% class(df_flags)){ # Check and make sure there is flag data to import
    print("Importing flags...")
    odbc::dbWriteTable(con, DBI::SQL(glue("{database}.{schema}.{ImportFlagTable}")), value = df_flags, append = TRUE)
  } else {
    print("No flags to import")
  }
  
  # Disconnect from db and remove connection obj
  dbDisconnect(con)
  rm(con)
  
  SendEmail(df = df_mayfly, 
            table = mayfly_tbl, 
            file = "N/A", 
            emaillist = emaillist, 
            username = username, 
            userlocation = userlocation)
  
  print(paste0("Mayfly Data finished importing at ", Sys.time()))
  return("Import Successful")
}

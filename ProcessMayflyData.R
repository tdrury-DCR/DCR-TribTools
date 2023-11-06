################################### HEADER ###################################
#  TITLE: ProcessMayflyData
#  DESCRIPTION: Reads mayfly logger csv file, calculates discharge, reformats for database
#  AUTHOR(S):
#  DATE LAST UPDATED:
#  GIT REPO:
#  R version 3.5.3 (2019-03-11)  i386
##############################################################################.


# mayfly_files <- list.files(paste0(wach_team_root, config[["Mayfly_Staging"]])) %>% print()
# mayfly_file <- mayfly_files[1]
# username <- "Dan Crocker"
# stage <- 1.48 ### Enter stage at time of data download (Numeric entry in Shiny App)

PROCESS_MAYFLY <- function(mayfly_file, username, userlocation){
  
print(paste0("Mayfly data started processing at ", Sys.time()))
  
### Extract the location information from the Plot Title listed in the file
file <- paste0(mayfly_data_dir,"/", mayfly_file)

loc <- str_split_fixed(mayfly_file, "_", n = 2) 
loc <- loc[1] %>% str_replace("WACH-","")

df <- read_csv(file, skip = 7, guess_max = 100, ### skip lines to header
               col_types = cols(
                 `Date and Time in UTC-5` = col_character(),
                 Hydros21cond = col_double(),
                 Hydros21depth = col_double(),
                 Hydros21temp = col_double(),
               )) %>%   
  select(c(1:4)) %>% 
  drop_na() %>%
  mutate("Location" = loc, "ID" = NA_integer_)

names(df) <- c("DateTimeUTC", "RawConductivity_uScm", "RawStage_ft", "Logger_temp_c", "Location","ID")
### Note - the time offset is fixed to UTC-5, so add 5 hrs to get back to UTC

### Format Date-Time stamp - Need two tries here because Excel will mess with date formats 
if(str_detect(df$DateTimeUTC[1], "/")) {
  print("Dates formatted with slashes")
  df$DateTimeUTC <- mdy_hm(df$DateTimeUTC, tz = "UTC")
  # df$DateTimeUTC <- parse_date_time(df$DateTimeUTC,"%m/%d/%y %H:%M", tz = "UTC") 
} else {
  if(str_detect(df$DateTimeUTC[1], "-")) {
    print("Dates formatted with dashes")
    df$DateTimeUTC <- ymd_hms(df$DateTimeUTC, tz = "UTC")
    # df$DateTimeUTC <- parse_date_time(df$DateTimeUTC,"%y-%m-%d %H:%M:%S", tz = "UTC")
  }
}

### If Location = MDO2 Mayfly is set to UTC, so no tz offset required. 
if(!loc %in% c("MD02")) {
  df <- df %>% 
    mutate(DateTimeUTC = DateTimeUTC + hours(5)) 
}
  
### Filter out records where all Hydros21 values are -9999 
na_recs <- which(rowSums(df[,2:4])  == -29997) %>% as.numeric()

if(length(na_recs) > 0){
  print(paste0(length(na_recs), " NA records were removed from the data."))
  df <- df[-na_recs,]
} 

### Convert any remaining -9999 values to NA
df <- df %>% naniar::replace_with_na(replace = list(RawConductivity_uScm = -9999, RawStage_ft = -9999, Logger_temp_c = -9999))

### Convert Stage from mm to ft
df$RawStage_ft <- round(df$RawStage_ft/304.8, 3)

### Add columns for Final Conductivity, Stage, and Discharge ####
df <- df %>% 
  mutate("Conductivity_uScm" = NA_real_,
         "Stage_ft" = NA_real_,
         "Discharge_cfs" = NA_real_)


### Connect to db in UTC time
dsn <- 'DCR_DWSP_App_R'
database <- "DCR_DWSP"
tz <- 'UTC'
con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)

# database <- "DCR_DWSP"
# con <- dbConnect(odbc::odbc(), database, timezone = 'UTC')
schema <- userlocation                 
mayfly_tbl <- "tblMayfly"

### Get existing Mayfly data for dup check
mayfly_existing <- dbGetQuery(con, glue("SELECT * FROM [{schema}].[{mayfly_tbl}] WHERE 
                                  [Location] = '{loc}'"))
### Check for duplicate existing data in database
duplicates <- semi_join(df, mayfly_existing, by="DateTimeUTC")

if (nrow(duplicates) > 0){
  stop(paste0("This file duplicates ",nrow(duplicates)," existing ",loc," Mayfly records in the database."))
}

### A function to fetch record IDs from the database table and assign record IDs to the new data
setIDs <- function(){
  qry <- dbGetQuery(con, glue("SELECT max(ID) FROM [{schema}].[{mayfly_tbl}]"))
  
  ### Get current max ID
  if(is.na(qry)) {
    qry <- 0
  } else {
    qry <- qry
  }
  ID_max <- as.numeric(unlist(qry))
  rm(qry)
  
  ### Set IDs
  df$ID <- seq.int(nrow(df)) + ID_max
}
df$ID <- setIDs()

dbDisconnect(con)
rm(con)

df_flags <- NULL
### df_Stage ####

### Connect to db  in America/New_York tz
dsn <- 'DCR_DWSP_App_R'
database <- "DCR_DWSP"
tz <- 'America/New_York'
con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)
# con <- dbConnect(odbc::odbc(), database, timezone = 'America/New_York')

### Bring in stage, temperature, and specific conductance manual measurements
df_stage <- dbGetQuery(con, glue("SELECT [Location], [DateTimeET], [Parameter], [FinalResult] 
                                  FROM [{schema}].[tblTribFieldParameters] WHERE [Parameter] = 'Staff Gauge Height'
                                  AND [Location] = '{loc}'"))

df_temp  <- dbGetQuery(con, glue("SELECT [Location], [DateTimeET], [Parameter], [FinalResult] 
                                  FROM [{schema}].[tblTribFieldParameters] WHERE [Parameter] = 'Water Temperature'
                                  AND [Location] = '{loc}'"))

df_conductivity  <- dbGetQuery(con, glue("SELECT [Location], [DateTimeET], [Parameter], [FinalResult] 
                                  FROM [{schema}].[tblTribFieldParameters] WHERE [Parameter] = 'Specific Conductance'
                                  AND [Location] = '{loc}'"))

### Disconnect from db and remove connection obj
dbDisconnect(con)
rm(con)

###  Filter to the date range of the HOBO data being imported
df_stage <- df_stage %>% 
  filter(DateTimeET >= min(df$DateTimeUTC) - hours(2), ### Note - tzs are comparable since stage data is converted to UTC when read into R
         DateTimeET <= max(df$DateTimeUTC) + hours(2))

df_temp <- df_temp %>% 
  filter(DateTimeET >= min(df$DateTimeUTC) - hours(2), ### Note - tzs are comparable since stage data is converted to UTC when read into R
         DateTimeET <= max(df$DateTimeUTC) + hours(2))

df_conductivity <- df_conductivity %>% 
  filter(DateTimeET >= min(df$DateTimeUTC) - hours(2), ### Note - tzs are comparable since stage data is converted to UTC when read into R
         DateTimeET <= max(df$DateTimeUTC) + hours(2))

### df_prior ####

### Grab last 1 days records to plot with new data to check for missed data corrections
t <- min(df$DateTimeUTC)

tz <- 'UTC'
con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)

mayfly_prior <- dbGetQuery(con, glue("SELECT * FROM [{schema}].[{mayfly_tbl}] WHERE 
                                  [Location] = '{loc}'"))
  
# hobo_prior$DateTimeUTC <-  force_tz(hobo_prior$DateTimeUTC, tzone = "UTC") 
mayfly_prior <- filter(mayfly_prior, Location == loc, DateTimeUTC >= (t - 86400), DateTimeUTC < t)

### Reorder columns to match db
col_order <- c(dbListFields(con, schema_name = schema, name = mayfly_tbl))
df <-  df[col_order]

### Disconnect from db and remove connection obj
dbDisconnect(con)
rm(con)

dfs <- list(
  "df" = df,
  "df_flag" = NA,
  "df_prior" = mayfly_prior,
  "df_stage" = df_stage,
  "df_temp" = df_temp,
  "df_conductivity" = df_conductivity)

print(paste0("Mayfly Data finished processing at ", Sys.time()))

return(dfs)

}

### Run function locally, comment out when deployed in Shiny
# dfs <- PROCESS_MAYFLY(mayfly_file = mayfly_file, username = "Dan Crocker", userlocation = userlocation)

PREVIEW_MAYFLY <- function(df_mayfly, df_prior = NULL, df_stage = NULL, df_temp = NULL, df_conductivity = NULL, var2 = NULL) {
  
  pd <- df_mayfly
  
  loc <- df_mayfly %>% 
    slice(1) %>% 
    pull(Location)
  
  ### cols is used as the ordering of data in the plot legend. If you change the order, you have to change the code for each parameter below
  cols <- c("Raw Stage (ft)" = "darkgreen", #cols[1]
            "Raw Stage (ft) - prior" = "darkseagreen4", #cols[2]
            "Stage (ft) - manual" = "darkorange2", #cols[3]
            "Water Temperature (C)" = "purple4", #cols[4]
            "Water Temperature (C) - prior" = "orchid4", #cols[5]
            "Water Temperature (C) - manual" = "magenta", #cols[6]
            # "Discharge (cfs)" = "blue4",  #cols[7]
            # "Discharge (cfs) - prior" = "steelblue", #cols[8]
            "Raw Conductivity (uS/cm)" = "gray35", #cols[7]
            "Raw Conductivity (uS/cm) - prior" = "gray65", #cols[8]
            "Conductivity (uS/cm) - manual" = "red1" #cols[9]
  )
 
   ### Create empty vectors that will be filled based on the parameters on each plot
  cols_legend <- NULL
  linetype_legend <- NULL
  shape_legend <- NULL
  
  if(nrow(df_prior) == 0){
    prior <-  FALSE
  } else {
    prior <-  TRUE
  }
  
  y1lim <- if(nrow(df_stage) > 0){
                  max(c(pd$RawStage_ft,df_stage$FinalResult))
                    } else {max(pd$RawStage_ft)}
  
  y2lim <- switch (var2,
    "Temperature" = if(!is.null(df_temp) && nrow(df_temp) > 0){
                          max(c(pd$Logger_temp_c,df_temp$FinalResult)) 
                      } else {max(pd$Logger_temp_c)},
    "Conductivity" = if(!is.null(df_conductivity) && nrow(df_conductivity) > 0){
                          max(c(pd$RawConductivity_uScm,df_conductivity$FinalResult))
                      } else {max(pd$RawConductivity_uScm)}#,
    # "Discharge" = max(pd$Discharge_cfs)
  )
  
  y2lab <- switch (var2,
      "Temperature" = "Temperature (C)",
      "Conductivity" = "Specific Conductance (uS/cm)"
      # "Discharge" = "Discharge (cfs)"
  )
  
  title <- switch (var2,
    "Temperature" = paste0("Stage and Water Temperature at Location ", loc),
    "Conductivity" = paste0("Stage and Specific Conductance at Location ", loc)
    # "Discharge" = paste0("Stage and Discharge at Location ", loc)
  )
  
  mult <- y1lim / abs(y2lim)
  
  plot  <- ggplot(pd, aes(x = DateTimeUTC)) +
    geom_line(aes(y = RawStage_ft, color = "Raw Stage (ft)"), linewidth = 1)  
  plot <- switch (var2,
        "Temperature" = plot + geom_line(aes(y = Logger_temp_c * mult, color = "Water Temperature (C)"), linewidth = 1),
        "Conductivity" = plot + geom_line(aes(y = RawConductivity_uScm * mult, color = "Raw Conductivity (uS/cm)"), linewidth = 1)
        # "Discharge" = plot + geom_line(aes(y = Discharge_cfs * mult, color = "Discharge (cfs)"), linewidth = 1)
    )
  ### Add legend items with colors for data being added to plot in this step
  cols_legend <- append(cols_legend,
                  switch(var2,
                         "Temperature" = c(cols[4], cols[1]),
                         "Conductivity" = c(cols[7],cols[1])))
                         # "Discharge" = c(cols[7],cols[1])))
  ### Add the linetype data being added to plot in this step (solid for line, NA for points)
  linetype_legend <- append(linetype_legend,
                            switch(var2,
                                   "Temperature" = c("Water Temperature (C)" = "solid", 
                                                     "Raw Stage (ft)" = "solid"),
                                   "Conductivity" = c("Raw Conductivity (uS/cm)" = "solid", 
                                                      "Raw Stage (ft)" = "solid")))
                                   # "Discharge" = c("Discharge (cfs)" = "solid", 
                                   #                 "Stage (ft)" = "solid")))
  
  ### Add the shape of the point being added to the plot in this step (NA for lines, 19 for points)
  shape_legend <- append(shape_legend,
                            switch(var2,
                                   "Temperature" = c("Water Temperature (C)" = NA, 
                                                     "Raw Stage (ft)" = NA),
                                   "Conductivity" = c("Raw Conductivity (uS/cm)" = NA, 
                                                      "Raw Stage (ft)" = NA)))
                                   # "Discharge" = c("Discharge (cfs)" = NA, 
                                   #                 "Stage (ft)" = NA)))

  # Check for prior data to plot 
  if(isTRUE(prior)){
    plot <- plot +  
      geom_line(data = df_prior, aes(x = DateTimeUTC, y = RawStage_ft, color = "Raw Stage (ft) - prior"), linewidth = 1) +
      geom_vline(xintercept = min(pd$DateTimeUTC), color = "gray10", linetype = 2, linewidth = 1.5, alpha = 0.8)
    plot <- switch (var2,
                    "Temperature" = plot + geom_line(data = df_prior, 
                                                     aes(x = DateTimeUTC, 
                                                         y = Logger_temp_c * mult, 
                                                         color = "Water Temperature (C) - prior"), 
                                                     linewidth = 1),
                    "Conductivity" = plot + geom_line(data = df_prior, 
                                                      aes(x = DateTimeUTC, 
                                                          y = RawConductivity_uScm * mult, 
                                                          color = "Raw Conductivity (uS/cm) - prior"), 
                                                      linewidth = 1))
    # "Discharge" = plot + geom_line(data = df_prior, aes(x = DateTimeUTC, y = Discharge_cfs * mult, color = "Discharge (cfs) - prior"), linewidth = 1) 
    ### Add legend items with colors for data being added to plot in this step
    cols_legend <- append(cols_legend,
                          switch(var2,
                                 "Temperature" = c(cols[5],cols[2]),
                                 "Conductivity" = c(cols[8],cols[2])))
    # "Discharge" = c(cols[8],cols[2])))
    ### Add the linetype data being added to plot in this step (solid for line, NA for points)
    
    linetype_legend <- append(linetype_legend,
                              switch(var2,
                                     "Temperature" = c("Water Temperature (C) - prior" = "solid",
                                                       "Raw Stage (ft) - prior" = "solid"),
                                     "Conductivity" = c("Conductivity (uS/cm) - prior" = "solid",
                                                        "Raw Stage (ft) - prior" = "solid")))
    # "Discharge" = c("Discharge (cfs) - prior" = "solid",
    #                 "Stage (ft) - prior" = "solid")))
    ### Add the shape of the point being added to the plot in this step (NA for lines, 19 for points)
    shape_legend <- append(shape_legend,
                           switch(var2,
                                  "Temperature" = c("Water Temperature (C) - prior" = NA,
                                                    "Raw Stage (ft) - prior" = NA),
                                  "Conductivity" = c("Conductivity (uS/cm) - prior" = NA,
                                                     "Raw Stage (ft) - prior" = NA)))
    # "Discharge" = c("Discharge (cfs) - prior" = NA,
    #                 "Stage (ft) - prior" = NA)))
    
  }
  if(nrow(df_stage) > 0){
    plot <- plot + 
      geom_point(data = df_stage, aes(x = DateTimeET, y = FinalResult, color = "Stage (ft) - manual"), size = 2)
    ### Add legend items with colors for data being added to plot in this step
    cols_legend <- append(cols_legend,c(cols[3]))
    ### Add the linetype data being added to plot in this step (solid for line, NA for points)
    linetype_legend <- append(linetype_legend,c("Stage (ft) - manual" = "blank"))
    ### Add the shape of the point being added to the plot in this step (NA for lines, 19 for points)
    shape_legend <- append(shape_legend,c("Stage (ft) - manual" = 19)) 
  } ### NOTE Manual stage gets converted to UTC during import, so it is plotted correctly on the x-axis in UTC time along with sensor data
  # plot
  
  if(!is.null(df_temp) && nrow(df_temp) > 0 && var2=="Temperature") {
    plot <- plot + 
      geom_point(data = df_temp, aes(x = DateTimeET, y = FinalResult*mult, color = "Water Temperature (C) - manual"), size = 2)
    ### Add legend items with colors for data being added to plot in this step
    cols_legend <- append(cols_legend,c(cols[6]))
    ### Add the linetype data being added to plot in this step (solid for line, NA for points)
    linetype_legend <- append(linetype_legend,c("Water Temperature (C) - manual" = "blank"))
    ### Add the shape of the point being added to the plot in this step (NA for lines, 19 for points)
    shape_legend <- append(shape_legend,c("Water Temperature (C) - manual" = 19))  
  }
  # plot
  if(!is.null(df_conductivity) && nrow(df_conductivity) > 0 && var2=="Conductivity") {
    plot <- plot + 
      geom_point(data = df_conductivity, aes(x = DateTimeET, y = FinalResult * mult, color = "Conductivity (uS/cm) - manual"), size = 2)
    
    ### Add legend items with colors for data being added to plot in this step
    cols_legend <- append(cols_legend,c(cols[9]))
    ### Add the linetype data being added to plot in this step (solid for line, NA for points)
    linetype_legend <- append(linetype_legend,c("Conductivity (uS/cm) - manual" = "blank"))
    ### Add the shape of the point being added to the plot in this step (NA for lines, 19 for points)
    shape_legend <- append(shape_legend,c("Conductivity (uS/cm) - manual" = 19))  
  }
  
  plot <- plot +    
    scale_y_continuous(breaks = pretty_breaks(),limits = c(0, 1.2 * y1lim), 
                       sec.axis = sec_axis(~./mult, breaks = pretty_breaks(), name = y2lab)) +
    scale_x_datetime(breaks = pretty_breaks(n=12)) + 
    scale_colour_manual(values = cols_legend[order(factor(names(cols_legend),levels = names(cols)))], #orders legend items and colors based on cols order
                        guide = guide_legend(override.aes = list(
                          linetype = linetype_legend[order(factor(names(linetype_legend),levels = names(cols)))], #orders legend linetypes based on cols order
                          shape = shape_legend[order(factor(names(shape_legend),levels = names(cols)))]))) + #orders legend point shapes based on cols order
        labs(y = "Stage (ft)",
         x = "Date",
         colour = "") +
    ggtitle(title) +
    theme_linedraw() +
    theme(plot.title = element_text(color= "black", face="bold", size=14, vjust = 1, hjust = 0.5),
          legend.position = "bottom",
          legend.text = element_text(margin = margin(r=0.8, unit="cm")),
          axis.title.x = element_text(angle = 0, face = "bold", color = "black"),
          axis.title.y = element_text(angle = 90, face = "bold", color = "black"))
  
  # plot
  return(plot)
}

# df_mayfly <- dfs[[1]]
# df_stage <- dfs[[4]]
# df_prior <- dfs[[3]]
# df_temp <-   dfs[[5]]
# df_conductivity <- dfs[[6]]
# var2 <- "Temperature"
# plot <- PREVIEW_MAYFLY(df_mayfly = df_mayfly, df_stage = df_stage, df_prior = df_prior, var2 = var2)
# plot
# # Comment out if running in shiny
# df_mayfly <- PROCESS_HOBO(hobo_file = hobo_file, stage = stage)

IMPORT_MAYFLY <- function(df_mayfly, mayfly_file, userlocation){
  print(paste0("Mayfly Data started importing at ", Sys.time()))
  
  file <- paste0(mayfly_data_dir,"/", mayfly_file)
  
  loc <- str_split_fixed(mayfly_file, "_", n = 2) 
  loc <- loc[1] %>% str_replace("WACH-","")
  
  mayfly_tbl <- "tblMayfly"
  
  dsn <- 'DCR_DWSP_App_R'
  database <- "DCR_DWSP"
  tz <- 'UTC'
  con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)
  schema <- userlocation
  
  odbc::dbWriteTable(con, DBI::SQL(glue("{database}.{schema}.{mayfly_tbl}")), value = df_mayfly, append = TRUE)
  
  # Disconnect from db and remove connection obj
  dbDisconnect(con)
  rm(con)
  
  ### Move the processed raw mayfly data file to the appropriate processed folder
  write_dir <- paste0(mayfly_data_processed, "/", loc)
  dir.create(write_dir)
 
  file.rename(file, paste0(write_dir, "/", mayfly_file))
  
  SendEmail(df=df_mayfly, table=mayfly_tbl, file=mayfly_file, emaillist=emaillist, username=username, userlocation=userlocation)
  
  print(paste0("Mayfly Data finished importing at ", Sys.time()))
  return("Import Successful")
}
  
  
  
  
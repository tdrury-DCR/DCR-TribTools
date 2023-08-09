###############################  HEADER  ######################################
#  TITLE: Ratings.R
#  DESCRIPTION: This script uses stage and discharge data to make rating curves
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: 2020-12-30
#  GIT REPO: 
#  R version 3.5.3 (2019-03-11)  x86_64
##############################################################################.

library(tidyverse)
library(odbc)
library(DBI)
library(stats)
library(nlstools)
# 
# #### Important Notes:
# # Analysis sequence
# # 1. pull in all required data: existing ratings (offset), discharge measurements 
# # 2. Select a station of interest
# # 3. Plot the stage - discharge and inspect relationship
#     # If there is an existing two part rating, then split the measurements into two groups and duplicate steps below for each group
# # 4. Cull out observations that do not apply to current rating - i.e. old measurements, poor quality, bad conditions, 
# # 5. Run the Bayesian model to predict the equation coefficients 
# # 6. Compare the new coefficients to the existing coefficients (if exist)
# # 7. Run a subset of stage data using both sets of coefficients - if more than 5% difference in predicted discharge then update the rating
# # 8. Decide if previous discharges need to be recalculated 
# # 9. Update the rating in tblRatings if nessesary
# #10. Record the 
# 
# 
## Get data configs from Launch File ####
#
### Set db for connection ####
## CONNECT TO A FRONT-END DATABASE ####
### Set DB
#   database <- 'DCR_DWSP'
#   schema <- 'Wachusett'
#   tz <- 'America/New_York'
#   ### Connect to Database
#   con <- dbConnect(odbc::odbc(), database, timezone = tz)
#   #
# 
# tbl_ratings <- dbReadTable(con, Id(schema = schema, table = 'tblRatings'))
# tbl_discharges <- dbReadTable(con, Id(schema = schema, table = 'tblDischargeMeasurements'))
# dbDisconnect(con)
# rm(con)

# tbl_ratings <- df_ratings
# tbl_discharges <- df_discharges
  
# ### FUNCTION ARGS ####
# locs <- unique(tbl_discharges$Location)
# locs # Look at the locations
# loc <- locs[9] # Pick a location
# ratingNo <-  1.02
# drop_meas <- NA
# offset1 <- 0.34
# offset2 <- 0.34
# offset3 <- 0.34
# break1 <- 0.75
# break2 <- 0.88
# loc <- "FHLN"
# new_rating <- 1.03
# axes = FALSE
# tbl_discharges <- disch
#_____________________________________________________________________________________________________________________________
### NEW RATING FROM MEASUREMENTS ####
MAKE_RATING <- function(tbl_discharges, tbl_ratings, loc, offset1, axes, drop_meas = NA, break1 = NA, offset2 =  NA, break2 = NA, offset3 = NA, new_rating = NA){
  
  # A function to pull characters from the right side of a string
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
# tbl_measurements <- data  
  
# Change break values of zero to NA
  
break1 <- ifelse(break1 == 0, NA, break1)  
break2 <- ifelse(break2 == 0, NA, break2)  
offset1 <- ifelse(offset1 == 0, NA, offset1)  
offset2 <- ifelse(offset2 == 0, NA, offset2)  
offset3 <- ifelse(offset3 == 0, NA, offset3)  
  
tbl_ratings <- tbl_ratings %>% 
  mutate(RatingDatumOffset = ifelse(is.na(RatingDatumOffset), 0, RatingDatumOffset))
  
quality <- c("Fair" = 70, "Good" = 85, "Excellent" = 100, "Poor" = 0)
  
loc_ratings <- tbl_ratings %>% filter(MWRA_Loc == substrRight(loc, 4))

current_rating <- tbl_ratings %>% filter(MWRA_Loc == substrRight(loc, 4), IsCurrent == TRUE)
active_rating <- current_rating$RatingNum

if(is.na(new_rating)){
  new_rating <- active_rating + 0.01
} 
  
### Filter discharge measurements for location of interest and only the measurements valid for the most recent rating (highest whole number)
### Also filter out any poor quality measurements

data1 <- tbl_discharges %>% 
  filter(Location == loc) %>%
  dplyr::select(c(2,4:11)) %>%
  mutate(Stage_ft = rowMeans(dplyr::select(.,starts_with("Stage")), na.rm = TRUE),
         Measurement_Weight = quality[Measurement_Rated]) %>%
  filter(RatingNumber > floor(new_rating), RatingNumber <= new_rating)

data1$Measurement_Weight <- replace_na(data1$Measurement_Weight, 70)
data1$Measurement_Rated <- replace_na(data1$Measurement_Rated, "NA")

if(!is.na(drop_meas)){
  data1 <- filter(data1, !MeasurementNumber %in% drop_meas)
}

apply_offset <- function(.x, .y){
  # correct stage datum to match current rating datum
  if(.x < new_rating){
    round(.y + loc_ratings$RatingDatumOffset[loc_ratings$RatingNum == .x],2)
  } else {
    .y
  }
}

data1 <- data1 %>% 
  mutate(Stage_ft = map2_dbl(.x = RatingNumber, .y = Stage_ft, apply_offset))
  
########################################################################.
###                 Develop a rating equation                       ####
########################################################################.

# Q = C*(h-a)^n, where C is a constant, h is head, a is an offset (pzf) and n is an exponent

gaugings <- data1[,c("Stage_ft","Discharge_cfs","MeasurementNumber","Measurement_Rated", "DateTimeStartET")]
names(gaugings) <- c("stage", "discharge","num","Measurement_Rated", "DateTimeStartET")

### Assign parts for rating ####
if(!is.na(break2)){
  parts <- 3
} else if(!is.na(break1)){
  parts <- 2
} else {
  parts <- 1
}
### A Single part rating  ####  
r_1part <- function(gaugings, offset1){

  stage1 <- gaugings$stage
  discharge1 <- gaugings$discharge
  weight <- gaugings$Measurement_Weight
### Part 1 ####  
  # Fitting the power law
  # Note that the start argument requires a list with initial estimates of the coefficients to be estimated
  power.nls <- nls(discharge1 ~ C * (stage1 - offset1)^n, data = gaugings, start = list(C = 1, n = 2), weights = weight)
  
  ### Generate confidence intervals from regression ###
  conf_int1 <- confint2(object = power.nls, level = 0.95)
  # Viewing the model summary and accessing estimated constants
  t_sum1 <- summary(power.nls)
  C1 <- round(coef(power.nls)["C"],4)
  a1 <- offset1
  n1 <-round(coef(power.nls)["n"], 4)
  eq1 <- paste0("Q = ",C1,"*(h-",a1,")^",n1)

### Add all coefficients, summaries, confidence intervals, and equations to list r  
  r <- list(C1 = C1,
            a1 = a1,
            n1 = n1,
            t_sum1 = t_sum1,
            conf_int1 = conf_int1,
            eq1 = eq1
  )
  return(r) 
}
### A two part rating ####
r_2part <- function(gaugings, offset1, break1, offset2){
  
  gaugings1 <- gaugings[gaugings$stage < break1,] 
  gaugings2 <- gaugings[gaugings$stage >= break1,]
  
  stage1 <- gaugings1$stage
  discharge1 <- gaugings1$discharge
  weight1 <- gaugings1$Measurement_Weight
  
  stage2 <- gaugings2$stage
  discharge2 <- gaugings2$discharge
  weight2 <- gaugings2$Measurement_Weight
  
### Part 1 ####  
  # Fitting the power law
  # Note that the start argument requires a list with initial estimates of the coefficients to be estimated
  power.nls1 <- nls(discharge ~ C * (stage - offset1)^n, data = gaugings1, start = list(C = 1, n = 2), weights = weight1)
  
  ### Generate confidence intervals from regression ###
  conf_int1 <- confint2(object = power.nls1, level = 0.95)
  # Viewing the model summary and accessing estimated constants
  t_sum1 <- summary(power.nls1)
  C1 <- round(coef(power.nls1)["C"],4)
  a1 <- offset1
  n1 <-round(coef(power.nls1)["n"], 4)
  eq1 <- paste0("Q = ",C1,"*(h-",a1,")^",n1)
  
### Part 2 ####    
  power.nls2 <- nls(discharge2 ~ C * (stage2 - offset2)^n, data = gaugings2, start = list(C = 1, n = 2),weights = weight2)
  
  ### Generate confidence intervals from regression ###
  conf_int2 <- confint2(object = power.nls2, level = 0.95)
  # Viewing the model summary and accessing estimated constants
  t_sum2 <- summary(power.nls2)
  C2 <- round(coef(power.nls2)["C"],4)
  a2 <- offset1
  n2 <-round(coef(power.nls2)["n"], 4)
  eq2 <- paste0("Q = ",C2,"*(h-",a2,")^",n2)
  
### Add all coefficients, summaries, confidence intervals, and equations to list r
  r <- list(C1 = C1,
            a1 = a1,
            n1 = n1,
            t_sum1 = t_sum1,
            conf_int1 = conf_int1,
            eq1 = eq1,
            C2 = C2,
            a2 = a2,
            n2 = n2,
            t_sum2 = t_sum2,
            conf_int2 = conf_int2,
            eq2 = eq2
  )
  return(r) 
}

### A three part rating ####
r_3part <- function(gaugings, offset1, break1, offset2, break2, offset3){

  gaugings1 <- gaugings[gaugings$stage < break1,]
  gaugings2 <- gaugings[gaugings$stage >= break1 & gaugings$stage < break2,]
  gaugings3 <- gaugings[gaugings$stage >= break2,]

  stage1 <- gaugings1$stage
  discharge1 <- gaugings1$discharge
  weight1 <- gaugings1$Measurement_Weight

  stage2 <- gaugings2$stage
  discharge2 <- gaugings2$discharge
  weight2 <- gaugings2$Measurement_Weight

  stage3 <- gaugings3$stage
  discharge3 <- gaugings3$discharge
  weight3 <- gaugings3$Measurement_Weight

### Part 1 ####
  # Fitting the power law
  # Note that the start argument requires a list with initial estimates of the coefficients to be estimated
  power.nls1 <- nls(discharge1 ~ C * (stage1 - offset1)^n, data = gaugings1, start = list(C = 1, n = 2), weights = weight1)

  ### Generate confidence intervals from regression ###
  conf_int1 <- confint2(object = power.nls1, level = 0.95)
  # Viewing the model summary and accessing estimated constants
  t_sum1 <- summary(power.nls1)
  C1 <- round(coef(power.nls1)["C"],4)
  a1 <- offset1
  n1 <-round(coef(power.nls1)["n"], 4)
  eq1 <- paste0("Q = ",C1,"*(h-",a1,")^",n1)

### Part 2 ####
  power.nls2 <- nls(discharge2 ~ C * (stage2 - offset2)^n, data = gaugings2, start = list(C = 1, n = 2), weights = weight2)

  ### Generate confidence intervals from regression ###
  conf_int2 <- confint2(object = power.nls2, level = 0.95)
  # Viewing the model summary and accessing estimated constants
  t_sum2 <- summary(power.nls2)
  C2 <- round(coef(power.nls2)["C"],4)
  a2 <- offset2
  n2 <-round(coef(power.nls2)["n"], 4)
  eq2 <- paste0("Q = ",C2,"*(h-",a2,")^",n2)

### Part 3 ####
  power.nls3 <- nls(discharge3 ~ C * (stage3 - offset3)^n, data = gaugings3, start = list(C = 1, n = 2), weights = weight3)

  ### Generate confidence intervals from regression ###
  conf_int3 <- confint2(object = power.nls3, level = 0.95)
  # Viewing the model summary and accessing estimated constants
  t_sum3 <- summary(power.nls3)
  C3 <- round(coef(power.nls3)["C"],4)
  a3 <- offset3
  n3 <-round(coef(power.nls3)["n"], 4)
  eq3 <- paste0("Q = ",C3,"*(h-",a3,")^",n3)

  ### Add all coefficients, summaries, confidence intervals, and equations to list r
  r <- list(C1 = C1,
            a1 = a1,
            n1 = n1,
            t_sum1 = t_sum1,
            conf_int1 = conf_int1,
            eq1 = eq1,
            C2 = C2,
            a2 = a2,
            n2 = n2,
            t_sum2 = t_sum2,
            conf_int2 = conf_int2,
            eq2 = eq2,
            C3 = C3,
            a3 = a3,
            n3 = n3,
            t_sum3 = t_sum3,
            conf_int3 = conf_int3,
            eq3 = eq3)

  return(r)
}

### Run nls function to make rating depending on how many parts the rating has ####
### Each function returns a list of a differing number of coefficients

findq <- function(stage, C, n, a) {
  C*(stage-a)^n
}
### Get the rating bounds, parts, then create a sequence of stage values to calculate rating curve
minstage <- offset1
maxstage <- round(max(gaugings$stage) + 0.25, digits = 1)

stages <- seq(minstage, maxstage, by = 0.02)

if(parts == 1){
  break1 <- NA
  break2 <- NA
  r <- r_1part(gaugings, offset1)
} else if(parts == 2) {
  break1 <- break1
  break2 <- NA
  r <-  r_2part(gaugings, offset1, break1, offset2)
} else {
  break1 <- break1
  break2 <- break2
  r <- r_3part(gaugings, offset1, break1, offset2, break2, offset3)
}
### Make list of stages, add extra variables and convert to tibble ####
l <- list(stage = stages, part = NA, Q = NA, lower = NA, upper = NA)
df_Q <- as_tibble(l)

### Assign the rating part to each stage ####
part <- function(x){  
  if(is.na(break1)){# There are no breaks, part is 1
    1
  } else if(is.na(break2)){ # There are only 2 parts, so check if part 1 or 2
    if(x < break1){
      1 
      } else {
        2
      }
  } else { # rating has 3 pars
    if(x < break1){
      1 
    } else if(x < break2){
      2
    } else {
      3
    }
  }
}

x <- df_Q$stage
df_Q$part <- mapply(part,x) %>% as.numeric()

# LOOP THROUGH ALL STAGE VALUES AND CALCULATE THE DISCHARGE USING RATING COEFFICIENTS ####
for (i in seq_along(df_Q$Q)) {
  i <- as.numeric(i)
  if(df_Q$stage[i] < minstage) { # Stage is below the rating curve (PZF) assign flow of zero and move to next record
    df_Q$Q[i] <- 0
  } #else {
    # if(df_Q$stage[i] > maxstage) { # Stage is above the rating curve and cannot be calculated -
    #   # Set the stage to the max stage 
    #   df_Q$stage[i] <- maxstage
    # }
    # Rating Coefficients part 1 ####
    C1 <- r$C1
    a1 <- r$a1
    n1 <- r$n1
    conf_int1 <- r$conf_int1
    # Rating Coefficients part 2 ####
    C2 <- r$C2
    a2 <- r$a2
    n2 <- r$n2
    conf_int2 <-  r$conf_int2
    # Rating Coefficients part 3 ####
    C3 <- r$C3
    a3 <- r$a3
    n3 <- r$n3
    conf_int3 <- r$conf_int3
    # Select the correct coefficient based on the part ####
    C <- paste0("C", df_Q$part[i])
    a <- paste0("a", df_Q$part[i])
    n <- paste0("n", df_Q$part[i])
    
    conf_int <- paste0("conf_int", df_Q$part[i])
  
### Use findq function to calculate discharge from each stage as well as upper and lower confidence bounds ####
  df_Q$Q[i] <- findq(stage = df_Q$stage[i], C = get(C), a = get(a), n = get(n))
  df_Q$lower[i] <- findq(stage = df_Q$stage[i], C = get(conf_int)[1,1], a = get(a), n = get(conf_int)[2,1])
  df_Q$upper[i] <- findq(stage = df_Q$stage[i], C = get(conf_int)[1,2], a = get(a), n = get(conf_int)[2,2])
}
# Round all the stage and discharge cols to 2 places
df_Q[,c(1,3:5)] <- round(df_Q[,c(1,3:5)], digits = 2)

### Make the plot ####
xmin <- 0
xmax <- ceiling(max(gaugings$discharge)+(0.1 * max(gaugings$discharge)))
ymin <- minstage
ymax <- maxstage
stages <- seq(ymin, ymax, by = 0.02)
title <- paste0("STAGE-DISCHARGE RATING CURVE FOR ", loc)
cols <- c("Poor" = "red", "Fair" = "orange", "Good" = "green", "Excellent" = "blue", "NA" = "black")

p <- ggplot() +
  geom_point(data = gaugings, aes(x=discharge, y=stage, color = Measurement_Rated,
                                  text = paste("Meas.No:", num, "<br>",
                                  "Stage:", stage, "<br>",
                                  "Discharge:",discharge,"<br>",
                                  "Date:", as_date(DateTimeStartET),"<br>",
                                  "Quality:", Measurement_Rated))) +
  geom_line(data = df_Q, aes(Q,stage), color = "red") +
  geom_line(data = df_Q, aes(lower,stage), color = "blue4", linetype = 3) +
  geom_line(data = df_Q, aes(upper,stage), color = "blue4", linetype = 3) +
  scale_color_manual(values = cols)

# Log Scale and Y-axis start at zero
if(axes == TRUE){
  p <- p +
    scale_x_log10(name = "Discharge (cfs)",limits = c(0.1,NA)) +
    scale_y_log10(name = "Stage (ft)")   
  } else {
    p <- p +
      scale_x_continuous(name = "Discharge (cfs)",limits = c(xmin,xmax)) +
      scale_y_continuous(name = "Stage (ft)", limits = c(ymin,ymax)) 
  }

p <- p +
  ggtitle(title) +
  theme_light() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(vjust = 2, face = "bold"),
        axis.title.x = element_text(vjust = 2, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  # annotate("text", x = 0.5 * xmax, y = ymax, label = paste0("Rating Equation: ", eq1), color ="blue")
  
  # Add rating equation(s) and break1 if exists
    if(parts == 2){
     p <- p + geom_hline(yintercept = break1, color = "darkgreen", linetype = 3) +
               annotate("text", x = 0.75 * xmax, y = break1 - 0.04, label = paste0("Rating Breakpoint 1 (",break1, " ft)"), color ="seagreen")
    } else if(parts == 3){
      p <- p + geom_hline(yintercept = break1, color = "darkgreen", linetype = 3) +
        annotate("text", x = 0.75 * xmax, y = break1 - 0.04, label = paste0("Rating Breakpoint 1 (",break1, " ft)") , color ="seagreen") +
        geom_hline(yintercept = break2, color = "darkgreen", linetype = 3) +
        annotate("text", x = 0.75 * xmax, y = break2 - 0.04, label = paste0("Rating Breakpoint 2 (",break2, " ft)"), color ="seagreen")
    } else {
      p <- p
    }
         
# p
p_rating <- plotly::ggplotly(p) #%>% 
  # layout(legend = list(x = 0, y = -0.2, orientation = 'h'))
# p_rating
# print(r)
dfs <- list(plot = p_rating,
            data = r
            )
return(dfs)
}

### RUN THE FUNCTION ####
# dfs <- MAKE_RATING(tbl_discharges = disch, tbl_ratings, axes = FALSE, loc, offset1, drop_meas = NA, break1, break2, offset2, offset3)

#_________________________________________________________________________________________________________________________________
### Plot discharge measurements ####
PLOT_MEASUREMENTS <- function(tbl_discharges, tbl_ratings, loc){
  
  tbl_ratings <- tbl_ratings %>% 
    mutate(RatingDatumOffset = ifelse(is.na(RatingDatumOffset), 0, RatingDatumOffset))
  
  loc_ratings <- tbl_ratings %>% filter(MWRA_Loc == substrRight(loc, 4))
  
  current_rating <- tbl_ratings %>% filter(MWRA_Loc == substrRight(loc, 4), IsCurrent == TRUE)
  active_rating <- current_rating$RatingNum
  
  new_rating <- active_rating
  
  number_measurements <- tbl_discharges %>% filter(Location == loc,
                                                   RatingNumber %in% loc_ratings$RatingNum) %>% 
    nrow() %>% as.numeric()
  
  # If taxa to be plotted is not in the current db, plot message, otherwise continue with plotting data
  if (number_measurements == 0){
    df <- data.frame()
    p <- ggplot(df) +
      theme_void() +
      annotate("text", x = 4, y = 25, label = paste0(
        "No discharge measurements are available for location ", loc),
        color = "blue", size = 5, fontface = "bold.italic")
    p_discharges <- plotly::ggplotly(p, tooltip = c("text")) %>% 
      layout(legend = list(x = 0, y = -0.2, orientation = 'h'))
    return(p_discharges)
  } else {
  
  data1 <- tbl_discharges %>% 
    filter(Location == loc,
           RatingNumber %in% loc_ratings$RatingNum) %>% ### This filters out discharge measurements with undocumented ratings (MD01)
    dplyr::select(c(2,4:11)) %>%
    mutate(Stage_ft = rowMeans(dplyr::select(.,starts_with("Stage")), na.rm = TRUE))
  
  data1$Measurement_Rated <- replace_na(data1$Measurement_Rated, "NA")
  # data1$RatingNumber <- as.factor(data1$RatingNumber)
  
  apply_offset <- function(.x, .y){
    # correct stage datum to match current rating datum
    if(.x < new_rating){
      round(.y + loc_ratings$RatingDatumOffset[loc_ratings$RatingNum == .x], 2)
    } else {
      .y
    }
  }
  data1 <- data1 %>% 
    mutate(Stage_ft = map2_dbl(.x = RatingNumber, .y = Stage_ft, apply_offset))   
  
  
  xmin <- 0
  xmax <- ceiling(max(data1$Discharge_cfs) + (0.1 * max(data1$Discharge_cfs)))
  ymin <- max(c(min(data1$Stage_ft)) - 0.25, 0)
  ymax <- max(data1$Stage_ft) + 0.25
  cols <- c("Poor" = "red", "Fair" = "orange", "Good" = "green", "Excellent" = "blue", "NA" = "black")
  title <- paste0("DISCHARGE MEASUREMENTS AT ", loc)
  p <- ggplot() +
    geom_point(data = data1, aes(x=Discharge_cfs, y= Stage_ft, color = Measurement_Rated,
                                text = paste("Meas.No:", MeasurementNumber, "<br>",
                                             "Stage:", Stage_ft, "<br>",
                                             "Discharge:", Discharge_cfs,"<br>",
                                             "Date:", as_date(DateTimeStartET),"<br>",
                                             "Quality:", Measurement_Rated))) + 
    scale_x_continuous(name = "Discharge (cfs)",limits = c(xmin,xmax)) +
    scale_y_continuous(name = "Stage (ft)", limits = c(ymin,ymax)) +
    scale_color_manual(values = cols) +
    # scale_shape_identity() +
    ggtitle(title) +
    theme_light() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.y = element_text(vjust = 2, face = "bold"),
          axis.title.x = element_text(vjust = 2, face = "bold"),
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
  p_discharges <- plotly::ggplotly(p, tooltip = c("text")) %>% 
    layout(legend = list(x = 0, y = -0.2, orientation = 'h'))
  
  return(p_discharges)
  }
}
### PLOT_MEASUREMENTS INTERACTIVE ####
# loc <- "TROUT BROOK - M110"
# p <- PLOT_MEASUREMENTS(tbl_discharges, tbl_ratings, loc)
# p

#_________________________________________________________________________________________________________________________________
### Plot Current/selected Rating ####
PLOT_RATING <- function(tbl_discharges, tbl_ratings, loc, ratingNo){

### A function to pull characters from the right side of a string
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
tbl_ratings <- tbl_ratings %>% 
    mutate(RatingDatumOffset = ifelse(is.na(RatingDatumOffset), 0, RatingDatumOffset))
  
quality <- c("Fair" = 70, "Good" = 85, "Excellent" = 100, "Poor" = 0)
  
loc_ratings <- tbl_ratings %>% filter(MWRA_Loc == substrRight(loc, 4))
  
rating <- tbl_ratings %>% filter(MWRA_Loc == substrRight(loc, 4), RatingNum == ratingNo)
  
current_rating <- tbl_ratings %>% filter(MWRA_Loc == substrRight(loc, 4), IsCurrent == TRUE)

active_rating <- current_rating$RatingNum

new_rating <- ratingNo
  
### Get the rating record from the table
# rating <- tbl_ratings[tbl_ratings$MWRA_Loc == substrRight(loc, 4) & tbl_ratings$RatingNum == ratingNo,]

### Pull the stage and discharge data for selected location and selected rating number from the discharge measurements table  
  data1 <- tbl_discharges %>% 
    filter(Location == loc) %>%
    dplyr::select(c(2,4:11)) %>%
    mutate(Stage_ft = rowMeans(dplyr::select(.,starts_with("Stage")), na.rm = TRUE)) %>% 
    filter(MeasurementNumber > floor(max(MeasurementNumber, na.rm = TRUE)), MeasurementNumber < ceiling(max(MeasurementNumber, na.rm = TRUE)))

  data1$Measurement_Rated <- replace_na(data1$Measurement_Rated, "NA")
  data1$RatingNumber <- as.numeric(data1$RatingNumber)
  ### Convert the rating number to factor (for plotting)  
  # data1$m <- as.factor(data1$RatingNumber)
  
  apply_offset <- function(.x, .y){
    # correct stage datum to match current rating datum
    if(.x < new_rating){
      round(.y + loc_ratings$RatingDatumOffset[loc_ratings$RatingNum == .x], 2)
    } else {
      .y
    }
  }
  data1 <- data1 %>% 
    mutate(Stage_ft = map2_dbl(.x = RatingNumber, .y = Stage_ft, apply_offset))  

  ### Get the rating bounds, parts, then create a sequence of stage values to calculate rating curve
minstage <- rating$MinStage
maxstage <- round(rating$MaxStage, digits = 1)
parts <- rating$Parts
stages <- seq(minstage, maxstage, by = 0.02)

if(parts == 1){
  break1 <- NA
  break2 <- NA
} else if(parts == 2) {
  break1 <- rating$Break1
  break2 <- NA
} else {
  break1 <- rating$Break1
  break2 <- rating$Break2
}

l <- list(stage = stages, part = NA, Q = NA)
df_Q <- as_tibble(l)

### Assign the rating part to each stage ####
part <- function(x){  
  if(is.na(break1)){# There are no breaks, part is 1
    1
  } else if(is.na(break2)){ # There are only 2 parts, so check if part 1 or 2
    if(x < break1){
      1 
    } else {
      2
    }
  } else { # rating has 3 pars
    if(x < break1){
      1 
    } else if(x < break2){
      2
    } else {
      3
    }
  }
}


### Assign parts for rating ####
if(!is.na(break2)){
  parts <- 3
  eq_part1 <- paste0("Q = ",rating$C1,"*(h-",rating$a1,")^",rating$n1)
  eq_part2 <- paste0("Q = ",rating$C2,"*(h-",rating$a2,")^",rating$n2)
  eq_part3 <- paste0("Q = ",rating$C3,"*(h-",rating$a3,")^",rating$n3)
} else if(!is.na(break1)){
  parts <- 2
  eq_part1 <- paste0("Q = ",rating$C1,"*(h-",rating$a1,")^",rating$n1)
  eq_part2 <- paste0("Q = ",rating$C2,"*(h-",rating$a2,")^",rating$n2)
  eq_part3 <- NA
} else {
  parts <- 1
  eq_part1 <- paste0("Q = ",rating$C1,"*(h-",rating$a1,")^",rating$n1)
  eq_part2 <- NA
  eq_part3 <- NA
}

### Assign the part to each stage value
x <- df_Q$stage
df_Q$part <- mapply(part,x) %>% as.numeric()
      
       # Define function to find Q:
       findq <- function(stage, C, n, a) {
         C*(stage-a)^n
       }
       # LOOP THROUGH ALL STAGE VALUES AND CALCULATE THE DISCHARGE USING RATING COEFFICIENTS
       for (i in seq_along(df_Q$Q)) {

         if(df_Q$stage[i] < minstage) { # Stage is below the rating curve (PZF) assign flow of zero and move to next record
           df_Q$Q[i] <- 0
         } else {
           if(df_Q$stage[i] > maxstage) { # Stage is above the rating curve and cannot be calculated -
             # Set the stage to the max stage and add ARC_Flags
             df_Q$stage[i] <- maxstage
           }
           # Rating Coefficients part 1
           C1 <- rating$C1
           a1 <- rating$a1
           n1 <- rating$n1
           # Rating Coefficients part 2
           C2 <- rating$C2
           a2 <- rating$a2
           n2 <- rating$n2
           # Rating Coefficients part 3
           C3 <- rating$C3
           a3 <- rating$a3
           n3 <- rating$n3

           C <- paste0("C", df_Q$part[i])
           a <- paste0("a", df_Q$part[i])
           n <- paste0("n", df_Q$part[i])
         }
         # Use findq function to calculate discharge from each stage
         df_Q$Q[i] <- findq(stage = df_Q$stage[i], C = get(C), a = get(a), n = get(n))
       }
  df_Q$Q <- round(df_Q$Q, digits = 2)
   
  ########################################################################.
  ###                              PLOT                                ####
  ########################################################################.   
  
  cols <- c("Poor" = "red", "Fair" = "orange", "Good" = "green", "Excellent" = "blue", "NA" = "black")
  title <- paste0("RATING # ", ratingNo," AT ", loc)
  xmin <- 0
  ### Need to use different plot elements for HLNW (no discharge measurements)
  if (data1 %>% nrow() == 0) {
    # xmax <- maxstage
    ymin <- 0
    ymax <- maxstage
    p <- ggplot()
  } else {
    # xmax <- ceiling(max(data1$Discharge_cfs) + (0.1 * max(data1$Discharge_cfs)))
    ymin <- max(c(min(data1$Stage_ft)) - 0.25,0)
    ymax <- max(data1$Stage_ft) + 0.25
    
    p <- ggplot() +
      geom_point(data = data1, aes(x=Discharge_cfs, y= Stage_ft, color = Measurement_Rated,
                                   text = paste("Meas.No:", MeasurementNumber, "<br>",
                                                "Stage:", Stage_ft, "<br>",
                                                "Discharge:",Discharge_cfs,"<br>",
                                                "Date:", as_date(DateTimeStartET),"<br>",
                                                "Quality:", Measurement_Rated)))
  }

  p <- p + geom_path(data = df_Q, aes(x = Q, stage), color = "red") +
    scale_x_continuous(name = "Discharge (cfs)",limits = c(0,NA)) +
    scale_y_continuous(name = "Stage (ft)", limits = c(ymin,ymax)) +
    scale_color_manual(values = cols) +
    # scale_shape_identity() +
    ggtitle(title) +
    theme_light() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.y = element_text(vjust = 2, face = "bold"),
          axis.title.x = element_text(vjust = 2, face = "bold"),
          plot.title = element_text(hjust = 0.5, face = "bold")) 

  # Add rating equation(s) and break1 if exists
  # if(parts == 1){
  #   p <- p +
  #   annotate("text", x = 0.5 * xmax, y = ymax, label = paste0("Rating Equation: ",eq_part1), color ="blue", hjust = "left")
  # } else {
  #   p <- p +
  #     annotate("text", x = 0.5 * xmax, y = ymax * 0.9, 
  #              label = paste0("Rating Eq.Part 1: ", eq_part1, "\nRating Eq. Part 2: ",eq_part2, "\nBreakpoint : ", break1," ft"), color ="blue", hjust = "left") +
  #     geom_hline(yintercept = break1, color = "darkgreen", linetype = 3) +
  #     annotate("text", x = 0.75 * xmax, y = break1 - 0.04, label = "Rating Breakpoint", color ="seagreen")
  # }

  # Add rating equation(s) and break1 if exists
  if(parts == 2){
    p <- p + geom_hline(yintercept = break1, color = "darkgreen", linetype = 3) +
      annotate("text", x = max(df_Q$Q) * 0.75, y = break1 - 0.1, label = paste0("Rating Breakpoint 1 (",break1, " ft)"), color ="seagreen")
  } else if(parts == 3){
    p <- p + geom_hline(yintercept = break1, color = "darkgreen", linetype = 3) +
      annotate("text", x = max(df_Q$Q) * 0.75, y = break1 - 0.1, label = paste0("Rating Breakpoint 1 (",break1, " ft)") , color ="seagreen") +
      geom_hline(yintercept = break2, color = "darkgreen", linetype = 3) +
      annotate("text", x = max(df_Q$Q) * 0.75, y = break2 - 0.1, label = paste0("Rating Breakpoint 2 (",break2, " ft)"), color ="seagreen")
  } else {
    p <- p
  }
  
  p_rating <- plotly::ggplotly(p, tooltip = c("text")) %>% 
    layout(legend = list(x = 0, y = -0.2, orientation = 'h'))
  # p_rating
}
### PLOT_RATING INTERACTIVE ####
# loc <- "FRENCH BROOK - M110"
# ratingNo <- 3.03
# p <- PLOT_RATING(tbl_discharges, tbl_ratings, loc, ratingNo = 3.03)
# p
# 
  # 
  # 
  # xmin <- 0
  # xmax <- ceiling(max(discharge)+(0.1 * max(discharge)))
  # ymin <- offset
  # ymax <- max(stage) + 0.25
  # stages <- seq(ymin,ymax,by = 0.02)
  # l <- list(x = findq(stages, C,n,offset), y = stages, 
  #           lower = findq(stages, conf_int[1,1],conf_int[2,1], offset),
  #           upper = findq(stages, conf_int[1,2],conf_int[2,2], offset))
  # 
  # curve <- as_tibble(l)
  # title <- paste0("STAGE-DISCHARGE RATING CURVE FOR ", loc)
  # p <- ggplot() +
  #   geom_point(data = gaugings,aes(x=discharge, y=stage, text = paste("Meas.No:", num, "<br>","Stage:", stage, "<br>","Discharge:",discharge))) +
  #   geom_line(data = curve, aes(x,y), color = "red") +
  #   geom_line(data = curve, aes(lower,y), color = "blue4", linetype = 3) +
  #   geom_line(data = curve, aes(upper,y), color = "blue4", linetype = 3) +
  #   scale_x_continuous(name = "Discharge (cfs)",limits = c(xmin,xmax)) +
  #   scale_y_continuous(name = "Stage (ft)", limits = c(ymin,ymax)) +
  #   ggtitle(title) +
  #   theme_light() +
  #   theme(legend.position = "none",
  #         legend.title = element_blank(),
  #         axis.title.y = element_text(vjust = 2, face = "bold"),
  #         axis.title.x = element_text(vjust = 2, face = "bold"),
  #         plot.title = element_text(hjust = 0.5, face = "bold")) +
  #   annotate("text", x = 0.5 * xmax, y = ymax, label = paste0("Rating Equation: ",eq), color ="blue")
  # # p
  # p_rating <- plotly::ggplotly(p, tooltip = c("text"))
  # # p_rating     
  
       
       
       
       
       
           
       



###____________________________________________________________________________________
#     Title: Ratings.R
#     Description: This script gathered stage and discharge data to make rating curves
#     Written by: Dan Crocker
#     Last Updated: Summer, 2018
###____________________________________________________________________________________
# 
# library(tidyverse)
# library(RODBC)
# library(odbc)
# library(DBI)
# library(stats)
# library(nlstools)
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
# ### Get data configs ####
# config <- read.csv("//env.govt.state.ma.us/enterprise/DCR-WestBoylston-WKGRP/WatershedJAH/EQStaff/WQDatabase/R-Shared/WAVE-WIT/Configs/WAVE_WIT_Config.csv", header = TRUE)
# config <- as.character(config$CONFIG_VALUE)
# 
# ### Set db for connection ####
# db <- config[3]
# 
# ### Connect to Database ####
# con <- dbConnect(odbc::odbc(),
#                  .connection_string = paste("driver={Microsoft Access Driver (*.mdb)}",
#                                             paste0("DBQ=", db), "Uid=Admin;Pwd=;", sep = ";"),
#                  timezone = "America/New_York")
# # 
# # ### FUNCTION ARGS ####
# tbl_discharges <- dbReadTable(con,"tblDischargeMeasurements")
# tbl_ratings <- dbReadTable(con,"tblRatings")
# locs <- unique(tbl_discharges$Location)
# locs # Look at the locations
# loc <- locs[9] # Pick a location
# ratingNo <-  1.01
# drop_meas <- NULL
# offset1 <- 0.75
# break1 <- 0
# offset2 <- 0
# break2 <- 0
# offset3 <- 0
#_________________________________________________________________________________________________________________________________
### NEW RATING FROM MEASUREMENTS ####
MAKE_RATING <- function(tbl_discharges, tbl_ratings, loc, offset1, drop_meas = NULL, break1 = NULL, offset2 =  NULL, break2 = NULL, offset3 = NULL){

# tbl_measurements <- data  
quality <- c("Fair", "Good", "Excellent", NA)
  
### Filter discharge measurements for location of interest and only the measurements valid for the most recent rating (highest whole number)
### Also filter out any poor quality measurements
data1 <- tbl_discharges %>% 
  filter(Location == loc, Measurement_Rated %in% quality) %>%
  dplyr::select(c(2,4:11)) %>%
  mutate(Stage_ft = rowMeans(dplyr::select(.,starts_with("Stage")), na.rm = TRUE)) %>%
  filter(MeasurementNumber > floor(max(MeasurementNumber, na.rm = TRUE)), MeasurementNumber < ceiling(max(MeasurementNumber, na.rm = TRUE)))

if(!is.null(drop_meas)){
  data1 <- filter(data1, !MeasurementNumber %in% drop_meas)
}

# x <- str_split(drop_meas, ",") %>%
#   lapply(function(x) as.numeric(x))
# as.vector(as.integer(unlist(x)))
# drop_meas <- unlist(x)
# drop_meas

# A function to pull characters from the right side of a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

###__________________________
# Develop a rating equation #
###__________________________

# Q = C*(h-a)^n, where C is a constant, h is head, a is an offset (pzf) and n is an exponent

gaugings <- data1[,c("Stage_ft","Discharge_cfs","MeasurementNumber","Measurement_Rated")]
names(gaugings) <- c("stage", "discharge","num","Measurement_Rated")

### Assign parts for rating ####
if(break2 > 0){
  parts <- 3
} else if(break1 > 0){
  parts <- 2
} else {
  parts <- 1
}
### A Single part rating  ####  
r_1part <- function(gaugings, offset1){

  stage1 <- gaugings$stage
  discharge1 <- gaugings$discharge
### Part 1 ####  
  # Fitting the power law
  # Note that the start argument requires a list with initial estimates of the coefficients to be estimated
  power.nls <- nls(discharge1 ~ C * (stage1 - offset1)^n, data = gaugings, start = list(C = 1, n = 2))
  
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
  
  stage2 <- gaugings2$stage
  discharge2 <- gaugings2$discharge
  
### Part 1 ####  
  # Fitting the power law
  # Note that the start argument requires a list with initial estimates of the coefficients to be estimated
  power.nls1 <- nls(discharge ~ C * (stage - offset1)^n, data = gaugings, start = list(C = 1, n = 2))
  
  ### Generate confidence intervals from regression ###
  conf_int1 <- confint2(object = power.nls1, level = 0.95)
  # Viewing the model summary and accessing estimated constants
  t_sum1 <- summary(power.nls1)
  C1 <- round(coef(power.nls1)["C"],4)
  a1 <- offset1
  n1 <-round(coef(power.nls1)["n"], 4)
  eq1 <- paste0("Q = ",C1,"*(h-",a1,")^",n1)
  
### Part 2 ####    
  power.nls2 <- nls(discharge2 ~ C * (stage2 - offset1)^n, data = gaugings2, start = list(C = 1, n = 2))
  
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

  stage2 <- gaugings2$stage
  discharge2 <- gaugings2$discharge

  stage3 <- gaugings3$stage
  discharge3 <- gaugings3$discharge

### Part 1 ####
  # Fitting the power law
  # Note that the start argument requires a list with initial estimates of the coefficients to be estimated
  power.nls1 <- nls(discharge1 ~ C * (stage1 - offset1)^n, data = gaugings1, start = list(C = 1, n = 2))

  ### Generate confidence intervals from regression ###
  conf_int1 <- confint2(object = power.nls1, level = 0.95)
  # Viewing the model summary and accessing estimated constants
  t_sum1 <- summary(power.nls1)
  C1 <- round(coef(power.nls1)["C"],4)
  a1 <- offset1
  n1 <-round(coef(power.nls1)["n"], 4)
  eq1 <- paste0("Q = ",C1,"*(h-",a1,")^",n1)

### Part 2 ####
  power.nls2 <- nls(discharge2 ~ C * (stage2 - offset2)^n, data = gaugings2, start = list(C = 1, n = 2))

  ### Generate confidence intervals from regression ###
  conf_int2 <- confint2(object = power.nls2, level = 0.95)
  # Viewing the model summary and accessing estimated constants
  t_sum2 <- summary(power.nls2)
  C2 <- round(coef(power.nls2)["C"],4)
  a2 <- offset2
  n2 <-round(coef(power.nls2)["n"], 4)
  eq2 <- paste0("Q = ",C2,"*(h-",a2,")^",n2)

### Part 3 ####
  power.nls3 <- nls(discharge3 ~ C * (stage3 - offset3)^n, data = gaugings3, start = list(C = 1, n = 2))

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
maxstage <- max(gaugings$stage) + 0.25

stages <- seq(minstage, maxstage, by = 0.02)

if(parts == 1){
  break1 <- NULL
  break2 <- NULL
  r <- r_1part(gaugings, offset1)
} else if(parts == 2) {
  break1 <- break1
  break2 <- NULL
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
  if(is.null(break1)){# There are no breaks, part is 1
    1
  } else if(is.null(break2)){ # There are only 2 parts, so check if part 1 or 2
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
  
  if(df_Q$stage[i] < minstage) { # Stage is below the rating curve (PZF) assign flow of zero and move to next record
    df_Q$Q[i] <- 0
  } else {
    if(df_Q$stage[i] > maxstage) { # Stage is above the rating curve and cannot be calculated -
      # Set the stage to the max stage 
      df_Q$stage[i] <- maxstage
    }
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
  }
  
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
stages <- seq(ymin,ymax,by = 0.02)
title <- paste0("STAGE-DISCHARGE RATING CURVE FOR ", loc)


p <- ggplot() +
  geom_point(data = gaugings, aes(x=discharge, y=stage, text = paste("Meas.No:", num, "<br>","Stage:", stage, "<br>","Discharge:",discharge,"<br>","Quality:", Measurement_Rated))) +
  geom_line(data = df_Q, aes(Q,stage), color = "red") +
  geom_line(data = df_Q, aes(lower,stage), color = "blue4", linetype = 3) +
  geom_line(data = df_Q, aes(upper,stage), color = "blue4", linetype = 3) +
  scale_x_continuous(name = "Discharge (cfs)",limits = c(xmin,xmax)) +
  scale_y_continuous(name = "Stage (ft)", limits = c(ymin,ymax)) +
  ggtitle(title) +
  theme_light() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_text(vjust = 2, face = "bold"),
        axis.title.x = element_text(vjust = 2, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold")) 
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
p_rating <- plotly::ggplotly(p, tooltip = c("text"))
# p_rating
# print(r)
dfs <- list(plot = p_rating,
            data = r
            )
return(dfs)
}

### RUN THE FUNCTION ####
# dfs <- MAKE_RATING(tbl_discharges, tbl_ratings, loc, offset1, drop_meas = drop_meas, break1 = NULL, break2 = NULL, offset2 = NULL, offset3 = NULL )

#_________________________________________________________________________________________________________________________________
### Plot discharge measurements ####
PLOT_MEASUREMENTS <- function(tbl_discharges, tbl_ratings, loc){
  
  data1 <- tbl_discharges %>% 
    filter(Location == loc) %>%
    dplyr::select(c(2,4:11)) %>%
    mutate(Stage_ft = rowMeans(dplyr::select(.,starts_with("Stage")), na.rm = TRUE))

    data1$RatingNumber <- as.factor(data1$RatingNumber)
  

  xmin <- 0
  xmax <- ceiling(max(data1$Discharge_cfs)+(0.1 * max(data1$Discharge_cfs)))
  ymin <- max(c(min(data1$Stage_ft)) - 0.25,0)
  ymax <- max(data1$Stage_ft) + 0.25
  cols <- c("Poor" = "red", "Fair" = "orange", "Good" = "green", "Excellent" = "blue")
  title <- paste0("DISCHARGE MEASUREMENTS AT ", loc)
  p <- ggplot() +
    geom_point(data = data1, aes(x=Discharge_cfs, y= Stage_ft, shape = RatingNumber, color = Measurement_Rated,
                                text = paste("Meas.No:", MeasurementNumber, "<br>",
                                             "Stage:", Stage_ft, "<br>",
                                             "Discharge:", Discharge_cfs,"<br>",
                                             "Quality:", Measurement_Rated))) + 
    scale_x_continuous(name = "Discharge (cfs)",limits = c(xmin,xmax)) +
    scale_y_continuous(name = "Stage (ft)", limits = c(ymin,ymax)) +
    scale_color_manual(values = cols) +
    ggtitle(title) +
    theme_light() +
    theme(legend.position = "bottom",
          legend.title = element_text(face = "bold"),
          axis.title.y = element_text(vjust = 2, face = "bold"),
          axis.title.x = element_text(vjust = 2, face = "bold"),
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
  p_discharges <- plotly::ggplotly(p, tooltip = c("text"))
  # p_discharges
  
}
# p <- PLOT_MEASUREMENTS(tbl_discharges, tbl_ratings, loc)
# p

#_________________________________________________________________________________________________________________________________
### Plot Current/selected Rating ####
PLOT_RATING <- function(tbl_discharges, tbl_ratings, loc, ratingNo){

### A function to pull characters from the right side of a string
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
### Get the rating record from the table
rating <- tbl_ratings[tbl_ratings$MWRA_Loc == substrRight(loc, 4) & tbl_ratings$RatingNum == ratingNo,]

### Pull the stage and discharge data for selected location and selected rating number from the discharge measurements table  
  data1 <- tbl_discharges %>% 
    filter(Location == loc) %>%
    dplyr::select(c(2,4:11)) %>%
    mutate(Stage_ft = rowMeans(dplyr::select(.,starts_with("Stage")), na.rm = TRUE)) %>% 
    filter(MeasurementNumber > floor(max(MeasurementNumber, na.rm = TRUE)), MeasurementNumber < ceiling(max(MeasurementNumber, na.rm = TRUE)))

  ### Convert the rating number to factor (for plotting)  
  data1$RatingNumber <- as.factor(data1$RatingNumber)

  ### Get the rating bounds, parts, then create a sequence of stage values to calculate rating curve
minstage <- rating$MinStage
maxstage <- rating$MaxStage
parts <- rating$Parts
stages <- seq(minstage, maxstage, by = 0.02)

if(parts == 1){
  break1 <- NULL
  break2 <- NULL
} else if(parts == 2) {
  break1 <- rating$Break1
  break2 <- NULL
} else {
  break1 <- rating$Break1
  break2 <- rating$Break2
}

l <- list(stage = stages, part = NA, Q = NA)
df_Q <- as_tibble(l)

### Assign the rating part to each stage ####
part <- function(x){  
  if(is.null(break1)){# There are no breaks, part is 1
    1
  } else if(is.null(break2)){ # There are only 2 parts, so check if part 1 or 2
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
if(!is.null(break2)){
  parts <- 3
  eq_part1 <- paste0("Q = ",rating$C1,"*(h-",rating$a1,")^",rating$n1)
  eq_part2 <- paste0("Q = ",rating$C2,"*(h-",rating$a2,")^",rating$n2)
  eq_part3 <- paste0("Q = ",rating$C3,"*(h-",rating$a3,")^",rating$n3)
} else if(!is.null(break1)){
  parts <- 2
  eq_part1 <- paste0("Q = ",rating$C1,"*(h-",rating$a1,")^",rating$n1)
  eq_part2 <- paste0("Q = ",rating$C2,"*(h-",rating$a2,")^",rating$n2)
  eq_part3 <- NULL
} else {
  parts <- 1
  eq_part1 <- paste0("Q = ",rating$C1,"*(h-",rating$a1,")^",rating$n1)
  eq_part2 <- NULL
  eq_part3 <- NULL
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
       
  xmin <- 0
  xmax <- ceiling(max(data1$Discharge_cfs)+(0.1 * max(data1$Discharge_cfs)))
  ymin <- max(c(min(data1$Stage_ft)) - 0.25,0)
  ymax <- max(data1$Stage_ft) + 0.25
  cols <- c("Poor" = "red", "Fair" = "orange", "Good" = "green", "Excellent" = "blue")
  title <- paste0("RATING # ", ratingNo," AT ", loc)
  p <- ggplot() +
    geom_point(data = data1,aes(x=Discharge_cfs, y= Stage_ft, shape = RatingNumber, color = Measurement_Rated,
                                text = paste("Meas.No:", MeasurementNumber, "<br>","Stage:", Stage_ft, "<br>","Discharge:",Discharge_cfs,"<br>","Quality:", Measurement_Rated))) + 
    geom_path(data = df_Q, aes(x = Q, stage), color = "red") +
    scale_x_continuous(name = "Discharge (cfs)",limits = c(xmin,xmax)) +
    scale_y_continuous(name = "Stage (ft)", limits = c(ymin,ymax)) +
    scale_color_manual(values = cols) +
    scale_shape_identity() +
    ggtitle(title) +
    theme_light() +
    theme(legend.position = "bottom",
          legend.title = element_text(face = "bold"),
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
      annotate("text", x = 0.75 * xmax, y = break1 - 0.04, label = paste0("Rating Breakpoint 1 (",break1, " ft)"), color ="seagreen")
  } else if(parts == 3){
    p <- p + geom_hline(yintercept = break1, color = "darkgreen", linetype = 3) +
      annotate("text", x = 0.75 * xmax, y = break1 - 0.04, label = paste0("Rating Breakpoint 1 (",break1, " ft)") , color ="seagreen") +
      geom_hline(yintercept = break2, color = "darkgreen", linetype = 3) +
      annotate("text", x = 0.75 * xmax, y = break2 - 0.04, label = paste0("Rating Breakpoint 2 (",break2, " ft)"), color ="seagreen")
  } else {
    p <- p
  }
  
  
  p_rating <- plotly::ggplotly(p, tooltip = c("text")) 
  # p_rating
}
  
# p <- PLOT_RATING(tbl_discharges, tbl_ratings, loc, ratingNo = 2.01)
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
  
       
       
       
       
       
           
       



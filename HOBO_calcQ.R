##############################################################################################################################
#     Title: HOBO_calcQ.R
#     Description: This script is called from an import function with stage values - it uses rating information to calculate
#                   discharges for each stage record
#     Written by: Dan Crocker
#     Last Update: April 2018
#
##############################################################################################################################
# filename_db <-  db
# loc <- loc
# df_HOBO <- df
# # Load libraries needed
# library(tidyverse)
# library(odbc)
# library(RODBC)
# library(DBI)
# library(lubridate)
# library(magrittr)

HOBOcalcQ <- function(schema, loc, df_HOBO) {

  # Set odbc connection  and get the rating table
  

  
### Connect to db  in America/New_York tz
  schema <- schema
  dsn <- 'DCR_DWSP_App_R'
  database <- "DCR_DWSP"
  tz <- 'America/New_York'
  con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[18], timezone = tz)
  
  # database <- "DCR_DWSP"
  # con <- dbConnect(odbc::odbc(), database, timezone = 'America/New_York')
  ratings <- dbReadTable(con, Id(schema = schema, table = "tblRatings"))
  
  # Disconnect from db and remove connection obj
  dbDisconnect(con)
  rm(con)
  # Assigntoday's date as the end date for ratings that are still valid - so that date test won't compare against NA values
  now <- format(Sys.time(), "%Y-%m-%d")
  maxtime <- max(df_HOBO$DateTimeUTC) + hours(5)
  ratings$DateTimeEndET[is.na(ratings$DateTimeEndET)] <- maxtime %>% ceiling_date("day") %>% as_date()

  ### Check the location to make sure there is a valid rating:
  if(loc %in% ratings$MWRA_Loc){ # IF true then a rating exists

    # Reduce rating table to locations within dataset and remove ratings with unspecified start times
    ratings2 <- ratings[ratings$MWRA_Loc %in% loc & !is.na(ratings$DateTimeStartET),]

    # For each stage value assign the appropriate rating number based on the date ranges of the ratings
    x <- as_date(df_HOBO$DateTimeUTC)
    y <- loc

    pickRating <- function(x,y){
      ratingNo <- ratings2$ID[ratings2$DateTimeStartET <= x & ratings2$DateTimeEndET >= x & ratings2$MWRA_Loc == y]
    }

    df_HOBO$ratingNo <- map2(x, y, pickRating) %>%
      as.numeric()

    #################################################################
    # Assign the rating part to each stage
    x <- df_HOBO$ratingNo
    y <- df_HOBO$Stage_ft
    # y <- df_HOBO$Stage_new  #use this line for data corrections
    
    part <- function(x,y) {
      if(ratings2$Parts[ratings2$ID == x] == 1) {# Rating has 1 part
        1
      } else {
        if(ratings2$Parts[ratings2$ID == x] == 2) { # Rating has 2 parts
          if(y < ratings2$Break1[ratings2$ID == x]) {# stage is less than breakpoint 1
            1
            } else {
            2
          }# Otherwise stage is >= breakpoint1
        # If no return yet, then the rating has 3 parts
        } else {
          if(y[df_Q$ratingNo == x] < ratings2$Break1[ratings2$ID == x]) { # stage is less than breakpoint 1
            1 # The stage is in the first part of the rating
          } else if(y[df_Q$ratingNo == x] >= ratings2$Break2[ratings2$ID == x]) { # stage is higher than breakpoint 2
            3
          } else 2
        }
      }
    }
    df_HOBO$part <- mapply(part,x,y) %>% as.numeric()

    df_HOBO <- mutate(df_HOBO, "q_cfs" = NA, "RatingFlag" = NA)

    # Define function to find Q:
    findq <- function(stage, C, n, a) {
      C*(stage-a)^n
    }
    # LOOP THROUGH ALL STAGE VALUES AND CALCULATE THE DISCHARGE USING RATING COEFFICIENTS
    for (i in seq_along(df_HOBO$q_cfs)) {
      # Get the min and max stage bounds for the current rating

      minstage <- ratings2$MinStage[ratings2$ID == df_HOBO$ratingNo[i]]
      maxstage <- ratings2$MaxStage[ratings2$ID == df_HOBO$ratingNo[i]]
      stages <- df_HOBO$Stage_ft
      # stages <- df_HOBO$Stage_new
      if(stages[i] < minstage) { # Stage is below the rating curve (PZF) assign flow of zero and move to next record
        df_HOBO$q_cfs[i] <- 0
        df_HOBO$RatingFlag[i] <- 113
        next
      } else {
        if(stages[i] > maxstage) { # Stage is above the rating curve and cannot be calculated -
          # Set the stage to the max stage and add ARC_Flags
          # df_HOBO$Stage_ft[i] <- maxstage
          # Disabled overwriting stage value with max stage - better to just flag the data and update if better rating is generated
          df_HOBO$RatingFlag[i] <- 111 # Have to flag the entire date, since output is daily, *Not all records on date may be affected
        }
        # Rating Coefficients part 1
        c1 <- ratings2$C1[ratings2$ID == df_HOBO$ratingNo[i]]
        a1 <- ratings2$a1[ratings2$ID == df_HOBO$ratingNo[i]]
        n1 <- ratings2$n1[ratings2$ID == df_HOBO$ratingNo[i]]
        # Rating Coefficients part 2
        c2 <- ratings2$C2[ratings2$ID == df_HOBO$ratingNo[i]]
        a2 <- ratings2$a2[ratings2$ID == df_HOBO$ratingNo[i]]
        n2 <- ratings2$n2[ratings2$ID == df_HOBO$ratingNo[i]]
        # Rating Coefficients part 3
        c3 <- ratings2$C3[ratings2$ID == df_HOBO$ratingNo[i]]
        a3 <- ratings2$a3[ratings2$ID == df_HOBO$ratingNo[i]]
        n3 <- ratings2$n3[ratings2$ID == df_HOBO$ratingNo[i]]

        C <- paste0("c", df_HOBO$part[i])
        a <- paste0("a", df_HOBO$part[i])
        n <- paste0("n", df_HOBO$part[i])
      }
      # Use findq function to calculate discharge from each stage
      df_HOBO$q_cfs[i] <- findq(stage = stages[i], C = get(C), a = get(a), n = get(n))
    }
    df_HOBO$q_cfs <- round(df_HOBO$q_cfs, digits = 6)
    df_HOBO <- dplyr::rename(df_HOBO, "Discharge_cfs" = q_cfs)
    # df_HOBO <- dplyr::rename(df_HOBO, "Discharge_new" = q_cfs) ### use this line for discharge from corrected stage values
  }

  return(df_HOBO)
} # End function

##############################################################################################

# x <- HOBOcalcQ(filename_db = db, loc = loc, df_HOBO = df)

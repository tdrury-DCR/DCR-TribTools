################################### HEADER ###################################
#  TITLE: outlook_email.R
#  DESCRIPTION: A function that will send an email through MS outlook
#  AUTHOR(S): Dan Crocker 
#  DATE LAST UPDATED: November 20, 2019
#  GIT REPO: DCR-WIT
#  R version 3.5.3 (2019-03-11)  i386
##############################################################################.


########################################################################.
###                  EMAIL THROUGH OUTLOOK                          ####
########################################################################.

# install.packages("RDCOMClient", repos = "http://www.omegahat.net/R") # Install Repo

library(RDCOMClient)

OL_EMAIL <- function(to, cc = "", bcc = "", subject, body){
  
  # Open Outlook
  Outlook <- COMCreate("Outlook.Application")
  
  # Create a new message
  Email = Outlook$CreateItem(0)
  
  # Set the recipient, subject, and body
  Email[["to"]] =  to # semi-colon separated email addresses as string no <>
  Email[["cc"]] = cc
  Email[["bcc"]] = bcc
  Email[["subject"]] = subject
  Email[["htmlbody"]] = body
  
  # Send the message
  Email$Send()
  
  # Close Outlook, clear the message
  rm(Outlook, Email)
  
  
  return(glue("Email notification was sent to {to}."))
}

SendEmail <- function(df, table, file, emaillist, username, userlocation) {
  out <- tryCatch(
    message("Trying to send email"),
    OL_EMAIL(to = emaillist, 
             subject = paste0("New Data has been Imported to a ",userlocation," Database"),
             body = paste0("<body><p>",username," has imported ", nrow(df), " new record(s) in the table: ",table, "</p><p>Filename = ", file,"</p></body>")
    ),
    
    error=function(cond) {
      message(paste("User cannot connect to SMTP Server, cannot send email", cond))
      return(1)
    },
    warning=function(cond) {
      message(paste("Send mail function caused a warning, but was completed successfully", cond))
      return(2)
    },
    finally={
      message(paste("Email notification attempted"))
    }
  )
  return(out)
}






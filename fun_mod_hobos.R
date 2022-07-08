###############################################################################.
#  TITLE: fun_mod_hobos.R
#  DESCRIPTION: Functions for the HOBO module         
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: 2022-07-08
#  GIT REPO: TribTools
#  R version 4.1.2 (2021-11-01)  x86_64
##############################################################################.

get_files <- function(updir, mayfly_data_dir) {
  mayfly_files <- list.files(mayfly_data_dir, recursive = F, full.names = T, include.dirs = TRUE, pattern = "^[^~$]+.csv$")
  hobo_txt_files  <- list.files(updir, recursive = F, full.names = F, include.dirs = TRUE, pattern = "^[^~$]+.txt$")
  barometer_files <- list.files(updir, recursive = F, full.names = F, include.dirs = TRUE, pattern = "^[^~$]+(_BARO_).*\\.txt$")
  all_files <- c(hobo_txt_files, mayfly_files)
  
  if(length(barometer_files) > 0){
    files <- barometer_files
  } else {
    files <- all_files 
  }
  return(files)
}
  
  
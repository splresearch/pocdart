library(lubridate)
library(shiny)
library(stringr)
dart.CRON <- function(){
  setwd("/srv/shiny-server/pocdart") ##Set Working Directory
  source("misc/global_variables.R") # includes, api call
  source("shiny_code_repo/redcap_api/redcapAPI.R")
  source("shiny_code_repo/survey_processing/expire_survey.R")
  source("shiny_code_repo/survey_processing/stash_surveys.R")
  stash_surveys()
}

dart.CRON()
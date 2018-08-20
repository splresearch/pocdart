library(lubridate)
library(shiny)
library(stringr)
update.Cron <- function(){
  setwd("/srv/shiny-server/pocdart") ##Set Working Directory
  source("misc/global_variables.R") # includes, api call
  source("shiny_code_repo/redcap_api/redcapAPI.R")
}

update.Cron()
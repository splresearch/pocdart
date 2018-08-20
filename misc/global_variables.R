source("shiny_code_repo/redcap_api/keys.R")
source("shiny_code_repo/survey_processing/shinyInput_custom.R")
events <- list()
survey_key <- list()
#LIST OF ASSAYS
assay_list <- list("Initial" = list("POCDART" = c("promisanx4v1","promissleepdisturb4v1","promisanger5v1","promisemotionsupport4v2")),
                   "Repeating" = list("POCDART" = c("promisanx4v1","promissleepdisturb4v1","promisanger5v1","promisemotionsupport4v2")))
public_survey_url <- "http://pocdart.org/redcap/surveys/?s=CR7A997E7Y"
project_string <- "POCDART"

FREQUENCY <- 7 #days
EXPIRATION <- 2 #days after frequency reached
INVITE <- 2 #days before frequency reached
FUTUREWINDOW <- FREQUENCY - 1 #Window to see upcoming surveys

admission_form <- "demographics_information"
starting_instrument <- 'survey_intro' #Instrument to begin Admission & Repeating surveys
discharge_instrument <- 'discharge_completion' #Instrument to begin Discharge surveys
expiration_instrument <- 'survey_expiration' #Expiration instrument

admission_complete <- paste0(admission_form, "_complete")
admission_timestamp <- paste0(admission_form, "_timestamp")


survey_key[[project_string]] <- getRedcapToken("pocdart")

events[[project_string]] <- c("admission" = "patient_admission_arm_1",
                               "repeating" = "patient_repeating_arm_1",
                               "discharge" = "patient_discharge_arm_1")

interval <- data.frame(Term = "Week ", Multiplier = 1, altTerm = "W")

source("shiny_code_repo/global_resource_loader.R")
#source("shiny_code_repo/redcap_api/redcapAPI.R")
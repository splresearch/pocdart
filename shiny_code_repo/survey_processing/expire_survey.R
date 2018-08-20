expire_surveys <- function(ref_date = today(),projects) {
  
  for (i in (i = 1:length(projects))){
    project = projects[i]
    # print(project)
    # Distributes surveys to participants, marks expired surveys complete
    # Called via daily cronjob
    
    # Load resources for cron
    source("misc/global_variables.R") # retrieve frequency and expiration and update data
    source("shiny_code_repo/redcap_api/extractData.R")
    
    
    first_survey_timestamp <- paste0(assay_list[["Initial"]][[project]][1], "_timestamp")
    first_survey_complete <- paste0(assay_list[["Initial"]][[project]][1], "_complete")
    
    ref_date <- as.Date(ref_date, format = "%Y-%m-%d")
    
    # Get data
    survey_data <- extractData("all", c("first_name", "last_name", "_complete$", "_timestamp$", "survey_expired___1$", "survey_expiration_date$"), survey = T)
    survey_data <- subset(survey_data, redcap_event_name %in% unlist(unname(events[[project]])))
    # Omit discharged patients
    dsc_patients <- survey_data$record_id[which(survey_data$redcap_event_name == events[[project]][["discharge"]] &
                                                  survey_data[first_survey_complete] == 2 |
                                                  survey_data$redcap_event_name == events[[project]][["discharge"]] &
                                                  survey_data$survey_expiration_complete == 2)]
    survey_data <- survey_data[which(!(survey_data$record_id %in% dsc_patients)),]
    if (nrow(survey_data) == 0){next}
    
    # Calculate current day in cycle
    survey_data$position <- NA
    for (patient in unique(survey_data$record_id)) {
      #print(patient)
      diff <- ref_date - as.Date(survey_data[which(survey_data$record_id == patient & 
                                                     survey_data$redcap_event_name == events[[project]][["admission"]]), admission_timestamp],
                                 format = "%Y-%m-%d %H:%M:%S")
      survey_data$position[which(survey_data$record_id == patient)] <- floor(as.numeric(diff)) %% FREQUENCY + 1
    }
    
    # print(survey_data$position)
    # Calculate days since survey was taken
    survey_data$days_elapsed <- NA
    
    for (row in 1:nrow(survey_data)) {
      if (!is.na(survey_data[row, first_survey_timestamp])) { #tag, manual entries need different timestamp
        if (survey_data[row, first_survey_timestamp] != "[not completed]") {
          survey_data$days_elapsed[row] <- ref_date - as.Date(survey_data[row, first_survey_timestamp], format = "%Y-%m-%d %H:%M:%S")
          survey_data$days_elapsed[row] <- floor(as.numeric(survey_data$days_elapsed[row])) + 1
        }
      }
    }
    
    ### Filter Data ###
    # Take only each patient's most recent repeat event, if any
    for (patient in unique(survey_data$record_id)) {
      if (length(na.omit(subset(survey_data, record_id == patient)$redcap_repeat_instance)) > 0) { # skip records with no repeat event data
        max_repeat <- max(subset(survey_data, record_id == patient)$redcap_repeat_instance, na.rm = T)
        survey_data <- subset(survey_data, record_id != patient |
                                (record_id == patient &
                                   redcap_repeat_instance == max_repeat))
      }
    }
    # print("---------- one repeat ----------")
    # print(survey_data$record_id)
    
    # Filter out discharge event
    survey_data <- subset(survey_data, redcap_event_name != events[[project]][["discharge"]])

    # Filter out subjects with admission timestamps that have not yet reached initial expiration point
    # Solution for first repeat instance reaching expiration (all others rely on ref to previous repeat instance)
    survey_data <- subset(survey_data, redcap_event_name != events[[project]][["admission"]] |
                            ref_date - as.Date(first_survey_timestamp, format = "%Y-%m-%d") > FREQUENCY + EXPIRATION)
    # print("---------- first repeat instance expiration ----------")
    # print(survey_data$record_id)

    # Filter out subjects with recently expired forms
    survey_data <- subset(survey_data, is.na(survey_expiration_date) |
                            ref_date - as.Date(survey_expiration_date, format = "%Y-%m-%d") > position - EXPIRATION)
    # print("---------- no recent expiration ----------")
    # print(survey_data[c("record_id", "position", "days_elapsed")])
    
    # Filter out subjects with invalid admission dates, within expiration window, no surveys completed in valid window
    survey_data <- subset(survey_data,
                          !is.na(position) & #valid admission survey
                            (position > EXPIRATION & position < FUTUREWINDOW) & #beyond expiration threshold in cycle #TAG
                            (days_elapsed > position + INVITE |
                               is.na(survey_data[first_survey_timestamp]))) #no survey taken since invite sent
    # print("---------- in expiration period and no recent survey ----------")
    # print(survey_data$record_id)
    
    expiry_data <- survey_data[c("record_id", "redcap_event_name", "redcap_repeat_instance", "survey_expired___1", paste0(starting_instrument, "_complete"),
                                 paste0(assay_list[["Repeating"]][[project]], "_complete"), "survey_expiration_date")]
    #print(expiry_data)
    # Create new repeat instance and set expired assay_list expired
    if (nrow(expiry_data) > 0) {
      # Increment repeat instances
      expiry_data$redcap_repeat_instance <- expiry_data$redcap_repeat_instance + 1
      
      # Set expiration fields to expired
      expiry_data$survey_expired___1 <- 1
      expiry_data$survey_expiration_complete <- 2
      expiry_data[paste0(assay_list[["Repeating"]][[project]], "_complete")] <- 0
      expiry_data[paste0(starting_instrument, "_complete")] <- 0
      expiry_data$survey_expiration_date <- as.character(ref_date)
      
      # Push exipry data
      # print(expiry_data)
      redcap_write(expiry_data, redcap_uri = api_url, token = survey_key)
    }
  }
  return(NULL)
}

#expire_surveys(projects = names(survey_key)) #uncomment for production

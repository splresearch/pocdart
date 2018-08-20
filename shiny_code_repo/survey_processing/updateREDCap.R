updateDemographics <- function(patient,value,item,project){
  #' Updates a REDCap patient's demographic information
  #'
  #' Pushes specific values to REDCap. 
  #' 
  #' Can accept vectors for value and item, can only push to one patient and one project at a time.
  #' @param patient The value to be squared
  #' @param value The REDCap value to be pushed
  #' @param item The REDCap field to be updated
  #' @param project The name of the project - specified in misc/global_variables.R
  push.Dat <- rep(NA,2+length(value))
  push.Dat <- c(patient,events[[project]][["admission"]],as.character(value))
  names(push.Dat) <- c("record_id","redcap_event_name",item)
  redcap_write(ds = as.data.frame(t(push.Dat)), token = survey_key[[project]][[1]], redcap_uri = api_url) 
}

copiedUpdate <- function(assay, patient, time,project){
  #' Updates a REDCap field for copied confirmation
  #'
  #' Updates a "****_copied" field in the patient's event
  #' 
  #' Can accept a vector of assays in assay.
  #' @param assay Instrument to be marked as copied.
  #' @param patient Patient to be updated
  #' @param time Value to determine correct event 
  #' @param project The name of the project - specified in misc/global_variables.R
  field <- paste0(assay,"_copied")
  
  if (time == 0){
    event <- events[[project]][["admission"]]
    repeating <- NA
  } else if (time == "D"){
    event <- events[[project]][["discharge"]]
    repeating <- NA
  } else {
    event <- events[[project]][["repeating"]]
    repeating <- time
  }
  
  push.Dat <- data.frame(0,0,0,0)
  push.Dat[1:4] <- c(patient,event,repeating,1)
  names(push.Dat) <- c("record_id","redcap_event_name","redcap_repeat_instance",field)
  redcap_write(ds = push.Dat, token = survey_key[[project]], redcap_uri = api_url) 
}

updateSurveys <- function(patient, project){
  #' Updates locally stashed surveys
  #'
  #' Updates locally stored data and generates the requisite survey links for new patients
  #' 
  #' Can accept a vector of patients, can only accept a single project value
  #' @param patient Patient(s) to have links generated.
  #' @param project The name of the project - specified in misc/global_variables.R
  # Refresh Patient Data and set Curl Handle
  getData(api_url, survey_key["POCDART"])
  outcomes_data <<- read.csv("data/outcomes_data.csv", header = TRUE, stringsAsFactors = F)
  handle <- getCurlHandle()

  for (i in patient){
    #Iterating through patient, generate QR links
    admissionBegin <- postForm(
      uri=api_url,
      token=survey_key[[project]],
      content='surveyLink',
      format='json',
      curl = handle,
      instrument=starting_instrument,
      event=events[[project]][["admission"]],
      record=i,
      repeat_instance=NA,
      returnFormat='json'
    )
    
    admissionExpire <- postForm(
      uri=api_url,
      token=survey_key[[project]],
      content='surveyLink',
      curl = handle,
      format='json',
      instrument=expiration_instrument,
      event=events[[project]][["admission"]],
      record=i,
      repeat_instance=NA,
      returnFormat='json'
    )
    
    dischargeBegin <- postForm(
      uri=api_url,
      curl = handle,
      token=survey_key[[project]],
      content='surveyLink',
      format='json',
      instrument=discharge_instrument,
      event=events[[project]][["discharge"]],
      record=i,
      repeat_instance=NA,
      returnFormat='json'
    )
    #Process QR links into buttons
    admissionQR <-shinyInput(actionButton, 1, paste0("button_",gsub(".*=","",admissionBegin),"_",i), label = "Show QR", 
                         onclick = paste0('Shiny.onInputChange(\"select_button_QR\",  this.id + "_" + Math.random())'))
    dischargeQR <- shinyInput(actionButton, 1, paste0("button_",gsub(".*=","",dischargeBegin),"_",i), label = "Show QR", 
                         onclick = paste0('Shiny.onInputChange(\"select_button_QR\",  this.id + "_" + Math.random())'))
    dischargeExpire <- shinyInput(actionButton, 1, paste0('button_',project,"_",i), label = "Administrative Discharge",
                                        onclick = paste0('Shiny.onInputChange(\"select_button_discharge','\",  Math.random()+this.id)'))
    #Prepare data and create Admission and Discharge Data Frames
    names <- outcomes_data %>% select(record_id,first_name,last_name) %>% filter(record_id == as.character(i))
    #"Record","Instance","Patient","Link","QR-Link","Expire","Project"
    admissionDF <- data.frame("Record" = as.character(i), "Instance" = NA, "Event" = events[[project]][["admission"]], "Patient" = paste0(names$last_name,", ",names$first_name), "Link" = admissionBegin, "QR-Link" = admissionQR,
                              "Expire" = admissionExpire, "Project" = "POCDART")
    
    dischargeDF <- data.frame("Record" = as.character(i), "Instance" = NA, "Patient" = paste0(names$last_name,", ",names$first_name), "Link" = dischargeBegin, "QR-Link" = dischargeQR,
                              "Expire" = dischargeExpire, "Project" = "POCDART")
    #Append data frames to existing CSVs
    write.table(admissionDF, paste0("data/incomplete_surveys.csv"), row.names = F,col.names = F,quote = TRUE,sep = ",",append = T,qmethod = "double")
    write.table(dischargeDF, paste0("data/discharge_surveys.csv"), row.names = F,col.names = F,quote = TRUE,sep = ",",append = T,qmethod = "double")
  }
}

manualDischarge <- function(patient,project) {
  #' Marks patient as administratively discharged
  #'
  #' Marks assays as incomplete, creates survey link for taking the assessment.
  #' @param patient Patient to be discharged.
  #' @param project The name of the project - specified in misc/global_variables.R
  # Mark all surveys for patient on discharge event incomplete and return reason survey
  completion_fields <- paste0(c(assay_list[["Initial"]][[project]], starting_instrument, discharge_instrument), "_complete")
  data <- data.frame(record_id = patient, redcap_event_name = events[[project]]["discharge"])
  data[completion_fields] <- 0
  
  # Upload discharge dat
  redcap_write(as.data.frame(data), redcap_uri = api_url, token = survey_key[[project]])
  
  # Get and output survey url
  library(RCurl)
  survey_url <- postForm(
    uri=api_url,
    token=survey_key[[project]],
    content='surveyLink',
    format='json',
    instrument='survey_expiration',
    event=events[[project]][["discharge"]],
    record=patient,
    returnFormat='json'
  )
  
  return(survey_url)
}
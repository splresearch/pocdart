progress.Chart <- function(outcomes_data,project){
  discharged_ids <- outcomes_data[which(outcomes_data$redcap_event_name == events[[project]][["discharge"]] &
                                          (outcomes_data[paste0(discharge_instrument,"_complete")] == 2 | 
                                             outcomes_data[paste0(expiration_instrument,"_complete")] == 2)),]$record_id
  active_ids <- unique(outcomes_data$record_id)[!unique(outcomes_data$record_id) %in% discharged_ids]
  if(length(active_ids) == 0){
    return(NULL)
  }
  admission_data <- outcomes_data[which(outcomes_data$redcap_event_name == events[[project]][["admission"]] &
                                        outcomes_data$record_id %in% active_ids),]
  Rows <- length(active_ids)
  data <- data.frame(ID = rep(0,Rows),Name = rep(NA,Rows), Interval = rep(NA,Rows), Project = rep(NA,Rows))
  data$ID <- active_ids
  #data$Clinician <- admission_data$psychiatrist
  for (i in 1:Rows){
    #print(i)
    #alert(i)
    patient_data <- outcomes_data[which(outcomes_data$record_id == data$ID[i]),]
    data$Name[i] <- as.character(paste(patient_data$last_name[1],patient_data$first_name[1],sep=","))
    data$Project[i] <- head(patient_data$project_label,1)
    #alert(data$Name[i])
    if (!is.na(patient_data$manual_admission_date[1])){
      time <- today() - as.Date(as.POSIXct(patient_data$manual_admission_date[1], format = "%Y-%m-%d"))
    } else {
      time <- today() - as.Date(as.POSIXct(patient_data[admission_timestamp][[1]], format = "%Y-%m-%d"))
    }
    data$Interval[i] <- (if(length(FREQUENCY) == 1){FREQUENCY}else{FREQUENCY[project][[1]]}*(nrow(patient_data))) - as.integer(time)
  }
  
  data$Action <- shinyInput(actionButton, nrow(data), 'button_', label = "Update Patient Specific Information", onclick = paste0('Shiny.onInputChange(\"update_button_pending\", this.id + "_" + Math.random())'))
  #alert(data)
  return(data)
}
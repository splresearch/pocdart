# Incomplete forms, incomplete events
# Store all "next" repeating forms in csv, updated daily
library(RCurl)
library(stringr)
library(shiny)
library(reshape2)
library(lubridate)
source("shiny_code_repo/redcap_api/extractData.R")
source("misc/global_variables.R")

getSurveyPatients <- function(timepoint, ref_date = today(), custom_timepoint = c(0,0),custom_label = NA,reset = "Y") {
  # Get data
  all_data <- extractData("all", c("first_name", "last_name", "project_label","_complete$", "_timestamp$"), survey = T)
  # Omit discharged patients
  for (j in (j = 1:length(unique(all_data$project_label)))){
    i <- unique(all_data$project_label)[j]
    #print(i)
    survey_data <- subset(all_data, subset = project_label %in% i)
    dsc_patients <- survey_data$record_id[which(survey_data$redcap_event_name == events[[i]][["discharge"]] &
                                                  ((survey_data[paste0(tail(assay_list[["Initial"]][[i]],1), "_complete")] == 2 |
                                                      !is.na(survey_data[paste0(tail(assay_list[["Initial"]][[i]],1), "_timestamp")])) |
                                                     (survey_data[paste0(expiration_instrument, "_complete")] == 2 |
                                                        !is.na(survey_data[paste0(expiration_instrument, "_timestamp")]))))]
    unfinished_patients <- survey_data$record_id[which(survey_data$redcap_event_name == events[[i]][["admission"]] &
                                                         survey_data[paste0(tail(assay_list[["Initial"]][[i]],1), "_complete")] != 2 &
                                                            survey_data[paste0(expiration_instrument, "_complete")] != 2)]
    if (any(colnames(all_data) == "emr_trigger_complete")){
    emr_triggered_patients <- survey_data$record_id[which(survey_data$redcap_event_name == events[[i]][["discharge"]] &
                                                            (survey_data[paste0("emr_trigger_complete")] == 2))]
    }
    #Get Incomplete Patients
    incomplete_admission <- subset(survey_data, subset = redcap_event_name %in% events[[i]]["admission"])[as.logical(rowSums(subset(survey_data, subset = redcap_event_name %in% events[[i]][["admission"]])[paste0(c(admission_form,assay_list[["Initial"]][[i]]),"_complete")]) != (length(assay_list[["Initial"]][[i]])+1)*2),]
    incomplete_repeating <- subset(survey_data, subset = redcap_event_name %in% events[[i]]["repeating"])[as.logical(rowSums(subset(survey_data, subset = redcap_event_name %in% events[[i]][["repeating"]])[paste0(c(assay_list[["Repeating"]][[i]]),"_complete")]) != (length(assay_list[["Repeating"]][[i]]))*2),]
    incomplete_discharge <- subset(survey_data, subset = redcap_event_name %in% events[[i]]["discharge"])[as.logical(rowSums(subset(survey_data, subset = redcap_event_name %in% events[[i]][["discharge"]])[paste0(c(discharge_instrument,assay_list[["Initial"]][[i]]),"_complete")]) != (length(assay_list[["Initial"]][[i]])+1)*2),]
    
    #incomplete_admission <- incomplete_admission[-which(incomplete_admission$record_id %in% incomplete_discharge$record_id),]
    #incomplete_repeating <- incomplete_repeating[-which(incomplete_repeating$record_id %in% incomplete_discharge$record_id),]
    #Remove Discharged
    survey_data <- survey_data[which(!(survey_data$record_id %in% dsc_patients)),]
    if(!timepoint %in% c("discharge","incomplete")){
    survey_data <- survey_data[which(!(survey_data$record_id %in% unfinished_patients)),]
    }
    if (nrow(survey_data) == 0){
      next
    }
    
    incomplete_patients <- rbind(incomplete_admission,incomplete_repeating,incomplete_discharge)
    #incomplete_patients <- survey_data[as.logical(rowSums(survey_data[paste0(c("demographics_information",assay_list),"_complete")] != 2)), ]
    incomplete_patients <- incomplete_patients[complete.cases(incomplete_patients$record_id),]
    #incomplete_patients <- incomplete_patients[-which(incomplete_patients$record_id %in% dsc_patients),]
    #Remove Expired patients from incomplete patient list
    
    # Remove discharged patients from incompletes
    if (nrow(incomplete_patients) != 0 & length(dsc_patients) != 0) {
      if (sum(incomplete_patients$record_id %in% dsc_patients) > 0) {
        incomplete_patients <- incomplete_patients[-which(incomplete_patients$record_id %in% dsc_patients),]
      }
      #incomplete_patients <- incomplete_patients[complete.cases(incomplete_patients[[paste0(expiration_instrument,"_complete")]]),]
      #expired_dsc <- survey_data$record_id[which(survey_data[[paste0(expiration_instrument,"_complete")]] == 2)]
      # if(any(str_detect(colnames(survey_data),"emr_trigger_complete"))){
      #   # expired_patients <- survey_data$record_id[which(survey_data$redcap_event_name == events[[i]][["discharge"]] &
      #   #                                                   (survey_data[paste0(expiration_instrument, "_complete")] == 2))]
      #   #if(length(expired_patients) != 0){
      #   #incomplete_patients <- incomplete_patients[-which(incomplete_patients$record_id %in% dsc_patients),]
      #   #}
      #   incomplete_patients <- incomplete_patients[-which(incomplete_patients$record_id %in% emr_triggered_patients),]
      # }
      #incomplete_patients <- incomplete_patients[which(incomplete_patients[[paste0(expiration_instrument,"_complete")]] != 2),]
    }
    
    if (timepoint == "incomplete" && nrow(incomplete_patients) == 0) {
      next
    }
    
    ### Set patients & event given timepoint ###
    if (timepoint == "discharge") {
      patients <- unique(survey_data$record_id)
    } else if (timepoint == "incomplete"){
      patients <- unique(c(incomplete_patients$record_id))
    } else {
      #patients <- c()
      
      # Calculate days since first survey event (admission)
      survey_data$position <- as.POSIXlt(ref_date, format = "%Y-%m-%d") - as.POSIXlt(survey_data[[admission_timestamp]], format = "%Y-%m-%d %H:%M:%S")
      survey_data$position[which(is.na(survey_data$position))] <- as.POSIXlt(ref_date, format = "%Y-%m-%d") - as.POSIXlt(survey_data[[paste0(expiration_instrument,"_timestamp")]][which(is.na(survey_data$position))], format = "%Y-%m-%d %H:%M:%S")
      units(survey_data$position) <- "days"
      survey_data$position <- ceiling(as.numeric(survey_data$position))
      
      for (patient in unique(survey_data$record_id)) {
        survey_data$position[which(survey_data$record_id == patient)] <- survey_data$position[which(survey_data$record_id == patient)[1]]
      }
      
      # Omit patients not due until after frequency reached
      patients_not_due <- survey_data$record_id[which(survey_data$position < if(length(FUTUREWINDOW) == 1){FUTUREWINDOW}else{FUTUREWINDOW[i][[1]]})] #tag, review
      if (length(patients_not_due) > 0) { survey_data <- survey_data[which(!(survey_data$record_id %in% patients_not_due)),] }
      
      # Calculate days since admission survey taken
      survey_data$position <- survey_data$position %% if(length(FREQUENCY) == 1){FREQUENCY}else{FREQUENCY[i][[1]]}
      
      # Omit patients who have already completed survey for this cycle
      patients_completed <- c()
      for (patient in unique(survey_data$record_id)) {
        #print(patient)
        # Pull patient's latest event
        repeat_instances <- na.omit(survey_data[which(survey_data$record_id == patient), "redcap_repeat_instance"])
        if (length(repeat_instances) != 0) {
          max_repeat_instance <- max(repeat_instances, na.rm = T)
          patient_data <- survey_data[which(survey_data$record_id == patient & survey_data$redcap_repeat_instance == max_repeat_instance),]
        } else {
          patient_data <- survey_data[which(survey_data$record_id == patient & survey_data$redcap_event_name == events[[i]]["admission"]),]
        }
        
        survey_date <- patient_data[1, paste0(starting_instrument, "_timestamp")]
        expire_date <- patient_data[1, "survey_expiration_timestamp"]
        if (!is.na(survey_date)) { 
          days_elapsed <- as.POSIXlt(ref_date, format = "%Y-%m-%d") - as.POSIXlt(survey_date, format = "%Y-%m-%d %H:%M:%S")
        } else {
          days_elapsed <- as.POSIXlt(ref_date, format = "%Y-%m-%d") - as.POSIXlt(expire_date, format = "%Y-%m-%d %H:%M:%S")
        }
        units(days_elapsed) <- "days"
        if (length(as.numeric(days_elapsed)) != 0 && !is.na(days_elapsed)) {
          # If patient has completed survey in frequency window AND not in current survey window
          if (ceiling(as.numeric(days_elapsed)) < patient_data$position + INVITE && patient_data$position < if(length(FREQUENCY) == 1){FREQUENCY}else{FREQUENCY[i][[1]]} - INVITE) { #tag, needs review
            patients_completed <- c(patients_completed, patient)
          }
        }
        
      }
      
      if (length(patients_completed) > 0) { 
        print(patients_completed)
        survey_data <- survey_data[which(!(survey_data$record_id %in% patients_completed)),] 
      }
      
      ### Timepoint logic ###
      adm_data <- survey_data[which(survey_data$redcap_event_name == events[[i]]["admission"]),]
      
      print(adm_data$record_id)
      
      if (timepoint == "custom") {
        custom_timepoint <- (as.numeric(if(length(FREQUENCY) == 1){FREQUENCY}else{FREQUENCY[i][[1]][[1]]}) + custom_timepoint) %% as.numeric(if(length(FREQUENCY) == 1){FREQUENCY}else{FREQUENCY[i][[1]]})
        if (custom_timepoint[1] <= custom_timepoint[2]) {
          patients <- adm_data$record_id[which(adm_data$position >= custom_timepoint[1] & adm_data$position <= custom_timepoint[2])]
        } else { patients <- adm_data$record_id[which(adm_data$position >= custom_timepoint[1] | adm_data$position <= custom_timepoint[2])] }
      } else if (timepoint == "today") {
        patients <- adm_data$record_id[which(adm_data$position == 0)]
      } else if (timepoint == "tomorrow") {
        patients <- adm_data$record_id[which(adm_data$position >= (if(length(FUTUREWINDOW) == 1){FUTUREWINDOW}else{FUTUREWINDOW[i][[1]]}))]
      } else if (timepoint == "pastDue") {
        patients <- adm_data$record_id[which(adm_data$position > 0 & 
                                               adm_data$position <= EXPIRATION)]
      } else if (timepoint == "expired") {
        patients <- adm_data$record_id[which(adm_data$position > EXPIRATION & adm_data$position < if(length(FUTUREWINDOW) == 1){FUTUREWINDOW}else{FUTUREWINDOW[i][[1]]})]
      }
    }
    patients <- melt(patients)
    if(nrow(patients) == 0){return(NULL)}
    patients$Project <- i
    if (j == 1){
      patients.Dat <- patients
    } else {
      if (exists("patients.Dat")){
        patients.Dat <- rbind(patients.Dat,patients)
      } else {
        patients.Dat <- patients
      }
    }
  }
  # print(reset)
  # print(custom_label)
  # print(timepoint)
  if(is.na(reset)){
    current_records <- read.csv(paste0("data/",if(is.na(custom_label)){timepoint}else{custom_label},"_surveys.csv"))
    patients.Dat <- patients.Dat[-which(patients.Dat$value %in% current_records$Record),]
  }
  
  
  if (!exists("patients.Dat")){
    return(NULL)
  }
  return(patients.Dat)
}

### Assemble output table ###

getSurveyLinks <- function(timepoint, patients, ids_only = NA) {
  # load survey_data
  survey_data <- extractData("all", c("first_name", "last_name", "project_label", "_complete$", "_timestamp$"), survey = T)
  if(nrow(patients) == 0){
    return(NULL)
  }
  #print(patients)
  table <- data.frame()
  names <- c()
  handle <- getCurlHandle()
  for (i in 1:nrow(patients)) {
    # print(i)
    patient_data <- survey_data[which(survey_data$record_id == patients$value[i] & survey_data$project_label == patients$Project[i]),]
    patient <- patient_data$record_id[1]
    project <- patient_data$project_label[1]
    # Set name if provided
    if (is.na(patient_data$first_name[1]) || is.na(patient_data$last_name[1])) { name <- patient 
    } else { name <- paste0(patient_data$last_name[1], ", ", patient_data$first_name[1]) }
    if (is.na(ids_only)) {
      # Set survey link generation parameters
      if (timepoint == "discharge") {
        event <- events[[project]][["discharge"]]
        survey_instrument <- discharge_instrument
        repeat_instance <- NA
        link_text <- "Open survey"
      } else if (timepoint == "incomplete"){
        patient_data <- patient_data[complete.cases(patient_data$record_id),]
        patient_data <- tail(patient_data,1)
        #Define event based on redcap_event from data
        event <- patient_data$redcap_event_name
        #Set repeat instance if in repeating arm
        if (event == events[[project]][["repeating"]]){
          repeat_instance <- patient_data$redcap_repeat_instance
        } else {
          repeat_instance <- NA
        }
        #Direct survey launch based upon completion 
        if (!is.na(patient_data[paste0(admission_form,"_complete")])){
          if (patient_data[paste0(admission_form,"_complete")] != 2){
            survey_instrument <- "demographics_information"
          }
        }
        if (patient_data[paste0(starting_instrument,"_complete")] != 2){
          survey_instrument <- starting_instrument
        } else {
          #Dynamically check all assays in assay_list for completion
          for (i in (i = 1:length(assay_list[["Initial"]][[project]]))){
            if (event == events[[project]][["repeating"]] && !(assay_list[["Initial"]][[project]][i] %in% assay_list[["Repeating"]][[project]])){next}
            if (patient_data[paste0(assay_list[["Initial"]][[project]][i],"_complete")] != 2){
              survey_instrument <- assay_list[["Initial"]][[project]][i]
              break
            }
          }
        }
        link_text <- "Open survey"
      } else {
        # Set event
        event <- events[[project]][["repeating"]]
        instances <- patient_data$redcap_repeat_instance[which(is.na(patient_data[admission_complete]))]
        
        if (timepoint == "expired") {
          # Determine instance number
          if (length(na.omit(instances)) > 0) { repeat_instance <- max(instances, na.rm = T)
          } else { repeat_instance <- 1 } #first instance in patient_repeating
          
          survey_instrument <- expiration_instrument
          link_text <- "Expiration Report"
        } else { 
          # Determine instance number
          if (length(na.omit(instances)) > 0) { repeat_instance <- max(instances, na.rm = T) + 1
          } else { repeat_instance <- 1 } #first instance in patient_repeating
          
          survey_instrument <- starting_instrument
          link_text <- "Open survey"
        }
      }
      if (!exists("survey_instrument")){next}
      
      # Generate survey url
      survey_url <- postForm(
        uri=api_url,
        token=survey_key[[project]],
        curl = handle,
        content='surveyLink',
        format='json',
        instrument=survey_instrument,
        event=event,
        record=patient,
        repeat_instance=repeat_instance,
        returnFormat='json'
      )
      qr_url <- gsub(".*=","",survey_url)
      
      if (timepoint == "expired") {
        #link <- HTML(paste0('<a href=', survey_url, ' target="_parent">', link_text, '</a>'))
        link <- survey_url
        table <- rbind(table, c(patient,repeat_instance,name, link,project))
      } else {
        # frameless_url <- gsub("^https", "frameless", survey_url)
        #link <- HTML(paste0('<a onclick=piwikLinker("Other") href=', survey_url, ' target="_parent">', link_text, '</a>'))
        link <- survey_url
        Action = shinyInput(actionButton, 1, paste0('button_',qr_url,"_",unique(patient_data$record_id)), label = "Show QR", 
                            onclick = paste0('Shiny.onInputChange(\"select_button_QR','\",  this.id + "_" + Math.random())'))
        if (timepoint == "discharge"){
          Discharge <- shinyInput(actionButton, 1, paste0('button_',unique(patient_data$project_label),"_",unique(patient_data$record_id)), label = "Administrative Discharge",
                                  onclick = paste0('Shiny.onInputChange(\"select_button_discharge','\",  Math.random()+this.id)'))
          table <- rbind(table, c(patient,repeat_instance,name, link, Action,Discharge,project))
        } else if (timepoint == "incomplete"){
          expiry_url <- postForm(
            uri=api_url,
            token=survey_key[[project]],
            curl = handle,
            content='surveyLink',
            format='json',
            instrument=expiration_instrument,
            event=event,
            record=patient,
            repeat_instance=repeat_instance,
            returnFormat='json'
          )
          expiry_text <- "Expire Survey"
          expirationlink <- expiry_url
          table <- rbind(table, c(patient, repeat_instance, event, name, link, Action, expirationlink, project))
        } else {
          table <- rbind(table, c(patient, repeat_instance, name, link, Action, project))
        }
      }
    } else {
      names <- c(names, name) }
  }
  
  colnames(table) <- switch(as.character(ncol(table)),"5" = c("Record","Instance","Patient", "Link","Project"),
                            "6" = c("Record","Instance","Patient", "Link", "QR-Link","Project"),
                            "7" = c("Record","Instance","Patient", "Link", "QR-Link","Expire","Project"),
                            "8" = c("Record","Instance","Event","Patient", "Link", "QR-Link","Expire","Project"))
  
  # if (ncol(table) == 6) { colnames(table) <- c("Record","Instance","Patient", "Link", "QR-Link","Project")
  # } else if (ncol(table) == 5) { colnames(table) <- c("Record","Instance","Patient", "Link","Project") 
  # } else if (ncol(table) == 7) { colnames(table) <- c("Record","Instance","Patient", "Link", "QR-Link","Expire","Project")
  # } else if (ncol(table) == 8) { colnames(table) <- c("Record","Instance","Event","Patient", "Link", "QR-Link","Expire","Project")}
  
  # print(paste(timepoint, "done"))
  
  if (nrow(table) == 0 && length(names) == 0) { return(NULL) }
  
  if (!is.na(ids_only)) {
    ids <- patients
    if (ids_only == "record_ids") { names(ids) <- patients
    } else if (ids_only == "names") { names(ids) <- names }
    return(ids)
  }
  
  return(table)
}

getSurveys <- function(timepoint, ref_date = today(), ids_only = NA, custom_timepoint = c(0,0), custom_label = NA,reset = "Y") {
  patients <- getSurveyPatients(timepoint, ref_date, custom_timepoint,custom_label,reset)
  if (is.null(patients)){return(NULL)}
  return(getSurveyLinks(timepoint, patients, ids_only))
}

getOutpatientSurveys <- function(program, ref_date = today(), ids_only = NA) {
  survey_data <- extractData("all", c("first_name", "last_name", "mrn", "_complete$", "_timestamp$"), survey = T)
  
  # Select file with timepoint
  upload_offset_table <- c("0" = NA, "1" = 2, "2" = 2, "3" = 2, "4" = 4, "5" = 4, "6" = NA)
  upload_offset <- unname(upload_offset_table(format(Sys.Date(), "%w")))
  
  outpatient_data <- read.csv(paste0("/home/outpatient-data/docs/prom_", format(Sys.Date() - upload_offset, "%Y%m%d"), ".csv"), header = T)
  program_data <- subset(outpatient_data, outpatient_data$DEPT == toupper(program))
  
  # Map mrns to record ids
  patients <- c()
  for (mrn in unique(program_data$MRN)) {
    patients <- c(patients, survey_data$record_id[which(survey_data$mrn == mrn)])
  }
  survey_table <- getSurveyLinks(timepoint = "today", patients, ids_only)
  
  # Append appointment times
  survey_table$appt_time <- NA
  for (row in 1:nrow(survey_table)) {
    mrn <- survey_data$mrn[which(survey_table$Record[row] == survey_data$record_id)]
    survey_table$appt_time[row] <- outpatient_data$APPT_TIME[which(outpatient_data$MRN == mrn)[1]]
  }
  return(survey_table)
}

# n <- 0
# getSurveys("today", today() + n)
# print(getSurveys("tomorrow", today() + n))
# print(getSurveys("pastDue", today() + n))
# print(getSurveys("expired", today() + n))
# print(getSurveys("incomplete", today()))
# print(getSurveys("discharge"))
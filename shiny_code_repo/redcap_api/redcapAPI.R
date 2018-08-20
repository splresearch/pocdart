# source("shiny_code_repo/redcap_api/keys.R")
library(redcapAPI)
library(REDCapR)
library(plyr)
library(dplyr)

getData <- function(api_url, key) {
  # for ( i in (i = 1:length(key))){
    #If using fields, set fields = redcap.Fields and define redcap.Fields in global_variables.R
    outcomes_data <- exportRecords(redcapConnection(url = api_url, token = key[[1]][[1]]), fields = NULL, survey = TRUE, factors = F)
    
    if(any(str_detect(outcomes_data$record_id,"diagnostic"))){
      diagnostic_info <- subset(outcomes_data,record_id == "diagnostic")
      diagnostic_info$project_label <- names(key)[1]
      write.csv(diagnostic_info, "data/diagnostic_data.csv")
      outcomes_data <- dplyr::filter(outcomes_data, !grepl("diagnostic",redcap_event_name))
    }
    
    if(exists("satisfaction_assay")){
      satisfaction_info <- subset(outcomes_data,redcap_event_name %in% satisfaction_assay)
      satisfaction_info$project_label <- "SATISFACTION"
      write.csv(satisfaction_info, "data/satisfaction_data.csv")
      outcomes_data <- outcomes_data[-which(outcomes_data$redcap_event_name %in% satisfaction_assay),]
    }
    
    
    outcomes_data <- outcomes_data %>% 
      mutate(project_label = "")
    for(i in (i = 1:length(events))){
      print(unlist(events[[i]],use.names = F))
      
      outcomes_data$project_label[which(outcomes_data$redcap_event_name %in% events[[i]])] <- names(events[i])
      # outcomes_data <- outcomes_data %>% group_by(redcap_event_name) %>%
      #   mutate(project_label = case_when(any(redcap_event_name == unlist(events[[i]],use.names = F)) ~ names(events[i]),
      #                                    TRUE ~ project_label
      #   ))
    }
    outcomes_data <- outcomes_data %>% filter(project_label %in% names(survey_key))
    
    
    
    #Workaround for missing completion fields
    if(!any(str_detect(colnames(outcomes_data),"_complete"))){
      #use alternate API call to get completion information
      completion_data <- redcap_read(redcap_uri = api_url, token = key[[i]], fields=NULL)$data
      completion_data <- completion_data[grep("_complete",colnames(completion_data))]
      #Bind to outcomes_data
      outcomes_data <- cbind(outcomes_data,completion_data)
    }
    #outcomes_data <- subset(outcomes_data, subset = redcap_event_name %in% unname(events[[names(key)[i]]]))
    
    # Manual admission override
    outcomes_data[admission_timestamp][which(!is.na(outcomes_data$manual_admission_date)),] <- paste0(outcomes_data$manual_admission_date[which(!is.na(outcomes_data$manual_admission_date))], " 00:00:00")
    #outcomes_data$project_label <- names(key)[i]
    # if (i == 1){
    #   collated_data <- outcomes_data
    # } else {
    #   collated_data <- rbind.fill(collated_data,outcomes_data)
    # }
  #}
  #print(collated_data)
  write.csv(outcomes_data, "data/outcomes_data.csv")
  return(NULL)
}

getData(api_url, survey_key)
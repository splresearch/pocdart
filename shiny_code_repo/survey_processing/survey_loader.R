library(redcapAPI)
library(REDCapR)
library(dplyr)
survey_loader <- function(csvTitle,max_check = NA){
  #' Loads and processes stashed surveys for appropriate display
  #'
  #' Checks to see if appropriate survey has been taken, removes from list if taken. Allows for post-processing of stashed links
  #' 
  #' @param csvTitle Title of survey to be loaded and prepared.
  #' @param max_check If set, surveys of a higher redcap repeat instance than the defined variable are not loaded
  
  #Load stashed CSVs
  if (csvTitle == "incomplete-new"){
    csv <- read.csv(paste0("data/incomplete_surveys.csv"))
  } else {
    csv <- read.csv(paste0("data/",csvTitle,"_surveys.csv"))
  }
  #Return NULL if CSV is empty
  if(is.na(csv[[1]][1])){return(NULL)}
  
  #Trim data and process data to make checks simpler
  csv$Record <- as.character(csv$Record)
  holder_data <- outcomes_data %>% group_by(record_id) %>%
    mutate(redcap_event_name = factor(redcap_event_name,unlist(events,use.names = FALSE))) %>% 
    arrange(redcap_event_name) %>% dplyr::select(record_id,redcap_repeat_instance,
                                          redcap_event_name,project_label,
                                          contains(paste0(expiration_instrument,"_complete")),
                                          contains(paste0(tail(unlist(assay_list,use.names = FALSE),1),"_complete")),
                                          contains(paste0(discharge_instrument,"_complete"))
                                          ) %>% arrange(record_id) %>% 
    slice(c(n())) %>% 
    replace(., is.na(.), 0) %>% ungroup() %>%
    mutate(record_id = as.character(record_id))
  colnames(holder_data) <- c("Record","redcap_repeat_instance","redcap_event_name","project","expiration","last_completion","discharge")
    csv <- dplyr::left_join(csv,subset(holder_data, Record %in% csv$Record), by = "Record")
    
    
  if (nrow(csv) == 1 && all(is.na(csv[1,]))){
    return(NULL)
  } else {
    #Clear CSV of completed surveys
    csv <- csv %>%
      group_by(Record)

    if (csvTitle %in% c("today","tomorrow","pastDue","custom")){
      
      csv <- csv %>%
        filter(Instance!=redcap_repeat_instance)

    } else if (csvTitle == "discharge") {
      csv <- csv %>% 
        mutate(discharge = case_when(
          redcap_event_name == events[[project[1]]][["discharge"]] & discharge %in% 2 ~ 'TRUE',
          TRUE ~ 'FALSE'
        )) %>%
        filter(discharge == "FALSE")
      
    } else if (csvTitle == "expired"){
      
      csv <- csv %>%
        filter(Instance != redcap_repeat_instance & expiration != 2)
    } else if (csvTitle == "incomplete"){
      csv <- csv %>%
        filter(last_completion != 2  & expiration != 2)
    } else if (csvTitle == "incomplete-new"){
      csv <- csv %>% 
        filter(redcap_repeat_instance == 0) %>%
        filter(last_completion != 2 & expiration != 2)  %>%
        dplyr::filter(!grepl("discharge",redcap_event_name))
    } else {
      csv <- csv %>%
        filter(Instance!=redcap_repeat_instance)
    }
  }
  
    #Process CSV for display
  if (nrow(csv) == 0){
    return(NULL)
  } else {
    csv <- csv %>% dplyr::select(-redcap_repeat_instance,-redcap_event_name,-project,-expiration,-last_completion,-discharge)
    if (csvTitle %in% c("today","tomorrow","pastDue","custom")){
      #csv$Link <- apply(csv["Link"], 1, function(x) paste0("http://pocdart.org/survey_redirect?survey_string=",regmatches(x,gregexpr("(?<==).*",x,perl=TRUE))) )
      csv$Link <- apply(csv["Link"], 1, function(x) HTML(paste0('<a href=', x, ' target="_parent">', "Open Survey", '</a>')) )
    } else if (csvTitle %in% c("incomplete","incomplete-new")){
      #csv$Link <- apply(csv["Link"], 1, function(x) paste0("http://pocdart.org/survey_redirect?survey_string=",regmatches(x,gregexpr("(?<==).*",x,perl=TRUE))) )
      csv$Link <- apply(csv["Link"], 1, function(x) HTML(paste0('<a href=', x, ' target="_parent">', "Open Survey", '</a>')) )
      
      #csv$Expire <- apply(csv["Expire"], 1, function(x) paste0("http://pocdart.org/survey_redirect?survey_string=",regmatches(x,gregexpr("(?<==).*",x,perl=TRUE))) )
      csv$Expire <- apply(csv["Expire"], 1, function(x) HTML(paste0('<a href=', x, ' target="_parent">', "Launch Expiration", '</a>')) )
    } else if (csvTitle %in% c("expired")){
      #csv$Link <- apply(csv["Link"], 1, function(x) paste0("http://pocdart.org/survey_redirect?survey_string=",regmatches(x,gregexpr("(?<==).*",x,perl=TRUE))) )
      csv$Link <- apply(csv["Link"], 1, function(x) HTML(paste0('<a href=', x, ' target="_parent">', "Launch Expiration", '</a>')) )
    } else {
      #csv$Link <- apply(csv["Link"], 1, function(x) paste0("http://pocdart.org/survey_redirect?survey_string=",regmatches(x,gregexpr("(?<==).*",x,perl=TRUE))) )
      csv$Link <- apply(csv["Link"], 1, function(x) HTML(paste0('<a href=', x, ' target="_parent">', "Open Survey", '</a>')) )
    }
    #Return CSV
    if (!is.na(max_check)){
      csv <- csv[!rowSums(csv['Instance'] >= 2),]
      if(nrow(csv) == 0){return(NULL)}
    }
    return(csv)
  }
}
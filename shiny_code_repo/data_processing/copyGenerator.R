copyGenerator <- function(assay,repeating = NA,projects){
  temp_rowHolder <- 0
  for (p in 1:length(projects)){
    project_data <- subset(outcomes_data, project_label == projects[p])
    
    for (i in (i = 1:length(assay))){
      #print(i)
      source(paste0("shiny_code_repo/survey_repo/",assay[i],".R"))
      namelist <- subset(project_data, redcap_event_name == events[[projects[p]]][["admission"]])
      for (j in unique(project_data$record_id)){
        project_data$first_name[which(project_data$record_id == j)] <- namelist$first_name[which(namelist$record_id == j)]
        project_data$last_name[which(project_data$record_id == j)] <- namelist$last_name[which(namelist$record_id == j)]
      }
      outcomes_cleaned <- subset(project_data, project_data[,paste0(assay[i],"_copied")] %in% NA)
      outcomes_cleaned <- subset(outcomes_cleaned, outcomes_cleaned[,paste0(assay[i],"_complete")] %in% 2)
      outcomes_cleaned <- subset(outcomes_cleaned, is.na(outcomes_cleaned[,str_detect(colnames(outcomes_cleaned),paste0(starting_instrument,"_timestamp"))]) == F)
      if (i == 1){
        uncopy.Dat <- outcomes_cleaned
      } else {
        uncopy.Dat <- rbind(uncopy.Dat,outcomes_cleaned)
      }
    }
    
    Rows <- nrow(uncopy.Dat)
    if (is.na(repeating)){
      data <- data.frame(ID = rep(0,Rows),MRN = rep(0,Rows), Time = rep(0,Rows), Action = shinyInput(actionButton, Rows, 'button_', label = "To Page", onclick = paste0('Shiny.onInputChange(\"select_button_',assay,'\",  this.id)')))
    } else {
      data <- data.frame(ID = rep(0,Rows),MRN = rep(0,Rows), Time = rep(0,Rows))
    }
    if (nrow(data) == 0){
      if (length(projects) == 1){
        return(data.frame(Data = "Nothing currently needs to be copied!"))
      } else {
        next()
      }
    }
    temp_rowHolder <- temp_rowHolder + Rows
    data$ID <- uncopy.Dat$record_id
    data$MRN <- uncopy.Dat$mrn
    data$Name <- paste0(uncopy.Dat$last_name,', ',uncopy.Dat$first_name)
    data$Project <- uncopy.Dat$project_label
    for (i in (i =1:Rows)){
      if (uncopy.Dat$redcap_event_name[i] == events[[projects[p]]][["admission"]]){
        data$Time[i] <- "Admission"
      } else if (uncopy.Dat$redcap_event_name[i] == events[[projects[p]]][["discharge"]]){
        data$Time[i] <- "Discharge"
      } else {
        data$Time[i] <- paste0("Repeating :",uncopy.Dat$redcap_repeat_instance[i])
      }
    }
    
    if (p == 1 || !exists("collected_table")){
      collected_table <- data
    } else {
      collected_table <- rbind(collected_table, data)
    }
    
  }
  if (!exists("collected_table")){return(NULL)}
  if (!is.na(repeating)){
    collected_table$Action <- shinyInput(actionButton, nrow(collected_table), 'button_', label = "To Page", onclick = paste0('Shiny.onInputChange(\"select_button_all','\",  this.id+ "_" + Math.random())'))
  }
  collected_table$Rows <- seq(1,nrow(collected_table))
  return(collected_table)
}
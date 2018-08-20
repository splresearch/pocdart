stash_surveys <- function(timepoints = c("today", "tomorrow", "pastDue", "expired", "discharge", "incomplete"), ref_date = today(), 
                          custom_timepoint = c(0,0), custom_label = NA,outpatients = F, program = NA,reset = "Y") {
  #' Iterates through timepoints and locally stores surveys to be taken
  #'
  #' 
  #' @param csvTitle Title of survey to be loaded and prepared.
  #' @param max_check If set, surveys of a higher redcap repeat instance than the defined variable are not loaded
  
  source("shiny_code_repo/survey_processing/getSurveys.R")
  # if (outpatients == T){timepoints <- append(timepoints,"today")}
  
  for (timepoint in timepoints) {
    print(paste(timepoint, 'running'))

    table <- getSurveys(timepoint = timepoint, ref_date = ref_date, custom_timepoint = custom_timepoint, custom_label = custom_label, reset = reset)
    
    if (is.null(table)) { table <- data.frame(Link = NA) }
    
    if (timepoint == "custom" && is.na(custom_label)) {
      timepoint <- paste("days", paste(custom_timepoint, collapse = "-"), sep = "-")
      print(timepoint)
    } else  if (timepoint == "custom" && !is.na(custom_label)){
      timepoint <- custom_label
    }

    if(is.na(reset)){
      write.table(table, paste0("data/", timepoint, "_surveys.csv"), row.names = F,col.names = F,quote = TRUE,sep = ",",append = T)
    } else {
      write.csv(table, paste0("data/", timepoint, "_surveys.csv"), row.names = F)
    }
    Sys.sleep(.5)
  }
  return(NULL)
}

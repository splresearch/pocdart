get.Data <- function(Patient,Time, Daily = NA, assay,project){
  
  source(paste0("shiny_code_repo/survey_repo/",assay,".R"))
  # print(Patient)
  # print(Daily)
  if (is.na(Daily)){
    fields <- fields[1]
  } 

  outcomes.Data <- extractData(Patient,fields,omit = F)
  
  if (assay == starting_instrument && length(outcomes.Data) > 5){
  outcomes.Data <- outcomes.Data[c(1:3,
                                   as.integer(3+which(colSums(is.na(outcomes.Data[4:length(outcomes.Data)])) != nrow(outcomes.Data[4:length(outcomes.Data)]))))]
  if (length(outcomes.Data) == 3){outcomes.Data$survey_intro_1 <- NA}
  }
  
  if (Time == "Enroll"){
    outcomes.Data <- subset(outcomes.Data, subset = redcap_event_name %in% events[[project]][["admission"]])
  } else if (Time == "Repeat"){
    outcomes.Data <- subset(outcomes.Data, subset = redcap_event_name %in% events[[project]][["repeating"]])
  } else if (Time == "Discharge"){
    outcomes.Data <- subset(outcomes.Data, subset = redcap_event_name %in% events[[project]][["discharge"]])
  }
  
  return(outcomes.Data)
}

sort.Data <- function(patient,Daily = NA, assay,project){
  Admission <- get.Data(patient, "Enroll", Daily,assay,project)
  if(nrow(Admission) == 0){
    return(NULL)
  }
  Discharge <- get.Data(patient, "Discharge", Daily,assay,project)
  Repeating <- get.Data(patient, "Repeat", Daily,assay,project)
  Admission$redcap_repeat_instance <- 0
  if(nrow(Discharge) != 0){
    Discharge$redcap_repeat_instance <- "D"
  }
  CombinedData <- rbind(Admission,Repeating,Discharge)
  #print(CombinedData)
  if (all(is.na(CombinedData[,4]))){
    return(NULL)
  }
  if (nrow(CombinedData) == 0){
    return(NULL)
  }
  if(assay == "adh_selfassessment"){
    CombinedData <- CombinedData[grep(paste0("record_id|redcap|screen|",assay),colnames(CombinedData))]
  } else {
    CombinedData <- CombinedData[grep(paste0("record_id|redcap|",assay),colnames(CombinedData))]
  }
  return(CombinedData)
}

time.Data <- function(Patient, assay,project){
  dat <- sort.Data(Patient,"total",assay = assay,project)
  if (is.null(dat)){
    return(NULL)
  }
  if (length(which(colnames(dat)=="redcap_repeat_instance"))==0){
    dat$redcap_repeat_instance <- 0
  }
  old.Val <- unique(dat$redcap_repeat_instance)
  for (i in (i = 1:nrow(dat))){
    time.String <- dat[i,3]
    if (time.String == 0){
      dat[i,3] <- "Admission"
    } else if (time.String == "D"){
      dat[i,3] <- "Discharge"
    } else {
      #NEEDS CUSTOMIZATION PER SURVEY -TAG-
      dat[i,3] <- paste0(interval$Term, as.character(as.numeric(time.String)*interval$Multiplier))
    }
  }
  names(old.Val) <- unique(dat$redcap_repeat_instance)
  return(old.Val)
}
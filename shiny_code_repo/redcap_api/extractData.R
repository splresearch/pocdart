outcomes_data <<- read.csv("data/outcomes_data.csv", header = TRUE, stringsAsFactors = F)

extractData <- function(patients, fields, omit = F, survey = F){
  # subsets data for given fields, refenced in most ggplot-construction scripts
  options(stringsAsFactors = FALSE)
  
  patient_data <- outcomes_data
  drop_indices <- grep("timestamp", colnames(patient_data))
  if (!survey && length(drop_indices) > 0){subset(patient_data, str_detect(colnames(patient_data),"timestamp") != T)}

  # Isolate patients for visualization
  if (patients[1] != "all"){
    patient_data <- subset(patient_data, patient_data$record_id %in% patients)
  }

  # Initialize assessment_data and pull record ID
  if (any(str_detect(colnames(patient_data),"redcap_repeat_instance")) == T){
    assessment_data <- patient_data[c("record_id", "redcap_event_name", "redcap_repeat_instance")]
  } else {
    patient_data$redcap_repeat_instance <- NA
    assessment_data <- patient_data[c("record_id", "redcap_event_name", "redcap_repeat_instance")]
  }
  
  # Populate with columns containing grep(fields)
  for (field in fields){
    assessment_data = c(assessment_data, patient_data[grep(field, colnames(patient_data))])
  }
  
  # Format data for export
  assessment_data <- data.frame(assessment_data)
  assessment_data[,1] <- sapply(assessment_data[,1], function(x) as.character(x))
  
  if (omit == TRUE) {assessment_data <- na.omit(assessment_data)}
  #assessment_data[rowSums(is.na(assessment_data))!=(nrow(assessment_data)-1)]
  return(assessment_data)
}

extract.Screen <- function(patients, fields, omit = F, survey = F){
  options(stringsAsFactors = FALSE)
  
  screen_data <- prescreeningData
  
  if (patients[1] != "all"){
    screen_data <- subset(screen_data, screen_data$record_id %in% patients)
  }
  
  # Initialize assessment_data and pull record ID
  assessment_data <- c(screen_data[grep("record_id", colnames(screen_data))])
  
  # Populate with columns containing grep(fields)
  for (field in fields){
    assessment_data = c(assessment_data, screen_data[grep(field, colnames(screen_data))])
  }
  
  # Format data for export
  assessment_data <- data.frame(assessment_data)
  assessment_data[,1] <- sapply(assessment_data[,1], function(x) as.character(x))
  
  if (omit == TRUE) {assessment_data <- na.omit(assessment_data)}
  #assessment_data[rowSums(is.na(assessment_data))!=(nrow(assessment_data)-1)]
  return(assessment_data)
  
}
excludePatients <- function(rawdata, exclude = c("TOLC00037", "TOLC00172")) {
  # Remove selected patients by record id
  for (id in exclude) {rawdata <- subset(rawdata, rawdata$record_id != id)}
  return(rawdata)
}

setMax <- function(data, default = 100, buffer = 20){
  # Set max to greatest given value score rounded up to next multiple of buffer
  max <- buffer*ceiling(max(as.numeric(data), na.rm = TRUE)/buffer)
  if (max < default) {max <- default}
  return(max)
}

setMin <- function(data, default = 0, buffer = 10){
  # Set min to least given value score rounded down to next multiple of buffer
  min <- buffer*floor(min(as.numeric(data), na.rm = TRUE)/buffer)
  if (min > default) {min <- default}
  return(min)
}

formatPlabel <- function(p.value) {
  if (p.value < .001) {p.label <- "<0.001**"
  } else if (p.value < .01) {p.label <- paste(round(p.value, 3), "**", sep = "")
  } else if (p.value < .05) {p.label <- paste(round(p.value, 3), "*", sep = "")
  } else {p.label <- paste(round(p.value, 3), sep = "")}
  
  return(p.label)
}

printList <- function(list) {
  return(cat("c(\"", paste0(list, collapse = "\", \""), "\")", sep = ""))
}

# elaborate <- function(gp, record_ids, groupdata) {
#   #not called, returned object doesn't compile
#   rows <- length(record_ids)
#   groupdata2 <- data.frame(rep(record_ids, 2),
#                            groupdata,
#                            c(rep("pre", rows), rep("post", rows)),
#                            stringsAsFactors = FALSE)
#   colnames(groupdata2) <- c("record_id", "scores", "prepost")
#   groupdata2$scores <- abs(jitter(as.numeric(groupdata2$scores), .5))
#   print(groupdata2)
#   gp <- gp +
#     geom_line(data = groupdata2, 
#               mapping = aes(groupdata2$prepost, groupdata2$scores,
#                             group = groupdata2$record_id, 
#                             color = groupdata2$record_id,
#                             alpha = .005),
#               show.legend = FALSE) +
#     geom_point(data = groupdata2, 
#                mapping = aes(groupdata2$prepost, groupdata2$scores, color = groupdata2$record_id, alpha = .005),
#                size = 3,
#                show.legend = FALSE)
#   return(gp)
# }

# addPrePost <- function(data) {
#   data$prepost <- c(rep("pre", nrow(data)/2), rep("post", nrow(data)/2))
#   return(data)
# }
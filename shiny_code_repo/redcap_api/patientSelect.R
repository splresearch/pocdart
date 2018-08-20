# Filters list of patients by physician/user
selectPatients <- function(user = 999) {
  # rewrite to export named list
  
  if (user == 999) {
    # display all as record ID's
    # Remove any data without an MRN value
    outcomes_data_cleaned <- subset(outcomes_data,  is.na(mrn) != T)
    
    #SUBSET DATA TITLING DEMO
    #rr_data_cleaned <- subset(rr_data_cleaned, str_detect(rr_data_cleaned$retreat_mrn,"DEMO") == T)
    #rr_data_cleaned <- subset(rr_data_cleaned, str_detect(rr_data_cleaned$retreat_mrn,"7|6") == T)
    #
    ids <- sapply((outcomes_data_cleaned$record_id), function(x) as.character(x))
    project <- sapply((outcomes_data_cleaned$project_label), function(x) as.character(x))
    patients <- ids
    rows <- seq(1,length(patients))
    #names(patients) <- ids
    names(patients) <- sapply((paste0(outcomes_data_cleaned$last_name,", ",outcomes_data_cleaned$first_name)), function(x) as.character(x))
    patient.Frame <- rbind(patients,project,rows)
    #patients <- patients[grep("ASDR",patients)]
    return(patient.Frame)
    #return(patients)
    #return(rbind(patients,patients))
    
  } else {
    # display as patient names
    # doesn't work for "other" physicians ("3")
    patient_subset <- prescreening_data[which(prescreening_data$sp_physician == user),]

    patients <- as.character(patient_subset$record_id)
    names(patients) <- paste(patient_subset$lastname,
                             patient_subset$firstname, sep = ", ")

    patients <- patients[order(names(patients))]
    #patients <- patients[grep("ASDR",patients)]
    
    return(patients)
  }
}

convertSplToName <- function(data, spl_id) {
  # Eventually should take in srData only
  firstname <- unique(data$firstname[which(data$spl_id == spl_id)])
  lastname <- unique(data$lastname[which(data$spl_id == spl_id)])
  return(paste(lastname[which(lastname != "")],
               firstname[which(firstname != "")], sep = ", "))
}

convertIDs <- function(ids, to = "tolc") {
  if (is.null(ids)) { return(NULL) }
  if (!(to %in% c("spl", "asdr", "bincd", "cde","ndd", "tolc"))) { return(rep(NA, length(ids))) }
  if (!exists("sr_data")) { sr_data <- read.csv("data/sr_data.csv", stringsAsFactors = F) }
  
  from <- paste0(tolower(substr(ids[1], 1, nchar(as.character(ids[1]))-5)), "_id")
  if (from == "spl_id") {from <- "record_id"}
  
  if (to != "name") {
    to <- paste0(to, "_id")
    if (to == "spl_id") {to <- "record_id"}
    return_ids <- sr_data[which(sr_data[,from] %in% ids), c(from, to)]
    
    return(return_ids[match(ids, return_ids[,from]), to])
  } else {
    return(rep(NA, length(ids)))
  }
}
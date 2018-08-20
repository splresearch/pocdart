library(redcapAPI)
library(REDCapR)
library(lubridate)
library(stringr)
library(RCurl)
library(qrencoder)
library(knitr)

formatPtName <- function(name) {
  nameList <- strsplit(name, " |,")
  formattedNames <- sapply(nameList, function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))))
  formattedNames <- c(formattedNames[1], paste(formattedNames[2:length(formattedNames)], collapse = " "))
  return(formattedNames)
}

formatProvName <- function(name) {
  nameList <- strsplit(name, " |,") #Split spaces and commas
  formattedNames <- sapply(nameList, function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))) #Title case all
  postNom_index <- ifelse(nchar(formattedNames[length(formattedNames)]) != 1, length(formattedNames) - 1, length(formattedNames) - 2) #Determine index of post-nominal title (req by optional middle initial)
  formattedNames[postNom_index] <- paste0(toupper(formattedNames[postNom_index]), ",") #Upper case post-nominal titles
  formattedNames[1] <- paste(sapply(strsplit(formattedNames[1], "-"), function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))), collapse = "-") #Title case hyphenated last names
  outputName <- paste(formattedNames, collapse = " ") #Concatenate
  outputName <- gsub("  ", " ", outputName) #Remove double spaces
  return(outputName)
}

getDisplayProvName <- function(name) {
  nameList <- strsplit(name, " ")[[1]]
  postNom_index <- which(str_detect(nameList, ","))
  
  if (toupper(nameList[postNom_index]) %in% c("MD,", "PHD,", "PMHNP,")) {
    outputName <- paste("Dr.", nameList[1:postNom_index-1])
  } else { outputName <- paste(nameList[(postNom_index+1):length(nameList)], nameList[1:(postNom_index-1)])}
  
  return(outputName)
}

getDisplayProvName_jdbc <- function(ProviderName, ProviderType) {
  nameList <- strsplit(ProviderName, " ")[[1]]
  ProviderName <- paste(c(nameList[2:length(nameList)], nameList[1]), collapse = " ")
  
  if (is.na(ProviderType)) {
    outputName <- ProviderName
  } else if (str_detect(ProviderType, "PSYCHIATRIST|RESIDENT|DIRECTOR|PSYCHOLOGIST|^MD$")) {
    outputName <- paste("Dr.", nameList[1])
  } else if (ProviderType == "LCSW-C") {
    outputName <- paste("Social Worker", ProviderName)
  } else if (str_detect(ProviderType, "NURSE PRACTITIONER")) {
    outputName <- paste("Nurse Practitioner", ProviderName)
  } else { outputName <- ProviderName}
  
  outputName <- toTitleCase(tolower(outputName))
  
  return(outputName)
}

validateEmail <- function(x) {
  if (grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)) {
    return(x)
  } else return(NA)
}

validatePhone <- function(x) {
  if (grepl("^(?:\\(?([2-9]0[1-9]|[2-9]1[02-9]|[2-9][2-9][0-9])\\)?)\\s*(?:[.-]\\s*)?([2-9]\\d{2})\\s*(?:[.-]\\s*)?(\\d{4})(?:\\s*(?:#|x\\.?|ext\\.?|extension)\\s*(\\d+))?$", as.character(x), ignore.case=TRUE)) {
    return(x)
  } else return(NA)
}

removeTestPatients <- function(df) {
  ref_data <- extractData("all", c("first_name", "last_name"))
  test_patients <- filter(ref_data, str_detect(first_name, regex("zztest", ignore_case = T))) %>% dplyr::select(., record_id)
  if (nrow(test_patients) > 0) { df_clean <- filter(df, !(record_id %in% test_patients)) }
  return(df_clean)
}

removeWhitespace <- function(x) {
  return(gsub("(\\s)+$|^(\\s)+", "", x))
}

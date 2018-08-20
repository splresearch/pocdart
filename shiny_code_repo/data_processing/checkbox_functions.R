
get_checkbox_label <- function(field, codes) {
  # Access metadata from global scope
  choices <- archive_metadata$Choices..Calculations..OR.Slider.Labels[which(archive_metadata$Variable...Field.Name == field)]
  # Parse string
  choices <- strsplit(as.character(choices), " \\| ")
  # Pull matching patterns and compile results
  results <- c()
  for (code in codes) {
    row <- which(sub(", .*", "", choices[[1]]) == code)
    string <- substr(choices[[1]][row], nchar(code) + 3, nchar(choices[[1]][row]))
    results <- c(results, string)
  }
  return(results)
}

get_checkbox_code <- function(field, labels) {
  # Temporary workaround while redcapAPI calls labels
  # Access metadata from global scope
  choices <- archive_metadata$Choices..Calculations..OR.Slider.Labels[which(archive_metadata$Variable...Field.Name == field)]
  # Parse string
  choices <- strsplit(as.character(choices), " \\| ")
  # Pull matching patterns and compile results
  results <- c()
  for (label in labels) {
    row <- which(sub(".*, ", "", choices[[1]]) == label)
    string <- sub(", .*", "", choices[[1]][row])
    results <- c(results, string)
  }
  return(results)
}

find_checked <- function(data, field, patient = NA, event_name = NA, return_format = "labels", omit_zero = T) {
  # Pull field names
  fields <- colnames(data)[grep(paste0(field, "___"), colnames(data))]
  #tag, Apply event_name filter
  if (length(fields) == 0) { return(NULL) }
  # Select patients, if supplied
  if (!is.na(patient)) { data <- data[which(data$record_id == patient), ] }
  # Select fields
  fields_dat <- data[fields]
  # Remove empty rows
  fields_dat <- fields_dat[which(rowSums(is.na(fields_dat)) != ncol(fields_dat)),]
  # Summarize checked columns
  totals <- colSums(fields_dat == "Checked")
  
  if (omit_zero) { totals <- totals[which(totals != 0)] }
  fields_checked <- names(totals)
  # Get labels for checked items
  codes <- sub(".*___", "", fields_checked)
  
  if (length(codes) == 0 ) { return(NULL)
  } else if (return_format == "table") { 
    # All Patients
    # Pair labels to totals
    omit_indices <- which(codes %in% c(96, 97, 99))
    if (length(omit_indices) > 0) {
      codes <- codes[-omit_indices]
      totals <- totals[-omit_indices]
    }
    df <- data.frame(label = sapply(codes, function(x) get_checkbox_label(field, x)),
                     value = totals)
    return(df)
  } else if (return_format == "labels") { return(get_checkbox_label(field, codes))
  } else if (return_format == "codes") { return(codes) }
}

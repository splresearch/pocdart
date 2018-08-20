rowDeleter <- function(table,patient,time,assay = "all"){
  if (time == "0"){
    time.Format <- "Admission"
  } else if (time == "D"){
    time.Format <- "Discharge"
  } else {
    time.Format <- paste0("Repeating :",time)
  }
  if (assay == "all"){
    if (length(which(table$Record == patient & table$Time == time.Format)) != 0){
    #print(paste0('Deleting: ',patient,time,assay))
    table <- table[-which(table$Record == patient & table$Time == time.Format), ]
    #print(table)
    }
  } else {
    table <- table[-which(table$Record == patient & Assay == assay & Time == time.Format), ]
  }
  # print(table)
  return(table)
}

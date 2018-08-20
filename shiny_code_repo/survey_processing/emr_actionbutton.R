emrAction <- function(table,patient,time,type){
  #print(Table)
  if (nrow(table) == 0){
    emr.Text <- "Confirm Copy to EMR"
    return(emr.Text)
  }
  
  
  
  if (time == "0"){
    time.Format <- "Admission"
  } else if (time == "D"){
    time.Format <- "Discharge"
  } else {
    time.Format <- paste0("Repeating :",time)
  }
  
  if (nrow(table[with(table, Record == patient & Time == time.Format), ]) == 1){
    if (type == "text"){
      emr.Text <- "Confirm Copy to EMR"
    } else {
      emr.Text <- "color: #fff; background-color: #ff9900; border-color: #b36b00"
    }
  } else {
    if (type == "text"){
      emr.Text <- "Copied to EMR"
    } else {
      emr.Text <- "color: #fff; background-color: #008000; border-color: #000000"
    }
  }
  
  return(emr.Text)
}
daily.Report <- function(patient,time,assay,repeating = NA, project){

  dat  <- sort.Data("all","Daily",assay,project)
  if (is.null(dat)){return(NULL)}
  source(paste0("shiny_code_repo/survey_repo/",assay,".R"))

  dat <- subset(dat, subset = record_id %in% patient)
  dat <- subset(dat, subset = redcap_repeat_instance %in% time)
  
  if (nrow(dat) == 0){
    return(data.frame(Data = "Missing Data"))
  }
  if (str_detect(colnames(dat[4]),"timestamp") == T){
    dat <- dat[-c(4)]
  }
  Table <- data.frame(Label = rep(NA,Rows), Value = rep(NA,Rows), Context = rep(NA,Rows))
  names(Table) <- c(Label,"Value","Context")
  
  for (i in (i = 1:nrow(Table))){
    #print(i)
    Table[i,1] <- colnames(dat)[3+i]
    if (str_detect(Table[i,1],display_term)){
      Table[i,2] <- dat[3+i]
      if (is.na(total.Frame$Result[findInterval(dat[3+i],total.Frame$Begin)])){
        Table[i,3] <- "Missing Data"
      } else {
        Table[i,3] <- total.Frame$Result[findInterval(dat[3+i],total.Frame$Begin)]
      }
    } else if (any(str_detect(Table[i,1],"_text_") == T)){
      Table[i,2] <- NA
      Table[i,3] <- dat[3+i]
    } else if (any(str_detect(Table[i,1],"_se$") == T)){
      Table[i,2] <- dat[3+i]
      Table[i,3] <- NA
    } else if (any(str_detect(Table[i,1],Contextual) == T)){
      Table[i,2] <- dat[3+i]
      Table[i,3] <- ContextualList[[Contextual[str_detect(Table[i,1],Contextual) == T]]][as.character(dat[3+i])]
    } else {
      Table[i,2] <- dat[3+i]
      if(is.na(dat[3+i])){
        Table[i,3] <- "-"
      } else {
        Table[i,3] <- Questions[as.character(dat[3+i])]
      }
    }
  }
  
  #ASSAY CUSTOMIZATIONS - TO BE ADDED TO AS MORE ASSAYS ARE GATHERED
  
  if (assay == "whodas"){
    Table <- Table[c(1:2,6:13,3:5,14:16),]
    Table[1,2] <- paste0(round((as.numeric(Table[1,2]) * 100),2),"%")
  }
  
  if (!is.na(repeating)){
    names(Table) <- c("Survey","Value","Context")
    Table[,1] <- Titles
    return(Table)
  }
  
  Table[,1] <- Titles
  
  return(Table)
}
dartReport <- function(patient,timepoint){
  #' DART Report Generator
  #'
  #' Generates the parameters used to create an HTML report through RMarkdown specific to the patient.
  #' 
  #' @param patient The patient to be generating a report
  #' @param timepoint The timepoint of the generated report
  
  #Subset data to be patient specific
  patient_data <- outcomes_data %>% 
    group_by(record_id) %>% 
    filter(record_id == patient)
  
  #Generate report title and other timepoint specific parameters
  report_title <- paste0("Report for ",patient_data$last_name[1],", ",patient_data$first_name[1])
  
  if(timepoint == "0"){
    timepoint_data <- patient_data %>%
      filter(redcap_event_name == events[[patient_data$project_label[1]]][["admission"]])
    current_instance <- "Admission"
    assay_type <- "Initial"
  } else if (timepoint == "D"){
    timepoint_data <- patient_data %>%
      filter(redcap_event_name == events[[patient_data$project_label[1]]][["discharge"]])
    current_instance <- "Discharge"
    assay_type <- "Repeating"
  } else {
    timepoint_data <- patient_data %>%
      filter(redcap_repeat_instance == timepoint)
    current_instance <- paste0(interval$Term," ",as.numeric(timepoint) * interval$Multiplier)
    assay_type <- "Initial"
  }
  selected_project <- timepoint_data$project_label
  completion_date <- if(!is.na(timepoint_data$assessment_completion_timestamp)){timepoint_data$assessment_completion_timestamp}else{as.character(today())}
  
  #Generate report tables for templated report
  assesmentWidget <- AssessmentTable(timepoint_data,assay_type,selected_project)
  dataWidget <- dataTable(patient_data,assay_type,timepoint,selected_project)
  summarydataWidget <- summaryTable(patient_data,assay_type,timepoint, selected_project)
  
  #Return named list of params
  return(list(assessment = assesmentWidget,
              data = dataWidget,
              summary = summarydataWidget,
              title = report_title,
              instance = current_instance,
              completion = completion_date))
}

AssessmentTable <- function(dat,assay_type,project){
  j <- 1
  #Define assessment table
  assessment_Table <- data.frame("Variant" = rep(NA,length(assay_list[[assay_type]][[project]])), "Value" = rep(NA,length(assay_list[[assay_type]][[project]])))
  #Fill table with values
  for (i in assay_list[[assay_type]][[project]]){
    print(i)
    source(paste0("shiny_code_repo/survey_repo/",i,".R"))
    Table <- data.frame("Variant" = NA, "Value" = NA)
    Table$Variant <- assay_label
    if (!any(str_detect(fields_all,display_term))){
      Table$Value <- "No Standard Score"
    } else {
      if (is.na(tail(dat[fields_all[which(str_detect(fields_all,display_term))]],1))){
        Score <- NA
      } else {
        if(!is.na(Percentage)){unformatted <- signif(tail(dat[fields_all[which(str_detect(fields_all,display_term))]],1),2)}
        Score <- ifelse(is.na(Percentage),round(signif(tail(dat[fields_all[which(str_detect(fields_all,display_term))]],1),3),0),paste0(signif(tail(dat[fields_all[which(str_detect(fields_all,display_term))]],1),2)*100,"%"))
      }
      Table$Value <- paste0("<strong><font size = 1>",ifelse(is.na(promis_flip),as.character(Score),paste0(as.character(Score), "*")),"</font></strong><br>",(total.Frame$Result[findInterval(ifelse(is.na(Percentage),ifelse(is.na(Score),Score,round(Score[[1]],0)),unformatted),total.Frame$Begin)]))
    }
    
    assessment_Table$Variant[j] <- Table$Variant
    assessment_Table$Value[j] <- Table$Value

    j <- j + 1
  }
  #Format table and input interpreted scores
  assessment_Table <- assessment_Table[!grepl("<font size = 1>NA</font>", assessment_Table$Value),]
  assessment_Table <- data.frame(lapply(assessment_Table, function(x) {gsub("\n", "<br>", x)}))
  length_matrix <- ceiling(length(assay_list[[assay_type]][[project]])/2) + ceiling(length(assay_list[[assay_type]][[project]])/2)-1
  assessment_matrix <- data.frame("1-variant" = rep(NA,length_matrix),"1-value" = rep(NA,length_matrix),"space" = '<br>',"2-variant" = rep(NA,length_matrix),"2-value" = rep(NA,length_matrix))
  matrix_holder <- 1
  for(i in (i=1:length((assay_list[[assay_type]][[project]])))){
    if((i %% 2) == 0){
      assessment_matrix$X2.variant[matrix_holder] <- as.character(assessment_Table$Variant[i])
      assessment_matrix$X2.value[matrix_holder] <- as.character(assessment_Table$Value[i])
      if(matrix_holder == length_matrix){break}
      assessment_matrix$space[(matrix_holder + 1)] <- "<br>"
      matrix_holder <- matrix_holder + 2
    } else {
      assessment_matrix$X1.variant[matrix_holder] <- as.character(assessment_Table$Variant[i])
      assessment_matrix$X1.value[matrix_holder] <- as.character(assessment_Table$Value[i])
    }
  }
  #Final formatting and return of table
  colnames(assessment_matrix)  <- c(rep('&nbsp;',length(assessment_matrix)))
  assessment_matrix[is.na(assessment_matrix)] <- ' '
  assessment_matrix <- assessment_matrix[!duplicated(assessment_matrix[,1]),]
  return(assessment_matrix)
}

dataTable <- function(dat, assay_type, timepoint, project){
  #Define Data Table
  dat <- dat %>% 
    mutate(redcap_repeat_instance = as.character(seq(0,nrow(dat)-1))) %>% 
    filter(redcap_repeat_instance <= timepoint)
  #Prepare data table output
  for (j in assay_list[[assay_type]][[project]]){
    print(j)
    source(paste0("shiny_code_repo/survey_repo/",j,".R"))
    row_holder <- Rows
    
    if ((any(str_detect(fields_all,display_term)))){
      if (is.na(tail(dat[fields_all[which(str_detect(fields_all,display_term))]],1))){
        RecentVal <- NA
      } else {RecentVal <- (ifelse(timepoint == 0, signif(tail(dat[fields_all[which(str_detect(fields_all,display_term))]],1),3),
                                   signif(tail(dat[fields_all[which(str_detect(fields_all,display_term))]],1)-dat[fields_all[which(str_detect(fields_all,display_term))]][[nrow(dat)-1,1]],3))[[1]])}
      if (is.na(head(dat[fields_all[which(str_detect(fields_all,display_term))]],1))){
        AdminVal <- NA
      } else {
        AdminVal <- ((signif(head(dat[fields_all[which(str_detect(fields_all,display_term))]],1),3))[[1]])
      }
    } else {
      RecentVal <- NA
      AdminVal <- NA
    }
    assay_data <- dat[grep(str_c(fields_all,collapse = "|"),colnames(dat))]
    assay_data <- assay_data[fields_all]
    assay_data <- if(is.na(tail(assay_data,1)[1])){assay_data}else{if(is.na(Percentage)){round(signif(assay_data,3),0)}else{signif(assay_data,3)}}
    row_table <- ifelse(mass_display == "N",1,row_holder)
    if (nrow(dat) == 1){
      Table <- data.frame("Titles" = rep(NA,row_table),"Score" = rep(NA,row_table),"Interpret" = rep(NA,row_table))
      Table$Titles <- Titles[1:row_table]
      Table$Score <- if(is.na(Percentage)){round(unlist(assay_data[1,])[1:row_table],0)}else{unlist(assay_data[1,])[1:row_table]}
    } else {
      Table <- data.frame("Admission" = rep(NA,row_table),stringsAsFactors = FALSE)
      
      Table$Admission <- as.character(paste0(if (is.na(Percentage)){round(signif(unlist(assay_data[1,])[1:row_table],3),0)} else {
        val <- signif(unlist(assay_data[1,])[1:row_table],2)
        val[1] <- paste0(val[1]*100,"%")
        val
      },""))
      Table_Completion <- data.frame("Titles"= rep(NA,row_table),"Score"= rep(NA,row_table),"Interpret"= rep(NA,row_table))
      Table_Completion$Titles <- Titles[1:row_table]
      Table_Completion$Score <- if(is.na(Percentage)){round(unlist(assay_data[nrow(assay_data),])[1:row_table],0)}else{unlist(assay_data[nrow(assay_data),])[1:row_table]}
      
      if (nrow(dat) >= 2){
        if (nrow(dat) == 2){
          Table <- cbind(Table$Admission,Table_Completion)
        } else {
          for (z in (z = 2:(nrow(dat)-1))){
            Table <- cbind(Table,if(is.na(Percentage)){round(signif(melt(unname(unlist(assay_data[z,])[1:row_table]))[1],3),0)}else{
              val <- signif(unlist(assay_data[z,])[1:row_table],2)
              val[1] <- paste0(val[1]*100,"%")
              val}
            )
          }
          if (length(Table) == 2){
            Table <- cbind(Table$Admission,paste0(Table[,2]," - "),Table_Completion)
          } else {
            Table <- cbind(Table$Admission,paste0(do.call(paste, c(Table[,2:length(Table)], sep = " - "))," - "),Table_Completion)
          }
        }
      }
    }
    #Fill table with interpreted values
    for (i in (i = 1:nrow(Table))){
      if (str_detect(fields_all[i],display_term)){
        if (is.na(total.Frame$Result[findInterval(Table$Score[i],total.Frame$Begin)])){
          Table$Interpret[i] <- "Missing Data"
        } else {
          Table$Interpret[i] <- as.character(total.Frame$Result[findInterval(Table$Score[i],total.Frame$Begin)])
        }
      } else if (any(str_detect(fields_all[i],"_text_") == T)){
        Table$Interpret[i] <- NA
      } else if (any(str_detect(fields_all[i],"_se$") == T)){
        Table$Interpret[i] <- NA
      } else if (any(str_detect(fields_all[i],Contextual) == T)){
        if(is.na(Table$Score[i])){
          Table$Interpret[i] <- ""
        } else{
          Table$Interpret[i] <- as.character(ContextualList[[Contextual[str_detect(fields_all[i],Contextual) == T]]][as.character(Table$Score[i])])
        }
      } else {
        if(is.na(Table$Score[i]) || any(str_detect(fields_all[i],'raw_score'))){
          Table$Interpret[i] <- ""
        } else {
          Table$Interpret[i] <- as.character(Questions[as.character(Table$Score[i])])
        }
      }
      Table$Interpret <- (Table$Interpret)
      Table$Titles <- str_wrap(Table$Titles,25)
    }
    if ((any(str_detect(fields_all,display_term)))){
      Table$Titles[1] <- assay_label
      Table$Interpret[1] <- paste0('<big><big>',Table$Interpret[1],'</big></big>')
      Table$Score[1] <- paste0('<i><big><big><big>',if (is.na(Percentage)){signif(Table$Score[1],3)} else {paste0(signif(Table$Score[1],2)*100,"%")},'</big></big></big></i>')
      
    } 
    
    if (j == assay_list[[assay_type]][[project]][1]){
      data_table <- Table
    } else {
      val <- rep('<br>',length(Table))
      val <- data.frame(as.list(val))
      names(val)<-names(Table)
      data_table <- if(any(str_detect(Table$Interpret,'Missing Data'))){rbind(data_table,Table)}else{rbind(data_table,val,Table)}
    }
  }
  #Format and output data table
  data_table <- data.frame(lapply(data_table, function(x) {gsub("\n", "<br>", x)}))
  data_table <- data_table[!grepl("<big><big>Missing Data</big></big>", data_table$Interpret),]
  
  
  table_string <- paste0(interval$Term,ifelse(nrow(dat) == 3,interval$Multiplier*1,paste0(interval$Multiplier*1,'-',interval$Multiplier*(nrow(dat)-2))))
  colnames(data_table) <- switch(as.character(length(data_table)), "3" = c("&nbsp;","today","&nbsp;"),
                                 "4" = c("admission","&nbsp;","today","&nbsp;"),
                                 "5" = c("admission",table_string,"&nbsp;","today","&nbsp;"))
  return(data_table)
}

summaryTable <- function(dat, assay_type, timepoint, project){
  #Define Data Table
  dat <- dat %>% 
    mutate(redcap_repeat_instance = as.character(seq(0,nrow(dat)-1))) %>% 
    filter(redcap_repeat_instance <= timepoint)
  #Prepare data table output
  summaryOverview <- rep(NA,length(assay_list[[assay_type]][[project]]))
  i <- 1
  for (j in assay_list[[assay_type]][[project]]){
    print(j)
    source(paste0("shiny_code_repo/survey_repo/",j,".R"))
    if (!(any(str_detect(fields_all,display_term)))){summaryOverview[i] <- paste0(paste0('<strong>',assay_label_unformatted,'</strong><br>No Standard Scores - See detailed results'))
    i <- i + 1
    next}
    assay_data <- dat[grep(str_c(fields_all,collapse = "|"),colnames(dat))]
    assay_data <- assay_data[fields_all]
    if (is.na(round(as.numeric(tail(assay_data[1],1)),0))){summaryOverview[i] <- "<REMOVE>"
    i <- i + 1
    next}
    RecentChange <- (ifelse(timepoint == 0, signif(tail(dat[fields_all[which(str_detect(fields_all,display_term))]],1),3),
                            signif(tail(dat[fields_all[which(str_detect(fields_all,display_term))]],1)-dat[fields_all[which(str_detect(fields_all,display_term))]][[nrow(dat)-1,1]],3))[[1]])
    AdminVal <- ((signif(head(dat[fields_all[which(str_detect(fields_all,display_term))]],1),3))[[1]])
    RecentVal <- if(nrow(dat)!= 1){(dat[fields_all[which(str_detect(fields_all,display_term))]][[nrow(dat)-1,1]][[1]])}else{RecentVal <- NA}
    AdminChange <- ifelse(is.na(Percentage),round(tail(assay_data[1],1) - AdminVal,0),tail(assay_data[1],1) - AdminVal)
    IntroVal <- tail(assay_data[1],1)
    if (is.na(Percentage)){IntroVal <- round(IntroVal[[1]],0)
    AdminVal <- round(AdminVal,0)
    RecentVal <- round(RecentVal,0)
    }
    recentSyntax <- Descriptors$emr_recent[findInterval(RecentChange,Ranges$Recent)+1]
    adminSyntax <- Descriptors$emr_adm[findInterval(AdminChange,Ranges$Recent)+1]
    titleText <- paste0(assay_label_unformatted,': <strong>',ifelse(is.na(Percentage),IntroVal,paste0(signif(IntroVal,2)*100,'%')),'</strong> - ',total.Frame$Result[findInterval(IntroVal,total.Frame$Begin)])
    if (is.na(RecentVal)){
      recentText <- ""
    } else {
      recentText <- paste0('- <i> ',recentSyntax,' (',ifelse(is.na(Percentage),RecentVal,paste0(signif(RecentVal,2)*100,'%')),')</i> ')
    }
    
    if (is.na(AdminVal)){
      adminText <- ""
    } else {
      adminText <- paste0('- <i> ',adminSyntax,' (',ifelse(is.na(Percentage),AdminVal,paste0(signif(AdminVal,2)*100,'%')),')</i> ')
    }
    
    summaryOverview[i] <- paste0(titleText,'<br>',assay_range)
    
    if(nrow(dat) != 1){
      summaryOverview[i] <- paste0(summaryOverview[i],'<br>',ifelse(nrow(dat) ==2 && names(events[[project]][which(events[[project]] %in% as.character(tail(dat$redcap_event_name,1)))]) == "discharge","",paste0(recentText,' - ')),adminText)
    }
    #Format table
    if (str_detect(j,"promis")){
      promisPercentile <- round(pnorm(as.numeric(tail(assay_data[1],1)), mean = 50, sd = 10)*100,0)
      
      promisText <- paste0("- T-score of ",round(as.numeric(tail(assay_data[1],1)),0)," = ",promisPercentile," percentile: ",ifelse(100-promisPercentile <= 35,'<i>only ','<i>'),100-promisPercentile,"% of the reference population score <u>",max_type,"</u></i>")
      summaryOverview[i] <- paste0(summaryOverview[i],'<br>',promisText)
    } 
    
    if(!is.na(relevant_questions$Question[1])){
      for (z in (z = 1:length(relevant_questions$Question))){
        summaryOverview[i] <- paste0(summaryOverview[i],'<br>','- <strong>',relevant_questions$Question[z],'</strong>: ',Questions[[as.character(tail(assay_data[relevant_questions$Variable[z]],1))]])
      }
    }

    i <- i + 1
  }
  #Return Table
  summaryOverview <- summaryOverview[which(summaryOverview != "<REMOVE>")]
  return(summaryOverview)
}
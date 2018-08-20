#library(dplyr)
cohens_d <- function(x, y) {
  lx <- length(x)- 1
  ly <- length(y)- 1
  md  <- abs(mean(x) - mean(y))        ## mean difference (numerator)
  csd <- lx * var(x) + ly * var(y)
  csd <- csd/(lx + ly)
  csd <- sqrt(csd)                     ## common sd computation
  
  cd  <- md/csd                        ## cohen's d
}

graph.Assessment <- function(patient = NA, assay, project,dat){
  if (patient == 0){return(NULL)}
  source(paste0("shiny_code_repo/survey_repo/",assay,".R"))
  names(Titles) <- fields_all
  dat <- dat %>% dplyr::select(record_id,instance_type,fields_all)
  current_dat <- tail(dat,1)
  display_field <- fields_all[which(str_detect(fields_all, display_term))]
  current_interval <- findInterval(current_dat[display_field][[1]],total.Frame$Begin)
  current_value <- if(is.na(Percentage)){current_dat[fields_all[which(str_detect(fields_all, display_term))]][[1]]}else{paste0(round(current_dat[fields_all[which(str_detect(fields_all, display_term))]][[1]] * 100,0))}
  #"<span class = 'assessment-title'>",assay_label_alternate,"</span></br>
  assessment_string <- if(!is.na(current_dat[fields_all[which(str_detect(fields_all, display_term))]][[1]])){
    paste0("<span class = 'assessment-value'>",current_value,
           '</span> ',total.Frame$Color[current_interval],"</br>",score_type,"</br>")
  } else {
    assessment_string <- "<span class = 'test'>Incomplete or Expired Recent Assessment</span>"
  }
  #<span class = 'current-text'>Current</span></br>
  if (nrow(dat) != 1){
    prior_dat <- dat[nrow(dat)-1,]
    dat.Diff <- current_dat[display_field] - prior_dat[display_field][[1]]
    if (!is.na(dat.Diff)){
      if (dat.Diff < 0 && is.na(promis_flip)){
        color <- "<div class ='arrow-down-green'></div>"
      } else if (dat.Diff > 0 && is.na(promis_flip)){
        color <- "<div class ='arrow-up-red'></div>"
      } else if (dat.Diff < 0 && !is.na(promis_flip)){
        color <- "<div class ='arrow-down-red'></div>"
      } else if (dat.Diff > 0 && !is.na(promis_flip)){
        color <- "<div class ='arrow-up-green'></div>"
      } else {
        color <- "no change"
      }
      if(!is.na(Percentage)){dat.Diff <- paste0(round(dat.Diff * 100,0),"%")}
      assessment_string <- paste0(assessment_string,color,'<strong>&nbsp;&nbsp;',if(dat.Diff==0 || dat.Diff == "0%"){''}else{dat.Diff},'</strong>')
    }
  }
  
  
  
  
  return(assessment_string)
} 

graph.Labels <- function(assay){
  source(paste0("shiny_code_repo/survey_repo/",assay,".R"))
  label_string <- paste(Titles[-1],collapse = "</br>")
  
  return(label_string)
}

graph.SingleLabel <- function(patient,assay,project,field = 1){
  source(paste0("shiny_code_repo/survey_repo/",assay,".R"))
  # dat <- outcomes_data %>% group_by(record_id) %>%
  #   mutate(redcap_event_name =  factor(redcap_event_name, levels = unname(unlist(events[[project]])))) %>%
  #   arrange(redcap_event_name) %>% subset(record_id %in% patient) %>%
  #   mutate(instance_type = case_when(redcap_event_name %in% events[[project]][[1]] ~ "Admission",
  #                                    redcap_event_name %in% events[[project]][[2]] ~ paste0(interval$altTerm," ", redcap_repeat_instance * interval$Multiplier),
  #                                    redcap_event_name %in% events[[project]][[3]] ~ "Discharge"
  #   )) %>% dplyr::select(record_id,instance_type,fields_all) %>% tail(1)
  # 
  #label_string <- paste(paste0("<span style='color:dimgray;'><strong>",dat[fields_all[1+field]],'</strong>'),paste0(Titles[1+field],'</span>'), sep = " - ")
  
  label_string <- paste(paste0("<span style='color:#696969;'>",Titles[1+field],'</span>'))
  
  return(label_string)
}

graph.Bubble <- function(patient = NA, assay, project){
  
  source(paste0("shiny_code_repo/survey_repo/",assay,".R"))
  names(titles_full) <- fields_all
  dat <- outcomes_data %>% group_by(record_id) %>%
    mutate(redcap_event_name =  factor(redcap_event_name, levels = unname(unlist(events[[project]])))) %>%
    arrange(redcap_event_name) %>% subset(record_id %in% patient) %>%
    mutate(instance_type = case_when(redcap_event_name %in% events[[project]][[1]] ~ "Admission",
                                     redcap_event_name %in% events[[project]][[2]] ~ paste0(interval$altTerm," ", redcap_repeat_instance * interval$Multiplier),
                                     redcap_event_name %in% events[[project]][[3]] ~ "Discharge"
    )) %>% dplyr::select(record_id,instance_type,fields_all)
  
  x_layout <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  
  y_layout <-list(fixedrange = T,title = "",
                  zeroline = FALSE,
                  showline = FALSE,
                  showticklabels = FALSE,
                  showgrid = TRUE,
                  side="right")
  
  bubble_plot <- (dat[,-c(1,3)]) %>%
    tidyr::gather(variable, value, -instance_type) %>%
    transform(id = as.integer(factor(variable))) %>% mutate(test_titles = titles_full[variable]) %>%
    plot_ly(x = ~instance_type, y = ~test_titles, color = ~variable, colors = "Dark2",type = 'scatter', mode = 'markers',
            hoverinfo = 'text',
            text = ~paste('Timepoint: ', instance_type,
                          '</br></br> PROM Value: ', value),
            marker = list(size = ~(value+1)*3, opacity = 0.5)) %>%
    layout(title = '',margin =list(l = 25, r = 25, t = 0, b = 0),
           xaxis = x_layout,
           yaxis = y_layout,
           showlegend = FALSE) %>% config(displayModeBar = F)
  
  return(bubble_plot)
  
}

graph.SingleBubble <- function(patient = NA, assay, project, field = 1){
  
  source(paste0("shiny_code_repo/survey_repo/",assay,".R"))
  names(titles_full) <- fields_all
  names(Titles) <- fields_all
  dat <- outcomes_data %>% group_by(record_id) %>%
    mutate(redcap_event_name =  factor(redcap_event_name, levels = unname(unlist(events[[project]])))) %>%
    arrange(redcap_event_name) %>% subset(record_id %in% patient) %>%
    mutate(instance_type = case_when(redcap_event_name %in% events[[project]][[1]] ~ "Admission",
                                     redcap_event_name %in% events[[project]][[2]] ~ paste0(interval$altTerm," ", redcap_repeat_instance * interval$Multiplier),
                                     redcap_event_name %in% events[[project]][[3]] ~ "Discharge"
    )) %>% dplyr::select(record_id,instance_type,fields_all)
  
  x_layout <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE,fixedrange=TRUE
  )
  
  y_layout <-list(fixedrange = T,title = "",
                  zeroline = FALSE,
                  showline = FALSE,
                  showticklabels = FALSE,
                  showgrid = TRUE,fixedrange=TRUE,
                  side="right")
  
  bubble_plot <- (dat[,c(2,3+field)]) %>%
    tidyr::gather(variable, value, -instance_type) %>%
    transform(id = as.integer(factor(variable))) %>% mutate(test_titles = Titles[variable]) %>%
    plot_ly(x = ~instance_type, y = ~test_titles, color = ~variable, colors = "#3d3d3d",type = 'scatter', mode = 'markers',hoverinfo="none",
            text = ~paste('Timepoint: ', instance_type,
                          '</br></br> PROM Value: ', value),
            marker = list(size = ~(value+1)*3, opacity = 0.5)) %>%
    layout(title = '',margin =list(l = 25, r = 25, t = 0, b = 0),
           xaxis = x_layout,
           yaxis = y_layout,
           showlegend = FALSE) %>% config(displayModeBar = F)
  
  return(bubble_plot)
  
}

graph.SingleSpark <- function(patient = NA, assay, project, field = 1,dat,selector = "Individual Data"){
  if (patient == "" && selector != "Group Data") {return(plotly_empty() %>% config(displayModeBar = F))}
  source(paste0("shiny_code_repo/survey_repo/",assay,".R"))
  #names(titles_full) <- fields_all
  names(Titles) <- fields_all
  
  #Set Data
  # dat <- outcomes_data %>% group_by(record_id) %>%
  #   mutate(redcap_event_name =  factor(redcap_event_name, levels = unname(unlist(events[[project]])))) %>%
  #   arrange(redcap_event_name) %>% subset(record_id %in% patient) %>%
  #   mutate(instance_type = case_when(redcap_event_name %in% events[[project]][[1]] ~ "Admission",
  #                                    redcap_event_name %in% events[[project]][[2]] ~ paste0(interval$altTerm," ", redcap_repeat_instance * interval$Multiplier),
  #                                    redcap_event_name %in% events[[project]][[3]] ~ "Discharge"
  #   )) %>% dplyr::select(record_id,instance_type,fields_all) # %>% #na.omit(cols=3)
  # dat <- dat %>% mutate(instance_type =  factor(instance_type, levels = dat$instance_type)) %>% 
  if (selector == "Individual Data"){
    # print(dat)
    dat <- dat %>% subset(record_id == patient) %>% mutate(instance_type =  factor(instance_type, levels = instance_type))
  }
  
  dat <- dat %>% dplyr::select(record_id,instance_type,fields_all) %>% dplyr::select(2,3+field)
  if(any(str_detect(colnames(dat[1,3]),Contextual))){
    term_context <- TRUE
    term <- Contextual[str_detect(colnames(dat[1,3]),Contextual)]
  } else {
    term_context <- FALSE
  }
  
  
  #alert(dat)
  
  #StackedBar
  #%>% mutate(test = .[[3]])
  #dat %>% mutate(test = Questions[.[[3]]])
  
  
  #Set Layouts
  x_layout <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE,fixedrange=TRUE,
    tickangle = 45,
    type = "category",
    range = c(-.5,length(unique(dat$instance_type))-.5)
  )
  
  y_layout <-list(fixedrange = T,title = "",
                  zeroline = FALSE,
                  showline = FALSE,
                  showticklabels = FALSE,
                  showgrid = FALSE,fixedrange=TRUE,
                  side="right")
  #Set Important Values for Line Bars
  # line <- list(
  #   type = "line",
  #   x0 = 0, 
  #   x1 = 1, 
  #   line = list(color = "white",width = 1),
  #   xref = "paper"
  # )
  # 
  # lines <- list()
  # for (i in c(1,2,3,4,5)) {
  #   line[c("y0", "y1")] <- i
  #   lines <- c(lines, list(line))
  # }
  
  
  #Sliced Bar Chart
  # spark_plot <- (dat[-1]) %>%
  #   tidyr::gather(variable, value, -instance_type) %>%
  #   transform(id = as.integer(factor(variable))) %>% mutate(test_titles = Titles[variable]) %>%
  #   plot_ly(x = ~instance_type, y = ~value, color = ~variable, colors = "darkgray", type = 'bar',hoverinfo="none") %>%
  #   layout(title = '',margin =list(l = 25, r = 25, t = 0, b = 0),
  #          xaxis = x_layout,
  #          yaxis = y_layout,
  #          shapes = lines,
  #          showlegend = FALSE) %>% config(displayModeBar = F)
  #dat
  
  #%>% mutate(test_titles = case_when(variable %in% "remainder" ~ "",
  # is.na(value) ~ "",
  # !is.na(value) ~ Questions[value])
  
  #result_string <- unname(Questions[as.character(na.omit(dat_transformed[which(dat_transformed$variable != 'remainder'),])$value)])
  
  
  
  
  if (selector == "Individual Data"){
    if(!term_context){
      dat <- dat  %>% mutate(remainder = as.numeric(max(names(Questions)))-.[[3]])
      dat_transformed <- (dat[-1]) %>% #mutate(test = ifelse(is.na(value),'', 'test')) #%>% mutate(interpret = case_when(is.na(.[[2]]) ~ ""))
        tidyr::gather(variable, value, -instance_type) %>%
        transform(id = as.integer(factor(variable)))
      result_string <- unname(Questions[as.character(na.omit(dat_transformed[which(dat_transformed$variable != 'remainder'),])$value)])
    } else {
      dat <- dat  %>% mutate(remainder = as.numeric(max(names(ContextualList[[term]])))-.[[3]])
      dat_transformed <- (dat[-1]) %>% #mutate(test = ifelse(is.na(value),'', 'test')) #%>% mutate(interpret = case_when(is.na(.[[2]]) ~ ""))
        tidyr::gather(variable, value, -instance_type) %>%
        transform(id = as.integer(factor(variable)))
      result_string <- unname(ContextualList[[term]][as.character(na.omit(dat_transformed[which(dat_transformed$variable != 'remainder'),])$value)])
    }
    if (all(is.na(dat_transformed$value))){return(plotly_empty() %>% config(displayModeBar = F))}
    #result_string <- unname(Questions[as.character(na.omit(dat_transformed[which(dat_transformed$variable != 'remainder'),])$value)])
    #lapply(na.omit(dat_transformed[which(dat_transformed$variable != 'remainder'),])$value, )
    spark_plot <-  dat_transformed %>%
      plot_ly(x = ~instance_type, y = ~value) %>% 
      add_trace(#color = ~variable, colors = c('#D3D3D3','white'), 
        type = 'bar',hoverinfo='none',
        marker = list(color = c(rep('#D3D3D3',nrow(na.omit(dat[,3]))-1),'#696969',rep('white',nrow(na.omit(dat[,3])))),
                      line = list(color = '#D3D3D3',width = .25))) %>%
      add_trace(data = filter(dat_transformed, variable == fields_all[1+field], !is.na(value)),
                y = ~value,mode = 'markers',type = 'scatter',marker = list(color = 'transparent'),hoverinfo = 'text',#,
                text = result_string
      ) %>%
      layout(title = '',margin =list(l = 0, r = 0, t = 0, b = 0),
             xaxis = x_layout,
             yaxis = y_layout,hoverlabel = list(font=list(size=9)),
             showlegend = FALSE,barmode = 'stack',hovermode='closest') %>% config(displayModeBar = F)
    
  } else {
    holder <<- dat
    dat_count <- dat %>% dplyr::group_by(instance_type) %>% dplyr::count(instance_type,get(fields_all[1+field])) %>% na.omit()
    names(dat_count)[2] <- "question_values"
    dat_count <- dat_count %>% mutate(question_answers = Questions[as.character(question_values)])
    dat_count <- dat_count %>% mutate(question_answers =  factor(question_answers, levels = unname(unlist(Questions))))
    dat_count <- dat_count %>% mutate(new_Val = (n / sum(n,na.rm = T)) * 100)
    # dat_stat <- dat %>% group_by(instance_type) %>% summarise(mean = mean(.[[3]],na.rm = T),sd = sd(.[[3]],na.rm = T)) %>%
    #   mutate(base = mean-sd) %>% mutate(top = mean+sd)
    
    spark_plot <- plot_ly(dat_count) %>%
      #marker = list(color = c(rep('#D3D3D3',nrow(dat)-1),'#696969',rep('white',nrow(dat))),
      #line = list(color = '#D3D3D3',width = .25)))
      # add_trace(x = ~instance_type,
      #           y = ~mean,
      #           color = ~instance_type,
      #           colors = "gray",type = "scatter",mode="markers",hoverinfo="text", marker = list(symbol = "135",size = 12,color = "slategray"),
      #           text = ~paste('Timepoint: ', instance_type,
      #                         '</br></br>Mean: ',if(is.na(Percentage)){round(mean,1)}else{paste0(round(mean*100,1),'%')},
      #                         '</br>SD: ',if(is.na(Percentage)){round(sd)}else{paste0(round(sd*100,1),'%')}
      #           )) %>%
      # add_trace(x = ~instance_type,y = ~(2*sd), base = ~base, type = "bar",color = ~instance_type,
    #           colors = "gray",hoverinfo = "none") %>% 
    add_trace(x = ~instance_type,
              y = ~new_Val,
              type = "bar", color = ~question_answers, colors = "Blues",
              hoverinfo = 'text', text = ~paste(question_answers),
              marker = list(line = list(color = '#D3D3D3',width = .25))) %>% layout(barmode = 'stack') %>% 
      layout(title = '',showlegend = FALSE,xaxis = x_layout,
             yaxis = y_layout,hoverlabel = list(font=list(size=9)),
             margin = list(l = 0, r = 0, t = 20, b = 10)
      )  %>% config(displayModeBar = F)
    #dat <- dat  %>% mutate(remainder = as.numeric(max(names(Questions)))-.[[3]])
  }
  
  
  
  
  
  
  return(spark_plot)
  
}

graph.Sparkline <- function(patient = NA, assay, project){
  
  source(paste0("shiny_code_repo/survey_repo/",assay,".R"))
  names(titles_full) <- fields_all
  dat <- outcomes_data %>% group_by(record_id) %>%
    mutate(redcap_event_name =  factor(redcap_event_name, levels = unname(unlist(events[[project]])))) %>%
    arrange(redcap_event_name) %>% subset(record_id %in% patient) %>%
    mutate(instance_type = case_when(redcap_event_name %in% events[[project]][[1]] ~ "Admission",
                                     redcap_event_name %in% events[[project]][[2]] ~ paste0(interval$altTerm," ", redcap_repeat_instance * interval$Multiplier),
                                     redcap_event_name %in% events[[project]][[3]] ~ "Discharge"
    )) %>% dplyr::select(record_id,instance_type,fields_all)
  
  x_layout <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  
  y_layout <-list(fixedrange = T,title = '',
                  zeroline = FALSE,
                  showline = FALSE,
                  showticklabels = FALSE,
                  showgrid = FALSE,
                  side="left")
  
  y_layout2 <-list(fixedrange = T,title = ~test_titles,
                   zeroline = FALSE,
                   showline = FALSE,
                   showticklabels = TRUE,
                   showgrid = FALSE,
                   side="right")
  t <- list(
    family = "sans serif",
    size = 14,
    color = toRGB("grey50"))
  
  text_plot <- (dat[,-c(1,3)]) %>%
    tidyr::gather(variable, value, -instance_type) %>%
    transform(id = as.integer(factor(variable))) %>% 
    mutate(test_titles = titles_full[variable]) %>% 
    mutate(test_titles = factor(test_titles, levels = rev(titles_full))) %>%
    filter(instance_type == tail(dat$instance_type,1)) %>%
    group_by(variable) %>%
    plot_ly(x = 1, y = ~test_titles, color = ~variable, colors = "gray",mode = 'text',type = 'scatter', text = ~value, textposition = 'middle right',textfont = list(color = '#000000', size = 20)) %>%
    layout(title = '',margin = list(l = 0, r = 800, t = 0, b = 0),
           xaxis = x_layout,
           yaxis = y_layout2,
           showlegend = FALSE)
  
  plot <- dat[,-c(1,3)] %>%
    tidyr::gather(variable, value, -instance_type) %>%
    transform(id = as.integer(factor(variable))) %>%
    mutate(test_titles = titles_full[variable]) %>%
    group_by(variable) %>%
    do (
      val = plot_ly(.,x = ~instance_type, y = ~value, color = ~variable, type = "scatter",mode = 'lines+markers',colors = "gray",hoverinfo = 'text', fill = 'tozeroy', fillcolor='#eaeaea',
                    text = ~paste('Timepoint: ', instance_type,
                                  '</br></br> PROM Value: ', value)) %>%
        layout(title = '',
               xaxis = x_layout,
               yaxis = y_layout,
               showlegend = FALSE,
               font = list(color = '#264E86',
                           family = 'sans serif',
                           size = 15),
               #add_lines(x = ~2:4, y = ~1:3, name = "slope of 1", yaxis = "y2") %>%
               margin = list(l = 500, r = 0, t = 0, b = 0)#,
               # annotations = list(text = ~paste(tail(value,1), test_titles), showarrow = F, xref='x', xanchor = 'left', xanchor = "center",
               #                    align = "center",
               #                    x = ~tail(instance_type,1), yref='paper', y = ~tail(value,1))
        )
    )%>%
    subplot(nrows = nrow(.), shareX = TRUE,titleY = TRUE, margin = c(50,50,0,0))
  
  test <- subplot(plot,text_plot)
  
  return(test)
  
}

graph.Alternate <- function(patient = NA, assay, selector, project,dat, effect = NA,flip = NA,repeating = NA){
  #print(patient)
  if (patient == "" && selector != "Group Data") {return(plotly_empty() %>% config(displayModeBar = F))}
  dat <- dat %>% filter(project_label == project)
  source(paste0("shiny_code_repo/survey_repo/",assay,".R"))
  promis_assessment <- str_detect(assay,"promis")
  # dat <- outcomes_data %>% group_by(record_id) %>%
  #   mutate(redcap_event_name =  factor(redcap_event_name, levels = unlist(events[[project]], use.names = FALSE))) %>%
  #   arrange(redcap_event_name) %>% subset(record_id == patient) %>%
  #   mutate(instance_type = case_when(redcap_event_name == events[[project]][[1]] ~ "adm<br>",
  #                                    redcap_event_name == events[[project]][[2]] ~ paste0(interval$altTerm, redcap_repeat_instance * interval$Multiplier),
  #                                    redcap_event_name == events[[project]][[3]] ~ "dsc<br>"
  #                                    )) %>% dplyr::select(record_id,instance_type,fields_all) #%>% #na.omit(cols=3) #%>%
  # dat <- mutate(dat, instance_type =  factor(instance_type, levels = dat$instance_type))
  
  if (selector == "Individual Data"){
    # print(dat)
    dat <- dat %>% subset(record_id == patient) %>% mutate(instance_type =  factor(instance_type, levels = instance_type))
  }
  
  
  f <- list(
    family = "helvetica",
    size = if(selector == "Individual Data"){8}else{if(is.na(effect)){7}else{6}},
    color = "#7f7f7f"
  )
  
  f_title <- list(
    family = "helvetica",
    size = 11,
    color = "#7f7f7f"
  )
  x_layout <- list(
    title = "",
    ticklen = 0,
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = TRUE,
    showgrid = FALSE,
    fixedrange=TRUE,
    side = if(is.na(flip)){"top"}else{"bottom"},
    tickfont = f,
    type = "category",
    range = if(is.na(effect)){if(is.na(repeating)){c(-.5,length(unique(dat$instance_type))-.5)}else{c(-.5,1.5)}}else{c(-.5,length(unique(dat$date))-.5)}
  )
  
  line <- list(
    type = "line",
    xref = "paper",
    yref = "y",
    x0 = 1,
    x1 = 1,
    line = list(width = 5)
  )
  
  lines <- list()
  for (i in (i = 1:nrow(total.Frame))) {
    #print(i/nrow(total.Frame))
    color_array <- if(is.na(promis_flip)){c('#5CA137','#FFD240','#FF7E79','#BE1E2E','darkred')}else{rev(c('#5CA137','#FFD240','#FF7E79','#BE1E2E'))}
    line["y0"] <- total.Frame$Begin[i]  * if(selector == "Group Data" && !is.na(Percentage)){100}else{1}
    line["y1"] <- if(i == nrow(total.Frame)){100}else{total.Frame$Begin[i+1] * if(selector == "Group Data" && !is.na(Percentage)){100}else{1}}
    line$line["color"] = list("color" = color_array[i])
    #line["line"] <- list("color" = color_array[i])
    lines <- c(lines, list(line))
  }
  
  
  #if(!is.na(Percentage)){.1}else{5}
  
  y_layout <-list(range = if (promis_assessment){c(15, 85)
  }else if (selector == "Group Data" && !is.na(Percentage)){
    c(assay_min,(assay_max+.1)*100)}else{c(assay_min,assay_max)
    }, fixedrange = T,title = "",
  zeroline = TRUE,
  zerolinecolor = toRGB("lightgray"),
  zerolinewidth = 1,
  showline = FALSE,
  showticklabels = FALSE,
  tickfont = f,
  font = f,
  showgrid = TRUE,
  fixedrange=TRUE,#tickformat = "%",
  #tickformat = if(!is.na(Percentage)){"%"}else{NULL},
  dtick = if (promis_assessment){10}else{if(!is.na(Percentage)){.1}else if(assay_max <= 5){.2}else{5}},
  #ntick = 10,
  tickvals = if (promis_assessment){seq(20,80,10)}else{seq(assay_min,assay_max,if(!is.na(Percentage)){.1}else if(assay_max <= 5){.2}else{5})*if(!is.na(Percentage)){100}else{1}},
  ticktext = if (promis_assessment){c("20","","","50","","","80")
  } else {c(if(!is.na(Percentage)){paste0(assay_min,'%')
  } else {
    assay_min
  },
  rep("",length(seq(assay_min,assay_max,if(!is.na(Percentage)){.1}else if(assay_max <= 5){.2}else{5}))-2),
  if(!is.na(Percentage)){paste0(tail(seq(assay_min,assay_max,if(!is.na(Percentage)){.1}else{5}),1)*100,"%")}else{tail(seq(assay_min,assay_max,if(!is.na(Percentage)){.1}else{5}),1)})}#,
  #ticktext=rep("val",6),
  #color = "gray",
  #line = list(color = "gray",width = 3)
  )
  
  #if(!is.na(Percentage)){.1}else{5}
  
  if (promis_assessment){
    
    plot_annotations <- list(text = c('20','80'),
                             font = list(size = c(25,15,10),color = 'gray'),
                             showarrow = FALSE,
                             xref = 'paper', x = c(-.05,-.05),
                             yref = 'paper', y = c(-.15,1.2))
  } else {
    plot_annotations <- list(text = c(assay_min,assay_max),
                             font = list(size = c(25,15,10),color = 'gray'),
                             showarrow = FALSE,
                             xref = 'paper', x = c(-.05,-.05),
                             yref = 'paper', y = c(-.15,1.2))
  }
  
  #print("PROMIS ASSESSMENT")
  
  
  hline <- function(y = 0, color = "darkgray") {
    list(
      type = "line", 
      x0 = 0, 
      x1 = 1, 
      xref = "paper",
      y0 = y, 
      y1 = y, 
      line = list(color = color,width = 1)
    )
  }
  
  #display_value <- fields_all[str_detect(fields_all,display_term)]
  if (selector == "Individual Data"){
    p <- plot_ly(dat) %>%
      #marker = list(color = c(rep('#D3D3D3',nrow(dat)-1),'#696969',rep('white',nrow(dat))),
      #line = list(color = '#D3D3D3',width = .25)))
      # add_trace(x = ~instance_type, 
      #           y = ~get(fields_all[str_detect(fields_all,display_term)]),
      #           color = ~instance_type,
      #           colors = "gray",type = "bar",hoverinfo="none") %>% 
      
      add_trace(x = ~instance_type, 
                y = ~get(fields_all[str_detect(fields_all,display_term)]),
                type = "bar",hoverinfo="none",marker = list(color = c(rep('#D3D3D3',nrow(na.omit(dat[,3]))-1),'#696969'))) %>% 
      add_trace(x = ~instance_type, 
                y = ~get(fields_all[str_detect(fields_all,display_term)]),type = "scatter",
                mode = 'lines+markers',
                #hoverinfo="none",
                line = list(color = 'blue', width = 1),
                marker = list(color = 'blue'),
                hoverinfo = 'text',
                text = ~paste('Timepoint: ', instance_type,
                              '</br></br>PROM Value: ',if(is.na(Percentage)){get(fields_all[str_detect(fields_all,display_term)])}else{paste0(round(get(fields_all[str_detect(fields_all,display_term)])*100,0),'%')},
                              '</br>Interpretation: ',total.Frame$unFormattedResult[findInterval(get(fields_all[str_detect(fields_all,display_term)]),total.Frame$Begin)]
                )) %>%
      layout(title = '',showlegend = FALSE,
             xaxis = x_layout,shapes = lines,#if(promis_assessment){list(hline(50))}else{lines},
             yaxis = y_layout,hoverlabel = list(font=list(size=7)),
             margin = list(l = 0, r = 0, t = 20, b = 10)#,
             #annotations = plot_annotations
      )  %>% config(displayModeBar = F)
  } else {
    #print(paste0("EFFECT: ",effect))
    # dat_counterer <- dat %>% filter(!is.na(phq9_total_score)) %>% ungroup() %>% group_by(instance_type) %>% count(instance_type)
    records <- dat %>% dplyr::filter(redcap_event_name == events[[project]][[1]])#filter(instance_type == "adm") 
    records <- records[complete.cases(records[fields_all[str_detect(fields_all,display_term)]]),] %>% dplyr::select(record_id,fields_all[1])
    
    if(is.na(effect)){
    #print("CORRECT")
    #%>% na.omit %>% pull(record_id)
    dat_counterer <- dat %>% dplyr::ungroup() %>% dplyr::filter(!is.na(get(fields_all[str_detect(fields_all,display_term)]))) %>% dplyr::group_by(instance_type) %>% dplyr::count(instance_type) #%>% na.omit()
    name_vector <- dat_counterer  %>% dplyr::pull(n)# %>% mutate_all(funs(replace(., is.na(.), 0)))
    names(name_vector) <- dat_counterer %>% dplyr::pull(instance_type)
    #records <- dat %>% filter(instance_type == "adm") %>% pull(record_id) %>% na.omit
    dat <- dat %>% dplyr::mutate(instance_type = paste0(instance_type,'(',name_vector[as.character(instance_type)],')')) 
    dat <- dat %>% dplyr::mutate(instance_type =  factor(instance_type, levels = unique(dat$instance_type)))
    #records <- dat %>% filter(instance_type == "adm") %>% pull(record_id) %>% na.omit
    dat.completion <- dat %>% dplyr::ungroup() %>% dplyr::group_by(instance_type) %>% dplyr::filter(record_id %in% records)
    
    
    #dat %>% ungroup() %>% group_by(instance_type) %>% summarise(test = 
    
    dat.mean <- dat %>% dplyr::ungroup() %>% dplyr::group_by(instance_type) %>% dplyr::summarise(mean = round(mean(get(fields_all[1]),na.rm = T)*if(is.na(Percentage)){1}else{100},2),
                                                                            sd = round(sd(get(fields_all[1]),na.rm = T)*if(is.na(Percentage)){1}else{100},2),
                                                                            min = round(min(get(fields_all[1]),na.rm = T)*if(is.na(Percentage)){1}else{100},2),
                                                                            max = round(max(get(fields_all[1]),na.rm = T)*if(is.na(Percentage)){1}else{100},2),
                                                                            #paired_mean = mean(na.omit(get(fields_all[1])[record_id %in% records$record_id]),na.rm = T),
                                                                            #paired_admission = mean(records[which(records$record_id %in% record_id),][[fields_all[str_detect(fields_all,display_term)]]]),
                                                                            #paired_length = length((na.omit(get(fields_all[1])[record_id %in% records$record_id]))),
                                                                            paired_records = paste(records$record_id[na.omit(match(record_id[which(complete.cases(get(fields_all[str_detect(fields_all,display_term)])))],records$record_id))],collapse = ","),
                                                                            #paired_adm_length = length(records[which(records$record_id %in% record_id),][[fields_all[str_detect(fields_all,display_term)]]]),
                                                                            paired_adm_records = paste(records$record_id[records$record_id %in% strsplit(paired_records,',')[[1]]],collapse=","),
                                                                            paired_vals = paste(get(fields_all[1])[which(record_id %in% strsplit(paired_records,',')[[1]])],collapse = ','),
                                                                            paired_adm = paste(records[[2]][which(records$record_id %in% strsplit(paired_records,',')[[1]])],collapse = ',')
                                                                            # p_val = case_when(length(strsplit(paired_records,',')[[1]]) <= 1 ~'INSUFF.',
                                                                            #                   TRUE ~ as.character(t.test((get(fields_all[1])[which(record_id %in% strsplit(paired_records,',')[[1]])]),
                                                                            #                                              (records[[2]][which(records$record_id %in% strsplit(paired_records,',')[[1]])]))$p.value)
                                                                                                #mean(get(fields_all[1])[which(record_id %in% strsplit(paired_records,',')[[1]])]))
                                                                                                #as.character(length(strsplit(paired_records,',')[[1]]))
                                                                                              #TRUE ~ as.character(mean(get(fields_all[1])[na.omit(match(record_id[which(complete.cases(get(fields_all[str_detect(fields_all,display_term)])))],records$record_id))]))
                                                                              
                                                                            ) #%>% 
      #group_by(instance_type) #%>%
      # mutate(p_vals = case_when(length(strsplit(paired_vals,",")) <= 1 ~ 'INSUFF',
      #                           TRUE ~ as.character(t.test(as.numeric(strsplit(paired_vals,',')[[1]]),as.numeric(strsplit(paired_adm,',')[[1]]),paired = T)$p.value))
      #        )
    #print(dat.mean)
    #print(dat)
      dat.mean$p_val <- ''
      dat.mean$eff <- ''
      for (i in (i = 1:nrow(dat.mean))){
        #print(length(strsplit(dat.mean$paired_records[i],",")[[1]]))
        if(length(strsplit(dat.mean$paired_records[i],",")[[1]]) <= 1){
          dat.mean$p_val[i] <- "INSUFF"
        } else {
          #print((strsplit(dat.mean$paired_vals[i],',')[[1]]))
          #print(strsplit(dat.mean$paired_adm[i],',')[[1]])
          p <- tryCatch({
            t.test(as.numeric(strsplit(dat.mean$paired_vals[i],',')[[1]]),as.numeric(strsplit(dat.mean$paired_adm[i],',')[[1]]),paired = T)$p.value
          },
          error = function(err) {
           NA
          })
          
          
          #p <- t.test(as.numeric(strsplit(dat.mean$paired_vals[i],',')[[1]]),as.numeric(strsplit(dat.mean$paired_adm[i],',')[[1]]),paired = T)$p.value
          if (!is.na(p)){
          if(p >= .01){p <- round(p,3)
          } else if (p <= .005){p <- '<0.01*'
          } else {p <- '<0.01'}
          } else {
            p <- 'NA'
          }
          
          dat.mean$p_val[i] <- as.character(p)
          
        }
        # if(promis_assessment){
        #   
        #   dat.mean$eff.size[i] <- round(abs(mean(as.numeric(strsplit(dat.mean$paired_adm[i],',')[[1]])) - mean(as.numeric(strsplit(dat.mean$paired_vals[i],',')[[1]]))),2)
        # 
        # } else {
        #  
          dat.mean$eff.size[i] <- round(cohens_d(as.numeric(strsplit(dat.mean$paired_adm[i],',')[[1]]),as.numeric(strsplit(dat.mean$paired_vals[i],',')[[1]])),2)

        #}
        
        
      }
      #print("ERROR PASSED")
      if(!is.na(repeating)){
      dat.mean <- dat.mean[complete.cases(dat.mean$mean),]# %>% droplevels(.$instance_type)
      dat.mean$instance_type <- droplevels(dat.mean$instance_type)
      # print(dat.mean)
      # print(dat)
      dat <- dat %>% dplyr::ungroup() %>% dplyr::filter(redcap_event_name %in% c(events[[project]][["admission"]],events[[project]][["discharge"]]))# %>% droplevels(.$instance_type)
      dat$instance_type <- droplevels(dat$instance_type)
      # print(dat.mean)
      # print(dat)
      }
      # print(dat)
      # print(dat.mean)
      p <- plot_ly(dat) %>%
        add_trace(data = dat, x = ~instance_type,name = "Group Data",
                  y = ~if(is.na(Percentage)){get(fields_all[str_detect(fields_all,display_term)])}else{round(get(fields_all[str_detect(fields_all,display_term)])*100,2)},
                  type = "box",hoverinfo = 'none') %>%
        add_trace(data = dat.mean, x = ~instance_type,name = "Group Mean",
                  y = ~mean,
                  type = "scatter",
                  mode = "lines+markers",
                  #hoverinfo ='all'
                  hoverinfo = 'text',
                  text = ~paste('mean [sd]:', mean,' [',sd,']',
                                '</br></br>min/max:', min,'/',max,
                                #'</br>min:', min,
                                '</br>effect-size [p]:', eff.size,' [',p_val,']')
        ) %>%
        layout(title = '',titlefont = f_title, showlegend = FALSE,
               xaxis = x_layout,#shapes = lines,#if(promis_assessment){list(hline(50))}else{lines},
               yaxis = y_layout,shapes = lines,hoverlabel = list(font=list(size=9)),#ticksuffix = "%",
               margin = list(l = 0, r = 10, t = 18, b = 20)#,
               #annotations = plot_annotations
        )  %>% config(displayModeBar = F) %>% layout(yaxis = list(ticksuffix = if(!is.na(Percentage)){"%"}else{""}))
      
      
    } else {
      dat.eff <- dat %>% 
        filter(redcap_event_name %in% c(events[[project]][[1]],events[[project]][[3]])) %>% 
        ungroup() %>% 
        filter(complete.cases(.[fields_all[1]])) %>% 
        group_by(month = floor_date(date,"month")) 
      
      dat.adm <- dat.eff %>% filter(redcap_event_name %in% c(events[[project]][[1]])) %>% select(record_id,month,fields_all[1])
      dat.dsc <- dat.eff %>% filter(redcap_event_name %in% c(events[[project]][[3]])) %>% select(record_id,month,fields_all[1])
      dat.result <- dplyr::inner_join(dat.adm,dat.dsc,by="record_id") %>% group_by(month.y) %>% arrange(month.y)
      names(dat.result) <- c('record_id','month.x','admission_value','month.y','discharge_value')
      dat.effect <- dat.result %>%
        summarise(effect_size = round(cohens_d(admission_value,discharge_value),2),n = n()) %>%
        mutate(tempMonth = format(month.y, format = '%m/%y')) %>%
        # summarise(effect_size = case_when(promis_assessment ~ round(abs(mean(admission_value) - mean(discharge_value)),2),
        #                                                              TRUE ~ round(cohens_d(admission_value,discharge_value),2))) %>%
        mutate(month.y = paste0(format(month.y, format = '%m/%y'),"<br>(",n,")"))
      if (nrow(dat.effect) == 0){return(plotly_empty() %>% config(displayModeBar = F))}
      dat.effect$month.y[nrow(dat.effect)] <- paste0('<i>',dat.effect$month.y[nrow(dat.effect)],'</i>')
      x_layout_special <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = TRUE,
        showgrid = FALSE,
        fixedrange=TRUE,
        side = "bottom",
        tickfont = f,
        type = "category",
        range = c(-.5,length(unique(dat.effect$month.y))-.5)
      )
      
      y_layout_special <-list(
      range = c(0,3),
      #   if(promis_assessment){c(0, 25)
      # }else{
      #   c(0,2)
      #   },
      fixedrange = T,
      title = "",
      zeroline = TRUE,
      zerolinecolor = toRGB("lightgray"),
      zerolinewidth = 1,
      showline = FALSE,
      showticklabels = TRUE,
      tickfont = f,
      font = f,
      side = "right",
      showgrid = TRUE,
      dtick = .25,
      tickvals = seq(0,3,.25),
      ticktext = c("0","","","","","","","","","","","","3")
        #if (promis_assessment){5}else{.25}
      )
      p <- plot_ly(dat.effect) %>%
        add_trace(x = ~month.y,name = "effect size",
                  y = ~effect_size,
                  type = "scatter",
                  mode = 'markers',
                  marker = list(color = c(rep('#1f77b4',
                                              if(nrow(na.omit(dat.effect)) >= 2){nrow(na.omit(dat.effect[,2]))-1}else{1}),
                                          '#279aea')),
                  hoverinfo = 'text',
                  text = ~paste('month:', tempMonth,
                                '</br></br>effect size:', effect_size)
        ) %>%
        layout(title = 'effect size',titlefont = f_title, showlegend = FALSE,
               xaxis = x_layout_special,
               yaxis = y_layout_special,hoverlabel = list(font=list(size=9)),
               margin = list(l = 0, r = 8, t = 18, b = 20)#,
               #annotations = plot_annotations
        )  %>% config(displayModeBar = F) %>% layout(yaxis = list(ticksuffix = if(!is.na(Percentage)){"%"}else{""}))
    }
        
  }
  return(p)
  
}

graph.Outcomes<- function(patient = NA,assay,selector, flip = NA,project){
  # print(selector)
  # print(project)
  # if (selector != "Group Data"){
  #   tryCatch(typeof(patient),error = return(error_plot()), finally = typeof(patient))
  # }
  # if (!is.na(patient)){
  # if (patient == "" && selector != "Group Data"){
  #   print(patient)
  # }
  # }
  if (length(project) == 0){
    project <- "Adult_Inpatient"
  }
  dat <- sort.Data("all", assay = assay,project = project)
  
  source(paste0("shiny_code_repo/survey_repo/",assay,".R"))
  
  if (is.null(dat)){
    return(error_plot())
  }
  
  if(nrow(dat) == 0){
    return(error_plot())
  }
  if (str_detect(colnames(dat[4]),"timestamp") == T){
    dat <- dat[-c(4)]
  }
  Min <- assay_min
  Max <- assay_max
  Range <- "Y"
  
  if (is.na(flip)){
    MinColor <- "#5CA137"
    MaxColor <-"#BE1E2E"
    max_string <- "worse"
    min_string <- "better"
  } else {
    MinColor <- "#BE1E2E"
    MaxColor <- "#5CA137"
    max_string <- "better"
    min_string <- "worse"
  }
  #Change of Time Point to Assay Time Points
  for (i in (i = 1:nrow(dat))){
    time.String <- dat[i,3]
    if (time.String == 0){
      dat[i,3] <- "Admission"
    } else if (time.String == "D"){
      dat[i,3] <- "Discharge"
    } else {
      #NEEDS CUSTOMIZATION PER SURVEY -TAG-
      dat[i,3] <- paste0(interval$altTerm, as.character(as.numeric(time.String)*interval$Multiplier))
    }
  }
  
  names(dat) <- c("ID","Instance","Assay","Value")
  dat$Assay <- factor(dat$Assay, levels = unique(dat$Assay))
  
  if (length(subset(selector, subset = selector %in% "Individual Data")) == 1){
    dat.Patient <- subset(dat, subset = ID %in% patient)
    if (nrow(dat.Patient) == 0){
      return(error_plot())
    }
    dat <- subset(dat, subset = Assay %in% unique(dat.Patient$Assay))
  } else {
    dat$Assay <- mapvalues(dat$Assay, from = as.character(levels(dat$Assay)), to=as.character(paste0(levels(dat$Assay)," (",table(unlist(dat$Assay)),")")))
  }
  
  gp <- omni_plot(dat,MIN = Min, MAX = Max, horizontal = "Y", customRange = Range)
  if (length(subset(selector, subset = selector %in% "Individual Data")) == 1){
    gp <- gp + geom_point(data = dat.Patient, aes(group = ID),color = "#2ca1fc",size = 4) +
      geom_path(data = dat.Patient, aes(group = ID),color = "#2ca1fc", linetype = 1)
  }
  
  if (length(subset(selector, subset = selector %in% "Group Data")) == 1){
    gp <- gp + geom_point(alpha = 0.00001)
    options <- c("Mean","SD","SE","Min","Max")
    gp <- general.Stat(dat,gp,options)
  }
  
  gp <- gp + theme(plot.title = element_text(family = 'Helvetica', 
                                             color = '#666666', 
                                             face = 'plain', 
                                             size = 20, 
                                             hjust = 0))
  
  max <- assay_max
  #print(max_string)
  gp <- gp + geom_hline(yintercept = max, linetype = 2, color = MaxColor) +
    geom_label(x = 1, y = max, aes(vjust = 0,hjust = 2), label = max_string,color = MaxColor, label.size = 0,alpha=0.1)
  
  min <- assay_min
  
  gp <- gp + geom_hline(yintercept = min, linetype = 2, color = MinColor)+
    geom_label(x = 1, y = min, aes(vjust = 0,hjust = 2), label =  min_string, color = MinColor, label.size = 0,alpha=0.1)
  
  #gp <- gp + geom_line(data = data.frame(x = c(rep(.1,1000)), y = seq(Min,Max,length.out = 1000)), aes(x=x, y=y, color=y),size=6) + guides(color = F)#+
  #   scale_colour_gradientn( colours = c( ifelse(is.na(flip), "#72ab4e", "#e6141a"), "#fdfc37",ifelse(is.na(flip), "#e6141a", "#72ab4e")),
  #                           breaks  = c( Min, (Max-Min)/2, Max),
  #                           labels  = c( "bad", "medium", "good"),
  #                           limits  = c( Min,Max)) + guides(color = F)
  if (length(unique(dat$Assay)) > 10){
    gp <- gp + theme(axis.text.x = element_text(angle=45))
  }
  return(gp)
}

spark.Outcomes<- function(patient = NA,assay, flip = NA,project){
  
  if (length(project) == 0){
    project <- "Adult_Inpatient"
  }
  dat <- sort.Data("all", assay = assay,project = project, Daily = "Y")
  dat <- subset(dat, record_id %in% patient)
  source(paste0("shiny_code_repo/survey_repo/",assay,".R"))
  valueFields <- fields[!str_detect(fields,display_term)]
  sparkTitle <- Titles[!str_detect(fields,display_term)]
  spark.Frame <- data.frame('allValues' = rep(NA,length(valueFields)), 'latestValue' = rep(NA,length(valueFields)), 'interpret' = rep(NA,length(valueFields)))
  
  
  
  
  for (i in (i = 1:length(valueFields))){
    spark.Frame$allValues[i] <- paste(dat[[valueFields[i]]], collapse=",")
    spark.Frame$latestValue[i] <- paste0('<span style="font-size: 2em;color:#666666;">',tail(dat[[valueFields[i]]],1),'</span>')
    spark.Frame$interpret[i] <- paste0('<span style="font-size: 2em;color:#666666;">',sparkTitle[i],'</span>')
  }
  
  return(spark.Frame)
}

table.Survey <- function(patient,assay,Time,options,project){
  Admission <- get.Data(patient, "Enroll",assay = assay,project = project)
  Discharge <- get.Data(patient, "Discharge",assay = assay,project = project)
  Repeating <- get.Data(patient, "Repeat",assay = assay,project = project)
  
  CombinedData <- rbind(Admission,Repeating,Discharge)
  
  if (nrow(CombinedData) == 0){
    return(NULL)
  }
  
  CombinedData$redcap_repeat_instance <- seq(0,nrow(CombinedData)-1,1)
  if (str_detect(colnames(CombinedData[4]),"timestamp") == T){
    CombinedData <- CombinedData[-c(4)]
    Admission <- Admission[-c(4)]
  }
  if (Time == "D"){
    Time = nrow(CombinedData)-1
  }
  
  Time <- as.numeric(Time)
  # if (is.na(CombinedData[Time]$phq9_total_score)){
  #   return(NULL)
  # } 
  
  Table <- table.Outcomes(table_title,1,CombinedData,Time, assay = assay,Admission)
  
  Table$change_adm <- sapply(Table$change_adm, function(x) round(x,1))
  Table$change_recent <- sapply(Table$change_recent, function(x) round(x,1))
  if (is.na(Table$score_primary)){
    return(NULL)
  }
  #print(Table)
  # for (i in (i = 2:6)){
  #   if (!is.na(Table[i])){
  #     if (i %in% c(3,4)){
  #       if (Table[i] < 0){
  #         Table[i] <- paste0('<span style="color:green">',Table[i],'</span>')
  #       } else if (Table[i] > 0){
  #         Table[i] <- paste0('<span style="color:red">',Table[i],'</span>')
  #       } else {
  #         Table[i] <- paste0('<span style="color:black">',Table[i],'</span>')
  #       }
  #     } else {
  #       if (Table[i] < Ranges$Intro[1]){
  #         Table[i] <- paste0('<span style="color:green">',Table[i],'</span>')
  #       } else if (Table[i] < Ranges$Intro[2]){
  #         Table[i] <- paste0('<span style="color:orange">',Table[i],'</span>')
  #       } else if (Table[i] < Ranges$Intro[3]){
  #         Table[i] <- paste0('<span style="color:darkorange">',Table[i],'</span>')
  #       } else if (Table[i] < Ranges$Intro[4]){
  #         Table[i] <- paste0('<span style="color:red">',Table[i],'</span>')
  #       } else if (Table[i] < Ranges$Intro[5]){
  #         Table[i] <- paste0('<span style="color:darkred">',Table[i],'</span>')
  #       }
  #     }
  #     
  #   }
  # }
  names(Table) <- c("Assay","Value at Time","Change: Recent","Change: Admission","Minimum Score","Maximum Score","Context")
  return(Table)
}

graph.PT <- function(dat.ALL, project,variant){
  
  dat.ALL <- dat.ALL %>% subset(project_label %in% project)
  # 
  # dat.ALL <- dat %>% 
  #   subset(project_label %in% project) %>%
  #   mutate(date = as.Date(case_when(
  #     redcap_event_name == events[[project]][[1]] & demographics_information_complete == 2 & is.na(manual_admission_date) ~ as.character(demographics_information_timestamp),
  #     redcap_event_name == events[[project]][[1]] & demographics_information_complete != 2 ~ as.character(survey_expiration_timestamp),
  #     redcap_event_name == events[[project]][[1]] ~ as.character(manual_admission_date),
  #     redcap_event_name == events[[project]][[2]] & !is.na(survey_intro_timestamp) ~ as.character(survey_intro_timestamp),
  #     redcap_event_name == events[[project]][[2]] & is.na(survey_expiration_timestamp) ~ as.character(survey_expiration_date),
  #     redcap_event_name == events[[project]][[2]] ~ as.character(survey_expiration_timestamp),
  #     redcap_event_name == events[[project]][[3]] & !is.na(survey_intro_timestamp) ~ as.character(survey_intro_timestamp),
  #     redcap_event_name == events[[project]][[3]] & is.na(survey_expiration_timestamp) ~ as.character(survey_expiration_date),
  #     redcap_event_name == events[[project]][[3]] ~ as.character(survey_expiration_timestamp)
  #   ))) %>% 
  #   ungroup() %>% 
  #   filter(date > "2018-01-01") %>%
  #   group_by(week=floor_date(date, "7 days"))
  
  
  # dat.ALL <- dat.ALL %>%
  #   mutate(date = floor_date(dat,"7 days"))
  #   
  
  #   
  #   
  #   
  # 
  # dat.adm <- dat %>% subset(project_label %in% project) %>% 
  #   subset(redcap_event_name %in% events[[project]][[1]]) %>% 
  #   mutate (date = ((as.Date(case_when(
  #                                     demographics_information_complete == 2 & is.na(manual_admission_date) ~ demographics_information_timestamp,
  #                                     demographics_information_complete != 2 ~ survey_expiration_timestamp,
  #                                     TRUE ~ manual_admission_date))))) %>% ungroup() %>% filter(date > "2018-01-01")
  # 
  # 
  # 
  # #%>% group_by(date) %>% count(date)
  # #as.Date
  # dat.rep <- dat %>%subset(project_label %in% project) %>%
  #   subset(redcap_event_name %in% events[[project]][[2]]) %>% 
  #   mutate (date = ((as.Date(case_when(
  #     !is.na(survey_intro_timestamp) ~ survey_intro_timestamp,
  #     is.na(survey_expiration_timestamp) ~ survey_expiration_date,
  #     TRUE ~ survey_expiration_timestamp))))) %>% 
  #   ungroup() %>% filter(date > "2018-01-01")
  # 
  # 
  # dat.dsc <- dat %>%subset(project_label %in% project) %>%
  #   subset(redcap_event_name %in% events[[project]][[3]]) %>% 
  #   mutate (date = ((as.Date(case_when(
  #     !is.na(survey_intro_timestamp) ~ survey_intro_timestamp,
  #     is.na(survey_expiration_timestamp) ~ survey_expiration_date,
  #     TRUE ~ survey_expiration_timestamp))))) %>% 
  #   ungroup() %>% filter(date > "2018-01-01")
  
  f <- list(
    family = "helvetica",
    size = 9,
    color = "#7f7f7f"
  )
  
  x_layout_alt <- list(
    title = "weeks",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = if(variant %in% 'length'){TRUE}else{FALSE},
    showgrid = FALSE,
    fixedrange=FALSE,
    side = "bottom",
    tickfont = f,
    titlefont = f,
    type = "category",
    ticklen = 1,
    tickwidth = 1,
    font = f,
    tickcolor = toRGB("blue")
  )
  
  x_layout <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = if(variant %in% 'length'){TRUE}else{FALSE},
    showgrid = FALSE,
    fixedrange=FALSE,
    side = "bottom",
    tickfont = f,
    type = "category"
  )
  
  y_layout <-list(
    fixedrange = F,title = "",
    zeroline = TRUE,
    zerolinecolor = toRGB("lightgray"),
    zerolinewidth = 1,
    showline = FALSE,
    showticklabels = TRUE,
    tickfont = f,
    font = f,
    showgrid = TRUE,
    #dtick = 2
    nticks = 6#tickformat = "%",
    #tickformat = if(!is.na(Percentage)){"%"}else{NULL},
  )
  
  
  if(variant == "count"){
    dat.adm <- dat.ALL %>% subset(redcap_event_name %in% events[[project]][1]) %>% count(week)
    dat.dsc <- dat.ALL %>% subset(redcap_event_name %in% events[[project]][3]) %>% count(week)
    dat.Graph <- dplyr::full_join(dat.adm,dat.dsc,by="week") %>% mutate_all(funs(replace(., is.na(.), 0))) %>% ungroup %>% arrange(week)#%>% mutate(total = cumsum(n.x) - cumsum(n.y))
    dat.Graph$total <- cumsum(dat.Graph$n.x) - cumsum(dat.Graph$n.y)
    dat.Graph <- dat.Graph %>% group_by(week)
    
  } else if (variant == "length"){
    dat.adm <- dat.ALL %>% ungroup() %>% subset(redcap_event_name %in% events[[project]][1]) %>% select(record_id,date)
    dat.dsc <- dat.ALL %>% ungroup() %>% subset(redcap_event_name %in% events[[project]][3]) %>% select(record_id,date)
    dat.Graph <- dplyr::right_join(dat.adm,dat.dsc,by="record_id") %>% 
      mutate(total = date.y - date.x) %>% 
      mutate(date.y = format(date.y, "%m/%y")) %>% 
      ungroup() %>% 
      group_by(date.y)# %>% summarise(mean(total),sd(total))
    
    summary <- dat.Graph %>% 
      summarise(mean = round(mean(total,na.rm = T),2), 
                sd = round(sd(total,na.rm = T),2), 
                min = round(min(total,na.rm = T),2), 
                max = round(max(total,na.rm = T),2))
  } else if (variant %in% c("completion",'expiration')){
    dat.adm.comp <- dat.ALL %>% subset(redcap_event_name %in% events[[project]][1]) %>% filter(survey_intro_complete == 2) %>% count(week)#mutate(date = format(date, "%m/%y"))  %>% group_by(date) %>% count(date)
    
    dat.rep.comp <- dat.ALL %>% subset(redcap_event_name %in% events[[project]][2]) %>% filter(survey_intro_complete == 2) %>% count(week)#mutate(date = format(date, "%m/%y"))  %>% group_by(date) %>% count(date) 
    
    dat.dsc.comp <- dat.ALL %>% subset(redcap_event_name %in% events[[project]][3]) %>% filter(survey_intro_complete == 2) %>% count(week)#mutate(date = format(date, "%m/%y"))  %>% group_by(date) %>% count(date)
    
    dat.adm.exp <- dat.ALL %>% subset(redcap_event_name %in% events[[project]][1]) %>% filter(survey_expiration_complete %in% c(1,2)) %>% count(week)#mutate(date = format(date, "%m/%y"))  %>% group_by(date) %>% count(date)
    
    dat.rep.exp <- dat.ALL %>% subset(redcap_event_name %in% events[[project]][2]) %>% filter(survey_expiration_complete %in% c(1,2)) %>% count(week)#mutate(date = format(date, "%m/%y"))  %>% group_by(date) %>% count(date) 
    
    dat.dsc.exp <- dat.ALL %>% subset(redcap_event_name %in% events[[project]][3]) %>% filter(survey_expiration_complete %in% c(1,2)) %>% count(week)
    
    
    
    dat.Graph.comp <- dplyr::full_join(dat.adm.comp,dat.rep.comp,by="week") %>% dplyr::full_join(dat.dsc.comp,by="week") %>% mutate_all(funs(replace(., is.na(.), 0))) %>% mutate(total = n.x+n.y+n)# %>% group_by(date)
    dat.Graph.exp <- dplyr::full_join(dat.adm.exp,dat.rep.exp,by="week") %>% dplyr::full_join(dat.dsc.exp,by="week") %>% mutate_all(funs(replace(., is.na(.), 0))) %>% mutate(total = n.x+n.y+n)
    
    dat.Graph <- full_join(dat.Graph.comp,dat.Graph.exp, by = "week") %>% 
      mutate_all(funs(replace(., is.na(.), 0))) %>% 
      mutate(percentage.complete = round((total.x / (total.x + total.y))*100,0)) %>%
      mutate(percentage.expire = round((total.y / (total.x + total.y))*100,0)) %>%
      mutate(expire.adm = round((n.x.y / (n.x.x + n.x.y))*100,0)) %>%
    mutate(expire.rep = round((n.y.y / (n.y.x + n.y.y))*100,0)) %>%
    mutate(expire.dsc = round((n.y.y.y / (n.x.x.x + n.y.y.y))*100,0)) %>% mutate_all(funs(replace(., is.na(.), 0)))
    #mutate(total = date.y - date.x) %>% mutate(date.y = format(date.y, "%m/%y")) %>% ungroup() %>% group_by(date.y)# %>% summarise(mean(total),sd(total))
    
  }
  # else if (variant == "expiration"){
  #   dat.adm <- dat.ALL %>% subset(redcap_event_name %in% events[[project]][1]) %>% filter(survey_expiration_complete %in% c(1,2)) %>% count(week)#mutate(date = format(date, "%m/%y"))  %>% group_by(date) %>% count(date)
  #   
  #   dat.rep <- dat.ALL %>% subset(redcap_event_name %in% events[[project]][2]) %>% filter(survey_expiration_complete %in% c(1,2)) %>% count(week)#mutate(date = format(date, "%m/%y"))  %>% group_by(date) %>% count(date) 
  #   
  #   dat.dsc <- dat.ALL %>% subset(redcap_event_name %in% events[[project]][3]) %>% filter(survey_expiration_complete %in% c(1,2)) %>% count(week)#mutate(date = format(date, "%m/%y"))  %>% group_by(date) %>% count(date)
  #   
  #   dat.Graph <- dplyr::full_join(dat.adm,dat.rep,by="week") %>% 
  #     dplyr::full_join(dat.dsc,by="week") %>% 
  #     mutate_all(funs(replace(., is.na(.), 0))) %>% 
  #     mutate(total = n.x+n.y+n) #%>% group_by(date)
  #   
  # }
  hline <- function(y = 0, color = "red") {
    list(
      type = "line", 
      x0 = 0, 
      x1 = 1, 
      xref = "paper",
      y0 = y, 
      y1 = y,
      line = list(color = color,width = 1,dash='dot')
    )
  }
  
  #'count'
  if (variant %in% c('count')){
    #print(dat.Graph)
    final_plot <- plot_ly(dat.Graph) %>%
      add_trace(x = ~week,name = 'adm', 
                y = ~`n.x`,
                type = 'scatter', mode = 'line', hoverinfo = 'text', text = ~paste('adm:',`n.x`,',',format(week,"%m/%d/%y"))) %>%
      add_trace(x = ~week,name = 'census',
                y = ~total,
                type = 'scatter', mode = 'line', hoverinfo = 'text', text = ~paste('census:',total,',',format(week,"%m/%d/%y"))) %>%
      layout(title = '',showlegend = TRUE,legend = list(orientation = 'h',
                                                        font = f,
                                                        xanchor = "right",  # use center of legend as anchor
                                                        x = 1,
                                                        yanchor = "bottom",
                                                        y = 1),
             xaxis = x_layout_alt,#shapes = lines,#if(promis_assessment){list(hline(50))}else{lines},
             yaxis = y_layout,hoverlabel = list(font=list(size=10)),#ticksuffix = "%",
             margin = list(l = 22, r = 0, t = 0, b = 20))  %>% config(displayModeBar = F)
  } else if (variant %in% c('completion','length')){
    
    final_plot <- plot_ly(dat.Graph) %>%
      add_trace(x = if(variant %in% c('count','completion')){~week}else if(variant == 'length'){~date.y}, 
                y = if(variant == 'completion'){~percentage.complete}else{~total},
                #y = ~total,
                type = if(variant %in% c('count','completion')){'scatter'}else if(variant == 'length'){'box'}, mode = 'line', hoverinfo = if(variant %in% c('count','completion')){'all'}else if(variant == 'length'){'none'}) %>%
      layout(title = '',showlegend = FALSE,
             xaxis = if(variant %in% c('count','completion')){x_layout_alt}else if(variant == 'length'){x_layout},#shapes = lines,#if(promis_assessment){list(hline(50))}else{lines},
             yaxis = y_layout,hoverlabel = list(font=list(size=10)),#ticksuffix = "%",
             margin = list(l = 24, r = 0, t = 10, b = 15))  %>% config(displayModeBar = F)
    if (variant == 'completion'){
      final_plot <- final_plot %>% layout(shapes = hline(90),yaxis = list(ntick = 6,fixedrange = TRUE, range = c(0,110),ticksuffix = "%"))
    } else if (variant == 'length'){
      final_plot <- final_plot %>%
        add_trace(data = summary, x = ~date.y, y = ~mean,type = 'scatter',
                  hoverinfo ='text',
                  text = ~paste('group mean: ', mean,
                                '</br></br>sd: ', sd,
                                '</br>max: ', max,
                                '</br>min: ', min))
    }
      
      
  } else if (variant == 'expiration'){
    final_plot <- plot_ly(dat.Graph,x = ~week) %>%
      add_trace(name ='adm',
                y = ~expire.adm,
                type = 'scatter', mode = 'line') %>%
      add_trace(
        name ='rep',
        y = ~expire.rep,
        type = 'scatter', mode = 'line') %>%
      add_trace(
        name ='dsc',
        y = ~expire.dsc,
        type = 'scatter', mode = 'line') %>%
      layout(title = '',showlegend = TRUE,legend = list(orientation = 'h',
                                                        font = f,
                                                        xanchor = "right",  # use center of legend as anchor
                                                        x = 1,
                                                        yanchor = "bottom",
                                                        y = 1),
             xaxis = x_layout_alt,#shapes = lines,#if(promis_assessment){list(hline(50))}else{lines},
             yaxis = y_layout,hoverlabel = list(font=list(size=10)),#ticksuffix = "%",
             margin = list(l = 24, r = 0, t = 0, b = 10))  %>% config(displayModeBar = F) %>%
      layout(yaxis = list(ntick = 6,fixedrange = TRUE, range = c(0,110),ticksuffix = "%"))
    
    
    
  }
  
  
  return(final_plot)
  
}

stats.PT <- function(dat.ALL,project){
  dat.ALL <- dat.ALL %>% filter(project_label == project)
    #subset(project_label == project)
  
  dat.Count <- full_join(dat.ALL %>% subset(redcap_event_name %in% events[[project]][1]) %>% count(week),
                                 dat.ALL %>% subset(redcap_event_name %in% events[[project]][3]) %>% count(week),
                                by="week") %>% 
                mutate_all(funs(replace(., is.na(.), 0)))
  
  dat.completion.adm <- dat.ALL %>% subset(redcap_event_name %in% events[[project]][1]) %>% filter(survey_intro_complete %in% 2) %>% count(week)
  dat.completion.rep <- dat.ALL %>% subset(redcap_event_name %in% events[[project]][2]) %>% filter(survey_intro_complete %in% 2) %>% count(week)
  dat.completion.dsc <- dat.ALL %>% subset(redcap_event_name %in% events[[project]][3]) %>% filter(survey_intro_complete %in% 2) %>% count(week)
  
  dat.length.adm <- dat.ALL %>% ungroup() %>% subset(redcap_event_name %in% events[[project]][1]) %>% select(record_id,date)
  dat.length.dsc <- dat.ALL %>% ungroup() %>% subset(redcap_event_name %in% events[[project]][3]) %>% select(record_id,date)
  
  dat.expr.adm <- dat.ALL %>% subset(redcap_event_name %in% events[[project]][1]) %>% filter(survey_expiration_complete %in% c(1,2)) %>% count(week)
  dat.expr.rep <- dat.ALL %>% subset(redcap_event_name %in% events[[project]][2]) %>% filter(survey_expiration_complete %in% c(1,2)) %>% count(week)
  dat.expr.dsc <- dat.ALL %>% subset(redcap_event_name %in% events[[project]][3]) %>% filter(survey_expiration_complete %in% c(1,2)) %>% count(week)
  
  dat.Length <- dplyr::right_join(dat.length.adm,
                                  dat.length.dsc,
                                  by="record_id") %>% 
                mutate(total = date.y - date.x) %>% 
                mutate(date.y = format(date.y, "%m/%y")) %>%
                ungroup() %>% group_by(date.y)
  
  dat.Completion <- dplyr::full_join(dat.completion.adm,
                                     dat.completion.rep,
                                     by="week") %>% 
                    dplyr::full_join(dat.completion.dsc,
                                     by="week") %>% 
                    mutate_all(funs(replace(., is.na(.), 0))) %>% 
                    mutate(total = n.x+n.y+n)
  
  dat.Expiration <- dplyr::full_join(dat.expr.adm,
                                     dat.expr.rep,
                                     by="week") %>% 
                    dplyr::full_join(dat.expr.dsc,
                                     by="week") %>% 
                    mutate_all(funs(replace(., is.na(.), 0))) %>% mutate(total = n.x+n.y+n)
  
  dat.Percentage <- dplyr::full_join(dat.Completion,dat.Expiration,by="week") %>% 
    mutate_all(funs(replace(., is.na(.), 0)))
  
  count <- list(median = median(dat.Count$n.x,na.rm = T), 
                mean = round(mean(dat.Count$n.x,na.rm = T),0),
                mean.census = round(mean((cumsum(dat.Count$n.x) - cumsum(dat.Count$n.y)),na.rm=T),0),
                total.adm = sum(dat.Count$n.x,na.rm = T),
                total.dsc = sum(dat.Count$n.y,na.rm = T))
  
  length <- list(median = as.double(median(dat.Length$total,na.rm = T)), max.length = as.double(max(dat.Length$total,na.rm = T)),min.length = as.double(min(dat.Length$total,na.rm = T)))
  
  completion <- list(sum = sum(dat.Completion$total,na.rm = T),
                     median.total = format(round(median(dat.Completion$total,na.rm = T),2),nsmall = 2),
                     median.adm = median(dat.Completion$n.x,na.rm = T),
                     median.dsc = median(dat.Completion$n,na.rm = T),
                     median.rep = median(dat.Completion$n.y,na.rm = T),
                     completion.percent = round(sum(dat.Percentage$total.x,na.rm = T)/(sum(dat.Percentage$total.x,na.rm = T)+sum(dat.Percentage$total.y,na.rm = T))*100,0)
                     )

  expiration <- list(sum = sum(dat.Expiration$total,na.rm = T),
                     median.adm = median(dat.Expiration$n.x,na.rm = T),
                     median.dsc = median(dat.Expiration$n,na.rm = T),
                     median.rep = median(dat.Expiration$n.y,na.rm = T),
                     percent.total = round(sum(dat.Percentage$total.y,na.rm = T)/(sum(dat.Percentage$total.x,na.rm = T)+sum(dat.Percentage$total.y,na.rm = T))*100,0),
                     percent.adm = round(sum(dat.Percentage$n.x.y,na.rm = T)/(sum(dat.Percentage$n.x.y,na.rm = T)+sum(dat.Percentage$n.x.x,na.rm = T))*100,0),
                     percent.rep = round(sum(dat.Percentage$n.y.y,na.rm = T)/(sum(dat.Percentage$n.y.y,na.rm = T)+sum(dat.Percentage$n.y.x,na.rm = T))*100,0),
                     percent.dsc = round(sum(dat.Percentage$n.y.y.y,na.rm = T)/(sum(dat.Percentage$n.y.y.y,na.rm = T)+sum(dat.Percentage$n.x.x.x,na.rm = T))*100,0)
                     )
  
  stat.Data <- list(count = count,length = length,complete = completion,expire = expiration)
  
  return(stat.Data)
}

stats.Assay <- function(dat.ALL,assay){
  source(paste0("shiny_code_repo/survey_repo/",assay,".R"))
  
  promis_assessment <- str_detect(assay,"promis")
  records <- dat.ALL %>% filter(instance_type == 'dsc') %>% pull(record_id)
  dat <- dat.ALL %>% filter(instance_type == 'adm' | instance_type == 'dsc') %>% ungroup %>% group_by(instance_type) %>% filter(record_id %in% records)
  dat.adm <- dat %>% filter(instance_type == 'adm') %>% ungroup() %>% group_by(record_id) %>% select((fields_all[str_detect(fields_all,display_term)]))
  dat.dsc <- dat %>% filter(instance_type == 'dsc') %>% ungroup() %>% group_by(record_id) %>% select((fields_all[str_detect(fields_all,display_term)]))
  dat.merge <- dplyr::full_join(dat.adm,dat.dsc,by = 'record_id') %>% na.omit()
  names(dat.merge) <- c("record","dat.adm","dat.dsc")
  test.Data <- list(adm.mean = if (nrow(dat.merge)!= 0){
                                                        mean(dat.merge$dat.adm)*if(is.na(Percentage)){1}else{100}
                                                       } else {NA},
                    dsc.mean = if (nrow(dat.merge)!= 0){
                                                        mean(dat.merge$dat.dsc)*if(is.na(Percentage)){1}else{100}
                                                       } else {NA},
                    p_val = if (nrow(dat.merge) >= 2 & nrow(na.omit(dat.merge)) >= 2){
                      t.test(dat.merge$dat.adm,dat.merge$dat.dsc,paired = TRUE)$p.value
                    } else {
                      'Not enough data'
                    },
                    amd.sd = if (nrow(dat.merge)!= 0){
                      sd(dat.merge$dat.adm)*if(is.na(Percentage)){1}else{100}
                    } else {NA},
                    dsc.sd = if (nrow(dat.merge)!= 0){
                      sd(dat.merge$dat.dsc)*if(is.na(Percentage)){1}else{100}
                    } else {NA}
                    )
  if(!is.na(Percentage)){
    test.Data$adm.mean <- round(test.Data$adm.mean,0)
    test.Data$dsc.mean <- round(test.Data$dsc.mean,0)
    test.Data$amd.sd <- round(test.Data$amd.sd,0)
    test.Data$dsc.sd <- round(test.Data$dsc.sd,0)
  } 
  # else {
  #   test.Data$adm.mean <- format(round(as.numeric(test.Data$adm.mean),2),nsmall = 2)
  #   test.Data$dsc.mean <- format(round(as.numeric(test.Data$dsc.mean),2),nsmall = 2)
  #   test.Data$amd.sd <- format(round(as.numeric(test.Data$amd.sd),2),nsmall = 2)
  #   test.Data$dsc.sd <- format(round(as.numeric(test.Data$dsc.sd),2),nsmall = 2)
  # }
  
  # if(promis_assessment){
  #   interval <- c(0,1,2.5,5)
  #   names(interval) <- c('nothing','small','medium','large')
  #   eff.size <- round(abs(as.numeric(test.Data["adm.mean"]) - as.numeric(test.Data["dsc.mean"])),2)
  #   interpret <- names(interval)[findInterval(eff.size,interval)]
  #   test.Data[["diff"]] <- paste0('Effect size: ',eff.size,' ',
  #                                 interpret,
  #                                 " [&#x25b3;T-Score]</br>")
  # } else {
    interval <- c(0,.2,.5,.8)
    names(interval) <- c('nothing','small','medium','large')
    eff.size <- round(cohens_d(dat.merge$dat.adm,dat.merge$dat.dsc),2)
    interpret <- names(interval)[findInterval(eff.size,interval)]
    test.Data[["diff"]] <- paste0('effect size (d): ',eff.size,' ',
                                  interpret,
                                  ' | ')
  #}
  
  
    #specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
  
  if(test.Data$p_val != 'Not enough data'){
    #options(scipen = 999)
    if(test.Data$p_val < .01){
      test.Data$p_val <- '< 0.01*'
    } else if(test.Data$p_val <= .05){
      test.Data$p_val <- paste0(
        #format(test.Data$p_val,nsmall = 2)
        round(test.Data$p_val,digits=2)
        ,"*")#'< 0.01*'
    } else {
      test.Data$p_val <- round(test.Data$p_val,2)
    }
    
  }
  
  return(test.Data)
}

running_tally <- function(dat,projects,variant = "adm"){
  #dat <- dat %>% dplyr::filter(survey_intro_complete != 0)
  f <- list(
    family = "helvetica",
    size = 9,
    color = "#7f7f7f"
  )
  
  x_layout_alt <- list(
    title = "weeks",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE,
    fixedrange=FALSE,
    side = "bottom",
    tickfont = f,
    titlefont = f,
    type = "category",
    ticklen = 1,
    tickwidth = 1,
    font = f,
    tickcolor = toRGB("blue")
  )
  
  x_layout <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE,
    fixedrange=FALSE,
    side = "bottom",
    tickfont = f,
    type = "category"
  )
  
  y_layout <-list(
    fixedrange = F,title = "",
    zeroline = TRUE,
    zerolinecolor = toRGB("lightgray"),
    zerolinewidth = 1,
    showline = FALSE,
    showticklabels = TRUE,
    tickfont = f,
    font = f,
    showgrid = TRUE,
    #dtick = 2
    nticks = 6#tickformat = "%",
    #tickformat = if(!is.na(Percentage)){"%"}else{NULL},
  )
  
  for (i in projects){
    dat.count <- dat %>% subset(project_label %in% i) #%>% filter(survey_expiration_complete != 2 & survey_expiration_complete != 1)
    dat.adm <- dat.count %>% subset(redcap_event_name %in% events[[i]][1] ) %>% dplyr::filter(survey_intro_complete != 0 | survey_expiration_complete !=0) %>% count(week)
    
    if(i == projects[1]){
      dat.compiled <- dat.adm
    } else {
      dat.compiled <- dplyr::full_join(dat.compiled,dat.adm,by="week")
    }
    
  }
  
  dat.output <- dat.compiled %>% ungroup() %>%
    replace(is.na(.), 0) %>%
    mutate(sum = rowSums(.[2:length(dat.compiled)])) %>%
    mutate(cumsum = cumsum(sum))
  
  dat.total <- dat %>% 
    # filter((survey_expiration_complete != 2 & survey_expiration_complete != 1) |
    #                             survey_intro_complete != 0) %>% count(week) %>% 
    filter(survey_intro_complete == 2) %>%
    count(week) %>%
    ungroup() %>%
    replace(is.na(.), 0) %>%
    #mutate(sum = rowSums(.[2:length(dat.compiled)])) %>%
    mutate(cumsum = cumsum(n))
  
  
  final_plot <- plot_ly(if(variant == "adm"){dat.output}else{dat.total}, x = ~week) %>%
    add_trace(name = 'adm tally',
              y = ~cumsum,
              type = 'scatter',
              mode = 'line') %>%
    layout(title = if(variant == "adm"){'Total Patients Admitted'}else{'Total Assessments Taken'},showlegend = FALSE,legend = list(orientation = 'h',
                                                      font = f,
                                                      xanchor = "right",  # use center of legend as anchor
                                                      x = 1,
                                                      yanchor = "bottom",
                                                      y = 1),
           xaxis = x_layout_alt,#shapes = lines,#if(promis_assessment){list(hline(50))}else{lines},
           yaxis = y_layout,hoverlabel = list(font=list(size=10)),#ticksuffix = "%",
           margin = list(l = 24, r = 0, t = 26, b = 15))  %>% config(displayModeBar = F)
  return(final_plot)  
}
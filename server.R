#POCDART SERVER
##Included in case PANDOC is used to transform output files
if(is.na(Sys.getenv("HOME", unset = NA))) {
  Sys.setenv(HOME = tempdir())
}

server <- function(input, output, session){
  source("misc/global_variables.R")
  source("misc/scr_loader.R")
  ##ISOLATED VALUES
 isolate({
   #Generate patient list
   patientList <- selectPatients(999)[1,]
   x <- c("Choose a patient" = "",
          patientList)
   updateSelectInput(session, "patient", choices = x, selected = input$patient[1])
   #If using, generate list of patients not copied.
   data.Holder <- copyGenerator("survey_intro",repeating = "Y",project = project_string)
   outcomes_data <<- read.csv("data/outcomes_data.csv", header = TRUE, stringsAsFactors = F) 
 })
  
  ##HELP EVENTS
  # If not using rIntroJS for an explanation, please comment out or remove this section along with the associated UI.
  steps <- reactive(data.frame(
    element=c("#temp","#introBoxes","#surveyBox","#registrationBox","#reportOutput","#graphicReports"),
    intro=c("Welcome to the POCDART demonstration website.\nFollowing will be a brief overview of POCDART functionality - and more in-depth questions can be directed to aready@sheppardpratt.org",
            "These boxes showcase current project related values that are generally applicable.",
            "As time passes, POCDART will automatically populate these boxes with new surveys depending on the interval set in global_variables.R.",
            "For the demo version of POCDART, subjects can be added individually (through REDCap or in the dashboard) or via uploading a CSV. If available, a database can also be used for regular upload of patients.",
            "The demo version of POCDART allows for users to copy and paste the summarized report of a subject's recent assessment or to generate a formatted HTML report for upload.",
            "The demo version of POCDART showcases four graphs using the POCDART graph template."),
    position=c("bottom","bottom","bottom", "right", "right","bottom"),
    options = list("nextLabel"="Next",
                   "prevLabel"="Previous",
                   "skipLabel"="Close")
  ))
  observeEvent(input$info, { introjs(session, 
                                     events = list(onbeforechange = readCallback("switchTabs")),
                                     options=list(steps=steps())) })
  ##
  #Allow the user to remove discharged patients from patient list
  observeEvent(input$patientSubset,{
    if(input$patientSubset){
      discharged <- subset(outcomes_data, redcap_event_name %in% events[[project_string]][["discharge"]])$record_id
      if(length(discharged != 0)){
      x <- c("Choose a patient" = "",
             patientList[-which(patientList %in% discharged)])
      }
    } else {
      x <- c("Choose a patient" = "",
             patientList)
    }
    updateSelectInput(session, "patient", choices = x, selected = input$patient[1])
  })
  
  #DPLYR FRAME
  dplyr.Frame <- reactive({
    dat <- outcomes_data %>% group_by(record_id) %>%
      dplyr::mutate(redcap_event_name =  factor(redcap_event_name, levels = unlist(events[[project_string]], use.names = FALSE))) %>%
      dplyr::arrange(redcap_event_name) %>%
      dplyr::mutate(instance_type = case_when(redcap_event_name %in% events[[project_string]][[1]] ~ "adm",
                                       redcap_event_name %in% events[[project_string]][[2]] ~ paste0(interval$altTerm, redcap_repeat_instance * interval$Multiplier),
                                       redcap_event_name %in% events[[project_string]][[3]] ~ "dsc"
      ))
    dat <- dat %>% dplyr::mutate(instance_type =  factor(instance_type, levels = unique(dat$instance_type)))
    return(dat)
  })
  
  #REACTIVE VALUES
  
  timeHolder <- reactiveValues(Value = 0)
  
  all_copied <- reactiveValues(
    data = data.frame(
      Record = data.Holder$ID,
      RowNum = data.Holder$Row,
      Name = data.Holder$Name,
      Time = data.Holder$Time,
      Action = data.Holder$Action,
      stringsAsFactors = FALSE
    ))
  
  #UI SELECTABLES
  #Generates UI element for selecting patient timepoints
  output$timepoint <- renderUI({
    req(input$patient)
    x <- time.Data(input$patient,starting_instrument,project = project_string)
    if (length(x) != 0){
      selectInput("timeSelect", "Timepoint:", choices = x)
    } else {
      return(NULL)
    }
  })
  
  #OBSERVATION EVENTS
  
  ##Patient Registration
  
  ###REDCAP Registration
  
  output$admission_survey_links <- renderUI(HTML(paste0('<button style = "-webkit-appearance: none;background-color: #e7e7e7;" class = "button" onclick="openPSU()"><font size = "100px">Register in REDCap</font></button>')))
#   output$admission_survey_links <- renderUI(HTML(paste0(HTML('<button style = "-webkit-appearance: none;background-color: #e7e7e7;" class = "button" onclick="window.open("'),
# HTML(public_survey_url),HTML('",\'_parent\');"><font size = "100px">Register in REDCap</font></button>'))))
  ###In-App Registration
  observeEvent(input$add_popup, {
    showModal(modalDialog(
      HTML("<center>Please record patient information below:"),
      textInput("add_mrn","Patient's MRN:"),
      hr(),
      textInput("add_visit","Patient's Visit Number:"),
      hr(),
      textInput("add_first","Patient's First Name:"),
      hr(),
      textInput("add_last","Patient's Last Name:"),
      hr(),
      selectizeInput("add_clinician","Clinician:",c("Doctor Strangelove" = "1","Doctor No" = "2","Doctor Evil" = "3")),
      actionButton("register_popup","Register Patient"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$register_popup,{
    if(any(input$add_mrn == "", input$add_visit == "",
           input$add_first == "", input$add_last == "",
           input$add_clinician == "")){
      sendSweetAlert(session, title = "Missing Information", text = 'Please fill out all patient information.', type = 'error',html = TRUE,
                     btn_labels = "Ok")
    } else {
      showModal(modalDialog(
        HTML("<center>Registering Patient, Please Wait...\n<div class=\"loaderSpecial\"></div></center>"),
        easyClose = TRUE
      ))
      maxPatient <- max(as.numeric(exportRecords(redcapConnection(url = api_url, token = survey_key[[1]][[1]]), fields = 'record_id', survey = TRUE, factors = F)$record_id))
      updateDemographics(maxPatient+1,c(input$add_mrn,input$add_visit,input$add_first,input$add_last,input$add_clinician,as.character(today()),2),
                                     c("mrn","visit_number","first_name","last_name","clinician","manual_admission_date","demographics_information_complete"),project_string)
      updateSurveys(maxPatient+1,project_string)
      Sys.sleep(.5)
      # discharged_survey <<- survey_loader("discharge")
      # newPatient_survey <<- survey_loader("incomplete-new")
      replaceData(proxyDischarge, survey_loader("discharge"),rownames = FALSE)
      replaceData(proxynewPatient, survey_loader("incomplete-new"),rownames = FALSE)
      removeModal()
      sendSweetAlert(session, title = "Registered Patient", text = 'Successfully registered new patient!', type = 'success',html = TRUE,
                     btn_labels = "Ok")
    }
  })
  
  ###Tabular Registration
  
  observeEvent(input$add_tabular, {
    showModal(modalDialog(
      HTML("<center>Please upload patient demographic information via a CSV:<br>MRN,Visit/Admission #, First Name, Last Name, Clinician<br>For this demo, clinician must be uploaded as 1, 2, or 3."),
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      hr(),
      checkboxInput("tabular_header", "Header", TRUE),
      actionButton("register_tabular","Register Patient(s)"),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$register_tabular,{
    if(is.null(input$file1)){
      sendSweetAlert(session, title = "Missing Information", text = 'Please make sure to have uploaded a CSV.', type = 'error',html = TRUE,
                     btn_labels = "Ok")
    } else {
      maxPatient <- max(as.numeric(exportRecords(redcapConnection(url = api_url, token = survey_key[[1]][[1]]), fields = 'record_id', survey = TRUE, factors = F)$record_id))
      tabularDF <- read.csv(input$file1$datapath,header = input$tabular_header)
      temp <<- tabularDF
      names(tabularDF) <- c("mrn","visit_number","first_name","last_name","clinician")
      if(!any(as.character(tabularDF$clinician) %in% c("1","2","3"))){
        sendSweetAlert(session, title = "Malformed CSV", text = 'Please check CSV formatting.', type = 'error',html = TRUE,
                       btn_labels = "Ok")
      } else {
        showModal(modalDialog(
          HTML("<center>Registering Patient, Please Wait...\n<div class=\"loaderSpecial\"></div></center>"),
          easyClose = TRUE
        ))
        tabularDF <- tabularDF %>% mutate(manual_admission_date = as.character(today()),record_id = (as.numeric(rownames(tabularDF)) + maxPatient), demographics_information_complete = 2)
      for(i in (i = 1:nrow(tabularDF))){

        updateDemographics(tabularDF[i,]$record_id,tabularDF[i,] %>% select(-record_id),
                           c("mrn","visit_number","first_name","last_name","clinician","manual_admission_date","demographics_information_complete"),project_string)
        
      }
      updateSurveys(tabularDF$record_id,project_string)
      Sys.sleep(.5)
      # discharged_survey <<- survey_loader("discharge")
      # newPatient_survey <<- survey_loader("incomplete-new")
      replaceData(proxyDischarge, survey_loader("discharge"),rownames = FALSE)
      replaceData(proxynewPatient, survey_loader("incomplete-new"),rownames = FALSE)
      removeModal()
      sendSweetAlert(session, title = "Registered Patient(s)", text = 'Successfully registered new patient(s)!', type = 'success',html = TRUE,
                     btn_labels = "Ok")
      }
    }
  })
  
  ## Regenerate all surveys
  observeEvent(input$regenerate, {
    showModal(modalDialog(
      HTML("<center>All Surveys Reloading, Please Wait...\n<div class=\"loaderSpecial\"></div></center>"),
      easyClose = TRUE,
      footer = NULL
    ))
    source("shiny_code_repo/redcap_api/redcapAPI.R")
    outcomes_data <<- read.csv("data/outcomes_data.csv", header = TRUE, stringsAsFactors = F) 
    source("shiny_code_repo/survey_processing/stash_surveys.R")
    stash_surveys()
    session$sendCustomMessage("resetShiny", "")
  })
  
  ## Refresh local data and reset
  observeEvent(input$reset, {
    showModal(modalDialog(
      HTML("<center>Refreshing Local Data, Please Wait...\n<div class=\"loaderSpecial\"></div></center>"),
      easyClose = TRUE,
      footer = NULL
    ))
    source("shiny_code_repo/redcap_api/redcapAPI.R")
    outcomes_data <<- read.csv("data/outcomes_data.csv", header = TRUE, stringsAsFactors = F) 
    session$sendCustomMessage("resetShiny", "")
  })
  
  
  observeEvent(input$allCopied, {
    session$sendCustomMessage("emrButton", "")
    all_copied$data <- rowDeleter(all_copied$data,input$patient,input$dailyTime,"all")
  })
  
  observeEvent(input$copyMerger_all,{
      copiedUpdate("survey_intro", input$patient, input$dailyTime, project = project_string)
  })
  
  observe({
    output$dailyTime <- renderUI({
      req(input$patient)
      req(timeHolder$Value)
      y <- time.Data(input$patient,starting_instrument,project = project_string)
      Holder <- as.character(isolate(timeHolder$Value))
      if (length(y) != 0){
        selectInput("dailyTime", "Timepoint:", choices = y, selected = Holder)
      } else {
        return(NULL)
      }
    })
    timeHolder$Value <- 0
  })
  
  observeEvent(input$select_button_discharge, {
    confirmSweetAlert(
      session = session,
      inputId = "confirm_discharge",
      type = "warning",
      title = "Confirm discharge?",
      danger_mode = TRUE
    )
  })
  
  observeEvent(input$confirm_discharge, {
    if (isTRUE(input$confirm_discharge)){
      id_string <- strsplit(input$select_button_discharge, "_")[[1]][3]
      id_string <- substr(id_string, 1, nchar(id_string)-1)
      project_string <- strsplit(input$select_button_discharge, "_")[[1]][2]
      showModal(modalDialog(
        HTML("<center>Manually discharging patient, please wait...\n<div class=\"loaderSpecial\"></div></center>"),
        easyClose = TRUE,
        footer = NULL
      ))
      discharge_survey <- manualDischarge(id_string,project_string)
      showModal(modalDialog(
        HTML(paste0("<iframe id=\"dc-survey\" src=\"", discharge_survey, "\" style=\"height:700px;width:100%;\"/>")),
        easyClose = FALSE,
        footer = NULL
      )
      )
      session$sendCustomMessage("disableButton",input$select_button_discharge)
      session$sendCustomMessage("closeModal", "")
      source("misc/global_variables.R")
      outcomes_data <<- read.csv("data/outcomes_data.csv", header = TRUE, stringsAsFactors = F) 
      replaceData(proxyDischarge, survey_loader("discharge"),rownames = FALSE)
      replaceData(proxytoday, survey_loader("today"),rownames = FALSE)
      replaceData(proxytomorrow, survey_loader("tomorrow"),rownames = FALSE)
      replaceData(proxypastDue, survey_loader("pastDue"),rownames = FALSE)
      replaceData(proxyexpired, survey_loader("expired"),rownames = FALSE)
      replaceData(proxyincomplete, survey_loader("incomplete"),rownames = FALSE)
      replaceData(proxynewPatient, survey_loader("incomplete-new"),rownames = FALSE)
    }
  })
  
  observeEvent(input$select_button_QR, priority = 100,{
    
    qr_url <- paste0("http://pocdart.org/redcap/surveys/?s=",strsplit(input$select_button_QR, "_")[[1]][2])
    qr_record <- strsplit(input$select_button_QR, "_")[[1]][3]
    qr_record <- substr(qr_record,1,nchar(qr_record)-1)
    patient_datum <- head(subset(outcomes_data, record_id == qr_record),1)
    Val <- renderPlot({
      par(mar=c(0,0,0,0))
      image(qrencode_raster(qr_url),
            asp=1, col=c("white", "black"), axes=FALSE, 
            xlab="", ylab="")
    })
    
    showModal(modalDialog(
      HTML(paste0('<button type="button" id="Print" onclick = "printR(',qr_record,')" class="btn btn-primary">Print</button>')),
      HTML(paste0('<div id="',qr_record,'"><center><big><big>')),paste0(patient_datum$first_name," ",substr(patient_datum$last_name,0,1),". : ",patient_datum$dob),HTML('</br>'),
      Val,HTML('</big></big></center></div>'),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  #UI OUTPUTS
  
  output$emrButton <- renderUI({req(input$patient)
    req(input$dailyTime)
    req(all_copied$data)
    if (nrow(all_copied$data) == 0){
      actionText <- "Copied to EMR"
      actionColor <- "color: #fff; background-color: #008000; border-color: #000000"
    } else {
      actionText <- emrAction(all_copied$data,input$patient,input$dailyTime,"text")
      actionColor <- emrAction(all_copied$data,input$patient,input$dailyTime,"color")
    }
    actionButton("allCopied", label = actionText, style = actionColor)
  })
  
  output$reportButton <- renderUI({req(input$patient)
    req(input$dailyTime)
    downloadButton("generateReport", label = "Generate Report")
  })

  output$generateReport <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "dart_template.Rmd")
      file.copy("shiny_code_repo/data_processing/dart_template.Rmd", tempReport, overwrite = TRUE)
      params <- dartReport(input$patient,input$dailyTime)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  #SURVEY BOXES
  discharged_survey <- reactiveValues()
  today_survey <- reactiveValues()
  tomorrow_survey <- reactiveValues()
  pastDue_survey <- reactiveValues()
  expired_survey <- reactiveValues()
  newPatient_survey <- reactiveValues()
  incomplete_survey <- reactiveValues()
  
  #Comment out when using Stashed Surveys
  # observe({req(input$today)
  #         discharged_survey <- getSurveys("discharge",input$today)
  #         today_survey <- getSurveys("today",input$today)
  #         tomorrow_survey <- getSurveys("tomorrow",input$today)
  #         pastDue_survey <- getSurveys("pastDue",input$today)
  #         expired_survey <- getSurveys("expired",input$today)
  #         incomplete_survey <- getSurveys("incomplete",input$today)})
  
  #Comment out when using Active Surveys
  discharged_survey <- survey_loader("discharge")
  today_survey <- survey_loader("today")
  tomorrow_survey <- survey_loader("tomorrow")
  pastDue_survey <- survey_loader("pastDue")
  expired_survey <- survey_loader("expired")
  incomplete_survey <- survey_loader("incomplete")
  newPatient_survey <- survey_loader("incomplete-new")

  output$today <- renderDataTable(({today_survey}), escape = F, rownames = FALSE,options = list(columnDefs = list(list(targets = {c(0:1,4:5)},
                                                                                                                       visible = FALSE))))
  output$tomorrow <- renderDataTable(({tomorrow_survey}), escape = F, rownames = FALSE,options = list(columnDefs = list(list(targets = {c(0:1,4:5)},
                                                                                                                             visible = FALSE))))
  output$pastDue <- renderDataTable(({pastDue_survey}), escape = F, rownames = FALSE,options = list(columnDefs = list(list(targets = {c(0:1,4:5)},
                                                                                                                           visible = FALSE))))
  output$expired <- renderDataTable(({expired_survey}), escape = F, rownames = FALSE,options = list(columnDefs = list(list(targets = {c(0:1,4)},
                                                                                                                           visible = FALSE))))
  
  output$discharge <- renderDataTable(({discharged_survey}), escape = F, rownames = FALSE, options = list(paging = FALSE,columnDefs = list(list(targets = {c(0:1,6)},
                                                                                                                                                visible = FALSE))))
  output$incomplete <- renderDataTable(({incomplete_survey}), escape = F, rownames = FALSE,options = list(columnDefs = list(list(targets = {c(0:2,7)},
                                                                                                                                 visible = FALSE))))
  
  output$newPatient <- renderDataTable(({newPatient_survey}), escape = F, rownames = FALSE,options = list(columnDefs = list(list(targets = {c(0:2,7)},
                                                                                                                                 visible = FALSE))))
  proxytoday <- dataTableProxy('today')
  proxytomorrow <- dataTableProxy('tomorrow')
  proxypastDue <- dataTableProxy('pastDue')
  proxyexpired <- dataTableProxy('expired')
  proxyDischarge <- dataTableProxy('discharge')
  proxyincomplete <- dataTableProxy('incomplete')
  proxynewPatient <- dataTableProxy('newPatient')
  #SURVEY NULL 
  
  output$today_none <- renderText("There are no surveys due today.")
  output$tomorrow_none <- renderText("There are no surveys due tomorrow.")
  output$pastDue_none <- renderText("There are no surveys past due.")
  output$expired_none <- renderText("There are no expired surveys.")
  output$incomplete_none <- renderText("There are no incompleted surveys.")
  output$newPatient_none <- renderText("There are no newly registered patients.")
  
  #SURVEY CONDITIONALS
  
  output$today_condition <- reactive({ return(is.null(isolate({today_survey})))})
  output$tomorrow_condition <- reactive({ return(is.null(isolate({tomorrow_survey})))})
  output$pastDue_condition <- reactive({ return(is.null(isolate({pastDue_survey})))})
  output$expired_condition <- reactive({ return(is.null(isolate({expired_survey})))})
  output$incomplete_condition <- reactive({ return(is.null(isolate({incomplete_survey})))})
  output$newPatient_condition <- reactive({ return(is.null(isolate({newPatient_survey})))})
  
  #ValueBoxes
  
  output$todayBox <- renderValueBox({
    if (is.null(nrow(isolate({today_survey})))){
      Output <- "0"
    } else {
      Output <- nrow(isolate({today_survey}))
    }
    valueBox(tags$p(paste0(Output), style = "font-size: 100%;"), "Due Today", icon = icon("calendar"))
  })
  
  output$newBox <- renderValueBox({
    if (is.null(nrow(isolate({newPatient_survey})))){
      Output <- "0"
    } else {
      Output <- nrow(isolate({newPatient_survey}))
    }
    valueBox(tags$p(paste0(Output), style = "font-size: 100%;"), "New patients requiring PROM", icon = icon("cog"),color = "green")
  })
  
  output$pastBox <- renderValueBox({
    if (is.null(nrow(isolate({pastDue_survey})))){
      Output <- "0"
    } else {
      Output <- nrow(isolate({pastDue_survey}))
    }
    valueBox(tags$p(paste0(Output), style = "font-size: 100%;"), "Past Due", icon = icon("calendar"), color = "yellow")
  })
  
  output$activeBox <- renderValueBox({
    Output <- length(unique(outcomes_data$record_id)) - length(which(outcomes_data$redcap_event_name == events[[project_string]][["discharge"]] &
                                                                       (outcomes_data[paste0(discharge_instrument,"_complete")] == 2 | 
                                                                          outcomes_data[paste0(expiration_instrument,"_complete")] == 2)))
    valueBox(tags$p(paste0(Output), style = "font-size: 100%;"), "Currently Active Patients", icon = icon("users"))
  })
  
  output$totalPatientBox <- renderValueBox({
    Output <- length(unique(outcomes_data$record_id))
    valueBox(tags$p(paste0(Output), style = "font-size: 100%;"), "Total Surveyed", icon = icon("users"))
  })
  
  #Reporting Graphics

  pocdartGraphsServer(input, output, session,"promisanx4v1",4,dplyr.Frame())

  pocdartGraphsServer(input, output, session,"promissleepdisturb4v1",4,dplyr.Frame())
  
  pocdartGraphsServer(input, output, session,"promisanger5v1",5,dplyr.Frame())
  
  pocdartGraphsServer(input, output, session,"promisemotionsupport4v2",4,dplyr.Frame())
  
  #Reporting Table
  tabularReporting(input, output, session)
}

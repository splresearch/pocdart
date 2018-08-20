## ui.R ##
function(request){
  #Call Required Libraries
  library(RCurl)
  library(shiny)
  library(shinydashboard)
  library(shinycssloaders)
  library(shinyjs)
  library(shinyWidgets)
  library(plotly)
  library(stringr)
  library(lubridate)
  library(DT)
  library(qrencoder)
  library(magrittr)
  library(dplyr)
  #Remove if not generating a report
  library(rmarkdown)
  library(kableExtra)
  #Remove if not using rintrojs
  library(rintrojs)
  
  #If using shinycssloaders, set spinner colors
  options(spinner.color="#be1e2e")
  
  #Create Sidebar for Dashboard
  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      actionButton("reset",'Refresh Data', style = 'font-size: 125%;',icon = icon("refresh")),
      actionButton("regenerate",'Regenerate Surveys', style = 'font-size: 125%;',icon = icon("refresh")),
      menuItem("Dashboards", tabName = "dashboard", icon = icon("calendar-o")),
      menuItem("Automation", tabName = "automate", icon = icon("calendar-o")),
      menuItem("Reporting", tabName = "report", icon = icon("file-text")),
      menuItem("Graphical", tabName = "graphics", icon = icon("line-chart")),
      #Conditional Panel to Select Patients
      conditionalPanel("(input.tabs == 'report' || input.tabs == 'graphics')",
                       materialSwitch(inputId = "patientSubset", 
                                      label = "Active Only", status = "primary", 
                                      right = TRUE, value = FALSE),
                       selectizeInput("patient", "Patient(s):",
                                      c(""))),
      #Conditional Panel to Select Timepoints for Report Generation
      conditionalPanel("(input.tabs == 'report')",
                       uiOutput("dailyTime"))
    ))
  
  #Create Body for Dashboard
  body <- dashboardBody(
    #Include relevant JS and CSS
    tags$head(includeScript("shiny_code_repo/java_repo/js_message_handlers.js"),
              includeScript("shiny_code_repo/java_repo/printThis.js"),
              includeScript("shiny_code_repo/java_repo/custom_scrollers.js"),
              includeScript("www/dart_js.js"),
              tags$link(rel = "stylesheet", type = "text/css", href = "dart_loading.css")),
    #If using, include the shinyJS and introJS UIs. If not including, remove
    useShinyjs(),
    introjsUI(),
    #Begin creation of Dashboard Body
    tabItems(
      tabItem(tabName = "report",copyReports('copyReports')),
      tabItem(tabName = "dashboard",censusBoxes('Census',c('today','incomplete','expire','tomorrow','past'))),
      tabItem(tabName = "graphics",div(id = "graphicReports",
                                       style=paste0("overflow-y: scroll;overflow-x: hidden;height:100%;width = 100%;"),
              pocdartGraphsUI("promis_anxiety_graphs","promisanx4v1",4,"Anxiety","PROMIS"),
              pocdartGraphsUI("promis_anxiety_graphs","promissleepdisturb4v1",4,"Sleep Disturbance","PROMIS"),
              pocdartGraphsUI("promis_anxiety_graphs","promisanger5v1",5,"Anger","PROMIS"),
              pocdartGraphsUI("promis_anxiety_graphs","promisemotionsupport4v2",4,"Emot. Support","PROMIS")
              )),
      tabItem(tabName = "automate",registerOptions('register',c('link','dash','csv'))
      )
    )
  )
  
#Create Header for Dashboard
  header <- dashboardHeader(title = "POCDART",tags$li(class = "dropdown", actionButton("info", "", icon = icon("info-circle"))))
  
#Assemble dashboard elements into a single page
  dashboardPage(
    header,
    sidebar,
    body
  )
}
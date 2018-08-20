# Copy Module UI function
copyReports <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    div(style="overflow-y: scroll;overflow-x: hidden;height: 800px;width = 90%;",
        box(title = "Daily Report: All Surveys",id = "reportOutput",width = 12, solidHeader = T, status = "primary", collapsible = TRUE,collapsed = FALSE,
            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("emrButton")),
            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("reportButton")),
            htmlOutput("Captioning"),br(),
            withSpinner(DT::dataTableOutput(("allDaily"))))
    )
  )
}


tabularReporting <- function(input, output, session) {
  output$allDaily <- renderDataTable({req(input$patient)
    req(input$dailyTime)
    rbind(daily.Report(input$patient,input$dailyTime,"promisanx4v1",repeating = "Y",project = project_string),c("","",""),
          daily.Report(input$patient,input$dailyTime,"promissleepdisturb4v1",repeating = "Y",project = project_string),c("","",""),
          daily.Report(input$patient,input$dailyTime,"promisanger5v1",repeating = "Y",project = project_string),c("","",""),
          daily.Report(input$patient,input$dailyTime,"promisemotionsupport4v2",repeating = "Y",project = project_string))},
    extensions = c("Buttons"),rownames = FALSE,escape=F,
    options = list(dom='Bt', paging = FALSE,ordering=F,
                   buttons = list(list(extend='copy',
                                       fieldSeparator = " - ",
                                       header = FALSE,
                                       newline = "\n",
                                       customize = JS('function( data ){',
                                                      'var Data = copyFormatter_all(data);',
                                                      'return Data;',
                                                      '}'))),
                   escape = FALSE))
  
  allCaption <- reactive({
    req(input$dailyTime)
    req(input$patient)
    if (is.null(table.Survey(input$patient,"promisanx4v1",input$dailyTime,input$table_options,project = project_string)[7])){
      return('<br>Patient survey is missing<br>')
    } else {
      return(paste0('<br>',table.Survey(input$patient,"promisanx4v1",input$dailyTime,input$table_options,project = project_string)[7],'<br>',
                    table.Survey(input$patient,"promissleepdisturb4v1",input$dailyTime,input$table_options,project = project_string)[7],'<br>',
                    table.Survey(input$patient,"promisanger5v1",input$dailyTime,input$table_options,project = project_string)[7],'<br>',
                    table.Survey(input$patient,"promisemotionsupport4v2",input$dailyTime,input$table_options,project = project_string)[7],'<br>'))
    }
  })
  
  output$Captioning <- renderUI(HTML(paste0('<span id = "TableCaption">',allCaption(),'</span>')))
  
  
}
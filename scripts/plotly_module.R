# Plotly Graph UI function
pocdartGraphsUI <- function(id,assay,numFields,assay_label = "REDCap Instrument",assay_type = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tabBox(id = paste0(assay,"tabs"),
         title = p(assay_label, em(paste0("(",assay_type,")"))),width = 4,
         selected = "graph",
         tabPanel(title = "Graphical", value = "graph",
                  div(style=paste0("overflow-y: scroll;overflow-x: hidden;height: ",280+80*numFields,"px;width = 90%;"),
                      withSpinner(plotlyOutput(paste0(assay,"Total"),height = "280px",width="70%")),
                      tags$div(id = paste0(assay,'subgraphs'),
                               lapply(1:numFields, function(i) {
                                 splitLayout(withSpinner(plotlyOutput(
                                   paste0(assay,"Split",i),height = "80px")),
                                   tags$div(style = "margin-top:26px",htmlOutput(paste0(assay,"Label",i))),
                                   cellWidths = c('70%','30%'),
                                   cellArgs = list(style = "display:inline-block;word-wrap:break-word;white-space:normal;overflow-y: hidden;"))
                               })
))))
}

pocdartGraphsServer <- function(input, output, session,assay,numFields,dat) {
  
  output[[paste0(assay,"Total")]] <- renderPlotly({
    graph.Alternate(input$patient,assay,"Individual Data",project = project_string,dat)
  })
  
  lapply(1:numFields, function(i) {
    output[[paste0(assay,"Split", i)]] <- renderPlotly({graph.SingleSpark(input$patient, assay,project = project_string,i,dat,selector = "Individual Data")})
  })
  lapply(1:numFields, function(i) {
    output[[paste0(assay,"Label", i)]] <- renderText(graph.SingleLabel(input$patient,assay,project = project_string,i))
  })
}
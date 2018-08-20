# Register Module UI function
registerOptions <- function(id,style = c('link','dash','csv')) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    div(id = "registrationBox",style="overflow-y: scroll;overflow-x: hidden;height: 800px;width = 90%;",
        box(title = "Patient Registration", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, 
            HTML('<center>'),
            if('link' %in% style){
            htmlOutput("admission_survey_links")},br(),
            if('dash' %in% style){
            actionButton("add_popup","Register in Dash",style='font-size:40px')},br(),br(),
            if('csv' %in% style){
            actionButton("add_tabular","Register with CSV",style='font-size:40px')},
            HTML('</center>')
        ))
  )
}
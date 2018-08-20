# Census Board UI function
censusBoxes <- function(id,boxes = c('incomplete','tomorrow','today','past','expire')) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    div(id = "coordinate", style="overflow-y: scroll;overflow-x: hidden;height: 800px;width = 90%;",
        div(id = "introBoxes",style = "height:350px;",   
            withSpinner(valueBoxOutput("todayBox", width = 6)),
            valueBoxOutput("pastBox", width = 6),
            valueBoxOutput("newBox", width = 12),
            valueBoxOutput("totalPatientBox", width = 6),
            valueBoxOutput("activeBox", width = 6)),
        box(title = "New Patients", id = "surveyBox", status = "success", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, dataTableOutput(("newPatient")),
            conditionalPanel(condition = "output.newPatient_condition", textOutput(("newPatient_none"))),
            div(style = "height:0px;visibility:hidden;",textOutput(("newPatient_condition")))),
        if('incomplete' %in% boxes ){
        box(title = "Incomplete Surveys", status = "warning", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, dataTableOutput(("incomplete")),
            conditionalPanel(condition = "output.incomplete_condition", textOutput(("incomplete_none"))),
            div(style = "height:0px;visibility:hidden;",textOutput(("incomplete_condition"))))},
        if('today' %in% boxes ){
        box(title = "Today", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, dataTableOutput(("today")),
            conditionalPanel(condition = "output.today_condition", textOutput(("today_none"))),
            div(style = "height:0px;visibility:hidden;",textOutput(("today_condition"))))},
        if('tomorrow' %in% boxes ){
        box(title = "Upcoming", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, dataTableOutput(("tomorrow")),
            conditionalPanel(condition = "output.tomorrow_condition", textOutput(("tomorrow_none"))),
            div(style = "height:0px;visibility:hidden;",textOutput(("tomorrow_condition"))))},
        if('past' %in% boxes ){
        box(title = "Past Due", status = "warning", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, dataTableOutput(("pastDue")),
            conditionalPanel(condition = "output.pastDue_condition", textOutput(("pastDue_none"))),
            div(style = "height:0px;visibility:hidden;",textOutput(("pastDue_condition"))))},
        if('expire' %in% boxes ){
        box(title = "Expired", status = "danger", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, dataTableOutput(("expired")),
            conditionalPanel(condition = "output.expired_condition", textOutput(("expired_none"))),
            div(style = "height:0px;visibility:hidden;",textOutput(("expired_condition"))))},
        box(title = "Discharge", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
            dataTableOutput(("discharge"))) 
    )
  )
}
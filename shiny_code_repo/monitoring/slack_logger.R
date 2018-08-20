slack_logger <- function(message = "error", source = getwd(),subject = "Shiny Error",email_destination = "") {
  # write file
  file_name <- paste0("dart_error_report_", format(Sys.time(), "%Y%m%d%H%M%S"), ".txt")
  fileConn<-file(paste0("/srv/shiny-server/shinyEmailQueue/", file_name))
  writeLines(message, fileConn)
  close(fileConn)
  
  # set job
  email_origin <- gsub(" ", "_", source)
  #subject <- "Shiny Error"
  scheduled_datetime <- as.numeric(as.POSIXlt(Sys.Date()))
  print_job <- paste(file_name,email_destination,email_origin,subject,scheduled_datetime, sep = ",")
  
  write.table(print_job, file = "/srv/shiny-server/shinyEmailQueue/emailJobSchedule.csv", row.names = F, col.names = F, quote = F, append = T)
}

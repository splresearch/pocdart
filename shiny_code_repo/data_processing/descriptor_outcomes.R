descriptor.Outcomes <- function(dat, time, domain, assay){
  #Source ranges and text
  source(paste0("shiny_code_repo/survey_repo/",assay,".R"))
  
  #Set values
  IntroVal <- (dat$score_primary[dat$domain %in% domain])
  RecentVal <- (dat$change_recent[dat$domain %in% domain])
  AdminVal <- (dat$change_adm[dat$domain %in% domain])
  MaxVal <- (dat$score_max[dat$domain %in% domain])
  
  #Determine correct intervals
  Intro.Text <- Descriptors$Intro[findInterval(IntroVal,Ranges$Intro)+1]
  if (is.na(RecentVal[[1]])){
    Recent.Text <- " "
  } else {
    Recent.Text <- Descriptors$Recent[findInterval(RecentVal,Ranges$Recent)+1]
  }
  #print(AdminVal)
  Admin.Text <- ifelse(is.na(AdminVal), ", admission not recorded",Descriptors$Admission[findInterval(AdminVal,Ranges$Admission)+1])
  Maximum.Text <- ifelse(MaxVal == IntroVal, Descriptors$Max[2],Descriptors$Max[1])
  if (time == 0){
    pro.Text <- paste0(domain," PRO: Patient self reports ",Intro.Text,". ")
  } else if (is.na(AdminVal)){ 
    pro.Text <- paste0(domain," PRO: Patient self reports ",Intro.Text,Recent.Text,Admin.Text,". ")
  } else {
    pro.Text <- paste0(domain," PRO: Patient self reports ",Intro.Text,Recent.Text,Admin.Text," of admission. ")
  }
  #Save value
  dat$description[dat$domain %in% domain] <- pro.Text
  
  return(dat)
}
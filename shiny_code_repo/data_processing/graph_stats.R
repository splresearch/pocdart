calc.Extremes <- function(dat,column,type){
  range <- rep(0,nrow(dat))
  
  for (i in (i = 1:nrow(dat))){
    if (str_detect(dat[i,column+3],"/") && !is.na(dat[i,column+3])){
      range[i] <- as.numeric(strsplit(dat[i,column+3],"/")[[1]][1])
    } else {
      range[i] <- as.numeric(dat[i,column+3])
    }
  }
  
  if (type == "min"){
    extreme <- min(range,na.rm =T)
  } else if (type == "max"){
    extreme <- max(range,na.rm = T)
  }
  
  return(extreme)
}

general.Stat <- function(dat,gp,options){
  stat.Frame <- data.frame(Mean = NA, SD = NA, SE = NA, position = NA, Min = NA, Max = NA)
  t <- 1
  n <- 1
  #dat <- subset(dat, !is.na(Value))
  for (i in unique(dat$Assay)){
    stat.Frame[t,1] <- mean(subset(dat, Assay == i)$Value,na.rm = T)
    stat.Frame[t,2] <- sd(subset(dat, Assay == i)$Value,na.rm = T)
    stat.Frame[t,3] <- sd(subset(dat, Assay == i)$Value,na.rm = T)/sqrt(length(subset(dat, Assay == i)$Value))
    stat.Frame[t,4] <- as.numeric(t)
    stat.Frame[t,5] <- min(subset(dat, Assay == i)$Value,na.rm = T)
    stat.Frame[t,6] <- max(subset(dat, Assay == i)$Value,na.rm = T)
    
    t <- t + 1
  }
  
  if ((length(subset(options, subset = options %in% "SD")) == 1)){
    gp <- gp + geom_rect(data = stat.Frame, fill = "#37373a", alpha=0.4, aes(x = NULL, y = NULL, ymin = Mean - SD, ymax = Mean + SD,
                                                                             xmin = position -.05, xmax = position +.05))
  }
  
  if ((length(subset(options, subset = options %in% "SE")) == 1)){
    gp <- gp + geom_rect(data = stat.Frame, fill = "#37373a", alpha=0.6,aes(x = NULL, y = NULL, ymin = Mean - SE, ymax = Mean + SE,
                                                                            xmin = position -.05, xmax = position +.05))
  }
  
  if ((length(subset(options, subset = options %in% "Mean")) == 1)){
    gp <- gp + geom_rect(data = stat.Frame,aes(x = NULL, y = NULL, ymin = Mean, ymax = Mean,
                                               xmin = position -.15, xmax = position +.15), color = "#000000")
  }
  
  if ((length(subset(options, subset = options %in% "Min")) == 1)){
    gp <- gp + geom_point(data = stat.Frame, aes(x = position, y= Min), alpha=0.4, color = "#37373a")
  }
  
  if ((length(subset(options, subset = options %in% "Max")) == 1)){
    gp <- gp + geom_point(data = stat.Frame, aes(x = position, y= Max), alpha=0.4, color = "#37373a")
  }
  return(gp)
}

Descriptor <- function(dat, time, domain){
  
  Descriptors <- list(Intro = c("in normal range","as mild","as moderate","as severe"),
                      Recent = c(", recent improvement",", unchanged",", recent deterioration"),
                      Admission = c("and has improved","and remains unchanged","and has deteriorated"),
                      Max = c("Patient is below maximum value","Patient is at the maximum value"))
  if (domain == "PHQ-9 Depression"){
    Descriptors$Intro[4:5] <- c("as moderately severe","as severe")
  }
  
  if (domain == "WHODAS - General Disability"){
    Descriptors$Intro[5] <- c("as extreme")
  }
  
  
  
  
  IntroVal <- (dat$score_primary[dat$domain %in% domain])
  RecentVal <- (dat$change_recent[dat$domain %in% domain])
  AdminVal <- (dat$change_adm[dat$domain %in% domain])
  MaxVal <- (dat$score_max[dat$domain %in% domain])
  if (domain == "PHQ-9 Depression"){
    pro.Text <- paste0(domain," PRO: Patient rates ",
                       ifelse(IntroVal < 4, Descriptors$Intro[1],
                              ifelse(IntroVal < 9, Descriptors$Intro[2],
                                     ifelse(IntroVal < 14, Descriptors$Intro[3],
                                            ifelse(IntroVal < 19, Descriptors$Intro[4],Descriptors$Intro[5])))),
                       ifelse(RecentVal < -2, Descriptors$Recent[3],
                              ifelse(RecentVal < 2, Descriptors$Recent[2],Descriptors$Recent[1])),
                       ", ",
                       ifelse(AdminVal < -2, Descriptors$Admission[3],
                              ifelse(AdminVal < 2, Descriptors$Admission[2],Descriptors$Admission[1])),
                       " from admission. ",
                       ifelse(MaxVal == IntroVal, Descriptors$Max[2],Descriptors$Max[1]))
  } else if (domain == "WHODAS - General Disability"){
    pro.Text <- paste0(domain," PRO: Patient rates ",
                       ifelse(IntroVal < .2, Descriptors$Intro[1],
                              ifelse(IntroVal < .4, Descriptors$Intro[2],
                                     ifelse(IntroVal < .6, Descriptors$Intro[3],
                                            ifelse(IntroVal < .8, Descriptors$Intro[4],Descriptors$Intro[5])))),
                       ifelse(RecentVal < -.1, Descriptors$Recent[1],
                              ifelse(RecentVal < .1, Descriptors$Recent[2],Descriptors$Recent[3])),
                       ", ",
                       ifelse(is.na(AdminVal), ", admission not recorded", ifelse(AdminVal < -.1, Descriptors$Admission[1],
                                                                                  ifelse(AdminVal < .1, Descriptors$Admission[2],Descriptors$Admission[3]))),
                       " from admission. ",
                       ifelse(MaxVal == IntroVal, Descriptors$Max[2],Descriptors$Max[1]))
  } else if (domain != "Pain Intensity"){
    pro.Text <- paste0(domain," PRO: Patient rates ",
                       ifelse(IntroVal < 55, Descriptors$Intro[1],
                              ifelse(IntroVal < 60, Descriptors$Intro[2],
                                     ifelse(IntroVal < 70, Descriptors$Intro[3], Descriptors$Intro[4]))),
                       ifelse(RecentVal < -5, Descriptors$Recent[1],
                              ifelse(RecentVal < 5, Descriptors$Recent[2],Descriptors$Recent[3])),
                       ", ",
                       ifelse(AdminVal < -5, Descriptors$Admission[1],
                              ifelse(AdminVal < 5, Descriptors$Admission[2],Descriptors$Admission[3])),
                       " from admission. ",
                       ifelse(MaxVal == IntroVal, Descriptors$Max[2],Descriptors$Max[1]))
  } else {
    pro.Text <- paste0(domain," PRO: Patient rates at ",IntroVal,
                       ifelse(RecentVal < -2, Descriptors$Recent[1],
                              ifelse(RecentVal < 2, Descriptors$Recent[2],Descriptors$Recent[3])),
                       ", ",
                       ifelse(AdminVal < -2, Descriptors$Admission[1],
                              ifelse(AdminVal < 2, Descriptors$Admission[2],Descriptors$Admission[3])),
                       " from admission. ",
                       ifelse(MaxVal == IntroVal, Descriptors$Max[2],Descriptors$Max[1])) 
  }
  
  
  
  
  
  dat$description[dat$domain %in% domain] <- pro.Text
  return(dat)
}
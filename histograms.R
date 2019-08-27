library(ggplot2)
library(ggpubr)
library(plyr)
library(tidyverse)
library(reshape)
library(data.table)

setwd("~/Documents/bevel_choice")

readin <- function(n){
  data <- read.delim(n)
  names(data) <- c("subj", "run", "pair", "choice","side", "outcome", "congruent", "RT")
  data$trial<- row.names(data)
  head(data)
  
  data_ab <- data[which(data$pair=="AB"), ]
  data_ab$trial_ab <- cumsum(data_ab$pair == "AB")
  
  data_ab$percent_corr_ab <- (cumsum(data_ab$choice=="A")) /
    (cumsum(data_ab$choice=="A" | data_ab$choice == "B"))
  
  data_ab$trialbin[data_ab$trial_ab <= 7]<- "bin1" 
  data_ab$trialbin[data_ab$trial_ab > 7 & data_ab$trial_ab <= 14]<- "bin2" 
  data_ab$trialbin[data_ab$trial_ab > 14 & data_ab$trial_ab <= 21]<- "bin3" 
  data_ab$trialbin[data_ab$trial_ab > 21 & data_ab$trial_ab <= 28]<- "bin4"
  data_ab$trialbin[data_ab$trial_ab > 28]<- "bin5" 
  
  summary(as.factor(data_ab$trialbin))
  
  plot1 <- ggplot(aes(x = trialbin, y = percent_corr_ab), data = data_ab) +
    stat_summary(fun.y = "mean", geom = "bar") + theme_minimal()
  
  #Select cd
  data_cd <- data[which(data$pair=="CD"), ]
  data_cd$trial_cd <- cumsum(data_cd$pair == "CD")
  
  data_cd$percent_corr_cd <- (cumsum(data_cd$choice=="C")) /
    (cumsum(data_cd$choice=="C" | data_cd$choice == "D"))
  
  data_cd$trialbin[data_cd$trial_cd <= 7]<- "bin1" 
  data_cd$trialbin[data_cd$trial_cd > 7 & data_cd$trial_cd <= 14]<- "bin2" 
  data_cd$trialbin[data_cd$trial_cd > 14 & data_cd$trial_cd <= 21]<- "bin3" 
  data_cd$trialbin[data_cd$trial_cd > 21 & data_cd$trial_cd <= 28]<- "bin4"
  data_cd$trialbin[data_cd$trial_cd > 28]<- "bin5" 
  
  summary(as.factor(data_cd$trialbin))
  
  plot2 <- ggplot(aes(x = trialbin, y = percent_corr_cd), data = data_cd) +
    stat_summary(fun.y = "mean", geom = "bar") + theme_minimal()
  
  #Select EF
  data_ef <- data[which(data$pair=="EF"), ]
  data_ef$trial_ef <- cumsum(data_ef$pair == "EF")
  
  data_ef$percent_corr_ef <- (cumsum(data_ef$choice=="E")) /
    (cumsum(data_ef$choice=="E" | data_ef$choice == "F"))
  
  data_ef$trialbin[data_ef$trial_ef <= 7]<- "bin1" 
  data_ef$trialbin[data_ef$trial_ef > 7 & data_ef$trial_ef <= 14]<- "bin2" 
  data_ef$trialbin[data_ef$trial_ef > 14 & data_ef$trial_ef <= 21]<- "bin3" 
  data_ef$trialbin[data_ef$trial_ef > 21 & data_ef$trial_ef <= 28]<- "bin4"
  data_ef$trialbin[data_ef$trial_ef > 28]<- "bin5" 
  
  summary(as.factor(data_ef$trialbin))
  
  plot3 <- ggplot(aes(x = trialbin, y = percent_corr_ef), data = data_ef) +
    stat_summary(fun.y = "mean", geom = "bar") + theme_minimal()
  
  plot4<-ggarrange(plot1,plot2, plot3,  
                   labels = c("80/20 pair", "70/30 pair", "60/40 pair"),
                   ncol = 3, nrow = 1)
  
  s1 = unlist(strsplit(n, split='/', fixed=TRUE))[5]
  s2 = unlist(strsplit(s1, split='.', fixed=TRUE))[1]
  j <- paste(s2, ".png",sep = "")
  ggsave(j, width = 7, height = 3, units = "in")

}

readin("by_participant_txtfiles/01.txt")


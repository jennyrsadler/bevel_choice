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
  
  mean_ab_1 <- (sum(data_ab$trialbin == "bin1" & data_ab$choice=="A"))/
    (sum(data_ab$trialbin == "bin1" & data_ab$choice=="A" | data_ab$trialbin == "bin1" & data_ab$choice == "B"))
  mean_ab_2 <- (sum(data_ab$trialbin == "bin2" & data_ab$choice=="A"))/
    (sum(data_ab$trialbin == "bin2" & data_ab$choice=="A" | data_ab$trialbin == "bin2" & data_ab$choice == "B"))
  mean_ab_3 <- (sum(data_ab$trialbin == "bin3" & data_ab$choice=="A"))/
    (sum(data_ab$trialbin == "bin3" & data_ab$choice=="A" | data_ab$trialbin == "bin3" & data_ab$choice == "B"))
  mean_ab_4 <- (sum(data_ab$trialbin == "bin4" & data_ab$choice=="A"))/
    (sum(data_ab$trialbin == "bin4" & data_ab$choice=="A" | data_ab$trialbin == "bin4" & data_ab$choice == "B"))
  mean_ab_5 <- (sum(data_ab$trialbin == "bin5" & data_ab$choice=="A"))/
    (sum(data_ab$trialbin == "bin5" & data_ab$choice=="A" | data_ab$trialbin == "bin5" & data_ab$choice == "B"))
  
  plot1 <- ggplot(aes(x = trialbin, y = percent_corr_ab), data = data_ab) +
    stat_summary(fun.y = "mean", geom = "bar") + theme_minimal()
  
  #Select cd
  data_cd <- data[which(data$pair=="CD"), ]
  data_cd$trial_cd <- cumsum(data_cd$pair == "CD")
  
  data_cd$trialbin[data_cd$trial_cd <= 7]<- "bin1" 
  data_cd$trialbin[data_cd$trial_cd > 7 & data_cd$trial_cd <= 14]<- "bin2" 
  data_cd$trialbin[data_cd$trial_cd > 14 & data_cd$trial_cd <= 21]<- "bin3" 
  data_cd$trialbin[data_cd$trial_cd > 21 & data_cd$trial_cd <= 28]<- "bin4"
  data_cd$trialbin[data_cd$trial_cd > 28]<- "bin5" 
  
  mean_cd_1 <- (sum(data_cd$trialbin == "bin1" & data_cd$choice=="C"))/
    (sum(data_cd$trialbin == "bin1" & data_cd$choice=="C" | data_cd$trialbin == "bin1" & data_cd$choice == "D"))
  mean_cd_2 <- (sum(data_cd$trialbin == "bin2" & data_cd$choice=="C"))/
    (sum(data_cd$trialbin == "bin2" & data_cd$choice=="C" | data_cd$trialbin == "bin2" & data_cd$choice == "D"))
  mean_cd_3 <- (sum(data_cd$trialbin == "bin3" & data_cd$choice=="C"))/
    (sum(data_cd$trialbin == "bin3" & data_cd$choice=="C" | data_cd$trialbin == "bin3" & data_cd$choice == "D"))
  mean_cd_4 <- (sum(data_cd$trialbin == "bin4" & data_cd$choice=="C"))/
    (sum(data_cd$trialbin == "bin4" & data_cd$choice=="C" | data_cd$trialbin == "bin4" & data_cd$choice == "D"))
  mean_cd_5 <- (sum(data_cd$trialbin == "bin5" & data_cd$choice=="C"))/
    (sum(data_cd$trialbin == "bin5" & data_cd$choice=="C" | data_cd$trialbin == "bin5" & data_cd$choice == "D"))
  
  data_cd$percent_corr_cd <- (cumsum(data_cd$choice=="C")) /
    (cumsum(data_cd$choice=="D" | data_cd$choice == "C"))
  
  plot2 <- ggplot(aes(x = trialbin, y = percent_corr_cd), data = data_cd) +
    stat_summary(fun.y = "mean", geom = "bar") + theme_minimal()
  
  #Select EF
  data_ef <- data[which(data$pair=="EF"), ]
  data_ef$trial_ef <- cumsum(data_ef$pair == "EF")
  
  data_ef$trialbin[data_ef$trial_ef <= 7]<- "bin1" 
  data_ef$trialbin[data_ef$trial_ef > 7 & data_ef$trial_ef <= 14]<- "bin2" 
  data_ef$trialbin[data_ef$trial_ef > 14 & data_ef$trial_ef <= 21]<- "bin3" 
  data_ef$trialbin[data_ef$trial_ef > 21 & data_ef$trial_ef <= 28]<- "bin4"
  data_ef$trialbin[data_ef$trial_ef > 28]<- "bin5" 
  
  mean_ef_1 <- (sum(data_ef$trialbin == "bin1" & data_ef$choice=="E"))/
    (sum(data_ef$trialbin == "bin1" & data_ef$choice=="E" | data_ef$trialbin == "bin1" & data_ef$choice == "F"))
  mean_ef_2 <- (sum(data_ef$trialbin == "bin2" & data_ef$choice=="E"))/
    (sum(data_ef$trialbin == "bin2" & data_ef$choice=="E" | data_ef$trialbin == "bin2" & data_ef$choice == "F"))
  mean_ef_3 <- (sum(data_ef$trialbin == "bin3" & data_ef$choice=="E"))/
    (sum(data_ef$trialbin == "bin3" & data_ef$choice=="E" | data_ef$trialbin == "bin3" & data_ef$choice == "F"))
  mean_ef_4 <- (sum(data_ef$trialbin == "bin4" & data_ef$choice=="E"))/
    (sum(data_ef$trialbin == "bin4" & data_ef$choice=="E" | data_ef$trialbin == "bin4" & data_ef$choice == "F"))
  mean_ef_5 <- (sum(data_ef$trialbin == "bin5" & data_ef$choice=="E"))/
    (sum(data_ef$trialbin == "bin5" & data_ef$choice=="E" | data_ef$trialbin == "bin5" & data_ef$choice == "F"))

  data_ef$percent_corr_ef <- (cumsum(data_ef$choice=="E")) /
    (cumsum(data_ef$choice=="F" | data_ef$choice == "E"))
  
  mean_bin <- (c(mean_ef_1, mean_ef_2, mean_ef_3, mean_ef_4, mean_ef_5))
  plot3 <- ggplot(aes(x = trialbin, y = percent_corr_ef), data = data_ef) +
    stat_summary(fun.y = "mean", geom = "bar") + theme_minimal()
  
  plot4<-ggarrange(plot1,plot2, plot3,  
                   labels = c("80/20 pair", "70/30 pair", "60/40 pair"),
                   ncol = 3, nrow = 1)
  
  s1 = unlist(strsplit(n, split='/', fixed=TRUE))[5]
  s2 = unlist(strsplit(s1, split='.', fixed=TRUE))[1]
  j <- paste(s2, ".png",sep = "")
  ggsave(j, width = 7, height = 3, units = "in")
  
  data2 <- subset(data_ab, trialbin == "bin1"| trialbin == "bin5")
  data3 <- subset(data_cd, trialbin == "bin1"| trialbin == "bin5")
  data4 <- subset(data_ef, trialbin == "bin1"| trialbin == "bin5")
  
}

readin("by_participant_txtfiles/02.txt")


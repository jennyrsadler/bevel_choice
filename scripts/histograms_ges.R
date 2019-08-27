library(ggplot2)
library(ggpubr)
library(plyr)
library(tidyverse)
library(reshape)
library(data.table)

data <- read.delim("~/Google Drive/bevel_task_behavior/by_participant_txt_left_and_right/by_participant_txtfiles/01.txt", sep = "\t", header=F)
head(data)
names(data) <- c("subj", "run", "pair", "choice","side", "outcome", "congruent", "RT")

data$trial<- row.names(data)

#Select AB
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
summary(data_ab$trialbin)
data_ab$trialbin<-as.factor(data_ab$trialbin)
plot1 <- ggplot(aes(x = trialbin, y = percent_corr_ab), data = data_ab) +
  stat_summary(fun.y = "mean", geom = "bar") + theme_minimal()
plot1

################################################################################
#Mean center
## will actually want to do this on all the subjects, else it is just centered on the subject mean (not the grand mean)
data_ab$percent_corr_ab_c<-scale(data_ab$percent_corr_ab, center = FALSE)

# Summarize function from helpers.R
## http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
d1 <- summarySE(data_ab, measurevar="percent_corr_ab_c", groupvars=c("trialbin"))
d1

# Error bars = standard error of the mean
p1<-ggplot(d1, aes(x=trialbin, y=mean, group=1)) + #here group is just 1 as a place holder, in the spaghetti plot we will want this to be subjects 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  geom_line() +
   theme_minimal()
p1 #this is ok, but we can do better ;)

# Violin plot
## http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization

p <- ggplot(data_ab, aes(x=trialbin, y=percent_corr_ab_c)) + 
  geom_violin()+ 
  stat_summary(fun.data=data_summary, color="pink")+
  geom_line(data=d1, aes(x=trialbin, y=mean, group=1))+ #this is pulling data from the SEsummary function
  theme_minimal()
p #this is goregous <3 

### Next steps ###
# 1. Read in all the data (use the function from yesterday)
# 2. Mean center the percent_corr per condition (so, all subjects ab, then all subjects cd, etc)
# 3. Create a "spaghetti plot" aka line, point plot
# 3a. Use the SEsummary function above with the group variables as the trial bins and subject groupvars=c(1,2)
# 3b. In ggplot use an additional group variable (subjects) to display all subjects line 
# 4. Make a violin plot with lines to summarize the data (either by a group or all the data by bin), try the line overlay
################################################################################

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
plot4

ggsave("~/Documents/bevel_choice/sub1.pdf", width = 7, height = 3, units = "in")

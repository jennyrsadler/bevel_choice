library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape)
library(ggpubr)
library(psych)

data <- read.delim("~/Documents/bevel_choice/all_subjects.txt")
names(data) <- c("subj", "run", "pair", "choice", "outcome", "congruent", "RT")

data <- data %>%
  group_by(.dots=c("subj","pair")) %>%
  mutate(trial=row_number())

data$trialbin[data$trial <= 7]<- "bin1" 
data$trialbin[data$trial > 7 & data$trial <= 14]<- "bin2" 
data$trialbin[data$trial > 14 & data$trial <= 21]<- "bin3" 
data$trialbin[data$trial > 21 & data$trial <= 28]<- "bin4"
data$trialbin[data$trial > 28]<- "bin5" 

    
data2 <- subset(data, trialbin == "bin1" | trialbin=="bin5")
data2$choice_num[data2$choice == "corr"]<- 1 
data2$choice_num[data2$choice == "incorr"]<- 0 

data2 <- data2 %>%
  group_by(.dots=c("subj","trialbin", "pair")) %>%
  summarise(
    count = n(),
    mean = mean(choice_num, na.rm = TRUE)
)

###WHOLE GROUP
t.test(data2$mean ~ data2$trialbin)

### Group by pair
data3 <- subset(data2, pair == "56")
t.test(data3$mean ~ data3$trialbin)

#### BY SUBJECT
ttest_ab <- function(n){
  x <- subset(data2, subj == n & pair == "12")
  t.test(x$choice_num ~ x$trialbin)

}

ttest_cd <- function(n){
  x <- subset(data2, subj == n & pair == "34")
  t.test(x$choice_num ~ x$trialbin)
  
}

ttest_ef <- function(n){
  x <- subset(data2, subj == n & pair == "56")
  t.test(x$choice_num ~ x$trialbin)
  
}
#Check participants with missing bin5 data
data3 <- subset(data2, pair == "56")
table(data3$subj, data3$trialbin)

#Subj Missing AB Bin5
#2, 16, 18, 23, 24, 25, 26, 28, 29, 30, 32, 44, 46, 49, 52, 62, 64, 73, 79, 82, 87, 90
ttest_ab(89)

sink('analysis-output.txt', append = TRUE)
cat("=============================\n")
cat("Subj 88 \nPair: 80/20 \n")
ttest_ab(88)
sink()


#Subj Missing CD Bin5
#2, 5, 6, 9, 12, 21, 22, 23, 24, 31, 34, 35, 37, 38, 43, 49, 52, 53, 55, 59, 61, 65, 67, 69, 71, 74, 75, 76, 78, 90
ttest_cd(89)

sink('analysis-output.txt', append = TRUE)
cat("=============================\n")
cat("Subj 82 \nPair: 70/30 \n")
ttest_cd(82)
sink()

#Subj Missing EF Bin5
#1, 3, 4, 7, 11, 15, 16, 17, 19, 21, 22, 23, 24, 26, 29, 30, 33, 39, 40, 42, 46, 49, 57, 60, 61, 65, 67, 79, 85, 88, 89  
ttest_ef(90)

sink('analysis-output.txt', append = TRUE)
cat("=============================\n")
cat("Subj 80 \nPair: 60/40 \n")
ttest_ef(80)
sink()


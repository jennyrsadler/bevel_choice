library(dplyr)
library(tidyr)
library(ggplot2)

data <- read.delim("~/Documents/bevel_choice/all_subjects.txt")
names(data) <- c("subj", "run", "pair", "choice", "outcome", "congruent", "RT")

data <- data %>%
  group_by(.dots=c("subj","pair")) %>%
  mutate(Count=row_number())

percent_correct_by_trial_ab <- function(n) {
  count <- sum(data$Count == n & data$choice=="corr" & data$pair == "12")
  countall <- sum(data$Count == n & data$pair == "12")
  return(count/countall)
}

percent_correct_by_trial_cd <- function(n) {
  count <- sum(data$Count == n & data$choice=="corr" & data$pair == "34")
  countall <- sum(data$Count == n & data$pair == "34")
  return(count/countall)
}

percent_correct_by_trial_ef <- function(n) {
  count <- sum(data$Count == n & data$choice=="corr" & data$pair == "56")
  countall <- sum(data$Count == n & data$pair == "56")
  return(count/countall)
}

percent_matched <- function(n) {
  count <- sum(data$Count == n & data$congruent=="congruent")
  countall <- sum(data$Count == n & data$pair == "56")
  return(count/countall)
}

#percent_correct_by_trial_ab(10)

x <- 1:46
output_ab <- lapply(x, percent_correct_by_trial_ab) 
output_cd <- lapply(x, percent_correct_by_trial_cd) 
output_ef <- lapply(x, percent_correct_by_trial_ef) 

df_ab <- data.frame(matrix(unlist(output_ab), nrow=length(output_ab), byrow=T))
df_cd <- data.frame(matrix(unlist(output_cd), nrow=length(output_cd), byrow=T))
df_ef <- data.frame(matrix(unlist(output_ef), nrow=length(output_ef), byrow=T))

colnames(df_ab)[colnames(df_ab)=="matrix.unlist.output_ab...nrow...length.output_ab...byrow...T."] <- "percent_correct_ab"
colnames(df_cd)[colnames(df_cd)=="matrix.unlist.output_cd...nrow...length.output_cd...byrow...T."] <- "percent_correct_cd"
colnames(df_ef)[colnames(df_ef)=="matrix.unlist.output_ef...nrow...length.output_ef...byrow...T."] <- "percent_correct_ef"

df_ab$trial <- seq.int(nrow(df_ab))
df_cd$trial <- seq.int(nrow(df_cd))
df_ef$trial <- seq.int(nrow(df_ef))

ggplot(data=df_ab, aes(x=trial, y=percent_correct_ab, group=1)) +
  geom_line()+
  geom_point()

ggplot(data=df_cd, aes(x=trial, y=percent_correct_cd, group=1)) +
  geom_line()+
  geom_point()

ggplot(data=df_ef, aes(x=trial, y=percent_correct_ef, group=1)) +
  geom_line()+
  geom_point()

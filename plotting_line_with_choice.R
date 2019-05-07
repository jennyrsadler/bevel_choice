library(ggplot2)
library(ggrepel)
library(data.table)
library(psych)

#df1 = read.table('/Users/jennygilbert/Documents/bevel_choice/by_participant_csv/02.csv', header=TRUE, sep=',')
#df2 = read.table('/Users/jennygilbert/Documents/bevel_choice/by_participant_csv/02.csv', header=TRUE, sep=',')

#Line plot of learned probabilities
## Need "long format" variable to plot all on the same graph via ggplot
f1 = function(w) {
setDT(w)
DT.m1 = melt(w, measure.vars = c("a_learnedprob_correct", "b_learnedprob_correct","c_learnedprob_correct",
                                    "d_learnedprob_correct", "e_learnedprob_correct", "f_learnedprob_correct"), 
             variable.name = "Shape", value.name = "learned_prob")
#Get rid of the NA's and missing
DT.m1[DT.m1==""] <- NA
df <- na.omit(DT.m1)

#Plot learned probability 
ggplot(df, aes(x=X, y=learned_prob)) + geom_line(aes(colour=Shape)) + 
  labs(title = "Learned Probability over time", x = "Trials", y = "Learned Probability", color = "Shapes")
}
#Make a distribution of response to reinforcers
f2 = function(w) {
  pdf(file = 'plot', width = 4, height = 4)
  counts <- prop.table(table(df$reinforcer_response))
  barplot(counts, main="Response Distribution", 
        ylab="Percent of Responses", xlab='Response to Reinforcement', col = rainbow(5))
  dev.off() }

setwd("/Users/jennygilbert/Documents/bevel_choice/by_participant_csv")
mylist <- list.files(pattern = ".csv")

for (i in mylist)
  print(i)
  df = read.table(i, header=TRUE, sep=',')
  f1(df)
  f2(df)
  
data = read.table('/Users/jennygilbert/Documents/bevel_choice/01_test.csv', header=TRUE, sep=',')

install.packages('ggrepel')

library(ggplot2)
library(ggrepel)

ggplot(data=subset(data, !is.na(a_corr_prob)), aes(x=reinforcer_running, y=a_corr_prob, label = outcome)) +
  geom_line() + geom_point() + geom_text()



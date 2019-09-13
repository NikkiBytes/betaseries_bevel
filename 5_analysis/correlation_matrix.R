install.packages("Hmisc")
library("Hmisc")
install.packages("corrplot")
library(corrplot)

setwd('/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/FSLNets_test/reward/')

mydata = read.csv("sub-001_reward.txt", header = FALSE, sep = '\t')
mydata.rcorr = rcorr(as.matrix(mydata))
mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P
corrplot(mydata.cor)
heatmap(x = mydata.cor, symm = TRUE)

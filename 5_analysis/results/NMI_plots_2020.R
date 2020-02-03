library(readxl)
library(ggplot2)
library(psych)
library(dplyr)

#sink('/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/results/node_analysis_output.txt')

df<- read.csv("/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/results/NMI.csv",sep=",", header = TRUE)
names(df)

#NMI Values 
NMI_choice_sex = 0.525575621852539
NMI_sweet_sex = 0.8358726441483001
NMI_bitter_sex = 0.7714836723402282
NMI_choice_learner = 0.5585026269033441
NMI_sweet_learner = 0.7735250658957238
NMI_bitter_learner = 0.8358726441483001
NMI_choice_BMI = 0.6955665770524795
NMI_sweet_BMI = 0.6691061868254723
NMI_bitter_BMI = 0.6848308134347534

mean(df$choice_sex)
mean(df$sweet_sex)
mean(df$bitter_sex)
mean(df$choice_learn)
mean(df$sweet_learn)
mean(df$bitter_learn)
mean(df$choice_wt)
mean(df$sweet_wt)
mean(df$bitter_wt)
SD(df)

#Make Distrubution Plots 
ggplot(df, aes(x=choice_sex)) + geom_histogram(binwidth=.15, fill="gray") + 
  geom_vline(aes(xintercept=NMI_choice_sex), color="black", linetype="solid", size=1) + theme_minimal(base_size = 15) + 
  labs(x = "Choice by Sex", y= "Frequency")

ggplot(df, aes(x=sweet_sex)) + geom_histogram(binwidth=.15, fill="gray") + 
  geom_vline(aes(xintercept=NMI_sweet_sex), color="black", linetype="solid", size=1) + theme_minimal(base_size = 15) + 
  labs(x = "Reward by Sex", y= "Frequency")

ggplot(df, aes(x=bitter_sex)) + geom_histogram(binwidth=.15, fill="gray") + 
  geom_vline(aes(xintercept=NMI_bitter_sex), color="black", linetype="solid", size=1) + theme_minimal(base_size = 15) + 
  labs(x = "Punishment by Sex", y= "Frequency", cex.lab=2)

#Learner Groups
ggplot(df, aes(x=choice_learn)) + geom_histogram(binwidth=.15, fill="gray") + 
  geom_vline(aes(xintercept=NMI_choice_learner), color="black", linetype="solid", size=1) + theme_minimal(base_size = 15) + 
  labs(x = "Choice by Leaner Groups", y= "Frequency")

ggplot(df, aes(x=sweet_learn)) + geom_histogram(binwidth=.15, fill="gray") + 
  geom_vline(aes(xintercept=NMI_sweet_learner), color="black", linetype="solid", size=1) + theme_minimal(base_size = 15) + 
  labs(x = "Reward by Leaner Groups", y= "Frequency")

ggplot(df, aes(x=bitter_learn)) + geom_histogram(binwidth=.15, fill="gray") + 
  geom_vline(aes(xintercept=NMI_bitter_learner), color="black", linetype="solid", size=1) + theme_minimal(base_size = 15) + 
  labs(x = "Punishment by Leaner Groups", y= "Frequency")

# BMI
ggplot(df, aes(x=choice_wt)) + geom_histogram(binwidth=.15, fill="gray") + 
  geom_vline(aes(xintercept=NMI_choice_BMI), color="black", linetype="solid", size=1) + theme_minimal(base_size = 15) + 
  labs(x = "Choice by BMI Groups", y= "Frequency")

ggplot(df, aes(x=sweet_wt)) + geom_histogram(binwidth=.15, fill="gray") + 
  geom_vline(aes(xintercept=NMI_sweet_BMI), color="black", linetype="solid", size=1) + theme_minimal(base_size = 15) + 
  labs(x = "Reward by BMI Groups", y= "Frequency")

ggplot(df, aes(x=bitter_wt)) + geom_histogram(binwidth=.15, fill="gray") + 
  geom_vline(aes(xintercept=NMI_bitter_BMI), color="black", linetype="solid", size=1) + theme_minimal(base_size = 15) + 
  labs(x = "Punishment by BMI Groups", y= "Frequency")

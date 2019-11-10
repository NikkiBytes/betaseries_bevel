library(readxl)
library(ggplot2)
library(psych)
library(dplyr)

#sink('/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/results/node_analysis_output.txt')
#combine DFs 
df1 <- read.csv("/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/results/network/node_centrality_punishment_weighted_median.csv")
df1$condition <- "punish"
df1$metric <- "centrality"

df2 <- read.csv("/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/results/network/node_clustering_punishment_weighted_median.csv")
df2$condition <- "punish"
df2$metric <- "clustering"

df3 <- read.csv("/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/results/network/node_pc_punishment_weighted_median.csv")
df3$condition <- "punish"
df3$metric <- "pc"

df4 <- read.csv("/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/results/network/node_centrality_reward_weighted_median.csv")
df4$condition <- "reward"
df4$metric <- "centrality"

df5 <- read.csv("/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/results/network/node_clustering_reward_weighted_median.csv")
df5$condition <- "reward"
df5$metric <- "clustering"

df6 <- read.csv("/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/results/network/node_pc_reward_weighted_median.csv")
df6$condition <- "reward"
df6$metric <- "pc"

temp <- bind_rows(df1,df2,df3,df4,df5,df6)
df<- temp[c("module0_mean", "module1_mean", "module2_mean", "module3_mean", "module4_mean", "module5_mean", "module6_mean", "condition", "metric")]

#remove negative values (these are not interpretable)
df[df < 0] <- NA
describe(df)

#Test For Differences in Clustering by Condition
df_clust <- df[which(df$metric == 'clustering'), ]
describeBy(df_clust, df_clust$condition)
t.test(module0_mean ~ condition, data = df_clust)
t.test(module1_mean ~ condition, data = df_clust)
t.test(module2_mean ~ condition, data = df_clust)
t.test(module3_mean ~ condition, data = df_clust)
t.test(module4_mean ~ condition, data = df_clust)
t.test(module5_mean ~ condition, data = df_clust)
t.test(module6_mean ~ condition, data = df_clust)


#Test For Differences in Participation Coefficient by Condition
df_pc <- df[which(df$metric == 'pc'), ]
describeBy(df_pc, df_pc$condition)
t.test(module0_mean ~ condition, data = df_pc)
t.test(module1_mean ~ condition, data = df_pc) #significantly different betwene reward and punishment
t.test(module2_mean ~ condition, data = df_pc)
t.test(module3_mean ~ condition, data = df_pc)
t.test(module4_mean ~ condition, data = df_pc)
t.test(module5_mean ~ condition, data = df_pc)
t.test(module6_mean ~ condition, data = df_pc)

#Test For Differences in Centrality by Condition
df_cent <- df[which(df$metric == 'centrality'), ]
describeBy(df_cent, df_cent$condition)
t.test(module0_mean ~ condition, data = df_cent)
t.test(module1_mean ~ condition, data = df_cent)
t.test(module2_mean ~ condition, data = df_cent)
t.test(module3_mean ~ condition, data = df_cent)
t.test(module4_mean ~ condition, data = df_cent)
t.test(module5_mean ~ condition, data = df_cent)
t.test(module6_mean ~ condition, data = df_cent)

##### MID ANALYSIS SUMMARY: 
# There are no differences in module clusting (segregation), participation coefficient (integration), 
#or betweenness centrality (hubness) between reward and punishment

##### Test for Associations with Individual Characteristics
#Test module clustering and centrality
#read-in the behavioral data
dat <- read.csv("/Users/jennygilbert/Google Drive/NIBL/Projects/18-0417 BeveL/data/paper_specific/betaseries/behavioral_data/betaseries_sample_november_update.csv", sep = ',')
print(dat$ID)


#Remove the blank lines added at the end (no clue why this happens)
names(dat)


################################################## NON SIGNIFICANT #################################################################################### 
##Relation to Posttest Performance 
#Reward
#Clustering
cor.test(dat$module0_clustering_reward, dat$sensitivity_reward)
cor.test(dat$module1_clustering_reward, dat$sensitivity_reward)
cor.test(dat$module2_clustering_reward, dat$sensitivity_reward)
cor.test(dat$module3_clustering_reward, dat$sensitivity_reward)
cor.test(dat$module4_clustering_reward, dat$sensitivity_reward)
cor.test(dat$module5_clustering_reward, dat$sensitivity_reward)
cor.test(dat$module6_clustering_reward, dat$sensitivity_reward)
cor.test(dat$module7_clustering_reward, dat$sensitivity_reward)

#Centrality
cor.test(dat$module0_centrality_reward, dat$sensitivity_reward)
cor.test(dat$module1_centrality_reward, dat$sensitivity_reward)
cor.test(dat$module2_centrality_reward, dat$sensitivity_reward)
cor.test(dat$module3_centrality_reward, dat$sensitivity_reward)
cor.test(dat$module4_centrality_reward, dat$sensitivity_reward)
cor.test(dat$module5_centrality_reward, dat$sensitivity_reward)
cor.test(dat$module6_centrality_reward, dat$sensitivity_reward)
cor.test(dat$module7_centrality_reward, dat$sensitivity_reward)

#PC
cor.test(dat$module0_pc_reward, dat$sensitivity_reward)
cor.test(dat$module1_pc_reward, dat$sensitivity_reward)
cor.test(dat$module2_pc_reward, dat$sensitivity_reward)
cor.test(dat$module3_pc_reward, dat$sensitivity_reward)
cor.test(dat$module4_pc_reward, dat$sensitivity_reward)
cor.test(dat$module5_pc_reward, dat$sensitivity_reward)
cor.test(dat$module6_pc_reward, dat$sensitivity_reward)
cor.test(dat$module7_pc_reward, dat$sensitivity_reward)

#Punishment
#Clustering
cor.test(dat$module0_clustering_punish, dat$sensitivity_punish)
cor.test(dat$module1_clustering_punish, dat$sensitivity_punish)
cor.test(dat$module2_clustering_punish, dat$sensitivity_punish)
cor.test(dat$module3_clustering_punish, dat$sensitivity_punish)
cor.test(dat$module4_clustering_punish, dat$sensitivity_punish)
cor.test(dat$module5_clustering_punish, dat$sensitivity_punish)
cor.test(dat$module6_clustering_punish, dat$sensitivity_punish)

#Centrality
cor.test(dat$module0_centrality_punish, dat$sensitivity_punish)
cor.test(dat$module1_centrality_punish, dat$sensitivity_punish)
cor.test(dat$module2_centrality_punish, dat$sensitivity_punish)
cor.test(dat$module3_centrality_punish, dat$sensitivity_punish)
cor.test(dat$module4_centrality_punish, dat$sensitivity_punish)
cor.test(dat$module5_centrality_punish, dat$sensitivity_punish)
cor.test(dat$module6_centrality_punish, dat$sensitivity_punish)

#PC
cor.test(dat$module0_pc_punish, dat$sensitivity_punish)
cor.test(dat$module1_pc_punish, dat$sensitivity_punish)
cor.test(dat$module2_pc_punish, dat$sensitivity_punish)
cor.test(dat$module3_pc_punish, dat$sensitivity_punish)
cor.test(dat$module4_pc_punish, dat$sensitivity_punish)
cor.test(dat$module5_pc_punish, dat$sensitivity_punish)
cor.test(dat$module6_pc_punish, dat$sensitivity_punish)

##### MID ANALYSIS SUMMARY: 
# There are no significant associations of module clusting (segregation), participation coefficient (integration), 
#or betweenness centrality (hubness) during reward and punishment with posttest performance. 

##Relation to the beverage selected
#Reward
#Clustering
res.aov <- aov(module0_clustering_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module1_clustering_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module2_clustering_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module3_clustering_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module4_clustering_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module5_clustering_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module6_clustering_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module7_clustering_reward ~ sweetstim_level, data = dat)
summary(res.aov)

#Centrality
res.aov <- aov(module0_centrality_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module1_centrality_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module2_centrality_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module3_centrality_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module4_centrality_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module5_centrality_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module6_centrality_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module7_centrality_reward ~ sweetstim_level, data = dat)
summary(res.aov)

#PC
res.aov <- aov(module0_pc_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module1_pc_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module2_pc_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module3_pc_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module4_pc_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module5_pc_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module6_pc_reward ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module7_pc_reward ~ sweetstim_level, data = dat)
summary(res.aov)

#punish
#Clustering
res.aov <- aov(module0_clustering_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module1_clustering_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module2_clustering_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module3_clustering_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module4_clustering_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module5_clustering_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module6_clustering_punish ~ bitterstim_level, data = dat)
summary(res.aov)

#Centrality
res.aov <- aov(module0_centrality_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module1_centrality_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module2_centrality_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module3_centrality_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module4_centrality_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module5_centrality_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module6_centrality_punish ~ bitterstim_level, data = dat)
summary(res.aov)

#PC
res.aov <- aov(module0_pc_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module1_pc_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module2_pc_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module3_pc_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module4_pc_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module5_pc_punish ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module6_pc_punish ~ bitterstim_level, data = dat)
summary(res.aov)

##Relation to BMI
#Reward
#Clustering
cor.test(dat$module0_clustering_reward, dat$BMI)
cor.test(dat$module1_clustering_reward, dat$BMI)
cor.test(dat$module2_clustering_reward, dat$BMI)
cor.test(dat$module3_clustering_reward, dat$BMI)
cor.test(dat$module4_clustering_reward, dat$BMI)
cor.test(dat$module5_clustering_reward, dat$BMI)
cor.test(dat$module6_clustering_reward, dat$BMI)
cor.test(dat$module7_clustering_reward, dat$BMI)

#Centrality
cor.test(dat$module0_centrality_reward, dat$BMI)
cor.test(dat$module1_centrality_reward, dat$BMI)
cor.test(dat$module2_centrality_reward, dat$BMI)
cor.test(dat$module3_centrality_reward, dat$BMI)
cor.test(dat$module4_centrality_reward, dat$BMI)
cor.test(dat$module5_centrality_reward, dat$BMI)
cor.test(dat$module6_centrality_reward, dat$BMI)
cor.test(dat$module7_centrality_reward, dat$BMI)

#PC
cor.test(dat$module0_pc_reward, dat$BMI)
cor.test(dat$module1_pc_reward, dat$BMI)
cor.test(dat$module2_pc_reward, dat$BMI)
cor.test(dat$module3_pc_reward, dat$BMI)
cor.test(dat$module4_pc_reward, dat$BMI)
cor.test(dat$module5_pc_reward, dat$BMI)
cor.test(dat$module6_pc_reward, dat$BMI)
cor.test(dat$module7_pc_reward, dat$BMI)

#Punishment
#Clustering
cor.test(dat$module0_clustering_punish, dat$BMI)
cor.test(dat$module1_clustering_punish, dat$BMI)
cor.test(dat$module2_clustering_punish, dat$BMI)
cor.test(dat$module3_clustering_punish, dat$BMI)
cor.test(dat$module4_clustering_punish, dat$BMI)
cor.test(dat$module5_clustering_punish, dat$BMI)
cor.test(dat$module6_clustering_punish, dat$BMI)

#Centrality
cor.test(dat$module0_centrality_punish, dat$BMI)
cor.test(dat$module1_centrality_punish, dat$BMI)
cor.test(dat$module2_centrality_punish, dat$BMI)
cor.test(dat$module3_centrality_punish, dat$BMI)
cor.test(dat$module4_centrality_punish, dat$BMI)
cor.test(dat$module5_centrality_punish, dat$BMI)
cor.test(dat$module6_centrality_punish, dat$BMI)

#PC
cor.test(dat$module0_pc_punish, dat$BMI)
cor.test(dat$module1_pc_punish, dat$BMI)
cor.test(dat$module2_pc_punish, dat$BMI)
cor.test(dat$module3_pc_punish, dat$BMI)
cor.test(dat$module4_pc_punish, dat$BMI)
cor.test(dat$module5_pc_punish, dat$BMI)
cor.test(dat$module6_pc_punish, dat$BMI)

##Relation to Liking
#Reward
#Clustering
cor.test(dat$module0_clustering_reward, dat$sweetstim_pleasent)
cor.test(dat$module1_clustering_reward, dat$sweetstim_pleasent)
cor.test(dat$module2_clustering_reward, dat$sweetstim_pleasent)
cor.test(dat$module3_clustering_reward, dat$sweetstim_pleasent)
cor.test(dat$module4_clustering_reward, dat$sweetstim_pleasent)
cor.test(dat$module5_clustering_reward, dat$sweetstim_pleasent)
cor.test(dat$module6_clustering_reward, dat$sweetstim_pleasent)
cor.test(dat$module7_clustering_reward, dat$sweetstim_pleasent)

#Centrality
cor.test(dat$module0_centrality_reward, dat$sweetstim_pleasent)
cor.test(dat$module1_centrality_reward, dat$sweetstim_pleasent)
cor.test(dat$module2_centrality_reward, dat$sweetstim_pleasent)
cor.test(dat$module3_centrality_reward, dat$sweetstim_pleasent)
cor.test(dat$module4_centrality_reward, dat$sweetstim_pleasent)
cor.test(dat$module5_centrality_reward, dat$sweetstim_pleasent)
cor.test(dat$module6_centrality_reward, dat$sweetstim_pleasent)
cor.test(dat$module7_centrality_reward, dat$sweetstim_pleasent)

#PC
cor.test(dat$module0_pc_reward, dat$sweetstim_pleasent)
cor.test(dat$module1_pc_reward, dat$sweetstim_pleasent)
cor.test(dat$module2_pc_reward, dat$sweetstim_pleasent)
cor.test(dat$module3_pc_reward, dat$sweetstim_pleasent)
cor.test(dat$module4_pc_reward, dat$sweetstim_pleasent)
cor.test(dat$module5_pc_reward, dat$sweetstim_pleasent)
cor.test(dat$module6_pc_reward, dat$sweetstim_pleasent)
cor.test(dat$module7_pc_reward, dat$sweetstim_pleasent)

##Relation to Desire
#Reward
#Clustering
cor.test(dat$module0_clustering_reward, dat$sweetstim_desire)
cor.test(dat$module1_clustering_reward, dat$sweetstim_desire)
cor.test(dat$module2_clustering_reward, dat$sweetstim_desire)
cor.test(dat$module3_clustering_reward, dat$sweetstim_desire)
cor.test(dat$module4_clustering_reward, dat$sweetstim_desire)
cor.test(dat$module5_clustering_reward, dat$sweetstim_desire)
cor.test(dat$module6_clustering_reward, dat$sweetstim_desire)
cor.test(dat$module7_clustering_reward, dat$sweetstim_desire)

#Centrality
cor.test(dat$module0_centrality_reward, dat$sweetstim_desire)
cor.test(dat$module1_centrality_reward, dat$sweetstim_desire)
cor.test(dat$module2_centrality_reward, dat$sweetstim_desire)
cor.test(dat$module3_centrality_reward, dat$sweetstim_desire)
cor.test(dat$module4_centrality_reward, dat$sweetstim_desire)
cor.test(dat$module5_centrality_reward, dat$sweetstim_desire)
cor.test(dat$module6_centrality_reward, dat$sweetstim_desire)
cor.test(dat$module7_centrality_reward, dat$sweetstim_desire)

#PC
cor.test(dat$module0_pc_reward, dat$sweetstim_desire)
cor.test(dat$module1_pc_reward, dat$sweetstim_desire)
cor.test(dat$module2_pc_reward, dat$sweetstim_desire)
cor.test(dat$module3_pc_reward, dat$sweetstim_desire)
cor.test(dat$module4_pc_reward, dat$sweetstim_desire)
cor.test(dat$module5_pc_reward, dat$sweetstim_desire)
cor.test(dat$module6_pc_reward, dat$sweetstim_desire)
cor.test(dat$module7_pc_reward, dat$sweetstim_desire)

##Relation to percent_matched_outcome
#Reward
#Clustering
cor.test(dat$module0_clustering_reward, dat$percent_matched_outcome)
cor.test(dat$module1_clustering_reward, dat$percent_matched_outcome)
cor.test(dat$module2_clustering_reward, dat$percent_matched_outcome)
cor.test(dat$module3_clustering_reward, dat$percent_matched_outcome)
cor.test(dat$module4_clustering_reward, dat$percent_matched_outcome)
cor.test(dat$module5_clustering_reward, dat$percent_matched_outcome)
cor.test(dat$module6_clustering_reward, dat$percent_matched_outcome)
cor.test(dat$module7_clustering_reward, dat$percent_matched_outcome)
#Centrality
cor.test(dat$module0_centrality_reward, dat$percent_matched_outcome)
cor.test(dat$module1_centrality_reward, dat$percent_matched_outcome)
cor.test(dat$module2_centrality_reward, dat$percent_matched_outcome)
cor.test(dat$module3_centrality_reward, dat$percent_matched_outcome)
cor.test(dat$module4_centrality_reward, dat$percent_matched_outcome)
cor.test(dat$module5_centrality_reward, dat$percent_matched_outcome)
cor.test(dat$module6_centrality_reward, dat$percent_matched_outcome)
cor.test(dat$module7_centrality_reward, dat$percent_matched_outcome)
#PC
cor.test(dat$module0_pc_reward, dat$percent_matched_outcome)
cor.test(dat$module1_pc_reward, dat$percent_matched_outcome)
cor.test(dat$module2_pc_reward, dat$percent_matched_outcome)
cor.test(dat$module3_pc_reward, dat$percent_matched_outcome)
cor.test(dat$module4_pc_reward, dat$percent_matched_outcome)
cor.test(dat$module5_pc_reward, dat$percent_matched_outcome)
cor.test(dat$module6_pc_reward, dat$percent_matched_outcome)
cor.test(dat$module7_pc_reward, dat$percent_matched_outcome)
#Punishment
#Clustering
cor.test(dat$module0_clustering_punish, dat$percent_matched_outcome)
cor.test(dat$module1_clustering_punish, dat$percent_matched_outcome)
cor.test(dat$module2_clustering_punish, dat$percent_matched_outcome)
cor.test(dat$module3_clustering_punish, dat$percent_matched_outcome)
cor.test(dat$module4_clustering_punish, dat$percent_matched_outcome)
cor.test(dat$module5_clustering_punish, dat$percent_matched_outcome)
cor.test(dat$module6_clustering_punish, dat$percent_matched_outcome)

#Centrality
cor.test(dat$module0_centrality_punish, dat$percent_matched_outcome)
cor.test(dat$module1_centrality_punish, dat$percent_matched_outcome)
cor.test(dat$module2_centrality_punish, dat$percent_matched_outcome)
cor.test(dat$module3_centrality_punish, dat$percent_matched_outcome)
cor.test(dat$module4_centrality_punish, dat$percent_matched_outcome)
cor.test(dat$module5_centrality_punish, dat$percent_matched_outcome)
cor.test(dat$module6_centrality_punish, dat$percent_matched_outcome)

#PC
cor.test(dat$module0_pc_punish, dat$percent_matched_outcome)
cor.test(dat$module1_pc_punish, dat$percent_matched_outcome)
cor.test(dat$module2_pc_punish, dat$percent_matched_outcome)
cor.test(dat$module3_pc_punish, dat$percent_matched_outcome)
cor.test(dat$module4_pc_punish, dat$percent_matched_outcome)
cor.test(dat$module5_pc_punish, dat$percent_matched_outcome)
cor.test(dat$module6_pc_punish, dat$percent_matched_outcome)

################################################## CHOICE #################################################################################### 
cor.test(dat$module0_clustering_choice, dat$percent_matched_outcome)
cor.test(dat$module1_clustering_choice, dat$percent_matched_outcome)
cor.test(dat$module2_clustering_choice, dat$percent_matched_outcome)
cor.test(dat$module3_clustering_choice, dat$percent_matched_outcome)
cor.test(dat$module4_clustering_choice, dat$percent_matched_outcome)
cor.test(dat$module5_clustering_choice, dat$percent_matched_outcome)
cor.test(dat$module6_clustering_choice, dat$percent_matched_outcome)
cor.test(dat$module7_clustering_choice, dat$percent_matched_outcome)

cor.test(dat$module0_centrality_choice, dat$percent_matched_outcome)
cor.test(dat$module1_centrality_choice, dat$percent_matched_outcome)
cor.test(dat$module2_centrality_choice, dat$percent_matched_outcome)
cor.test(dat$module3_centrality_choice, dat$percent_matched_outcome)
cor.test(dat$module4_centrality_choice, dat$percent_matched_outcome)
cor.test(dat$module5_centrality_choice, dat$percent_matched_outcome)
cor.test(dat$module6_centrality_choice, dat$percent_matched_outcome)
cor.test(dat$module7_centrality_choice, dat$percent_matched_outcome)

cor.test(dat$module0_pc_choice, dat$percent_matched_outcome)
cor.test(dat$module1_pc_choice, dat$percent_matched_outcome)
cor.test(dat$module2_pc_choice, dat$percent_matched_outcome)
cor.test(dat$module3_pc_choice, dat$percent_matched_outcome)
cor.test(dat$module4_pc_choice, dat$percent_matched_outcome)
cor.test(dat$module5_pc_choice, dat$percent_matched_outcome)
cor.test(dat$module6_pc_choice, dat$percent_matched_outcome)
cor.test(dat$module7_pc_choice, dat$percent_matched_outcome)

#Sensitivity to Reward
cor.test(dat$module0_clustering_choice, dat$sensitivity_reward)
cor.test(dat$module1_clustering_choice, dat$sensitivity_reward)
cor.test(dat$module2_clustering_choice, dat$sensitivity_reward)
cor.test(dat$module3_clustering_choice, dat$sensitivity_reward)
cor.test(dat$module4_clustering_choice, dat$sensitivity_reward)
cor.test(dat$module5_clustering_choice, dat$sensitivity_reward)
cor.test(dat$module6_clustering_choice, dat$sensitivity_reward)
cor.test(dat$module7_clustering_choice, dat$sensitivity_reward)

cor.test(dat$module0_centrality_choice, dat$sensitivity_reward)
cor.test(dat$module1_centrality_choice, dat$sensitivity_reward)
cor.test(dat$module2_centrality_choice, dat$sensitivity_reward)
cor.test(dat$module3_centrality_choice, dat$sensitivity_reward)
cor.test(dat$module4_centrality_choice, dat$sensitivity_reward)
cor.test(dat$module5_centrality_choice, dat$sensitivity_reward)
cor.test(dat$module6_centrality_choice, dat$sensitivity_reward)
cor.test(dat$module7_centrality_choice, dat$sensitivity_reward)

cor.test(dat$module0_pc_choice, dat$sensitivity_reward)
cor.test(dat$module1_pc_choice, dat$sensitivity_reward)
cor.test(dat$module2_pc_choice, dat$sensitivity_reward)
cor.test(dat$module3_pc_choice, dat$sensitivity_reward)
cor.test(dat$module4_pc_choice, dat$sensitivity_reward)
cor.test(dat$module5_pc_choice, dat$sensitivity_reward)
cor.test(dat$module6_pc_choice, dat$sensitivity_reward)
cor.test(dat$module7_pc_choice, dat$sensitivity_reward)

#Sensitivity to punishment
cor.test(dat$module0_clustering_choice, dat$sensitivity_punish) #*signifiant 
cor.test(dat$module1_clustering_choice, dat$sensitivity_punish)
cor.test(dat$module2_clustering_choice, dat$sensitivity_punish)
cor.test(dat$module3_clustering_choice, dat$sensitivity_punish)
cor.test(dat$module4_clustering_choice, dat$sensitivity_punish)
cor.test(dat$module5_clustering_choice, dat$sensitivity_punish)
cor.test(dat$module6_clustering_choice, dat$sensitivity_punish)
cor.test(dat$module7_clustering_choice, dat$sensitivity_punish)

cor.test(dat$module0_centrality_choice, dat$sensitivity_punish)
cor.test(dat$module1_centrality_choice, dat$sensitivity_punish)
cor.test(dat$module2_centrality_choice, dat$sensitivity_punish)
cor.test(dat$module3_centrality_choice, dat$sensitivity_punish)
cor.test(dat$module4_centrality_choice, dat$sensitivity_punish)
cor.test(dat$module5_centrality_choice, dat$sensitivity_punish)
cor.test(dat$module6_centrality_choice, dat$sensitivity_punish)
cor.test(dat$module7_centrality_choice, dat$sensitivity_punish)

cor.test(dat$module0_pc_choice, dat$sensitivity_punish)
cor.test(dat$module1_pc_choice, dat$sensitivity_punish)
cor.test(dat$module2_pc_choice, dat$sensitivity_punish)
cor.test(dat$module3_pc_choice, dat$sensitivity_punish)
cor.test(dat$module4_pc_choice, dat$sensitivity_punish)
cor.test(dat$module5_pc_choice, dat$sensitivity_punish)
cor.test(dat$module6_pc_choice, dat$sensitivity_punish)
cor.test(dat$module7_pc_choice, dat$sensitivity_punish)

#NBack Accuracy
cor.test(dat$module0_clustering_choice, dat$nback_accuracy)
cor.test(dat$module1_clustering_choice, dat$nback_accuracy)
cor.test(dat$module2_clustering_choice, dat$nback_accuracy)
cor.test(dat$module3_clustering_choice, dat$nback_accuracy)
cor.test(dat$module4_clustering_choice, dat$nback_accuracy)
cor.test(dat$module5_clustering_choice, dat$nback_accuracy)
cor.test(dat$module6_clustering_choice, dat$nback_accuracy)
cor.test(dat$module7_clustering_choice, dat$nback_accuracy)

cor.test(dat$module0_centrality_choice, dat$nback_accuracy)
cor.test(dat$module1_centrality_choice, dat$nback_accuracy)
cor.test(dat$module2_centrality_choice, dat$nback_accuracy)
cor.test(dat$module3_centrality_choice, dat$nback_accuracy)
cor.test(dat$module4_centrality_choice, dat$nback_accuracy)
cor.test(dat$module5_centrality_choice, dat$nback_accuracy)
cor.test(dat$module6_centrality_choice, dat$nback_accuracy)
cor.test(dat$module7_centrality_choice, dat$nback_accuracy)

cor.test(dat$module0_pc_choice, dat$nback_accuracy)
cor.test(dat$module1_pc_choice, dat$nback_accuracy)
cor.test(dat$module2_pc_choice, dat$nback_accuracy)
cor.test(dat$module3_pc_choice, dat$nback_accuracy)
cor.test(dat$module4_pc_choice, dat$nback_accuracy)
cor.test(dat$module5_pc_choice, dat$nback_accuracy)
cor.test(dat$module6_pc_choice, dat$nback_accuracy)
cor.test(dat$module7_pc_choice, dat$nback_accuracy)

#BMI
cor.test(dat$module0_clustering_choice, dat$BMI)
cor.test(dat$module1_clustering_choice, dat$BMI)
cor.test(dat$module2_clustering_choice, dat$BMI)
cor.test(dat$module3_clustering_choice, dat$BMI)
cor.test(dat$module4_clustering_choice, dat$BMI)
cor.test(dat$module5_clustering_choice, dat$BMI)
cor.test(dat$module6_clustering_choice, dat$BMI)
cor.test(dat$module7_clustering_choice, dat$BMI)

cor.test(dat$module0_centrality_choice, dat$BMI)
cor.test(dat$module1_centrality_choice, dat$BMI)
cor.test(dat$module2_centrality_choice, dat$BMI)
cor.test(dat$module3_centrality_choice, dat$BMI)
cor.test(dat$module4_centrality_choice, dat$BMI)
cor.test(dat$module5_centrality_choice, dat$BMI)
cor.test(dat$module6_centrality_choice, dat$BMI)
cor.test(dat$module7_centrality_choice, dat$BMI)

cor.test(dat$module0_pc_choice, dat$BMI)
cor.test(dat$module1_pc_choice, dat$BMI)
cor.test(dat$module2_pc_choice, dat$BMI)
cor.test(dat$module3_pc_choice, dat$BMI)
cor.test(dat$module4_pc_choice, dat$BMI)
cor.test(dat$module5_pc_choice, dat$BMI)
cor.test(dat$module6_pc_choice, dat$BMI)
cor.test(dat$module7_pc_choice, dat$BMI)


################################################## PLOTTING #################################################################################### 
plot(dat$module0_clustering_choice, dat$sensitivity_punish, col = "black", 
     xlab="Clustering Coefficient of Choice Module 1", ylab="Sensitivity to Punishment", xlim=c(.6, 1),
     cex.main=1.0, cex.lab=1.0, cex.axis=1.0)
abline(lm(dat$sensitivity_punish ~dat$module0_clustering_choice), col = "deeppink3")



### OLD
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
df2 <- data_summary(df_pc, varname="module1_mean", 
                    groupnames=c("condition"))

p <- ggplot(df2, aes(x=condition, y=module1_mean)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin= module1_mean -sd, ymax=module1_mean + sd), width=.2, position=position_dodge(.9))

p+labs(title="Module 2 Participation Coefficient in Reward and Punishment", x="Reinforcer", y = "Module 2 PC")+
  theme_classic()


df3 <- data_summary(dat, varname="module1_clustering_reward", 
                    groupnames=c("sweetstim_level"))

p<- ggplot(df3, aes(x=sweetstim_level, y=module1_clustering_reward)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=module1_clustering_reward-sd, ymax=module1_clustering_reward+sd), width=.2,
                position=position_dodge(.9)) 
p+labs(title="Module 2 Clustering Coefficient related to Sweetness", x="Reward Sweetness", y = "Clustering Coefficient")+
  theme_classic()


plot(dat$module0_clustering_choice, dat$sensitivity_punish, col = "black", 
     xlab="Clustering Coefficient of Choice Module 1", ylab="Sensitivity to Punishment", xlim=c(.6, 1),
     cex.main=1.0, cex.lab=1.0, cex.axis=1.0)
abline(lm(dat$sensitivity_punish ~dat$module0_clustering_choice), col = "deeppink3")
#abline(lm(dat$sweetstim_desire ~dat$module3_clustering_reward), col = "orange")
#abline(lm(dat$sweetstim_desire ~dat$module5_clustering_reward), col = "chartreuse3")
#abline(lm(dat$sweetstim_desire ~dat$module6_clustering_reward), col = "mediumorchid4")
#legend("topleft", inset=.05, title="Module",
#       c("3","4","6","7"), fill=c('deeppink3', "orange", "chartreuse3", "mediumorchid4"))



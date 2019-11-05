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
t.test(module1_mean ~ condition, data = df_pc)
t.test(module3_mean ~ condition, data = df_pc)
t.test(module4_mean ~ condition, data = df_pc)
t.test(module5_mean ~ condition, data = df_pc)
t.test(module6_mean ~ condition, data = df_pc)

#Test For Differences in Centrality by Condition
df_cent <- df[which(df$metric == 'centrality'), ]
describeBy(df_cent, df_cent$condition)
t.test(module0_mean ~ condition, data = df_cent)
t.test(module1_mean ~ condition, data = df_cent)
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

#Centrality
cor.test(dat$module0_centrality_reward, dat$sensitivity_reward)
cor.test(dat$module1_centrality_reward, dat$sensitivity_reward)
cor.test(dat$module2_centrality_reward, dat$sensitivity_reward)
cor.test(dat$module3_centrality_reward, dat$sensitivity_reward)
cor.test(dat$module4_centrality_reward, dat$sensitivity_reward)
cor.test(dat$module5_centrality_reward, dat$sensitivity_reward)
cor.test(dat$module6_centrality_reward, dat$sensitivity_reward)

#PC
cor.test(dat$module0_pc_reward, dat$sensitivity_reward)
cor.test(dat$module1_pc_reward, dat$sensitivity_reward)
cor.test(dat$module2_pc_reward, dat$sensitivity_reward)
cor.test(dat$module3_pc_reward, dat$sensitivity_reward)
cor.test(dat$module4_pc_reward, dat$sensitivity_reward)
cor.test(dat$module5_pc_reward, dat$sensitivity_reward)
cor.test(dat$module6_pc_reward, dat$sensitivity_reward)

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
describeBy(dat$module1_clustering_reward, dat$sweetstim_level)

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

#Centrality
cor.test(dat$module0_centrality_reward, dat$BMI)
cor.test(dat$module1_centrality_reward, dat$BMI)
cor.test(dat$module2_centrality_reward, dat$BMI)
cor.test(dat$module3_centrality_reward, dat$BMI)
cor.test(dat$module4_centrality_reward, dat$BMI)
cor.test(dat$module5_centrality_reward, dat$BMI)
cor.test(dat$module6_centrality_reward, dat$BMI)

#PC
cor.test(dat$module0_pc_reward, dat$BMI)
cor.test(dat$module1_pc_reward, dat$BMI)
cor.test(dat$module2_pc_reward, dat$BMI)
cor.test(dat$module3_pc_reward, dat$BMI)
cor.test(dat$module4_pc_reward, dat$BMI)
cor.test(dat$module5_pc_reward, dat$BMI)
cor.test(dat$module6_pc_reward, dat$BMI)

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

#Centrality
cor.test(dat$module0_centrality_reward, dat$sweetstim_pleasent)
cor.test(dat$module1_centrality_reward, dat$sweetstim_pleasent)
cor.test(dat$module2_centrality_reward, dat$sweetstim_pleasent)
cor.test(dat$module3_centrality_reward, dat$sweetstim_pleasent)
cor.test(dat$module4_centrality_reward, dat$sweetstim_pleasent)
cor.test(dat$module5_centrality_reward, dat$sweetstim_pleasent)
cor.test(dat$module6_centrality_reward, dat$sweetstim_pleasent)

#PC
cor.test(dat$module0_pc_reward, dat$sweetstim_pleasent)
cor.test(dat$module1_pc_reward, dat$sweetstim_pleasent)
cor.test(dat$module2_pc_reward, dat$sweetstim_pleasent)
cor.test(dat$module3_pc_reward, dat$sweetstim_pleasent)
cor.test(dat$module4_pc_reward, dat$sweetstim_pleasent)
cor.test(dat$module5_pc_reward, dat$sweetstim_pleasent)
cor.test(dat$module6_pc_reward, dat$sweetstim_pleasent)

##Relation to Liking
#Reward
#Clustering
cor.test(dat$module0_clustering_reward, dat$sweetstim_desire)
cor.test(dat$module1_clustering_reward, dat$sweetstim_desire)
cor.test(dat$module2_clustering_reward, dat$sweetstim_desire)
cor.test(dat$module3_clustering_reward, dat$sweetstim_desire)
cor.test(dat$module4_clustering_reward, dat$sweetstim_desire)
cor.test(dat$module5_clustering_reward, dat$sweetstim_desire)
cor.test(dat$module6_clustering_reward, dat$sweetstim_desire)

#Centrality
cor.test(dat$module0_centrality_reward, dat$sweetstim_desire)
cor.test(dat$module1_centrality_reward, dat$sweetstim_desire)
cor.test(dat$module2_centrality_reward, dat$sweetstim_desire)
cor.test(dat$module3_centrality_reward, dat$sweetstim_desire)
cor.test(dat$module4_centrality_reward, dat$sweetstim_desire)
cor.test(dat$module5_centrality_reward, dat$sweetstim_desire)
cor.test(dat$module6_centrality_reward, dat$sweetstim_desire)

#PC
cor.test(dat$module0_pc_reward, dat$sweetstim_desire)
cor.test(dat$module1_pc_reward, dat$sweetstim_desire)
cor.test(dat$module2_pc_reward, dat$sweetstim_desire)
cor.test(dat$module3_pc_reward, dat$sweetstim_desire)
cor.test(dat$module4_pc_reward, dat$sweetstim_desire)
cor.test(dat$module5_pc_reward, dat$sweetstim_desire)
cor.test(dat$module6_pc_reward, dat$sweetstim_desire)

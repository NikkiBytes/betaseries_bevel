library(readxl)
library(ggplot2)
library(psych)

#sink('/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/results/node_analysis_output.txt')

df <- read.csv("/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/results/network/node_metrics_by_sub.csv")
names(df)

describe(df)

df_long <- read.csv("/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/results/network/node_metrics_by_sub_long.csv")
names(df_long)

#Select Clustering
df_clust <- df_long[which(df_long$metric == 'clustering'), ]
describeBy(df_clust, df_clust$condition)
t.test(module1 ~ condition, data = df_clust)
t.test(module2 ~ condition, data = df_clust)
t.test(module3 ~ condition, data = df_clust)
t.test(module4 ~ condition, data = df_clust)

t.test(Amygdala_L ~ condition, data = df_clust)
t.test(Amygdala_R ~ condition, data = df_clust)
t.test(Dorsal_striatum_L ~ condition, data = df_clust)
t.test(Dorsal_striatum_R ~ condition, data = df_clust)
t.test(Fusiform_gyrus_L ~ condition, data = df_clust)
t.test(Fusiform_gyrus_R ~ condition, data = df_clust)
t.test(Hippocampus_L ~ condition, data = df_clust)
t.test(Hippocampus_R ~ condition, data = df_clust)
t.test(Insula_L ~ condition, data = df_clust)
t.test(Insula_R ~ condition, data = df_clust)
t.test(Intracalcarine_cortex_L ~ condition, data = df_clust)
t.test(Intracalcarine_cortex_R ~ condition, data = df_clust)
t.test(lOFC_L ~ condition, data = df_clust)
t.test(lOFC_R ~ condition, data = df_clust)
t.test(mOFC_L ~ condition, data = df_clust)
t.test(mOFC_R ~ condition, data = df_clust)
t.test(Oral_somatosensory_cortex_R ~ condition, data = df_clust)
t.test(Oral_somatosensory_cortex_L ~ condition, data = df_clust)
t.test(Precuneus_L ~ condition, data = df_clust)
t.test(Precuneus_R ~ condition, data = df_clust)
t.test(Ventral_striatum_L ~ condition, data = df_clust)
t.test(Ventral_striatum_R ~ condition, data = df_clust)
t.test(vlPFC_L ~ condition, data = df_clust)
t.test(vlPFC_R ~ condition, data = df_clust)
t.test(vlThalamus_L ~ condition, data = df_clust)
t.test(vlThalamus_R ~ condition, data = df_clust)
t.test(vmPFC_L ~ condition, data = df_clust)
t.test(vmPFC_R ~ condition, data = df_clust)

#Select Centrality
df_cent <- df_long[which(df_long$metric == 'centrality'), ]
describeBy(df_cent, df_cent$condition)
t.test(module1 ~ condition, data = df_cent)
t.test(module2 ~ condition, data = df_cent)
t.test(module3 ~ condition, data = df_cent)
t.test(module4 ~ condition, data = df_cent)
#Test by node
t.test(Amygdala_L ~ condition, data = df_cent)
t.test(Amygdala_L ~ condition, data = df_cent)
t.test(Amygdala_R ~ condition, data = df_cent)
t.test(Dorsal_striatum_L ~ condition, data = df_cent)
t.test(Dorsal_striatum_R ~ condition, data = df_cent)
t.test(Fusiform_gyrus_L ~ condition, data = df_cent)
t.test(Fusiform_gyrus_R ~ condition, data = df_cent)
t.test(Hippocampus_L ~ condition, data = df_cent)
t.test(Hippocampus_R ~ condition, data = df_cent)
t.test(Insula_L ~ condition, data = df_cent)
t.test(Insula_R ~ condition, data = df_cent)
t.test(Intracalcarine_cortex_L ~ condition, data = df_cent)
t.test(Intracalcarine_cortex_R ~ condition, data = df_cent)
t.test(lOFC_L ~ condition, data = df_cent)
t.test(lOFC_R ~ condition, data = df_cent)
t.test(mOFC_L ~ condition, data = df_cent)
t.test(mOFC_R ~ condition, data = df_cent)
t.test(Oral_somatosensory_cortex_R ~ condition, data = df_cent)
t.test(Oral_somatosensory_cortex_L ~ condition, data = df_cent)
t.test(Precuneus_L ~ condition, data = df_cent)
t.test(Precuneus_R ~ condition, data = df_cent)
t.test(Ventral_striatum_L ~ condition, data = df_cent)
t.test(Ventral_striatum_R ~ condition, data = df_cent)
t.test(vlPFC_L ~ condition, data = df_cent)
t.test(vlPFC_R ~ condition, data = df_cent)
t.test(vlThalamus_L ~ condition, data = df_cent)
t.test(vlThalamus_R ~ condition, data = df_cent)
t.test(vmPFC_L ~ condition, data = df_cent)
t.test(vmPFC_R ~ condition, data = df_cent)
#sink()



#Test module clustering and centrality
#read-in the behavioral data
df2 <- read.csv("/Users/jennygilbert/Google Drive/NIBL/Projects/18-0417 BeveL/data/betaseries/behavioral_data/betaseries_sample.csv", sep = ',')
print(df2$ID)

#Remove the blank lines added at the end (no clue why this happens)
dat <- df2[-c(86,87), ]
print(dat$ID)
names(dat)
## Test for significant correlation between module metrics and behavioral data
#No meaningful relationship with pleasantness for sweet 
cor.test(dat$module1_reward_clustering, dat$sweetstim_pleasent)
cor.test(dat$module2_reward_clustering, dat$sweetstim_pleasent)
cor.test(dat$module3_reward_clustering, dat$sweetstim_pleasent)
cor.test(dat$module4_reward_clustering, dat$sweetstim_pleasent)

cor.test(dat$module1_reward_centrality, dat$sweetstim_pleasent)
cor.test(dat$module2_reward_centrality, dat$sweetstim_pleasent)
cor.test(dat$module3_reward_centrality, dat$sweetstim_pleasent)
cor.test(dat$module4_reward_centrality, dat$sweetstim_pleasent)

#No relation to pleasentness for bitter
cor.test(dat$module1_punishment_clustering, dat$bitterstim_pleasent)
cor.test(dat$module2_punishment_clustering, dat$bitterstim_pleasent)
cor.test(dat$module3_punishment_clustering, dat$bitterstim_pleasent)
cor.test(dat$module4_punishment_clustering, dat$bitterstim_pleasent)

cor.test(dat$module1_punishment_centrality, dat$bitterstim_pleasent)
cor.test(dat$module2_punishment_centrality, dat$bitterstim_pleasent)
cor.test(dat$module3_punishment_centrality, dat$bitterstim_pleasent)
cor.test(dat$module4_punishment_centrality, dat$bitterstim_pleasent)

#No meaningful relationship with desire for sweet 
cor.test(dat$module1_reward_clustering, dat$sweetstim_desire)
cor.test(dat$module2_reward_clustering, dat$sweetstim_desire)
cor.test(dat$module3_reward_clustering, dat$sweetstim_desire)
cor.test(dat$module4_reward_clustering, dat$sweetstim_desire)

cor.test(dat$module1_reward_centrality, dat$sweetstim_desire)
cor.test(dat$module2_reward_centrality, dat$sweetstim_desire)
cor.test(dat$module3_reward_centrality, dat$sweetstim_desire)
cor.test(dat$module4_reward_centrality, dat$sweetstim_desire)

#No relation to desire for bitter
cor.test(dat$module1_punishment_clustering, dat$bitterstim_desire)
cor.test(dat$module2_punishment_clustering, dat$bitterstim_desire)
cor.test(dat$module3_punishment_clustering, dat$bitterstim_desire)
cor.test(dat$module4_punishment_clustering, dat$bitterstim_desire)

cor.test(dat$module1_punishment_centrality, dat$bitterstim_desire)
cor.test(dat$module2_punishment_centrality, dat$bitterstim_desire)
cor.test(dat$module3_punishment_centrality, dat$bitterstim_desire)
cor.test(dat$module4_punishment_centrality, dat$bitterstim_desire)

#Module parameters relates to Sweet Stim Level
res.aov <- aov(module1_reward_clustering ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module2_reward_clustering ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module3_reward_clustering ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module4_reward_clustering ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module1_reward_centrality ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module2_reward_centrality ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module3_reward_centrality ~ sweetstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module4_reward_centrality ~ sweetstim_level, data = dat)
summary(res.aov)

# Doesnt matter for the punishment 
res.aov <- aov(module1_punishment_clustering ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module2_punishment_clustering ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module3_punishment_clustering ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module4_punishment_clustering ~ bitterstim_level, data = dat)
summary(res.aov)

res.aov <- aov(module1_punishment_centrality ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module2_punishment_centrality ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module3_punishment_centrality ~ bitterstim_level, data = dat)
summary(res.aov)
res.aov <- aov(module4_punishment_centrality ~ bitterstim_level, data = dat)
summary(res.aov)

names(dat)

#Posttest Accuracy
cor.test(dat$module1_reward_clustering, dat$sensitivity_reward)
cor.test(dat$module2_reward_clustering, dat$sensitivity_reward)
cor.test(dat$module3_reward_clustering, dat$sensitivity_reward)
cor.test(dat$module4_reward_clustering, dat$sensitivity_reward)
cor.test(dat$module1_reward_centrality, dat$sensitivity_reward)
cor.test(dat$module2_reward_centrality, dat$sensitivity_reward)
cor.test(dat$module3_reward_centrality, dat$sensitivity_reward)
cor.test(dat$module4_reward_centrality, dat$sensitivity_reward)

#Punishment parameters are highly associated with sensitivity to punishment
cor.test(dat$module1_punishment_clustering, dat$sensitivity_punish)
cor.test(dat$module2_punishment_clustering, dat$sensitivity_punish)
cor.test(dat$module3_punishment_clustering, dat$sensitivity_punish)
cor.test(dat$module4_punishment_clustering, dat$sensitivity_punish)
cor.test(dat$module1_punishment_centrality, dat$sensitivity_punish)
cor.test(dat$module2_punishment_centrality, dat$sensitivity_punish)
cor.test(dat$module3_punishment_centrality, dat$sensitivity_punish)
cor.test(dat$module4_punishment_centrality, dat$sensitivity_punish)

describe(dat$sensitivity_punish)

#Plot
plot(dat$module1_punishment_clustering, dat$sensitivity_punish, col = "deeppink3", 
     xlab="Clustering Coefficient", ylab="% Sensitivity to Punishment", 
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5) 
points(dat$module2_punishment_clustering, dat$sensitivity_punish, col = "orange")
points(dat$module3_punishment_clustering, dat$sensitivity_punish, col = "chartreuse3")
points(dat$module4_punishment_clustering, dat$sensitivity_punish, col = "mediumorchid4")
legend("topright", inset=.05, title="Module",
       c("1","2","3","4"), fill=c('deeppink3', "orange", "chartreuse3", "mediumorchid4"))

abline(lm(dat$sensitivity_punish ~dat$module1_punishment_clustering), col = "deeppink3")
abline(lm(dat$sensitivity_punish ~dat$module2_punishment_clustering), col = "orange")
abline(lm(dat$sensitivity_punish ~dat$module3_punishment_clustering), col = "chartreuse3")
abline(lm(dat$sensitivity_punish ~dat$module4_punishment_clustering), col = "mediumorchid4")

plot(dat$module1_punishment_centrality, dat$sensitivity_punish, col = "deeppink3", 
     xlab="Betweenness Centrality", ylab="% Sensitivity to Punishment", xlim=c(0, 0.1),
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
points(dat$module2_punishment_centrality, dat$sensitivity_punish, col= "orange")
points(dat$module3_punishment_centrality, dat$sensitivity_punish, col= "chartreuse3")
points(dat$module4_punishment_centrality, dat$sensitivity_punish, col="mediumorchid4")
legend("topright", inset=.05, title="Module",
       c("1","2","3","4"), fill=c('deeppink3', "orange", "chartreuse3", "mediumorchid4"))

abline(lm(dat$sensitivity_punish ~dat$module1_punishment_centrality), col = "deeppink3")
abline(lm(dat$sensitivity_punish ~dat$module2_punishment_centrality), col = "orange")
abline(lm(dat$sensitivity_punish ~dat$module3_punishment_centrality), col = "chartreuse3")
abline(lm(dat$sensitivity_punish ~dat$module4_punishment_centrality), col = "mediumorchid4")


### SWEETNESS
df_sw <- read.csv("/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/results/network/node_metrics_rew_long_sweet.csv")
names(df_sw)
df_sw$sweet_level <- as.factor(df_sw$sweet_level)
df_sw$module <- as.factor(df_sw$module)

p<-ggplot(df_sw, aes(x=module, y=clustering, color=sweet_level)) +
  geom_boxplot()
p + geom_jitter(shape=16, position=position_jitter(0.2))

ggplot(df_sw, aes(x=module, y=clustering, fill=sweet_level)) + geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center',
                                                                                             position=position_dodge(1), dotsize=0.2) +
  theme_minimal() + scale_fill_manual(values=c("grey95", "grey75", "grey55","grey35"))

ggplot(df_sw, aes(x=module, y=centrality, fill=sweet_level)) + geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center',
                                                                                             position=position_dodge(1), dotsize=0.2) +
  theme_minimal() + scale_fill_manual(values=c("grey95", "grey75", "grey55","grey35"))


#Does it work for the flip? 
cor.test(dat$module1_punishment_clustering, dat$sensitivity_reward)
cor.test(dat$module2_punishment_clustering, dat$sensitivity_reward)
cor.test(dat$module3_punishment_clustering, dat$sensitivity_reward)
cor.test(dat$module4_punishment_clustering, dat$sensitivity_reward)
cor.test(dat$module1_punishment_centrality, dat$sensitivity_reward)
cor.test(dat$module2_punishment_centrality, dat$sensitivity_reward)
cor.test(dat$module3_punishment_centrality, dat$sensitivity_reward)
cor.test(dat$module4_punishment_centrality, dat$sensitivity_reward)

#Module 3 during reward may significantly associate with sensitivity to punishment.... 
cor.test(dat$module1_reward_clustering, dat$sensitivity_punish)
cor.test(dat$module2_reward_clustering, dat$sensitivity_punish)
cor.test(dat$module3_reward_clustering, dat$sensitivity_punish)
cor.test(dat$module4_reward_clustering, dat$sensitivity_punish)
cor.test(dat$module1_reward_centrality, dat$sensitivity_punish)
cor.test(dat$module2_reward_centrality, dat$sensitivity_punish)
cor.test(dat$module3_reward_centrality, dat$sensitivity_punish)
cor.test(dat$module4_reward_centrality, dat$sensitivity_punish)


#BMI
cor.test(dat$module1_punishment_clustering, dat$BMI)
cor.test(dat$module2_punishment_clustering, dat$BMI)
cor.test(dat$module3_punishment_clustering, dat$BMI)
cor.test(dat$module4_punishment_clustering, dat$BMI)
cor.test(dat$module1_punishment_centrality, dat$BMI)
cor.test(dat$module2_punishment_centrality, dat$BMI)
cor.test(dat$module3_punishment_centrality, dat$BMI)
cor.test(dat$module4_punishment_centrality, dat$BMI)

cor.test(dat$module1_reward_clustering, dat$BMI)
cor.test(dat$module2_reward_clustering, dat$BMI)
cor.test(dat$module3_reward_clustering, dat$BMI)
cor.test(dat$module4_reward_clustering, dat$BMI)
cor.test(dat$module1_reward_centrality, dat$BMI)
cor.test(dat$module2_reward_centrality, dat$BMI)
cor.test(dat$module3_reward_centrality, dat$BMI)
cor.test(dat$module4_reward_centrality, dat$BMI)

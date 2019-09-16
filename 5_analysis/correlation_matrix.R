library(Hmisc)
library(corrplot)

setwd('/Users/jennygilbert/Documents/betaseries_bevel/4_combine_timeseries/sync_9_16/reward/')

mydata = read.csv("sub-003_reward.txt", header = FALSE, sep = '\t')
names(mydata) <- c("Amygdala_L", "Amygdala_R", "Dorsal_striatum_L", "Dorsal_striatum_R", "Fusiform_gyrus_L", "Fusiform_gyrus_R", "Hippocampus_L", "Hippocampus_R", "Insula_L",
                 "Insula_R", "Intracalcarine_cortex_L", "Intracalcarine_cortex_R", "lOFC_R", "mOFC_L", "mOFC_R", "Oral_somatosensory_cortex_L", "Oral_somatosensory_cortex_R", "Precuneus_L", 
                 "Precuneus_R", "Ventral_striatum_L", "Ventral_striatum_R", "vlPFC_L", "vlPFC_R", "vlThalamus_L" , "vlThalamus_R", "vmPFC_L", "vmPFC_R")

mydata.rcorr = rcorr(as.matrix(mydata))
print(mydata.rcorr)

mydata.coeff = mydata.rcorr$r
print(mydata.coeff)
write.table(mydata.coeff, file = "sub-003_reward_corrcoeff.csv", append = FALSE, sep = ",", dec = ".",
            row.names = TRUE, col.names = TRUE)

mydata.p = mydata.rcorr$P
print(mydata.p) 
write.table(mydata.p, file = "sub-003_reward_pvalue.csv", append = FALSE, sep = ",", dec = ".",
            row.names = TRUE, col.names = TRUE)

pdf("sub-003_reward_rplot.pdf")
corrplot(mydata.rcorr$r, method = "color", p.mat = mydata.rcorr$P, sig.level = 0.001, insig = "label_sig")
dev.off() 

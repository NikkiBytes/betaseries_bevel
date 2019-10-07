library(psych)
library(corrplot)
library(RColorBrewer)

ph.data<-read.csv("/Users/jennygilbert/Documents/betaseries_bevel/punish_coor_mat.csv", header=TRUE, 
                  row.names = c('Amygdala (L)',	'Amygdala (R)',	
                                'Dorsal Striatum (L)', 'Dorsal Striatum (R)',	
                                'Fusiform Gyrus (L)', 'Fusiform_gyrus (R)',	
                                'Hippocampus (L)', 'Hippocampus (R)',	
                                'Insula (L)','Insula (R)',
                                'Intracalcarine_cortex (L)','Intracalcarine_cortex (R)',
                                'lOFC (L)','lOFC (R)',
                                'mOFC (L)','mOFC (R)',
                                'Oral_somatosensory_cortex (R)','Oral_somatosensory_cortex_L',
                                'Precuneus (L)','Precuneus_R',
                                'Ventral_striatum (L)','Ventral_striatum_R',
                                'vlPFC (L)','vlPFC_R',
                                'vlThalamus_L','vlThalamus_R',
                                'vmPFC_L','vmPFC_R')) 
rwd.data<-read.csv("/Users/jennygilbert/Documents/betaseries_bevel/reward_coor_mat.csv", header=TRUE, 
                   row.names = c('Amygdala_L',	'Amygdala_R',	
                                 'Dorsal_striatum_L', 'Dorsal_striatum_R',	
                                 'Fusiform_gyrus_L', 'Fusiform_gyrus_R',	
                                 'Hippocampus_L', 'Hippocampus_R',	
                                 'Insula_L','Insula_R',
                                 'Intracalcarine_cortex_L','Intracalcarine_cortex_R',
                                 'lOFC_L','lOFC_R',
                                 'mOFC_L','mOFC_R',
                                 'Oral_somatosensory_cortex_R','Oral_somatosensory_cortex_L',
                                 'Precuneus_L','Precuneus_R',
                                 'Ventral_striatum_L','Ventral_striatum_R',
                                 'vlPFC_L','vlPFC_R',
                                 'vlThalamus_L','vlThalamus_R',
                                 'vmPFC_L','vmPFC_R'))

ph.M<-as.matrix(ph.data)
rwd.M<-as.matrix(rwd.data)


#corrplot(rwd.M,  tl.col="black", tl.srt=45,col=brewer.pal(n=8, name="PuOr"))
#corrplot(rwd.M, col=c("black", "white"), bg="lightblue", tl.col="black", tl.srt=45)

#corrplot(ph.M,  tl.col="black", tl.srt=45, col=brewer.pal(n=8, name="PuOr"))
#corrplot(ph.M, col=c("black", "white"), bg="lightblue",  tl.col="black", tl.srt=45)


pooled.corr <- corr.test(rwd.M, ph.M)

print(corr.p(pooled.corr$r, n=85),short=FALSE)
corrplot(pooled.corr$r, 
         sig.level = 0.01, insig="blank", tl.col="black",tl.cex = .5, tl.srt=45)
pooled.corr$ci

both <- lowerUpper(ph.M, rwd.M, diff=TRUE)
#pooled.lowUp
corPlot(both,numbers=TRUE,
        main="Differences between punishment and reward",
        stars = TRUE, 0.5)


#Plot difference in correlations, only showing significant
corrplot(pooled.corr$r, method="color", 
         tl.col="black", tl.cex = .7, tl.srt=45, diag=FALSE)
         #p.mat = pooled.corr$p, sig.level = 0.05, insig = "blank")

#Save out
p<- pooled.corr$p
p_corrected <- p.adjust(p, method = "fdr", n = length(p))
output <- matrix(unlist(p_corrected), ncol = 28, byrow = TRUE)
t<- pooled.corr$t
r <-pooled.corr$r


write.csv(t, file = "/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/results/FC/tstat.csv")
write.csv(p, file = "/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/results/FC/pvalue.csv")
write.csv(t, file = "/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/results/FC/comp_corr.csv")
write.csv(output, file = "/Users/jennygilbert/Documents/betaseries_bevel/5_analysis/results/FC/pvalue_fdr.csv")

library(Hmisc)
library(corrplot)

setwd('/Users/jennygilbert/Documents/betaseries_bevel/4_combine_timeseries/punishment/')

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

#This function makes the corr.csv, p.csv, and rplot of the correlation matrix for each participant
corr_func <- function(name){
  data <- read.delim(name, header = FALSE, sep = "\t")
  names(data) <- c("Amygdala_L", "Amygdala_R", "Dorsal_striatum_L", "Dorsal_striatum_R", "Fusiform_gyrus_L", "Fusiform_gyrus_R", "Hippocampus_L", "Hippocampus_R", "Insula_L",
                     "Insula_R", "Intracalcarine_cortex_L", "Intracalcarine_cortex_R", "lOFC_R", "mOFC_L", "mOFC_R", "Oral_somatosensory_cortex_L", "Oral_somatosensory_cortex_R", "Precuneus_L", 
                     "Precuneus_R", "Ventral_striatum_L", "Ventral_striatum_R", "vlPFC_L", "vlPFC_R", "vlThalamus_L" , "vlThalamus_R", "vmPFC_L", "vmPFC_R")
  data.rcorr = rcorr(as.matrix(data))
  data.coeff = data.rcorr$r
  filename = paste(name, "_corr.csv")
  write.table(data.coeff, file = filename, append = FALSE, sep = ",", dec = ".",
              row.names = TRUE, col.names = TRUE)
  
  data.p = data.rcorr$P
  filenamep = paste(name, "_p.csv")
  write.table(data.p, file = filenamep, append = FALSE, sep = ",", dec = ".",
              row.names = TRUE, col.names = TRUE)
  
  pdfname = paste(name, "_corrplot.pdf")
  pdf(pdfname)
  corrplot(data.rcorr$r, method = "color", p.mat = data.rcorr$P, sig.level = 0.001, insig = "label_sig")
  dev.off() 
}

corr_func('sub-031_reward.txt')

#make a list of files
mypath <- '/Users/jennygilbert/Documents/betaseries_bevel/4_combine_timeseries/punishment/'
filelist <- list.files(path = mypath, pattern = "*_punish.txt")
print(filelist)

#Apply the corr_func to all files
lapply(filelist, corr_func)


# Now we want to make an average correlation matrix across all subjects
# Function to read in all csv correlation files
filenames <- list.files(path = "/Users/jennygilbert/Documents/betaseries_bevel/4_combine_timeseries/punishment/",
                        pattern="*_corr.csv")

##Create list of data frame names without the ".csv" part 
names <-substring(filenames, 1, 19)

###Load all files
for(i in names){
  filepath <- file.path("/Users/jennygilbert/Documents/betaseries_bevel/4_combine_timeseries/punishment/",paste(i,".csv",sep=""))
  assign(i, read.delim(filepath, header = FALSE, sep = ","))
}


outmat <-apply(simplify2array(mylist), c(1,2), mean)

#BeveL Data Manipulation for betaseries
#JRS

library(tidyselect)
setwd("/Users/jennygilbert/Documents/betaseries_bevel/1_level_1/onsets/choice_onsets")

######## TEST ################
#Read data into R.
data <- read.delim('bevel30_run03_rinse.txt', header = FALSE, sep = "\t", dec = ".")
print(data)

'bevel30_run03.txt'
name='bevel30_run03' 

######## TEST LOOP ################
splitfunc <- function(name){
  data <- read.delim(name, header = FALSE, sep = "\t", dec = ".")
  
  for (i in 1:nrow(data)) {
    x <- as.data.frame(data[i,], drop=false)
    y <- as.data.frame(data[-i,], drop=false)
    trialevfilename <- file.path('/Users/jennygilbert/Documents/betaseries_bevel/1_level_1/onsets_reformatted/choice/', paste0(name, "_", "trial",i,".txt"))
    nuisevfilename <- file.path('/Users/jennygilbert/Documents/betaseries_bevel/1_level_1/onsets_reformatted/choice/', paste0(name, "_", "nuis",i,".txt"))
    write.table(x, file = trialevfilename, sep = "\t", row.names = FALSE, col.names = FALSE)
    write.table(y, file = nuisevfilename, sep = "\t", row.names = FALSE, col.names = FALSE)
  }
}

splitfunc('bevel30_run03_rinse.txt')

######## TEST GETTING FILENAMES ################
mypath <- '/Users/jennygilbert/Documents/betaseries_bevel/1_level_1/onsets/choice_onsets/'
filelist <- list.files(mypath, all.files = TRUE)
print(filelist)
#subrun <- file_path_sans_ext(basename(filelist))

lapply(filelist[4:358], splitfunc)

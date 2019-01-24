#BeveL Data Manipulation for betaseries
#JRS

library(tidyselect)
setwd("/Users/jennygilbert/Documents/betaseries_bevel/onsets/")

######## TEST ################
#Read data into R.
data <- read.delim('bevel30_run03.txt', header = FALSE, sep = "\t", dec = ".")
view(data)

'bevel30_run03.txt'
paste0(subrun, "_", "trial",i,".txt")


######## TEST LOOP ################
splitfunc <- function(name){
  data <- read.delim(name, header = FALSE, sep = "\t", dec = ".")
  
  for (i in 1:nrow(data)) {
    a <- as.data.frame(data[i,], drop=false)
    b <- as.data.frame(data[-i,], drop=false)
    trialevfilename <- file.path('/Users/jennygilbert/Documents/betaseries_bevel/test', paste0(name, "_", "trial",i,".txt"))
    nuisevfilename <- file.path('/Users/jennygilbert/Documents/betaseries_bevel/test', paste0(name, "_", "nuis",i,".txt"))
    
    write.table(a, file = trialevfilename, sep = "\t", row.names = FALSE, col.names = FALSE)
    write.table(b, file = nuisevfilename, sep = "\t", row.names = FALSE, col.names = FALSE)
  }
}

splitfunc('bevel30_run03.txt')
splitfunc('bevel30_run04.txt')

######## TEST GETTING FILENAMES ################
mypath <- '/Users/jennygilbert/Documents/betaseries_bevel/onsets'
filelist <- list.files(mypath, all.files = TRUE)
subrun <- file_path_sans_ext(basename(filelist))

lapply(filelist[4:359], splitfunc)

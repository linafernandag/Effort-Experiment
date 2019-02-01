## Code for training and retraining data:

source("functions/trainANDretrain.R")

fun2 <- function(x){
  cleandata1.1 <-cleandata1(x)
  newcolumns1.1 <-newcolumns(cleandata1.1)
  cleandata2.1 <- cleandata2(x)
  newcolumns2.1<- newcolumns2(cleandata2.1)
  sum1 <- summary1(newcolumns1.1)
  sum2 <-summary2(newcolumns2.1)
  all<-merge(sum1,sum2)
  return(all)
}

# how to import multiple files at once

#setwd("Whatever working directory.cv") #set working directory
temp <- list.files(pattern="*.csv")
myfiles <- lapply(temp, read.csv)
#lapply(myfiles, head)

# run fun2 over all
clean_output <- lapply(myfiles, fun2)

## sve them> FIRST create a new folder named cleandata

for(i in 1:length(temp)){
  write.csv(clean_output[[i]], paste0("cleandata/",paste0("clean_", substr(temp[[i]],1,9) ),".csv") )
}
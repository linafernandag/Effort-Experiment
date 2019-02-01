cleandata1 <- function(mydata){
  library(tidyverse)
  data2<- mydata %>%  # select the columns with the data we need and save them in a new data frame for first set of results
    select(Subject,Time.Start,Time.Finish, Duration,Enter.State, A1, A2,A3)
  colnames(data2)[6:8] <-c("Number.A1", "Number.A2", "Number.A3")
  data2.2 <-data2%>% # create a new data frame with only Number A1, Number A2 and Number A3
    select(Number.A1,Number.A2,Number.A3)
  data2.2.2 <- data2.2[!apply(is.na(data2.2) | data2.2 == "", 1, all),] # remove all empty and NA cells
  #data2.2.3<-data2.2.2[-1,] # delete the first row
  data2.3<-data2%>% # create a new data frame with the other columns
    select(Subject,Time.Start,Time.Finish, Duration,Enter.State)
  data2.3.1<- data2.3[!apply(is.na(data2.3) | data2.3 == "", 1, all),]
  #data2.3.2<-data2.3.1[-nrow(data2.3.1),] # delete last row, so ther is an equal number of rows and this data frame can be merged with the one wih A1 A2 and A3
  cleandata0.1 <-cbind(data2.3.1, data2.2.2) # create a new data frame combining the previous 2
  cleandata0.1$Subject <-gsub("_", "",cleandata0.1$Subject) # removes _ from name
  data2$Duration<-as.numeric(data2$Duration) # treat duration as numeric
  return(cleandata0.1)
}

cleandata2 <- function(mydata){
  library(tidyverse)
  dataextra <- mydata %>%  # data for nosepokes in response state
    select(Subject.1,Time, Current.State,Transition.State,A1..,A2..,A3..)
  colnames(dataextra)[1:7] <-c("Subject", "Time", "Current.State", "Transition.State", "A1","A2", "A3") # change names
  dataextra2 <- dataextra[!apply(is.na(dataextra) | dataextra == "", 1, all),] # remove NA and empty cells
  dataextra2$Subject <-gsub("_", "",dataextra2$Subject) # removes _ from name
  return(dataextra2)
}

newcolumns <- function (cleandata0.1){
  library(tidyverse)
  mydata2 <- cleandata0.1%>%
    mutate(correct.LP= ifelse(Enter.State==3,1,0))%>% # create new column with the corect LP (food sate)
    mutate(timeafterLP = ifelse(Number.A3>0 & Enter.State==8,Duration/10,""))%>% # crate new column with the time it took to NP in food cup after correct LP
    mutate(inactiveLP = ifelse(Number.A1>0,Number.A1,0)) # create new column with the LP on the inactive lever
  mydata2$timeafterLP<-as.numeric(mydata2$timeafterLP) # treat the column with the time as numeric and not character so the mean can be calculated later
  #mydata2.2 <- mydata2[!apply(is.na(mydata2) | mydata2 == "", 1, all),]
  return(mydata2)
}

newcolumns2<- function(cleandata2){
  library(tidyverse)
  dataextra <- cleandata2 %>%  # data for nosepokes in response state
    mutate(NP_respstate= ifelse(Current.State==2,A3,0)) # get column with NP during response state S2
  dataextra2<-dataextra[!apply(is.na(dataextra) | dataextra == "", 1, all),]
  return(dataextra2)
}

summary1 <- function (mydata2){
  library(tidyverse)
  results1.1 <- mydata2%>%
    group_by(Subject)%>% # group by subject
    summarise(total_correct = sum(correct.LP),total_inactiveLP = sum(inactiveLP),time_np = mean(timeafterLP, na.rm=TRUE))#summarise the mean of total correct LP, time to NP, and LP in inactive lever
  return(results1.1)
}

summary2<- function (mydata3){
  library(tidyverse)
  results2 <-mydata3%>%
    group_by(Subject)%>% # group by subject
    summarise(NP_responsestate = sum(NP_respstate),TotalA2 = sum (A2))
  return(results2)
}

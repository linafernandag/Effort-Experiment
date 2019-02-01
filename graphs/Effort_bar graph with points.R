
data<- read.csv("graphs/for r.csv")

str(data)

library(tidyverse)
library(dplyr)
library(skimr)

data %>% 
  group_by(Group,Effort_f)%>%
  skim(Criteria.met_total.session) 

#Using dplyr for data creation

data <- mutate(data, 
                    Effort_f=factor(Effort,
                                 levels=c("1LP", "2LP", "4LP", "8LP", "16LP"))) # setting order of var effort

data3<- data %>% 
  group_by(Group,Effort_f) %>% 
  summarize(mean = mean(Criteria.met_total.session, na.rm=TRUE),
            sd = sd(Criteria.met_total.session, na.rm=TRUE),
            n = n(),
            se = sd/sqrt(n),
            ymin=mean - se,
            ymax= mean + se,
  min = min(Criteria.met_total.session, na.rm=TRUE), 
  max = max(Criteria.met_total.session, na.rm=TRUE), 
  median = median(Criteria.met_total.session, na.rm=TRUE))

data$Criteria.met_total.session<-as.numeric(data$Criteria.met_total.session)

str(data)

data4<- data %>% 
  group_by(Group,Effort_f) %>% 
  summarize(mean = mean(Total.inactive_total.session, na.rm=TRUE),
            sd = sd(Total.inactive_total.session, na.rm=TRUE),
            n = n(),
            se = sd/sqrt(n),
            ymin=mean - se,
            ymax= mean + se,
            min = min(Total.inactive_total.session, na.rm=TRUE), 
            max = max(Total.inactive_total.session, na.rm=TRUE), 
            median = median(Total.inactive_total.session, na.rm=TRUE))

data$Total.inactive_total.session<-as.numeric(data$Total.inactive_total.session)

plot<-ggplot(data=data4,aes(x=Effort_f,y=mean,fill=Group))+
  geom_col(position=position_dodge(),color="black", width=0.8)+
  geom_point(data=data,aes(x=Effort_f,y=Total.inactive_total.session, shape=Group),stat="identity", position_jitterdodge(dodge.width =0.9), size=3)+
  scale_shape_manual(values=c(16, 1))+
  scale_fill_manual(values=c("gray","white")) + 
  geom_errorbar(data=data4,aes(ymin = mean - se, ymax=mean + se),width=.2,
                position=position_dodge(.9))
  
my_title <- expression(paste(bold("B. "),"Inactive Lever Presses"))

plot+ theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.title=element_blank(),
            #legend.text=element_text(size=17),
           #legend.justification=c(1,0), 
            #legend.position=c(0.8,0.7),
            text=element_text(size=17,family="sans"),
            plot.title = element_text(size=20, face="plain",hjust = 0.5),
            axis.text.x=element_text(size=17),
            axis.text.y=element_text(size=17))+
  xlab("\nEffort")+
  ylab("Mean?SEM\n")+
  ggtitle(my_title)+
  coord_cartesian(ylim=c(0,30,10))
#scale_y_continuous(expand = c(0, 0)) this is to have the X axis starting at 0
  

plot<-ggplot(data=data3,aes(x=Effort_f,y=mean,fill=Group))+
  geom_col(position=position_dodge(),color="black", width=0.8)+
  geom_point(data=data,aes(x=Effort_f,y=Criteria.met_total.session, shape=Group),stat="identity", position_jitterdodge(dodge.width =0.9), size=3)+
  scale_shape_manual(values=c(16, 1))+
  scale_fill_manual(values=c("gray","white")) + 
  geom_errorbar(data=data3,aes(ymin = mean - se, ymax=mean + se),width=.2,
                position=position_dodge(.9))
  
  #geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.2)+


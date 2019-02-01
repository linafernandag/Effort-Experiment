setwd("E:/Effort/ALL train/retrain/cleandata")
str(correct)
library(car)

correct<- read.csv("correct.csv")
contrasts(correct$Group)=contr.sum
contrasts(correct$Group)
myidatac=expand.grid(day=c("1","2","3"),effort=c("1","2","4","8","16"))
mymodelc=lm(cbind(correct.1.1,correct.2.1,correct.3.1,correct.1.2,correct.2.2,
                  correct.3.2,correct.1.4,correct.2.4,correct.3.4,correct.1.8,
                  correct.2.8,correct.3.8,correct.1.16,correct.2.16,correct.3.16)~Group,data=correct)
myaovc=Anova(mymodelc,idata=myidatac,idesign=~day*effort,data=correct,type=3)
summary(myaovc,multivariate=F)

setwd("E:/Effort/ALL train/retrain/cleandata")
inactive <- read.csv("inactive.csv")
str(inactive)

inactive$inactive.1.1
contrasts(inactive$Group)=contr.sum
contrasts(inactive$Group)
myidatai=expand.grid(day=c("1","2","3"),effort=c("1","2","4","8","16"))
mymodeli=lm(cbind(inactive.1.1,inactive.2.1,inactive.3.1,inactive.1.2,inactive.2.2,
                  inactive.3.2,inactive.1.4,inactive.2.4,inactive.3.4,inactive.1.8,
                  inactive.2.8,inactive.3.8,inactive.1.16,inactive.2.16,inactive.3.16)~Group,data=inactive)
myaovi=Anova(mymodeli,idata=myidatai,idesign=~day*effort,data=inactive,type=3)
summary(myaovi,multivariate=F)

#testing  simple interaction group x effort in day

#day1
myidata1=expand.grid(effort=c("1","2","4","8","16"))
mymodel1=lm(cbind(inactive.1.1,inactive.1.2,inactive.1.4,inactive.1.8,inactive.1.16)~Group,data=inactive)
myaov1=Anova(mymodel1,idata=myidata1,idesign=~effort,data=inactive,type=3)
summary(myaov1,multivariate=F)

t.test(inactive[inactive$Group=="Control","inactive.1.1"],
       inactive[inactive$Group=="Subjugated","inactive.1.1"],var.equal=T)

t.test(inactive[inactive$Group=="Control","inactive.1.2"],
       inactive[inactive$Group=="Subjugated","inactive.1.2"],var.equal=T)

t.test(inactive[inactive$Group=="Control","inactive.1.4"],
       inactive[inactive$Group=="Subjugated","inactive.1.4"],var.equal=T)

t.test(inactive[inactive$Group=="Control","inactive.1.8"],
       inactive[inactive$Group=="Subjugated","inactive.1.8"],var.equal=T)

t.test(inactive[inactive$Group=="Control","inactive.1.16"],
       inactive[inactive$Group=="Subjugated","inactive.1.16"],var.equal=T)

# testing interaction group x day x effort
#


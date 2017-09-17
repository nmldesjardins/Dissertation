setwd("~/Dropbox/UO/Dissertation/data/Processed files/Study 1")
s1rr<-read.csv("Study 1_RR for analysis.csv")
library(ggplot2); library(pander);library(psych); library(dplyr); library(reshape2)


head(s1rr)

describe(s1rr$know)
summary(as.factor(s1rr$know))


s1rr2<-s1rr[s1rr$drop==0,]
n<-length(unique(s1rr2$PerceiverID))
k<-length(unique(s1rr2$groupID))

s1rrPS<-s1rr2[s1rr2$task==1,]
nPS<-length(unique(s1rrPS$PerceiverID))
kPS<-length(unique(s1rrPS$groupID))

s1rrLGD<-s1rr2[s1rr2$task==2,]
nLGD<-length(unique(s1rrLGD$PerceiverID))
kLGD<-length(unique(s1rrLGD$groupID))

s1rrLOM<-s1rr2[s1rr2$task==3,]
nLOM<-length(unique(s1rrLOM$PerceiverID))
kLOM<-length(unique(s1rrLOM$groupID))

s1rrSOC<-s1rr2[s1rr2$task==4,]
nSOC<-length(unique(s1rrSOC$PerceiverID))
kSOC<-length(unique(s1rrSOC$groupID))

kSOC+kPS+kLOM+kLGD

xtabs(~groupID+task,data=s1rr2)


#### Compute B5 ####
s1rr2$cold.r = 11-s1rr2$cold
x<-data.frame(s1rr2$cold.r, s1rr2$cold)
cor(x, use="pairwise.complete.obs")

s1rr2$reserved.r = 11-s1rr2$reserved
s1rr2$lazy.r = 11-s1rr2$lazy
s1rr2$relaxed.r = 11-s1rr2$relaxed
s1rr2$fewart.r = 11-s1rr2$fewart

s1rr2$ext = (s1rr2$outgoing + s1rr2$reserved.r)/2
s1rr2$agr = (s1rr2$cold.r + s1rr2$trusting)/2
s1rr2$con = (s1rr2$thorough + s1rr2$lazy.r)/2
s1rr2$neu = (s1rr2$nervous + s1rr2$relaxed.r)/2
s1rr2$ope = (s1rr2$actimag + s1rr2$fewart.r)/2


#### Variance Decomp ####
library(TripleR)
names(s1rr2)

#### get rr effects
vars<-c(names(s1rr2)[8:33], names(s1rr2)[42:46])
effects<-getEffects(~PerceiverID*target.id | groupID, index="enhance", 
                    data=s1rr2, varlist=vars,na.rm=T )
write.csv(effects,"Study 1 RR effects.csv")

# get uncentered effects
effects.gm<-getEffects(~PerceiverID*target.id | groupID, 
                       data=s1rr2, varlist=vars,na.rm=T,gm=TRUE )
write.csv(effects.gm,"Study 1 uncentered RR effects.csv")

#### RR descriptives ####

# this file has post-task ratings, IDs, RR ratings, and task results
# it's long by participant and group (each row is one p in one group)
s1grp<-read.csv("Study 1_post task by task_RR_IDs_4.7.16.csv")
names(s1grp)

s1grp<-s1grp[s1grp$drop==0,]

describe(s1grp[191:281])
describeBy(s1grp[191:281],as.factor(s1grp$task))
describe(s1grp[304:308])


setwd("~/Dropbox/UO/Dissertation/data/Processed files/Study 1")
library(psych); library(dplyr); library(ggplot2)

# this file has post-task ratings, IDs, RR ratings, and task results
# it's long by participant and group (each row is one p in one group)
# task: 1 = PS, 2 = LGD, 3 = LOM, 4 = social
# s1grp<-read.csv("Study 1_post task by task_RR_IDs_4.7.16.csv")

# this is the same, but includes the group responses (not just individuals)
s1grp2<-read.csv("Study 1_post task by task_RR__group results 5.16.16.csv")

head(s1grp2)

#describeBy(s1grp[c(271,272,176,177,322)], s1grp$task)

# trim down the orig dataset for merging later
# will ultimately create a dataset that just has task scores without 
# all of the individual items
names(s1grp2)

s1grp<-s1grp2[,c(1:16,32:35,66:281,312:322,338)]
head(s1grp)


#### PS ####
# Score problem solving

ps<-s1grp2[,c(1:6,35:67,282:311)]
head(ps)
str(ps)
summary(ps)

# 6 = missing; valid responses are 1:5

# note: no correct ans for q20 (typo in prompt)
# note: 2 right answers for 27 (a,c)
# so, don't include 20 and 27 in computations.
q<-seq(1:30)
answers<-c(1,4,1,1,4,3,4,1,2,5,5,2,4,5,3,3,1,3,4,4,1,5,5,4,3,4,1,1,4,2)
key<-as.data.frame(cbind(q,answers))
key

key[which(key$answers==1),]

ps$PS.1s<-ifelse(ps$PS.1==1,1,0)
ps$PS.2s<-ifelse(ps$PS.2==4,1,0)
ps$PS.3s<-ifelse(ps$PS.3==1,1,0)
ps$PS.4s<-ifelse(ps$PS.4==1,1,0)
ps$PS.5s<-ifelse(ps$PS.5==4,1,0)
ps$PS.6s<-ifelse(ps$PS.6==3,1,0)
ps$PS.7s<-ifelse(ps$PS.7==4,1,0)
ps$PS.8s<-ifelse(ps$PS.8==1,1,0)
ps$PS.9s<-ifelse(ps$PS.9==2,1,0)
ps$PS.10s<-ifelse(ps$PS.10==5,1,0)
ps$PS.11s<-ifelse(ps$PS.11==5,1,0)
ps$PS.12s<-ifelse(ps$PS.12==2,1,0)
ps$PS.13s<-ifelse(ps$PS.13==4,1,0)
ps$PS.14s<-ifelse(ps$PS.14==5,1,0)
ps$PS.15s<-ifelse(ps$PS.15==3,1,0)
ps$PS.16s<-ifelse(ps$PS.16==3,1,0)
ps$PS.17s<-ifelse(ps$PS.17==1,1,0)
ps$PS.18s<-ifelse(ps$PS.18==3,1,0)
ps$PS.19s<-ifelse(ps$PS.19==4,1,0)
#ps$PS.20s<-ifelse(ps$PS.20==4,1,0)
ps$PS.21s<-ifelse(ps$PS.21==1,1,0)
ps$PS.22s<-ifelse(ps$PS.22==5,1,0)
ps$PS.23s<-ifelse(ps$PS.23==5,1,0)
ps$PS.24s<-ifelse(ps$PS.24==4,1,0)
ps$PS.25s<-ifelse(ps$PS.25==3,1,0)
ps$PS.26s<-ifelse(ps$PS.26==4,1,0)
#ps$PS.27s<-ifelse(ps$PS.27==1,1,0)
ps$PS.28s<-ifelse(ps$PS.28==1,1,0)
ps$PS.29s<-ifelse(ps$PS.29==4,1,0)
ps$PS.30s<-ifelse(ps$PS.30==2,1,0)


ps$PS.1.gs<-ifelse(ps$PS.1.g==1,1,0)
ps$PS.2.gs<-ifelse(ps$PS.2.g==4,1,0)
ps$PS.3.gs<-ifelse(ps$PS.3.g==1,1,0)
ps$PS.4.gs<-ifelse(ps$PS.4.g==1,1,0)
ps$PS.5.gs<-ifelse(ps$PS.5.g==4,1,0)
ps$PS.6.gs<-ifelse(ps$PS.6.g==3,1,0)
ps$PS.7.gs<-ifelse(ps$PS.7.g==4,1,0)
ps$PS.8.gs<-ifelse(ps$PS.8.g==1,1,0)
ps$PS.9.gs<-ifelse(ps$PS.9.g==2,1,0)
ps$PS.10.gs<-ifelse(ps$PS.10.g==5,1,0)
ps$PS.11.gs<-ifelse(ps$PS.11.g==5,1,0)
ps$PS.12.gs<-ifelse(ps$PS.12.g==2,1,0)
ps$PS.13.gs<-ifelse(ps$PS.13.g==4,1,0)
ps$PS.14.gs<-ifelse(ps$PS.14.g==5,1,0)
ps$PS.15.gs<-ifelse(ps$PS.15.g==3,1,0)
ps$PS.16.gs<-ifelse(ps$PS.16.g==3,1,0)
ps$PS.17.gs<-ifelse(ps$PS.17.g==1,1,0)
ps$PS.18.gs<-ifelse(ps$PS.18.g==3,1,0)
ps$PS.19.gs<-ifelse(ps$PS.19.g==4,1,0)
#ps$PS.20.gs<-ifelse(ps$PS.20.g==4,1,0)
ps$PS.21.gs<-ifelse(ps$PS.21.g==1,1,0)
ps$PS.22.gs<-ifelse(ps$PS.22.g==5,1,0)
ps$PS.23.gs<-ifelse(ps$PS.23.g==5,1,0)
ps$PS.24.gs<-ifelse(ps$PS.24.g==4,1,0)
ps$PS.25.gs<-ifelse(ps$PS.25.g==3,1,0)
ps$PS.26.gs<-ifelse(ps$PS.26.g==4,1,0)
#ps$PS.27.gs<-ifelse(ps$PS.27.g==1,1,0)
ps$PS.28.gs<-ifelse(ps$PS.28.g==1,1,0)
ps$PS.29.gs<-ifelse(ps$PS.29.g==4,1,0)
ps$PS.30.gs<-ifelse(ps$PS.30.g==2,1,0)

# total correct
ps$PSscore<-rowSums(ps[70:97], na.rm=T)
ps$PSscore.g<-rowSums(ps[98:125], na.rm=T)

summary(ps)
View(ps)

# number of questions answered
ps$PS.totans<-30-rowSums(is.na(ps[70:97]))
ps$PS.totans.g<-30-rowSums(is.na(ps[98:125]))
ps[25:30,]

# % correct of total answered
ps$PS.percor<-100*(ps$PSscore/ps$PS.totans)
ps$PS.percor.g<-100*(ps$PSscore.g/ps$PS.totans.g)

summary(ps[126:131])

# is individual's response the same as the group response?

ps$PS.1inf<-ifelse(ps$PS.1==ps$PS.1.g,1,0)
ps$PS.2inf<-ifelse(ps$PS.2==ps$PS.2.g,1,0)
ps$PS.3inf<-ifelse(ps$PS.3==ps$PS.3.g,1,0)
ps$PS.4inf<-ifelse(ps$PS.4==ps$PS.4.g,1,0)
ps$PS.5inf<-ifelse(ps$PS.5==ps$PS.5.g,1,0)
ps$PS.6inf<-ifelse(ps$PS.6==ps$PS.6.g,1,0)
ps$PS.7inf<-ifelse(ps$PS.7==ps$PS.7.g,1,0)
ps$PS.8inf<-ifelse(ps$PS.8==ps$PS.8.g,1,0)
ps$PS.9inf<-ifelse(ps$PS.9==ps$PS.9.g,1,0)
ps$PS.10inf<-ifelse(ps$PS.10==ps$PS.10.g,1,0)
ps$PS.11inf<-ifelse(ps$PS.11==ps$PS.11.g,1,0)
ps$PS.12inf<-ifelse(ps$PS.12==ps$PS.12.g,1,0)
ps$PS.13inf<-ifelse(ps$PS.13==ps$PS.13.g,1,0)
ps$PS.14inf<-ifelse(ps$PS.14==ps$PS.14.g,1,0)
ps$PS.15inf<-ifelse(ps$PS.15==ps$PS.15.g,1,0)
ps$PS.16inf<-ifelse(ps$PS.16==ps$PS.16.g,1,0)
ps$PS.17inf<-ifelse(ps$PS.17==ps$PS.17.g,1,0)
ps$PS.18inf<-ifelse(ps$PS.18==ps$PS.18.g,1,0)
ps$PS.19inf<-ifelse(ps$PS.19==ps$PS.19.g,1,0)
#ps$PS.20inf<-ifelse(ps$PS.20==ps$PS.20.g,1,0)
ps$PS.21inf<-ifelse(ps$PS.21==ps$PS.21.g,1,0)
ps$PS.22inf<-ifelse(ps$PS.22==ps$PS.22.g,1,0)
ps$PS.23inf<-ifelse(ps$PS.23==ps$PS.23.g,1,0)
ps$PS.24inf<-ifelse(ps$PS.24==ps$PS.24.g,1,0)
ps$PS.25inf<-ifelse(ps$PS.25==ps$PS.25.g,1,0)
ps$PS.26inf<-ifelse(ps$PS.26==ps$PS.26.g,1,0)
#ps$PS.27inf<-ifelse(ps$PS.27==ps$PS.27.g,1,0)
ps$PS.28inf<-ifelse(ps$PS.28==ps$PS.28.g,1,0)
ps$PS.29inf<-ifelse(ps$PS.29==ps$PS.29.g,1,0)
ps$PS.30inf<-ifelse(ps$PS.30==ps$PS.30.g,1,0)

# number of questions answered by the group that == the p's response
ps$PSinfluence<-rowSums(ps[132:159], na.rm=T)

# % of group's total answered questions that == p's response
ps$PS.perinf<-100*(ps$PSinfluence/ps$PS.totans.g)
summary(ps$PS.perinf)

# of those responses that influenced the group, how many were correct?
ps$PS.1.ic<-ifelse((ps$PS.1inf==1 & ps$PS.1s==1),1,0)
ps$PS.2.ic<-ifelse((ps$PS.2inf==1 & ps$PS.2s==1),1,0)
ps$PS.3.ic<-ifelse((ps$PS.3inf==1 & ps$PS.3s==1),1,0)
ps$PS.4.ic<-ifelse((ps$PS.4inf==1 & ps$PS.4s==1),1,0)
ps$PS.5.ic<-ifelse((ps$PS.5inf==1 & ps$PS.5s==1),1,0)
ps$PS.6.ic<-ifelse((ps$PS.6inf==1 & ps$PS.6s==1),1,0)
ps$PS.7.ic<-ifelse((ps$PS.7inf==1 & ps$PS.7s==1),1,0)
ps$PS.8.ic<-ifelse((ps$PS.8inf==1 & ps$PS.8s==1),1,0)
ps$PS.9.ic<-ifelse((ps$PS.9inf==1 & ps$PS.9s==1),1,0)
ps$PS.10.ic<-ifelse((ps$PS.10inf==1 & ps$PS.10s==1),1,0)
ps$PS.11.ic<-ifelse((ps$PS.11inf==1 & ps$PS.11s==1),1,0)
ps$PS.12.ic<-ifelse((ps$PS.12inf==1 & ps$PS.12s==1),1,0)
ps$PS.13.ic<-ifelse((ps$PS.13inf==1 & ps$PS.13s==1),1,0)
ps$PS.14.ic<-ifelse((ps$PS.14inf==1 & ps$PS.14s==1),1,0)
ps$PS.15.ic<-ifelse((ps$PS.15inf==1 & ps$PS.15s==1),1,0)
ps$PS.16.ic<-ifelse((ps$PS.16inf==1 & ps$PS.16s==1),1,0)
ps$PS.17.ic<-ifelse((ps$PS.17inf==1 & ps$PS.17s==1),1,0)
ps$PS.18.ic<-ifelse((ps$PS.18inf==1 & ps$PS.18s==1),1,0)
ps$PS.19.ic<-ifelse((ps$PS.19inf==1 & ps$PS.19s==1),1,0)
#ps$PS.20.ic<-ifelse((ps$PS.20inf==1 & ps$PS.20s==1),1,0)
ps$PS.21.ic<-ifelse((ps$PS.21inf==1 & ps$PS.21s==1),1,0)
ps$PS.22.ic<-ifelse((ps$PS.22inf==1 & ps$PS.22s==1),1,0)
ps$PS.23.ic<-ifelse((ps$PS.23inf==1 & ps$PS.23s==1),1,0)
ps$PS.24.ic<-ifelse((ps$PS.24inf==1 & ps$PS.24s==1),1,0)
ps$PS.25.ic<-ifelse((ps$PS.25inf==1 & ps$PS.25s==1),1,0)
ps$PS.26.ic<-ifelse((ps$PS.26inf==1 & ps$PS.26s==1),1,0)
#ps$PS.27.ic<-ifelse((ps$PS.27inf==1 & ps$PS.27s==1),1,0)
ps$PS.28.ic<-ifelse((ps$PS.28inf==1 & ps$PS.28s==1),1,0)
ps$PS.29.ic<-ifelse((ps$PS.29inf==1 & ps$PS.29s==1),1,0)
ps$PS.30.ic<-ifelse((ps$PS.30inf==1 & ps$PS.30s==1),1,0)

# number of influenced answers that were correct
ps$PSinfluencecor<-rowSums(ps[162:189], na.rm=T)
summary(ps$PSinfluencecor)

# percentage of influenced answers that were correct
ps$PS.perinfcor<-100*(ps$PSinfluencecor/ps$PSinfluence)
summary(ps$PS.perinfcor)

# perct of total answers that were correctly influenced
ps$PS.perinfcor_tot<-100*(ps$PSinfluencecor/ps$PS.totans.g)
summary(ps$PS.perinfcor_tot)

# number of influenced answers that were incorrect
ps$infwrong<-ps$PSinfluence-ps$PSinfluencecor

# perct of incorrect influences
ps$PS.perinfwrong<-100*(ps$infwrong/ps$PSinfluence)

# perct of total incorrect
ps$PS.perinfwrong_tot<-100*(ps$infwrong/ps$PS.totans.g)

ps_score<-ps[,c(1:7,126:131,160:161,190:195)]
write.csv(ps_score,"problem solving_scored variables.csv")

ps_score <- ps_score %>% filter(task == 1)
head(ps_score)

# merge back into aggregated data
df <- read.csv('Study 1_post task by task_RR_IDs_4.7.16.csv')

df2<- left_join(df,ps_score, by = c('groupID','PerceiverID','task','Group','table','session','PS.Table'))

#### LOM ####
lom<-s1grp2[,c(1:6,15:31,69,323:337)]
names(lom)

# correct ranks
lom$rank.matches <- 15
lom$rank.food <- 4
lom$rank.rope <- 6
lom$rank.parachute <- 8
lom$rank.heat <- 13
lom$rank.milk <- 12
lom$rank.oxygen <- 1
lom$rank.map <- 3
lom$rank.raft <- 9
lom$rank.water <- 2
lom$rank.flares <- 10
lom$rank.firstaid <- 7
lom$rank.fm <- 5
lom$rank.pistols <- 11
lom$rank.compass <- 14

## produce a restructured file to get correlations for rank-order accuracy
lom2<-lom[,c(1:3,9:23,25:54)]
names(lom2)

# get just the data for the lom task
head(lom2[which(lom2$task==3),])
lom2<-lom2[which(lom2$task==3),]

# some missing is coded as 99
summary(lom2)
lom2$LOM.pistols[lom2$LOM.pistols==99]<-NA
lom2$LOM.compass[lom2$LOM.compass==99]<-NA


# rename variables so reshape can parse them more easily
# .i = individual; .g = group, .r = rank

colnames(lom2)<-c("groupID","PerceiverID","task","matches.i","food.i","rope.i",
                  "parachute.i","heat.i","milk.i","oxygen.i","map.i",
                  "raft.i","water.i","flares.i","firstaid.i","fm.i",
                  "pistols.i","compass.i","matches.g","food.g","rope.g",
                  "parachute.g","heat.g","pistols.g","milk.g","oxygen.g",
                  "map.g","raft.g","compass.g","water.g","flares.g",
                  "firstaid.g","fm.g","matches.r","food.r","rope.r",
                  "parachute.r","heat.r","milk.r","oxygen.r","map.r",
                  "raft.r","water.r","flares.r","firstaid.r","fm.r",
                  "pistols.r","compass.r")

head(lom2)

lom_long<-reshape(lom2,dir="long", varying=4:48,
                  idvar=1:3,
                  v.names=c("i","g","r"),
                  times=c("matches","food","rope","parachute",
                          "heat","milk","oxygen","map","raft",
                          "water","flares","firstaid","fm","pistols",
                          "compass"))

head(lom_long)
View(lom_long)


lom_long<-lom_long[order(lom_long$PerceiverID,lom_long$time),]
head(lom_long)

# correlations

# this wasn't getting separate estimates for each perciever?
# persgroup<-lom_long %>%
#        group_by(PerceiverID) %>%
#         do({
#                 gi<-cor(lom_long$i,lom_long$g, use="pairwise.complete.obs")
#                 data.frame(gi)
#         })

library(magrittr)

lom_long %>% select(g, i, PerceiverID) %>% group_by(PerceiverID) %$% cor(g,i)
# corr bet person's answers and groups answers
func<-function(lom_long)
{
        return(data.frame(LOM.pergrp_correl = cor(lom_long$i,lom_long$g,use="pairwise.complete.obs")))
}

#this tells it to analyze by perceiver id
#persongroup<-plyr::ddply(lom_long,.(PerceiverID),func)

persongroup<-plyr::ddply(lom_long,~PerceiverID,func)

# corr person's answers and correct answers
func<-function(lom_long)
{
        return(data.frame(LOM.perscorrect_correl = cor(lom_long$i,lom_long$r,use="pairwise.complete.obs")))
}

personright<-plyr::ddply(lom_long,~PerceiverID,func)

# corr groups answers and right answers
func<-function(lom_long)
{
        return(data.frame(LOM.grpcorrect_correl = cor(lom_long$g,lom_long$r,use="pairwise.complete.obs")))
}

groupright<-plyr::ddply(lom_long,~groupID,func)


# merge
lom2<-merge(lom2,persongroup, by = c('PerceiverID'))
lom2<-merge(lom2,personright, by = c('PerceiverID'))
lom2<-merge(lom2,groupright, by = 'groupID')



# are the answers exactly correct
lom$matches.e<-ifelse(lom$LOM.matches==lom$rank.matches,1,0)
lom$food.e<-ifelse(lom$LOM.food==lom$rank.food,1,0)
lom$rope.e<-ifelse(lom$LOM.rope==lom$rank.rope,1,0)
lom$parachute.e<-ifelse(lom$LOM.parachute==lom$rank.parachute,1,0)
lom$heat.e<-ifelse(lom$LOM.heat==lom$rank.heat,1,0)
lom$milk.e<-ifelse(lom$LOM.milk==lom$rank.milk,1,0)
lom$oxygen.e<-ifelse(lom$LOM.oxygen==lom$rank.oxygen,1,0)
lom$map.e<-ifelse(lom$LOM.map==lom$rank.map,1,0)
lom$raft.e<-ifelse(lom$LOM.raft==lom$rank.raft,1,0)
lom$water.e<-ifelse(lom$LOM.water==lom$rank.water,1,0)
lom$flares.e<-ifelse(lom$LOM.flares==lom$rank.flares,1,0)
lom$firstaid.e<-ifelse(lom$LOM.firstaid==lom$rank.firstaid,1,0)
lom$fm.e<-ifelse(lom$LOM.fm==lom$rank.fm,1,0)
lom$pistols.e<-ifelse(lom$LOM.pistols==lom$rank.pistols,1,0)
lom$compass.e<-ifelse(lom$LOM.compass==lom$rank.compass,1,0)

lom$matches.e.g<-ifelse(lom$LOM.matches.g==lom$rank.matches,1,0)
lom$food.e.g<-ifelse(lom$LOM.food.g==lom$rank.food,1,0)
lom$rope.e.g<-ifelse(lom$LOM.rope.g==lom$rank.rope,1,0)
lom$parachute.e.g<-ifelse(lom$LOM.parachute.g==lom$rank.parachute,1,0)
lom$heat.e.g<-ifelse(lom$LOM.heat.g==lom$rank.heat,1,0)
lom$milk.e.g<-ifelse(lom$LOM.milk.g==lom$rank.milk,1,0)
lom$oxygen.e.g<-ifelse(lom$LOM.oxygen.g==lom$rank.oxygen,1,0)
lom$map.e.g<-ifelse(lom$LOM.map.g==lom$rank.map,1,0)
lom$raft.e.g<-ifelse(lom$LOM.raft.g==lom$rank.raft,1,0)
lom$water.e.g<-ifelse(lom$LOM.water.g==lom$rank.water,1,0)
lom$flares.e.g<-ifelse(lom$LOM.flares.g==lom$rank.flares,1,0)
lom$firstaid.e.g<-ifelse(lom$LOM.firstaid.g==lom$rank.firstaid,1,0)
lom$fm.e.g<-ifelse(lom$LOM.receiver.g==lom$rank.fm,1,0)
lom$pistols.e.g<-ifelse(lom$LOM.pistols.g==lom$rank.pistols,1,0)
lom$compass.e.g<-ifelse(lom$LOM.compass.g==lom$rank.compass,1,0)

# total exactly correct
lom$LOM.exact<-rowSums(lom[55:69], na.rm=T)
lom$LOM.exact.g<-rowSums(lom[70:84], na.rm=T)

summary(lom[85:86])

# number of questions answered
lom$LOM.totans<-15-rowSums(is.na(lom[55:69]))
lom$LOM.totans.g<-15-rowSums(is.na(lom[70:84]))

# % exactly correct
lom$LOM.per.exact<-100*(lom$LOM.exact/lom$LOM.totans)
lom$LOM.per.exact.g<-100*(lom$LOM.exact.g/lom$LOM.totans.g)

summary(lom[89:90])

## influence
lom$matches.inf <- ifelse(lom$LOM.matches==lom$LOM.matches.g, 1, 0)
lom$food.inf <- ifelse(lom$LOM.food==lom$LOM.food.g, 1, 0)
lom$rope.inf <- ifelse(lom$LOM.rope==lom$LOM.rope.g, 1, 0)
lom$parachute.inf <- ifelse(lom$LOM.parachute==lom$LOM.parachute.g, 1, 0)
lom$heat.inf <- ifelse(lom$LOM.heat==lom$LOM.heat.g, 1, 0)
lom$milk.inf <- ifelse(lom$LOM.milk==lom$LOM.milk.g, 1, 0)
lom$oxygen.inf <- ifelse(lom$LOM.oxygen==lom$LOM.oxygen.g, 1, 0)
lom$map.inf <- ifelse(lom$LOM.map==lom$LOM.map.g, 1, 0)
lom$raft.inf <- ifelse(lom$LOM.raft==lom$LOM.raft.g, 1, 0)
lom$water.inf <- ifelse(lom$LOM.water==lom$LOM.water.g, 1, 0)
lom$flares.inf <- ifelse(lom$LOM.flares==lom$LOM.flares.g, 1, 0)
lom$firstaid.inf <- ifelse(lom$LOM.firstaid==lom$LOM.firstaid.g, 1, 0)
lom$fm.inf <- ifelse(lom$LOM.fm==lom$LOM.receiver.g, 1, 0)
lom$pistols.inf <- ifelse(lom$LOM.pistols==lom$LOM.pistols.g, 1, 0)
lom$compass.inf <- ifelse(lom$LOM.compass==lom$LOM.compass.g, 1, 0)

# total # of responses that were the same
lom$LOM.influence<-rowSums(lom[91:105], na.rm=T)

# % of groups' responses that were influenced
lom$LOM.perc.inf<-lom$LOM.influence/lom$LOM.totans.g

# # of correct influences
lom$matches.ic <- ifelse(lom$matches.inf==1 & lom$matches.e==1, 1, 0)
lom$food.ic <- ifelse(lom$food.inf==1 & lom$food.e==1, 1, 0)
lom$rope.ic <- ifelse(lom$rope.inf==1 & lom$rope.e==1, 1, 0)
lom$parachute.ic <- ifelse(lom$parachute.inf==1 & lom$parachute.e==1, 1, 0)
lom$heat.ic <- ifelse(lom$heat.inf==1 & lom$heat.e==1, 1, 0)
lom$milk.ic <- ifelse(lom$milk.inf==1 & lom$milk.e==1, 1, 0)
lom$oxygen.ic <- ifelse(lom$oxygen.inf==1 & lom$oxygen.e==1, 1, 0)
lom$map.ic <- ifelse(lom$map.inf==1 & lom$map.e==1, 1, 0)
lom$raft.ic <- ifelse(lom$raft.inf==1 & lom$raft.e==1, 1, 0)
lom$water.ic <- ifelse(lom$water.inf==1 & lom$water.e==1, 1, 0)
lom$flares.ic <- ifelse(lom$flares.inf==1 & lom$flares.e==1, 1, 0)
lom$firstaid.ic <- ifelse(lom$firstaid.inf==1 & lom$firstaid.e==1, 1, 0)
lom$fm.ic <- ifelse(lom$fm.inf==1 & lom$fm.e==1, 1, 0)
lom$pistols.ic <- ifelse(lom$pistols.inf==1 & lom$pistols.e==1, 1, 0)
lom$compass.ic <- ifelse(lom$compass.inf==1 & lom$compass.e==1, 1, 0)

# total correct influences
lom$LOM.influence_cor<-rowSums(lom[108:122], na.rm=T)
# % of influences that were correct
lom$LOM.influence_cor_per<-100*(lom$LOM.influence_cor/lom$LOM.influence)
# % of total answers that were correct influences
lom$LOM.influence_c_p_tot<-100*(lom$LOM.influence_cor/lom$LOM.totans.g)

# total incorrect influences
lom$LOM.influence_wrong<-lom$LOM.influence-lom$LOM.influence_cor

# % of total influences that were wrong
lom$LOM.influence_wro_per<-100*(lom$LOM.influence_wrong/lom$LOM.influence)
# % of total answers that were wrong influences
lom$LOM.influence_w_p_tot<-100*(lom$LOM.influence_wrong/lom$LOM.totans.g)


lomscores<-lom[,c(1:8,24,85:90,106:107,123:128)]
write.csv(lomscores,"lom scores.csv")


## merge
#s1grp<-merge(s1grp,lomscores, by=1:6)
lomscores <- lomscores %>% filter(task == 3)
head(lomscores)

# merge back into aggregated data

df2<- left_join(df2,lomscores, by = c('groupID','PerceiverID','task','Group','table','session'))


## merge w/ rank order consistency
lom3<-lom2[,c(1:3,49:51)]

df2<- left_join(df2,lom3, by = c('groupID','PerceiverID','task'))


#### lgd ####
# most of the lgd scores were computed in spss; just update so they make sense in r

s1grp$LGD.winner_name<-as.factor(as.character(ifelse(s1grp$LGD.winner==1, "Alex Chambers",
                              ifelse(s1grp$LGD.winner==2,"Barbara Philips",
                                     ifelse(s1grp$LGD.winner==3,"Catherine Sullivan",
                                            ifelse(s1grp$LGD.winner==4,"Donald Yee",
                                                   ifelse(s1grp$LGD.winner==5, "Erica Dupre", NA)))))))


summary(as.factor(s1grp$LGD.winner))
summary(as.factor(s1grp$LGD.winner_name))

s1grp$LGD.winner_name<-as.factor(as.character(ifelse(s1grp$LGD.winner==1, "Alex Chambers",
                                                     ifelse(s1grp$LGD.winner==2,"Barbara Philips",
                                                            ifelse(s1grp$LGD.winner==3,"Catherine Sullivan",
                                                                   ifelse(s1grp$LGD.winner==4,"Donald Yee",
                                                                          ifelse(s1grp$LGD.winner==5, "Erica Dupre", NA)))))))

s1grp$LGD.2ndplace_name<-as.factor(as.character(ifelse(s1grp$LGD.2ndplace==1, "Alex Chambers",
                                                     ifelse(s1grp$LGD.2ndplace==2,"Barbara Philips",
                                                            ifelse(s1grp$LGD.2ndplace==3,"Catherine Sullivan",
                                                                   ifelse(s1grp$LGD.2ndplace==4,"Donald Yee",
                                                                          ifelse(s1grp$LGD.2ndplace==5, "Erica Dupre", NA)))))))

s1grp$LGD.3rdplace_name<-as.factor(as.character(ifelse(s1grp$LGD.3rdplace==1, "Alex Chambers",
                                                       ifelse(s1grp$LGD.3rdplace==2,"Barbara Philips",
                                                              ifelse(s1grp$LGD.3rdplace==3,"Catherine Sullivan",
                                                                     ifelse(s1grp$LGD.3rdplace==4,"Donald Yee",
                                                                            ifelse(s1grp$LGD.3rdplace==5, "Erica Dupre", NA)))))))

s1grp$LGD.4thplace_name<-as.factor(as.character(ifelse(s1grp$LGD.4thplace==1, "Alex Chambers",
                                                       ifelse(s1grp$LGD.4thplace==2,"Barbara Philips",
                                                              ifelse(s1grp$LGD.4thplace==3,"Catherine Sullivan",
                                                                     ifelse(s1grp$LGD.4thplace==4,"Donald Yee",
                                                                            ifelse(s1grp$LGD.4thplace==5, "Erica Dupre", NA)))))))

s1grp$LGD.5thplace_name<-as.factor(as.character(ifelse(s1grp$LGD.5thplace==1, "Alex Chambers",
                                                       ifelse(s1grp$LGD.5thplace==2,"Barbara Philips",
                                                              ifelse(s1grp$LGD.5thplace==3,"Catherine Sullivan",
                                                                     ifelse(s1grp$LGD.5thplace==4,"Donald Yee",
                                                                            ifelse(s1grp$LGD.5thplace==5, "Erica Dupre", NA)))))))

s1grp$LGD.lastplace_name<-as.factor(as.character(ifelse(s1grp$LGD.lastplace==1, "Alex Chambers",
                                                       ifelse(s1grp$LGD.lastplace==2,"Barbara Philips",
                                                              ifelse(s1grp$LGD.lastplace==3,"Catherine Sullivan",
                                                                     ifelse(s1grp$LGD.lastplace==4,"Donald Yee",
                                                                            ifelse(s1grp$LGD.lastplace==5, "Erica Dupre", NA)))))))

s1grp$LGD.nominee_name<-as.factor(as.character(ifelse(s1grp$LGD.nominee==1, "Alex Chambers",
                                                        ifelse(s1grp$LGD.nominee==2,"Barbara Philips",
                                                               ifelse(s1grp$LGD.nominee==3,"Catherine Sullivan",
                                                                      ifelse(s1grp$LGD.nominee==4,"Donald Yee",
                                                                             ifelse(s1grp$LGD.nominee==5, "Erica Dupre", NA)))))))


s1grp$lgd.lose<-ifelse(s1grp$LGD.nominee==s1grp$LGD.lastplace, 1,0)

s1grp$lgd.place<-ifelse(s1grp$LGD.nominee==s1grp$LGD.winner,1,
                        ifelse(s1grp$LGD.nominee==s1grp$LGD.2ndplace,2,
                               ifelse(s1grp$LGD.nominee==s1grp$LGD.3rdplace,3,
                                      ifelse((s1grp$LGD.nominee==s1grp$LGD.4thplace & s1grp$LGD.lose==0),4,
                                             ifelse(s1grp$LGD.nominee==s1grp$LGD.5thplace,5,
                                                    ifelse((s1grp$LGD.nominee==s1grp$LGD.4thplace & s1grp$LGD.lose==1),5,NA))))))


# total prize money
# prize money is a factor because of the commas in the values from data entry
# get rid of the commas
s1grp$LGD.prize1n<-as.numeric(gsub(",","",s1grp$LGD.prize1))
s1grp$LGD.prize2n<-as.numeric(gsub(",","",s1grp$LGD.prize2))
s1grp$LGD.prize3n<-as.numeric(gsub(",","",s1grp$LGD.prize3))
s1grp$LGD.prize4n<-as.numeric(gsub(",","",s1grp$LGD.prize4))
s1grp$LGD.prize5n<-as.numeric(gsub(",","",s1grp$LGD.prize5))


s1grp$lgd.prizemoney<-ifelse(s1grp$LGD.nominee==s1grp$LGD.winner,s1grp$LGD.prize1n,
                        ifelse(s1grp$LGD.nominee==s1grp$LGD.2ndplace,s1grp$LGD.prize2n,
                               ifelse(s1grp$LGD.nominee==s1grp$LGD.3rdplace,s1grp$LGD.prize3n,
                                      ifelse(s1grp$LGD.nominee==s1grp$LGD.4thplace,s1grp$LGD.prize4n,
                                             ifelse(s1grp$LGD.nominee==s1grp$LGD.5thplace,s1grp$LGD.prize5n,NA)))))

summary(s1grp$lgd.prizemoney)


lgdscores <- s1grp %>% filter(task == 2)
names(lgdscores)

lgdscores<- lgdscores %>% select(groupID, PerceiverID, task, Group, table, "LGD.prize1",          "LGD.prize2",          "LGD.prize3",          "LGD.prize4",         
                      "LGD.prize5",          "LGD.winner",          "LGD.2ndplace",        "LGD.3rdplace" ,       "LGD.4thplace",       
                     "LGD.5thplace",        "LGD.lastplace",       "lgd.win",             "LGD.winner_name",     "LGD.2ndplace_name" , 
                     "LGD.3rdplace_name",   "LGD.4thplace_name",   "LGD.5thplace_name",   "LGD.lastplace_name" , "LGD.nominee_name",   
                     "lgd.lose",            "lgd.place",           "LGD.prize1n",         "LGD.prize2n",         "LGD.prize3n" ,       
                     "LGD.prize4n",         "LGD.prize5n",         "lgd.prizemoney"  )



df2 <- left_join(df2, lgdscores, by=c('groupID', 'PerceiverID','task','Group','table'))
head(df2)

df2$task_name<-ifelse(df2$task==1,"PS",
                     ifelse(df2$task==2,"LGD",
                            ifelse(df2$task==3,"LOM",
                                   ifelse(df2$task==4,"social",NA))))

write.csv(df2,"Study 1_alldata_w scored outcomes_91617.csv")



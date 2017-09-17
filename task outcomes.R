#### set up ####

setwd("~/Dropbox/UO/Dissertation/data/Processed files/Study 1")
library(psych); library(dplyr); library(ggplot2); library

# this file has post-task ratings, IDs, RR ratings, and scored task results
# it's long by participant and group (each row is one p in one group)
# task: 1 = PS, 2 = LGD, 3 = LOM, 4 = social
# this is the same, but includes the group responses (not just individuals)
s1grp<-read.csv("Study 1_alldata_w scored outcomes.csv")
head(s1grp)
# recode task
s1grp$task_n<-ifelse(s1grp$task==1,"PS",
                     ifelse(s1grp$task==2,"LGD",
                            ifelse(s1grp$task==3,"LOM",
                                   ifelse(s1grp$task==4,"social",NA))))

summary(as.factor(s1grp$task))
summary(as.factor(s1grp$task_n))

colnames(s1grp)[262]<-"PS.infwrong"
colnames(s1grp)[268:269]<-c("LOM.exact","LOM.exact.g")
colnames(s1grp)[272:273]<-c("LOM.per.exact","LOM.per.exact.g")

s1grp<-s1grp[,-c(1)]
#### preliminary task outcome analyses ####

# correlation w/ liking,status,respect
names(s1grp)
outcomes<-cor(s1grp[c(107,113,121,207,212,218,248,291:293,250:263,267:283)], 
    use="pairwise.complete.obs")[,1:6]

write.csv(outcomes,"status liking respect w outcomes.csv")

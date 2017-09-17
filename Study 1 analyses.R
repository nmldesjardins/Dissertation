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


## Whole sample
RR.style("p")
status<-RR(status~PerceiverID*target.id | groupID, data=s1rr2, na.rm=T)
status

liking<-RR(like~PerceiverID*target.id | groupID, data=s1rr2, na.rm=T)
liking

respect<-RR(respect~PerceiverID*target.id | groupID, data=s1rr2, na.rm=T)
respect

infl<-RR(influence~PerceiverID*target.id | groupID, data=s1rr2, na.rm=T)
infl

## PS

statusPS<-RR(status~PerceiverID*target.id | groupID, data=s1rrPS, na.rm=T)
statusPS

likingPS<-RR(like~PerceiverID*target.id | groupID, data=s1rrPS, na.rm=T)
likingPS

respectPS<-RR(respect~PerceiverID*target.id | groupID, data=s1rrPS, na.rm=T)
respectPS

inflPS<-RR(influence~PerceiverID*target.id | groupID, data=s1rrPS, na.rm=T)
inflPS

### LGD
statusLGD<-RR(status~PerceiverID*target.id | groupID, data=s1rrLGD, na.rm=T)
statusLGD

liking<-RR(like~PerceiverID*target.id | groupID, data=s1rrLGD, na.rm=T)
liking

respectLGD<-RR(respect~PerceiverID*target.id | groupID, data=s1rrLGD, na.rm=T)
respectLGD

inflLGD<-RR(influence~PerceiverID*target.id | groupID, data=s1rrLGD, na.rm=T)
inflLGD

### LOM
statusLOM<-RR(status~PerceiverID*target.id | groupID, data=s1rrLOM, na.rm=T)
statusLOM

likingLOM<-RR(like~PerceiverID*target.id | groupID, data=s1rrLOM, na.rm=T)
likingLOM

respectLOM<-RR(respect~PerceiverID*target.id | groupID, data=s1rrLOM, na.rm=T)
respectLOM

inflLOM<-RR(influence~PerceiverID*target.id | groupID, data=s1rrLOM, na.rm=T)
inflLOM

### SOCIAL
statusSOC<-RR(status~PerceiverID*target.id | groupID, data=s1rrSOC, na.rm=T)
statusSOC

likingSOC<-RR(like~PerceiverID*target.id | groupID, data=s1rrSOC, na.rm=T)
likingSOC

respectSOC<-RR(respect~PerceiverID*target.id | groupID, data=s1rrSOC, na.rm=T)
respectSOC

inflSOC<-RR(influence~PerceiverID*target.id | groupID, data=s1rrSOC, na.rm=T)
inflSOC


#### get rr effects
vars<-c(names(s1rr2)[8:33], names(s1rr2)[42:46])
effects<-getEffects(~PerceiverID*target.id | groupID, index="enhance", 
                    data=s1rr2, varlist=vars,na.rm=T )
write.csv(effects,"Study 1 RR effects.csv")

# get uncentered effects
effects.gm<-getEffects(~PerceiverID*target.id | groupID, 
                       data=s1rr2, varlist=vars,na.rm=T,gm=TRUE )
write.csv(effects.gm,"Study 1 uncentered RR effects.csv")


rru<-read.csv("Study 1 uncentered RR effects.csv")
summary(rru)
hist(rru$status.t.gm)
hist(s1rr2$status)
hist(rru$like.t.gm)
hist(s1rr2$like)
hist(rru$respect.t.gm)
hist(rru$respect.p)
hist(s1rr2$respect)
hist(rru$influence.t.gm)

# get 10 random groups
rrx<-rru[sample(nrow(rru), 10),]
rrx$group.id.gm
rrx2<-rru[(rru$group.id.gm==1311)|(rru$group.id.gm==1014)|
                  (rru$group.id.gm==1323)|(rru$group.id.gm==547)|
                  (rru$group.id.gm==533)|(rru$group.id.gm==522)|
                  (rru$group.id.gm==1011)|(rru$group.id.gm==713)|
                  (rru$group.id.gm==1013)|(rru$group.id.gm==623) ,]


p<-ggplot(rrx2,aes(x=status.t.gm,fill=group.id.gm))
p<- p + geom_histogram() + facet_wrap(~group.id.gm,nrow=2)
p
write.csv(rrx2,"random groups for histos.csv")

names(rru)<-paste(names(rru),"gm",sep=".")
write.csv(rru,"Study 1 uncentered RR effects.csv")

#### RR descriptives ####

# this file has post-task ratings, IDs, RR ratings, and task results
# it's long by participant and group (each row is one p in one group)
s1grp<-read.csv("Study 1_post task by task_RR_IDs_4.7.16.csv")
names(s1grp)

s1grp<-s1grp[s1grp$drop==0,]

describe(s1grp[191:281])
describeBy(s1grp[191:281],as.factor(s1grp$task))
describe(s1grp[304:308])

#### Group - level analyses ####
## manip checks
describe(s1grp[7:14])
# by task
describeBy(s1grp[7:14], as.factor(s1grp$task))
# by time (should be the sameish)
describeBy(s1grp[7:14], as.factor(s1grp$Group))
names(s1grp)

# corrs
manip<-data.frame(s1grp[7:14])
cor(manip, use="pairwise.complete.obs")

# test for differences
diff<-(aov(task_diff~as.factor(task), data=s1grp))
TukeyHSD(diff)

enjoy<-(aov(task_enjoy~as.factor(task), data=s1grp))
TukeyHSD(enjoy)

notagain<-(aov(task_notagain~as.factor(task), data=s1grp))
TukeyHSD(notagain)

coop<-(aov(task_coop~as.factor(task), data=s1grp))
TukeyHSD(coop)

comp<-(aov(task_comp~as.factor(task), data=s1grp))
TukeyHSD(comp)

knowwell<-(aov(task_knowwell~as.factor(task), data=s1grp))
TukeyHSD(knowwell)

spknow<-(aov(task_spknow~as.factor(task), data=s1grp))
TukeyHSD(spknow)

likegrp<-(aov(task_likegrp~as.factor(task), data=s1grp))
TukeyHSD(likegrp)

## create contrasts
# task vs. rest
s1grp$PScon<-as.factor(ifelse(s1grp$task==1,1,0))
s1grp$LGDcon<-as.factor(ifelse(s1grp$task==2,1,0))
s1grp$LOMcon<-as.factor(ifelse(s1grp$task==3,1,0))
s1grp$SOCcon<-as.factor(ifelse(s1grp$task==4,1,0))

summary(lm(task_comp~LGDcon, data=s1grp))
summary(lm(task_comp~PScon, data=s1grp))
summary(lm(task_comp~LOMcon, data=s1grp))
summary(lm(task_comp~SOCcon, data=s1grp))

summary(lm(task_coop~LGDcon, data=s1grp))
summary(lm(task_coop~PScon, data=s1grp))
summary(lm(task_coop~LOMcon, data=s1grp))
summary(lm(task_coop~SOCcon, data=s1grp))


#### correlations amongst target effects ####
s1rrfx<-read.csv("Study 1 RR effects.csv")
rrc<-cor(s1rrfx[4:123], use = "pairwise.complete.obs")
write.csv(rrc,"RR correlations.csv")

library(Hmisc)
x<-data.frame(s1rrfx[61],s1rrfx[85],s1rrfx[91],s1rrfx[99])
rcorr(x)

#### status * task ####
stat<-aov(status.t.gm~as.factor(task), data=s1grp)
TukeyHSD(stat)

## SR B5
s1grp$taskn<-as.factor(ifelse(s1grp$task==1,"PS",
                    ifelse(s1grp$task==2, "LGD",
                           ifelse(s1grp$task==3,"LOM", "SOC"))))

s1grp$task<-as.factor(s1grp$task)
summary(s1grp$taskn)
summary(as.factor(s1grp$task))

library(lme4)
library(lmerTest)
options(scipen=999)

null<-lmer(status.t.gm~1+(1|PerceiverID) +(1|groupID), data=s1grp)
summary(null)

## E ##
summary(s1grp$bfie)
s1grp$bfie_c = s1grp$bfie - 3.343

e1<-lmer(status.t.gm~bfie_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(e1)

# e should predict in all of them, so just use dummy codes
e2<-lmer(status.t.gm~bfie_c+taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(e2)

e3<-lmer(status.t.gm~bfie_c*taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(e3)

## A ##
x<-s1grp[(s1grp$taskn=="LGD"|s1grp$taskn=="SOC"),]
ax<-lmer(status.t.gm~bfia_c*taskn+(1|PerceiverID) + (1|groupID),
        data=x)
summary(ax)


summary(s1grp$bfia)
s1grp$bfia_c = s1grp$bfia - 3.814

a1<-lmer(status.t.gm~bfia_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(a1)

# a should be highest in social (affiliative), and lowest in LGD (competitive)
# contrasts will compare all to social, all to lgd
# then, becuase both ps and lom could be coop, compare those to each other
# ps    lgd     lom     soc
# -1/3  -1/3    -1/3    1
# -1/3  1       -1/3    -1/3
#  -1   0       1       0
((-1/3)*(-1/3)*-1)+((-1/3)*1*0)+((-1/3)*(-1/3)*1)+(1*(-1/3)*0)

mat2<-cbind(c(-1/3,-1/3,-1/3,1),c(-1/3,1,-1/3,-1/3),c(-1,0,1,0))
mat2

## second set of codes: compare soc to lgd
# ps    lgd     lom     soc
# 0      1      0        -1
# 1      0      -1        0
# -1/3  -1/3   -1/3       1
(0*1*(-1/3))+(0*1*(-1/3))+(0*-1*(-1/3))+(0*1*-1)

mat2a<-cbind(c(0,1,0,-1),c(1,0,-1,0),c(-1/3,-1/3,-1/3,1))

## third: soc + coop vs. ps + lgd
# ps    lgd     lom     soc
# -1      -1      1       1
# 0      0      -1        1
# -1    1        0       0
mat2b<-cbind(c(-1,-1,1,1),c(0,0,-1,1),c(-1,1,0,0))

a2<-lmer(status.t.gm~bfia_c+task+(1|PerceiverID) + (1|groupID),
         contrasts=list(task=mat2a),data=s1grp)
summary(a2)

a3<-lmer(status.t.gm~bfia_c*task+(1|PerceiverID) + (1|groupID),
         contrasts=list(task=mat2b),data=s1grp)
summary(a3)

# for plot
a3<-lmer(status.t.gm~bfia_c*taskn+(1|PerceiverID) + (1|groupID),
         data=s1grp)
summary(a3)
a3<-lmer(status.t.gm~a_compa_c*taskn+(1|PerceiverID) + (1|groupID),
         data=s1grp)
summary(a3)
a3<-lmer(status.t.gm~a_pol_c*taskn+(1|PerceiverID) + (1|groupID),
         data=s1grp)
summary(a3)


## C ##
summary(s1grp$bfic)
s1grp$bfic_c = s1grp$bfic - 3.556

c1<-lmer(status.t.gm~bfic_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(c1)

# c should, potentially, matter most for PS; might also matter for LOM 
# (organizing people, ranks); test ps vs rest, lom v rest, and soc v lgd
# ps    lgd     lom     soc
# 1     -1/3    -1/3    -1/3
# -1/3  -1/3    1    -1/3
#  0    -1      0       1
((-1/3)*(-1/3)*-1)+((-1/3)*1*0)+((-1/3)*(-1/3)*1)+(1*(-1/3)*0)

mat3<-cbind(c(1,-1/3,-1/3,-1/3),c(-1/3,-1/3,1,-1/3),c(0,-1,0,1))
mat3


## second set of codes: compare ps to soc
# ps    lgd     lom     soc
# 1      0      0        -1
# 0      1      -1        0
# 1  -1/3   -1/3        -1/3

mat3a<-cbind(c(1,0,0,-1),c(0,1,-1,0),c(1,-1/3,-1/3,-1/3))


c2<-lmer(status.t.gm~bfic_c+task+(1|PerceiverID) + (1|groupID), 
         contrasts=list(task=mat3),data=s1grp)
summary(c2)


c3<-lmer(status.t.gm~bfic_c*task+(1|PerceiverID) + (1|groupID), 
         contrasts=list(task=mat3a),data=s1grp)
summary(c3)

c3<-lmer(status.t.gm~bfic_c*task+(1|PerceiverID) + (1|groupID), 
data=s1grp)
summary(c3)


## N ##
summary(s1grp$bfin)
s1grp$bfin_c = s1grp$bfin-2.938

n1<-lmer(status.t.gm~bfin_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(n1)

# n might be especially bad for social, but could maybe be ok for ps
# soc v all, ps v all, lom v lgd
# ps    lgd     lom     soc
# -1/3  -1/3    -1/3    1
# 1      -1/3     -1/3   -1/3
#  0       -1       1       0
((-1/3)*(-1/3)*-1)+((-1/3)*1*0)+((-1/3)*(-1/3)*1)+(1*(-1/3)*0)

mat4<-cbind(c(-1/3,-1/3,-1/3,1),c(1,-1/3,-1/3,-1/3),c(0,-1,1,0))
mat4


n2<-lmer(status.t.gm~bfin_c+task+(1|PerceiverID) + (1|groupID), 
         contrasts=list(task=mat4),data=s1grp)
summary(n2)

n3<-lmer(status.t.gm~bfin_c*task+(1|PerceiverID) + (1|groupID), 
         contrasts=list(task=mat2a),data=s1grp)
summary(n3)

## O ##
summary(s1grp$bfio)
s1grp$bfio_c = s1grp$bfio-3.42

o1<-lmer(status.t.gm~bfio_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(o1)

# no real hypotheses here; maybe especially helpful in PS and LOM?
# test ps v. rest; lom v. rest, lgd v. soc.
# ps    lgd     lom     soc
# 1     -1/3    -1/3    -1/3
# -1/3  -1/3     1         -1/3
#  0       -1      0       1

mat5<-cbind(c(1,-1/3,-1/3,-1/3),c(-1/3,-1/3,1,-1/3),c(0,-1,0,1))
mat5

o2<-lmer(status.t.gm~bfio_c+task+(1|PerceiverID) + (1|groupID), 
         contrasts=list(task=mat5), data=s1grp)
summary(o2)

o3<-lmer(status.t.gm~bfio_c*task+(1|PerceiverID) + (1|groupID), 
         contrasts=list(task=mat3a), data=s1grp)
summary(o3)


## bfas , w/ same contrasts ##
summary(s1grp$E_Enthus)
s1grp$e_enthus_c = s1grp$E_Enthus - 3.756
e1enth<-lmer(status.t.gm~e_enthus_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(e1enth)

# e should predict in all of them, so just use dummy codes
e2enth<-lmer(status.t.gm~e_enthus_c+taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(e2enth)

e3enth<-lmer(status.t.gm~e_enthus_c*taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(e3enth)

summary(s1grp$E_Assert)
s1grp$e_assert_c = s1grp$E_Assert - 3.438
e1ass<-lmer(status.t.gm~e_assert_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(e1ass)

# e should predict in all of them, so just use dummy codes
e2ass<-lmer(status.t.gm~e_assert_c+taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(e2ass)

e3ass<-lmer(status.t.gm~e_assert_c*taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(e3ass)

## a ##
summary(s1grp$A_Compa)
s1grp$a_compa_c = s1grp$A_Compa - 3.933

a1com<-lmer(status.t.gm~a_compa_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(a1com)

a2com<-lmer(status.t.gm~a_compa_c+task+(1|PerceiverID) + (1|groupID),
         contrasts=list(task=mat2),data=s1grp)
summary(a2com)

a3com<-lmer(status.t.gm~a_compa_c*task+(1|PerceiverID) + (1|groupID),
         contrasts=list(task=mat2a),data=s1grp)
summary(a3com)

summary(s1grp$A_Polite)
s1grp$a_pol_c = s1grp$A_Polite - 3.665

a1pol<-lmer(status.t.gm~a_pol_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(a1pol)

a2pol<-lmer(status.t.gm~a_pol_c+task+(1|PerceiverID) + (1|groupID),
            contrasts=list(task=mat2),data=s1grp)
summary(a2pol)

a3pol<-lmer(status.t.gm~a_pol_c*task+(1|PerceiverID) + (1|groupID),
            contrasts=list(task=mat2a),data=s1grp)
summary(a3pol)

## c ##
summary(s1grp$C_Indust)
s1grp$c_indust_c = s1grp$C_Indust - 3.59
        
c1ind<-lmer(status.t.gm~c_indust_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(c1ind)

c2ind<-lmer(status.t.gm~c_indust_c+task+(1|PerceiverID) + (1|groupID), 
         contrasts=list(task=mat3),data=s1grp)
summary(c2ind)


c3ind<-lmer(status.t.gm~c_indust_c*task+(1|PerceiverID) + (1|groupID), 
         contrasts=list(task=mat3a),data=s1grp)
summary(c3ind)

summary(s1grp$C_Order)
s1grp$c_order_c = s1grp$C_Order - 3.629

c1ord<-lmer(status.t.gm~c_order_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(c1ord)

c2ord<-lmer(status.t.gm~c_order_c+task+(1|PerceiverID) + (1|groupID), 
            contrasts=list(task=mat3),data=s1grp)
summary(c2ord)


c3ord<-lmer(status.t.gm~c_order_c*task+(1|PerceiverID) + (1|groupID), 
            contrasts=list(task=mat3a),data=s1grp)
summary(c3ord)


## n ##
summary(s1grp$N_Withdrawl)
s1grp$n_withd_c = s1grp$N_Withdrawl-2.464

n1with<-lmer(status.t.gm~n_withd_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(n1with)

n2wit<-lmer(status.t.gm~n_withd_c+task+(1|PerceiverID) + (1|groupID), 
         contrasts=list(task=mat4),data=s1grp)
summary(n2wit)

n3with<-lmer(status.t.gm~n_withd_c*task+(1|PerceiverID) + (1|groupID), 
         contrasts=list(task=mat2a),data=s1grp)
summary(n3with)


summary(s1grp$N_Volatile)
s1grp$n_vol_c = s1grp$N_Volatile-2.731

n1vol<-lmer(status.t.gm~n_vol_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(n1vol)

n2vol<-lmer(status.t.gm~n_vol_c+task+(1|PerceiverID) + (1|groupID), 
            contrasts=list(task=mat4),data=s1grp)
summary(n2vol)

n3vol<-lmer(status.t.gm~n_vol_c*task+(1|PerceiverID) + (1|groupID), 
             contrasts=list(task=mat2a),data=s1grp)
summary(n3vol)


## o ##
summary(s1grp$O_Intell)
s1grp$o_int_c = s1grp$O_Intell-3.51

o1intel<-lmer(status.t.gm~o_int_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(o1intel)

o2intel<-lmer(status.t.gm~o_int_c+task+(1|PerceiverID) + (1|groupID), 
         contrasts=list(task=mat5), data=s1grp)
summary(o2intel)

o3intel<-lmer(status.t.gm~o_int_c*task+(1|PerceiverID) + (1|groupID), 
         contrasts=list(task=mat3a), data=s1grp)
summary(o3intel)

summary(s1grp$O_Open)
s1grp$o_open_c = s1grp$O_Open-3.593

o1op<-lmer(status.t.gm~o_open_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(o1op)

o2op<-lmer(status.t.gm~o_open_c+task+(1|PerceiverID) + (1|groupID), 
              contrasts=list(task=mat5), data=s1grp)
summary(o2op)

o3op<-lmer(status.t.gm~o_open_c*task+(1|PerceiverID) + (1|groupID), 
              contrasts=list(task=mat3a), data=s1grp)
summary(o3op)


## dominance ##
# like e, no real hypotheses -- just run w/ LGD as ref
summary(s1grp$dominance)
s1grp$dom_c = s1grp$dominance - 3.248
dom1<-lmer(status.t.gm~dom_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(dom1)
dom2<-lmer(status.t.gm~dom_c+taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(dom2)
dom3<-lmer(status.t.gm~dom_c*taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(dom3)

## prestige ##
summary(s1grp$prestige)
s1grp$pres_c = s1grp$prestige - 5.213
pres1<-lmer(status.t.gm~pres_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(pres1)
pres2<-lmer(status.t.gm~pres_c+taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(pres2)
pres3<-lmer(status.t.gm~pres_c*taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(pres3)

## use power ##
summary(s1grp$usepower)
s1grp$usep_c = s1grp$usepower - 4.152
usep1<-lmer(status.t.gm~usep_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(usep1)
usep2<-lmer(status.t.gm~usep_c+taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(usep2)
usep3<-lmer(status.t.gm~usep_c*taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(usep3)

## sense pow ##
summary(s1grp$sensepower)
s1grp$senp_c = s1grp$sensepower - 4.852
senp1<-lmer(status.t.gm~senp_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(senp1)
senp2<-lmer(status.t.gm~senp_c+taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(senp2)
senp3<-lmer(status.t.gm~senp_c*taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(senp3)

## npi ##
summary(s1grp$npi_sum)
s1grp$npi_c = s1grp$npi_sum - 4.684
npi1<-lmer(status.t.gm~npi_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(npi1)
npi2<-lmer(status.t.gm~npi_c+taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(npi2)
npi3<-lmer(status.t.gm~npi_c*taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(npi3)

## need to belong
summary(s1grp$ntb)
s1grp$ntb_c = s1grp$ntb - 3.393

ntb1<-lmer(status.t.gm~ntb_c+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(ntb1)

ntb2<-lmer(status.t.gm~ntb_c+taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(ntb2)

ntb3<-lmer(status.t.gm~ntb_c*taskn+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(ntb3)

### run everything again, but with PS as ref ###
e3a<-lmer(status.t.gm~bfie_c*task+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(e3a)
e3aenth<-lmer(status.t.gm~e_enthus_c*task+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(e3aenth)
e3aass<-lmer(status.t.gm~e_assert_c*task+(1|PerceiverID) + (1|groupID), data=s1grp)
summary(e3aass)

a3a<-lmer(status.t.gm~bfia_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(a3a)
a3acomp<-lmer(status.t.gm~a_compa_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(a3acomp)
a3apol<-lmer(status.t.gm~a_pol_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(a3apol)

c3a<-lmer(status.t.gm~bfic_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(c3a)
c3inda<-lmer(status.t.gm~c_indust_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(c3inda)
c3orda<-lmer(status.t.gm~c_order_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(c3orda)

n3a<-lmer(status.t.gm~bfin_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(n3a)
n3awith<-lmer(status.t.gm~n_withd_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(n3awith)
n3avol<-lmer(status.t.gm~n_vol_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(n3avol)

o3a<-lmer(status.t.gm~bfio_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(o3a)
o3aint<-lmer(status.t.gm~o_int_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(o3aint)
o3aop<-lmer(status.t.gm~o_open_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(o3aop)

npi3a<-lmer(status.t.gm~npi_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(npi3a)
dom3a<-lmer(status.t.gm~dom_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(dom3a)
pres3a<-lmer(status.t.gm~pres_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(pres3a)
usep3a<-lmer(status.t.gm~usep_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(usep3a)
senp3a<-lmer(status.t.gm~senp_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(senp3a)

ntb3a<-lmer(status.t.gm~ntb_c*task+(1|PerceiverID) + (1|groupID),data=s1grp)
summary(ntb3a)


#### post-task questions ####
## first, aggregate (to use as L2 predictors of status)
taskagg<-aggregate(s1grp[7:14],by=list(s1grp$groupID),FUN=mean,na.rm=T)
head(taskagg)
colnames(taskagg)<-paste(colnames(taskagg),"agg",sep=".")

s1grp2<-merge(s1grp,taskagg,by.y="Group.1.agg",by.x="groupID")
head(s1grp2)

## center
summary(s1grp2$task_diff.agg)
s1grp2$task_diff.agg_c = s1grp2$task_diff.agg - 3.32
summary(s1grp2$task_enjoy.agg)
s1grp2$task_enjoy.agg_c = s1grp2$task_enjoy.agg - 5.751
summary(s1grp2$task_notagain.agg)
s1grp2$task_notagain.agg_c = s1grp2$task_notagain.agg - 4.733
summary(s1grp2$task_coop.agg)
s1grp2$task_coop.agg_c = s1grp2$task_coop.agg - 8.54
summary(s1grp2$task_comp.agg)
s1grp2$task_comp.agg_c = s1grp2$task_comp.agg - 2.766
summary(s1grp2$task_knowwell.agg)
s1grp2$task_knowwell.agg_c = s1grp2$task_knowwell.agg - 4.642
summary(s1grp2$task_spknow.agg)
s1grp2$task_spknow.agg_c = s1grp2$task_spknow.agg - 4.323
summary(s1grp2$task_likegrp.agg)
s1grp2$task_likegrp.agg_c = s1grp2$task_likegrp.agg - 7.545


## regressions w/ relevant post-task items

# e and a for compete
comp1e<-lmer(status.t.gm~bfie_c*task_comp.agg_c+(1|PerceiverID) + (1|groupID),
             data=s1grp2)
summary(comp1e)
comp2enth<-lmer(status.t.gm~e_enthus_c*task_comp.agg_c+(1|PerceiverID) + (1|groupID),
             data=s1grp2)
summary(comp2enth)

s1grp2$m_comp <- 6.82985 + (.48520*s1grp2$e_enthus_c)
s1grp2$hi_comp <- 6.82985 + (.48520*s1grp2$e_enthus_c) + (-.06887*1.92) +
        (-.0156*s1grp2$e_enthus_c*1.92)
s1grp2$low_comp <- 6.82985 + (.48520*s1grp2$e_enthus_c) + (-.06887*-1.92) +
        (-.0156*s1grp2$e_enthus_c*-1.92)

p<-ggplot(s1grp2,aes(e_enthus_c,m_comp))
p<-p+geom_line()+geom_line(aes(e_enthus_c,hi_comp), color="red")+
                                   geom_line(aes(e_enthus_c,low_comp),color="blue")
p
# if i did that right, enthusiasm is a stronger predictor of status in LESS
# competitive tasks

comp2ass<-lmer(status.t.gm~e_assert_c*task_comp.agg_c+(1|PerceiverID) + (1|groupID),
                data=s1grp2)
summary(comp2ass)


comp1a<-lmer(status.t.gm~bfia_c*task_comp.agg_c+(1|PerceiverID) + (1|groupID),
             data=s1grp2)
summary(comp1a)

# e and a for cooperate

coop1a<-lmer(status.t.gm~bfia_c*task_coop.agg_c+(1|PerceiverID) + (1|groupID),
             data=s1grp2)
summary(coop1a)
coop1acomp<-lmer(status.t.gm~a_compa_c*task_coop.agg_c+(1|PerceiverID) + (1|groupID),
             data=s1grp2)
summary(coop1acomp)
coop1apol<-lmer(status.t.gm~a_pol_c*task_coop.agg_c+(1|PerceiverID) + (1|groupID),
                 data=s1grp2)
summary(coop1apol)

coop1e<-lmer(status.t.gm~bfie_c*task_coop.agg_c+(1|PerceiverID) + (1|groupID),
             data=s1grp2)
summary(coop1e)
coop1eass<-lmer(status.t.gm~e_assert_c*task_coop.agg_c+(1|PerceiverID) + (1|groupID),
                 data=s1grp2)
summary(coop1eass)
coop1eenth<-lmer(status.t.gm~e_enthus_c*task_coop.agg_c+(1|PerceiverID) + (1|groupID),
                data=s1grp2)
summary(coop1eenth)

# special knowledge
spknow_c<-lmer(status.t.gm~bfic_c*task_spknow.agg_c+(1|PerceiverID) + (1|groupID),
               data=s1grp2)
summary(spknow_c)

spknow_c2<-lmer(status.t.gm~c_indust_c*task_spknow.agg_c+(1|PerceiverID) + (1|groupID),
               data=s1grp2)
summary(spknow_c2)
spknow_c3<-lmer(status.t.gm~c_order_c*task_spknow.agg_c+(1|PerceiverID) + (1|groupID),
                data=s1grp2)
summary(spknow_c3)

spknow_o<-lmer(status.t.gm~bfio_c*task_spknow.agg_c+(1|PerceiverID) + (1|groupID),
               data=s1grp2)
summary(spknow_o)

spknow_o2<-lmer(status.t.gm~o_int_c*task_spknow.agg_c+(1|PerceiverID) + (1|groupID),
                data=s1grp2)
summary(spknow_o2)
spknow_o3<-lmer(status.t.gm~o_open_c*task_spknow.agg_c+(1|PerceiverID) + (1|groupID),
                data=s1grp2)
summary(spknow_o3)

#### Simple Slopes ####
# E ps
.53+.01-.26
# Enthus ps
.55+.01-.39
# A soc v rest
.15423+(.1272)+.36457
.15423+(.1272*-1)+(.36457*-1)
# A lom vs ps
.15423+.05215-.17322
.15423+(.05215*-1)+(-.17322*-1)
# Indust ps v rest
.32525+.12857+(-.46325)
.32525+(.12857*-1)+(-.46325*-1)
# N lom v ps
-.05493+(.03814)+(.1767)
-.05493+(.03814*-1)+(.1767*-1)
# intell ps v soc
0.32809+(-.12391)+(.45581)
0.32809+(-.12391*-1)+(.45581*-1)


### ps as referent
## enthus
#lgd
.16227+(-.01257)+.38727
#lom
.16227-.10625+.62723
## A
#lgd
-.1405+(-.007)+.37552
#lom
-.1405+(-0.1043)+.34644
#social
-.1405+0.07242+.45702
## C
#lom
.151328-.093051+.361727
##N
#lom
.10943-.07628-.3534

## O
# soc
.5+.07589-.53337

## dominance
#LOM
.158175-.089652-.230394
#social
.158175+.076633-.248407
#### Stability ####
# restructure to be wide by TASK
library(reshape2)

x<-data.frame(s1grp[1:6],s1grp[335], s1grp[152], s1grp[158], s1grp[166], 
              s1grp[128], s1grp[124], s1grp[140])

s1grp.wide.task<-reshape(x,timevar="taskn", idvar="PerceiverID", direction="wide")
head(s1grp.wide.task)

s1grp.wide.task2<-data.frame(s1grp.wide.task[7:12],s1grp.wide.task[18:23],
                             s1grp.wide.task[29:34],s1grp.wide.task[40:45])
head(s1grp.wide.task2)

rtask<-cor(s1grp.wide.task2, use="pairwise.complete.obs")
write.csv(rtask,"likerespstat rs across task.csv")

# wide by TIME
s1grp.wide.time<-reshape(x,timevar="Group", idvar="PerceiverID", direction="wide")
head(s1grp.wide.time)

s1grp.wide.time2<-data.frame(s1grp.wide.time[7:12],s1grp.wide.time[18:23],
                             s1grp.wide.time[29:34],s1grp.wide.time[40:45])
head(s1grp.wide.time2)

rtime<-cor(s1grp.wide.time2, use="pairwise.complete.obs")
write.csv(rtime,"likerespstat rs across time.csv")

### self-report corrs
names(s1grp)
sr<-data.frame(s1grp[304:330])
head(sr)

write.csv(cor(sr,use="pairwise.complete.obs"), "s1 sr rs.csv")

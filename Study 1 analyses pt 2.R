#### Aim 3 Analyses ####

# get IDs w/ informants
s1inf<-read.csv("Study 1 IDs long by informant.csv")
head(s1inf)
summary(s1inf$IDonly)
s1inf$lab<-is.na(s1inf$IDonly)
summary(s1inf$lab)

s1inf.l<-s1inf[s1inf$lab=="TRUE",]
head(s1inf.l)
# aggregate personality items across informants
inf.agg<-aggregate(c(s1inf.l[98],s1inf.l[94],s1inf.l[117:135]),by=list(s1inf.l$perceiverID),FUN=mean,na.rm=T)
head(inf.agg)
colnames(inf.agg)<-paste(colnames(inf.agg),"agg",sep=".")
colnames(inf.agg)[1]<-"PerceiverID"

# merge w/ other file
s1grp3<-merge(s1grp2,inf.agg,by="PerceiverID")
head(s1grp3)


#### Informants vs. Selves vs. Peers predicting status: Rs ####
## correlations b/t status and informant reports
# whole sample
names(s1grp3)
isprs<-data.frame(s1grp3[166],s1grp3[72:165], s1grp3[167:189], s1grp3[309:330],
                  s1grp3[377:397])
head(isprs)
allr<-cor(isprs,use="pairwise.complete.obs")
write.csv(allr,"InfSelfPeer pred status.csv")

x<-data.frame(s1grp3[166], s1grp3[329:330])
cor(x,use="pairwise.complete.obs")
# by task
#ps
s1grp3ps<-s1grp3[s1grp3$taskn=="PS",]
isprs_ps<-data.frame(s1grp3ps[166],s1grp3ps[72:165], s1grp3ps[167:189], 
                     s1grp3ps[309:330],s1grp3ps[377:397])
head(isprs_ps)
psr<-cor(isprs_ps,use="pairwise.complete.obs")
write.csv(psr,"InfSelfPeer pred status_PS.csv")

x<-data.frame(s1grp3ps[166], s1grp3ps[329:330])
cor(x,use="pairwise.complete.obs")

#lgd
s1grp3lgd<-s1grp3[s1grp3$taskn=="LGD",]
isprs_lgd<-data.frame(s1grp3lgd[166],s1grp3lgd[72:165], s1grp3lgd[167:189], 
                      s1grp3lgd[309:330],s1grp3lgd[377:397])
head(isprs_lgd)
lgdr<-cor(isprs_lgd,use="pairwise.complete.obs")
write.csv(lgdr,"InfSelfPeer pred status_LGD.csv")

x<-data.frame(s1grp3lgd[166], s1grp3lgd[329:330])
cor(x,use="pairwise.complete.obs")
#lom
s1grp3lom<-s1grp3[s1grp3$taskn=="LOM",]
isprs_lom<-data.frame(s1grp3lom[166],s1grp3lom[72:165], s1grp3lom[167:189], 
                      s1grp3lom[309:330],s1grp3lom[377:397])
head(isprs_lom)
lomr<-cor(isprs_lom,use="pairwise.complete.obs")
write.csv(lomr,"InfSelfPeer pred status_LOM.csv")

x<-data.frame(s1grp3lom[166], s1grp3lom[329:330])
cor(x,use="pairwise.complete.obs")

#SOC
s1grp3soc<-s1grp3[s1grp3$taskn=="SOC",]
isprs_soc<-data.frame(s1grp3soc[166],s1grp3soc[72:165], s1grp3soc[167:189], 
                      s1grp3soc[309:330],s1grp3soc[377:397])
head(isprs_soc)
socr<-cor(isprs_soc,use="pairwise.complete.obs")
write.csv(socr,"InfSelfPeer pred status_SOC.csv")

x<-data.frame(s1grp3soc[166], s1grp3soc[329:330])
cor(x,use="pairwise.complete.obs")

#### Self vs. Other predict Status Attainment ####
## Just B5; separate analyses for each task (group)
## Because using an aggregate (.t) for peers, also just use aggregate for 
## informants

## Center informant vars
summary(s1grp3[383:387])
s1grp3$inf.agg.e_c = s1grp3$inf.bfie.agg - 3.61
s1grp3$inf.agg.a_c = s1grp3$inf.bfia.agg - 3.874
s1grp3$inf.agg.c_c = s1grp3$inf.bfic.agg - 3.751
s1grp3$inf.agg.n_c = s1grp3$inf.bfin.agg - 2.769
s1grp3$inf.agg.o_c = s1grp3$inf.bfio.agg - 3.917


## Whole Sample

# E

e.s<-lmer(status.t.gm~bfie_c+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(e.s)
e.inf<-lmer(status.t.gm~inf.agg.e_c+(1|PerceiverID) + (1|groupID),
             data=s1grp3)
summary(e.inf)

e.peer<-lmer(status.t.gm~ext.t+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(e.peer)

e.ip<-lmer(status.t.gm~ext.t+inf.agg.e_c+(1|PerceiverID) + (1|groupID),
           data=s1grp3)
summary(e.ip)
e.is<-lmer(status.t.gm~inf.agg.e_c+bfie_c+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(e.is)

e.ips<-lmer(status.t.gm~ext.t.gm+inf.agg.e_c+bfie_c+(1|PerceiverID) + (1|groupID),
           data=s1grp3)
summary(e.ips)

# A
a.s<-lmer(status.t.gm~bfia_c+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(a.s)
a.inf<-lmer(status.t.gm~inf.agg.a_c+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(a.inf)
a.peer<-lmer(status.t.gm~agr.t+(1|PerceiverID) + (1|groupID),
             data=s1grp3)
summary(a.peer)
a.ip<-lmer(status.t.gm~agr.t+inf.agg.a_c+(1|PerceiverID) + (1|groupID),
           data=s1grp3)
summary(a.ip)
a.is<-lmer(status.t.gm~inf.agg.a_c+bfia_c+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(a.is)
a.ips<-lmer(status.t.gm~agr.t+inf.agg.a_c+bfia_c+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(a.ips)


# C
c.s<-lmer(status.t.gm~bfic_c+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(c.s)

c.inf<-lmer(status.t.gm~inf.agg.c_c+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(c.inf)
c.peer<-lmer(status.t.gm~con.t+(1|PerceiverID) + (1|groupID),
             data=s1grp3)
summary(c.peer)
c.ip<-lmer(status.t.gm~con.t+inf.agg.c_c+(1|PerceiverID) + (1|groupID),
           data=s1grp3)
summary(c.ip)
c.is<-lmer(status.t.gm~inf.agg.c_c+bfic_c+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(c.is)
c.ips<-lmer(status.t.gm~con.t+inf.agg.c_c+bfic_c+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(c.ips)


# N
n.s<-lmer(status.t.gm~bfin_c+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(n.s)

n.inf<-lmer(status.t.gm~inf.agg.n_c+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(n.inf)
n.peer<-lmer(status.t.gm~neu.t+(1|PerceiverID) + (1|groupID),
             data=s1grp3)
summary(n.peer)
n.ip<-lmer(status.t.gm~neu.t_c+inf.agg.n_c+(1|PerceiverID) + (1|groupID),
           data=s1grp3)
summary(n.ip)
summary(s1grp3$neu.t.gm)
s1grp3$neu.t_c = s1grp3$neu.t.gm-4.311

n.is<-lmer(status.t.gm~inf.agg.n_c+bfin_c+(1|PerceiverID) + (1|groupID),
           data=s1grp3)
summary(n.is)
n.ips<-lmer(status.t.gm~neu.t+inf.agg.n_c+bfin_c+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(n.ips)



# O
o.s<-lmer(status.t.gm~bfio_c+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(o.s)

o.inf<-lmer(status.t.gm~inf.agg.o_c+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(o.inf)
o.peer<-lmer(status.t.gm~ope.t+(1|PerceiverID) + (1|groupID),
             data=s1grp3)
summary(o.peer)
o.ip<-lmer(status.t.gm~ope.t+inf.agg.o_c+(1|PerceiverID) + (1|groupID),
           data=s1grp3)
summary(o.ip)
o.is<-lmer(status.t.gm~inf.agg.o_c+bfio_c+bfio_c+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(o.is)
o.ips<-lmer(status.t.gm~ope.t+inf.agg.o_c+bfio_c+bfio_c+(1|PerceiverID) + (1|groupID),
            data=s1grp3)
summary(o.ips)


## PS
# for these, only 1 observation per person, so can't/don't need to account
# for multiple entries for each person (i.e., no percieverID variance)

# E
e.s<-lmer(status.t.gm~bfie_c+(1|groupID), data=s1grp3ps)
summary(e.s)
e.inf<-lmer(status.t.gm~inf.agg.e_c + (1|groupID),data=s1grp3ps)
summary(e.inf)

e.peer<-lmer(status.t.gm~ext.t+ (1|groupID), data=s1grp3ps)
summary(e.peer)

e.ip<-lmer(status.t.gm~ext.t+inf.agg.e_c+ (1|groupID),data=s1grp3ps)
summary(e.ip)
e.is<-lmer(status.t.gm~inf.agg.e_c+bfie_c+ (1|groupID),data=s1grp3ps)
summary(e.is)

e.ips<-lmer(status.t.gm~ext.t+inf.agg.e_c+bfie_c + (1|groupID), data=s1grp3ps)
summary(e.ips)

# A
a.s<-lmer(status.t.gm~bfia_c + (1|groupID), data=s1grp3ps)
summary(a.s)
a.inf<-lmer(status.t.gm~inf.agg.a_c+ (1|groupID), data=s1grp3ps)
summary(a.inf)
a.peer<-lmer(status.t.gm~agr.t+ (1|groupID),data=s1grp3ps)
summary(a.peer)
a.ip<-lmer(status.t.gm~agr.t+inf.agg.a_c + (1|groupID),data=s1grp3ps)
summary(a.ip)
a.is<-lmer(status.t.gm~inf.agg.a_c+bfia_c + (1|groupID), data=s1grp3ps)
summary(a.is)
a.ips<-lmer(status.t.gm~agr.t+inf.agg.a_c+bfia_c + (1|groupID), data=s1grp3ps)
summary(a.ips)


# C
c.s<-lmer(status.t.gm~bfic_c+ (1|groupID), data=s1grp3ps)
summary(c.s)

c.inf<-lmer(status.t.gm~inf.agg.c_c + (1|groupID),data=s1grp3ps)
summary(c.inf)
c.peer<-lmer(status.t.gm~con.t + (1|groupID),data=s1grp3ps)
summary(c.peer)
c.ip<-lmer(status.t.gm~con.t+inf.agg.c_c + (1|groupID),data=s1grp3ps)
summary(c.ip)
c.is<-lmer(status.t.gm~inf.agg.c_c+bfic_c + (1|groupID),data=s1grp3ps)
summary(c.is)
c.ips<-lmer(status.t.gm~con.t+inf.agg.c_c+bfic_c + (1|groupID),data=s1grp3ps)
summary(c.ips)


# N
n.s<-lmer(status.t.gm~bfin_c + (1|groupID), data=s1grp3ps)
summary(n.s)

n.inf<-lmer(status.t.gm~inf.agg.n_c + (1|groupID),data=s1grp3ps)
summary(n.inf)
n.peer<-lmer(status.t.gm~neu.t + (1|groupID),data=s1grp3ps)
summary(n.peer)
n.ip<-lmer(status.t.gm~neu.t+inf.agg.n_c + (1|groupID), data=s1grp3ps)
summary(n.ip)
n.is<-lmer(status.t.gm~inf.agg.n_c+bfin_c+ (1|groupID),data=s1grp3ps)
summary(n.is)
n.ips<-lmer(status.t.gm~neu.t+inf.agg.n_c+bfin_c+(1|groupID),data=s1grp3ps)
summary(n.ips)



# O
o.s<-lmer(status.t.gm~bfio_c+ (1|groupID),data=s1grp3ps)
summary(o.s)

o.inf<-lmer(status.t.gm~inf.agg.o_c+ (1|groupID),data=s1grp3ps)
summary(o.inf)
o.peer<-lmer(status.t.gm~ope.t + (1|groupID),data=s1grp3ps)
summary(o.peer)
o.ip<-lmer(status.t.gm~ope.t+inf.agg.o_c+ (1|groupID),data=s1grp3ps)
summary(o.ip)
o.is<-lmer(status.t.gm~inf.agg.o_c+bfio_c+bfio_c+ (1|groupID), data=s1grp3ps)
summary(o.is)
o.ips<-lmer(status.t.gm~ope.t+inf.agg.o_c+bfio_c+bfio_c+ (1|groupID), data=s1grp3ps)
summary(o.ips)


## LGD
# for these, only 1 observation per person, so can't/don't need to account
# for multiple entries for each person (i.e., no percieverID variance)

# E
e.s<-lmer(status.t.gm~bfie_c+(1|groupID), data=s1grp3lgd)
summary(e.s)
e.inf<-lmer(status.t.gm~inf.agg.e_c + (1|groupID),data=s1grp3lgd)
summary(e.inf)

e.peer<-lmer(status.t.gm~ext.t+ (1|groupID), data=s1grp3lgd)
summary(e.peer)

e.ip<-lmer(status.t.gm~ext.t+inf.agg.e_c+ (1|groupID),data=s1grp3lgd)
summary(e.ip)
e.is<-lmer(status.t.gm~inf.agg.e_c+bfie_c+ (1|groupID),data=s1grp3lgd)
summary(e.is)

e.ips<-lmer(status.t.gm~ext.t+inf.agg.e_c+bfie_c + (1|groupID), data=s1grp3lgd)
summary(e.ips)

# A
a.s<-lmer(status.t.gm~bfia_c + (1|groupID), data=s1grp3lgd)
summary(a.s)
a.inf<-lmer(status.t.gm~inf.agg.a_c+ (1|groupID), data=s1grp3lgd)
summary(a.inf)
a.peer<-lmer(status.t.gm~agr.t+ (1|groupID),data=s1grp3lgd)
summary(a.peer)
a.ip<-lmer(status.t.gm~agr.t+inf.agg.a_c + (1|groupID),data=s1grp3lgd)
summary(a.ip)
a.is<-lmer(status.t.gm~inf.agg.a_c+bfia_c + (1|groupID), data=s1grp3lgd)
summary(a.is)
a.ips<-lmer(status.t.gm~agr.t+inf.agg.a_c+bfia_c + (1|groupID), data=s1grp3lgd)
summary(a.ips)


## C
c.s<-lmer(status.t.gm~bfic_c+ (1|groupID), data=s1grp3lgd)
summary(c.s)

c.inf<-lmer(status.t.gm~inf.agg.c_c + (1|groupID),data=s1grp3lgd)
summary(c.inf)
c.peer<-lmer(status.t.gm~con.t + (1|groupID),data=s1grp3lgd)
summary(c.peer)
c.ip<-lmer(status.t.gm~con.t+inf.agg.c_c + (1|groupID),data=s1grp3lgd)
summary(c.ip)
c.is<-lmer(status.t.gm~inf.agg.c_c+bfic_c + (1|groupID),data=s1grp3lgd)
summary(c.is)
c.ips<-lmer(status.t.gm~con.t+inf.agg.c_c+bfic_c + (1|groupID),data=s1grp3lgd)
summary(c.ips)


# N
n.s<-lmer(status.t.gm~bfin_c + (1|groupID), data=s1grp3lgd)
summary(n.s)

n.inf<-lmer(status.t.gm~inf.agg.n_c + (1|groupID),data=s1grp3lgd)
summary(n.inf)
n.peer<-lmer(status.t.gm~neu.t + (1|groupID),data=s1grp3lgd)
summary(n.peer)
n.ip<-lmer(status.t.gm~neu.t+inf.agg.n_c + (1|groupID), data=s1grp3lgd)
summary(n.ip)
n.is<-lmer(status.t.gm~inf.agg.n_c+bfin_c+ (1|groupID),data=s1grp3lgd)
summary(n.is)
n.ips<-lmer(status.t.gm~neu.t+inf.agg.n_c+bfin_c+(1|groupID),data=s1grp3lgd)
summary(n.ips)



# O
o.s<-lmer(status.t.gm~bfio_c+ (1|groupID),data=s1grp3lgd)
summary(o.s)

o.inf<-lmer(status.t.gm~inf.agg.o_c+ (1|groupID),data=s1grp3lgd)
summary(o.inf)
o.peer<-lmer(status.t.gm~ope.t + (1|groupID),data=s1grp3ps)
summary(o.peer)
o.ip<-lmer(status.t.gm~ope.t+inf.agg.o_c+ (1|groupID),data=s1grp3lgd)
summary(o.ip)
o.is<-lmer(status.t.gm~inf.agg.o_c+bfio_c+bfio_c+ (1|groupID), data=s1grp3lgd)
summary(o.is)
o.ips<-lmer(status.t.gm~ope.t+inf.agg.o_c+bfio_c+bfio_c+ (1|groupID), data=s1grp3lgd)
summary(o.ips)

## LOM
# for these, only 1 observation per person, so can't/don't need to account
# for multiple entries for each person (i.e., no percieverID variance)

# E
e.s<-lmer(status.t.gm~bfie_c+(1|groupID), data=s1grp3lom)
summary(e.s)
e.inf<-lmer(status.t.gm~inf.agg.e_c + (1|groupID),data=s1grp3lom)
summary(e.inf)

e.peer<-lmer(status.t.gm~ext.t+ (1|groupID), data=s1grp3lom)
summary(e.peer)

e.ip<-lmer(status.t.gm~ext.t+inf.agg.e_c+ (1|groupID),data=s1grp3lom)
summary(e.ip)
e.is<-lmer(status.t.gm~inf.agg.e_c+bfie_c+ (1|groupID),data=s1grp3lom)
summary(e.is)

e.ips<-lmer(status.t.gm~ext.t+inf.agg.e_c+bfie_c + (1|groupID), data=s1grp3lom)
summary(e.ips)

# A
a.s<-lmer(status.t.gm~bfia_c + (1|groupID), data=s1grp3lom)
summary(a.s)
a.inf<-lmer(status.t.gm~inf.agg.a_c+ (1|groupID), data=s1grp3lom)
summary(a.inf)
a.peer<-lmer(status.t.gm~agr.t+ (1|groupID),data=s1grp3lom)
summary(a.peer)
a.ip<-lmer(status.t.gm~agr.t+inf.agg.a_c + (1|groupID),data=s1grp3lom)
summary(a.ip)
a.is<-lmer(status.t.gm~inf.agg.a_c+bfia_c + (1|groupID), data=s1grp3lom)
summary(a.is)
a.ips<-lmer(status.t.gm~agr.t+inf.agg.a_c+bfia_c + (1|groupID), data=s1grp3lom)
summary(a.ips)


## C
c.s<-lmer(status.t.gm~bfic_c+ (1|groupID), data=s1grp3lom)
summary(c.s)

c.inf<-lmer(status.t.gm~inf.agg.c_c + (1|groupID),data=s1grp3lom)
summary(c.inf)
c.peer<-lmer(status.t.gm~con.t + (1|groupID),data=s1grp3lom)
summary(c.peer)
c.ip<-lmer(status.t.gm~con.t+inf.agg.c_c + (1|groupID),data=s1grp3lom)
summary(c.ip)
c.is<-lmer(status.t.gm~inf.agg.c_c+bfic_c + (1|groupID),data=s1grp3lom)
summary(c.is)
c.ips<-lmer(status.t.gm~con.t+inf.agg.c_c+bfic_c + (1|groupID),data=s1grp3lom)
summary(c.ips)


# N
n.s<-lmer(status.t.gm~bfin_c + (1|groupID), data=s1grp3lom)
summary(n.s)

n.inf<-lmer(status.t.gm~inf.agg.n_c + (1|groupID),data=s1grp3lom)
summary(n.inf)
n.peer<-lmer(status.t.gm~neu.t + (1|groupID),data=s1grp3lom)
summary(n.peer)
n.ip<-lmer(status.t.gm~neu.t+inf.agg.n_c + (1|groupID), data=s1grp3lom)
summary(n.ip)
n.is<-lmer(status.t.gm~inf.agg.n_c+bfin_c+ (1|groupID),data=s1grp3lom)
summary(n.is)
n.ips<-lmer(status.t.gm~neu.t+inf.agg.n_c+bfin_c+(1|groupID),data=s1grp3lom)
summary(n.ips)



# O
o.s<-lmer(status.t.gm~bfio_c+ (1|groupID),data=s1grp3lom)
summary(o.s)

o.inf<-lmer(status.t.gm~inf.agg.o_c+ (1|groupID),data=s1grp3lom)
summary(o.inf)
o.peer<-lmer(status.t.gm~ope.t + (1|groupID),data=s1grp3lom)
summary(o.peer)
o.ip<-lmer(status.t.gm~ope.t+inf.agg.o_c+ (1|groupID),data=s1grp3lom)
summary(o.ip)
o.is<-lmer(status.t.gm~inf.agg.o_c+bfio_c+ (1|groupID), data=s1grp3lom)
summary(o.is)
o.ips<-lmer(status.t.gm~ope.t+inf.agg.o_c+bfio_c+ (1|groupID), data=s1grp3lom)
summary(o.ips)

## Social
# for these, only 1 observation per person, so can't/don't need to account
# for multiple entries for each person (i.e., no percieverID variance)

# E
e.s<-lmer(status.t.gm~bfie_c+(1|groupID), data=s1grp3soc)
summary(e.s)
e.inf<-lmer(status.t.gm~inf.agg.e_c + (1|groupID),data=s1grp3soc)
summary(e.inf)

e.peer<-lmer(status.t.gm~ext.t+ (1|groupID), data=s1grp3soc)
summary(e.peer)

e.ip<-lmer(status.t.gm~ext.t+inf.agg.e_c+ (1|groupID),data=s1grp3soc)
summary(e.ip)
e.is<-lmer(status.t.gm~inf.agg.e_c+bfie_c+ (1|groupID),data=s1grp3soc)
summary(e.is)

e.ips<-lmer(status.t.gm~ext.t+inf.agg.e_c+bfie_c + (1|groupID), data=s1grp3soc)
summary(e.ips)

# A
a.s<-lmer(status.t.gm~bfia_c + (1|groupID), data=s1grp3soc)
summary(a.s)
a.inf<-lmer(status.t.gm~inf.agg.a_c+ (1|groupID), data=s1grp3soc)
summary(a.inf)
a.peer<-lmer(status.t.gm~agr.t+ (1|groupID),data=s1grp3soc)
summary(a.peer)
a.ip<-lmer(status.t.gm~agr.t+inf.agg.a_c + (1|groupID),data=s1grp3soc)
summary(a.ip)
a.is<-lmer(status.t.gm~inf.agg.a_c+bfia_c + (1|groupID), data=s1grp3soc)
summary(a.is)
a.ips<-lmer(status.t.gm~agr.t+inf.agg.a_c+bfia_c + (1|groupID), data=s1grp3soc)
summary(a.ips)


## C
c.s<-lmer(status.t.gm~bfic_c+ (1|groupID), data=s1grp3soc)
summary(c.s)

c.inf<-lmer(status.t.gm~inf.agg.c_c + (1|groupID),data=s1grp3soc)
summary(c.inf)
c.peer<-lmer(status.t.gm~con.t + (1|groupID),data=s1grp3soc)
summary(c.peer)
c.ip<-lmer(status.t.gm~con.t+inf.agg.c_c + (1|groupID),data=s1grp3soc)
summary(c.ip)
c.is<-lmer(status.t.gm~inf.agg.c_c+bfic_c + (1|groupID),data=s1grp3soc)
summary(c.is)
c.ips<-lmer(status.t.gm~con.t+inf.agg.c_c+bfic_c + (1|groupID),data=s1grp3soc)
summary(c.ips)


# N
n.s<-lmer(status.t.gm~bfin_c + (1|groupID), data=s1grp3soc)
summary(n.s)

n.inf<-lmer(status.t.gm~inf.agg.n_c + (1|groupID),data=s1grp3soc)
summary(n.inf)
n.peer<-lmer(status.t.gm~neu.t + (1|groupID),data=s1grp3soc)
summary(n.peer)
n.ip<-lmer(status.t.gm~neu.t+inf.agg.n_c + (1|groupID), data=s1grp3soc)
summary(n.ip)
n.is<-lmer(status.t.gm~inf.agg.n_c+bfin_c+ (1|groupID),data=s1grp3soc)
summary(n.is)
n.ips<-lmer(status.t.gm~neu.t+inf.agg.n_c+bfin_c+(1|groupID),data=s1grp3soc)
summary(n.ips)



# O
o.s<-lmer(status.t.gm~bfio_c+ (1|groupID),data=s1grp3soc)
summary(o.s)

o.inf<-lmer(status.t.gm~inf.agg.o_c+ (1|groupID),data=s1grp3soc)
summary(o.inf)
o.peer<-lmer(status.t.gm~ope.t + (1|groupID),data=s1grp3soc)
summary(o.peer)
o.ip<-lmer(status.t.gm~ope.t+inf.agg.o_c+ (1|groupID),data=s1grp3soc)
summary(o.ip)
o.is<-lmer(status.t.gm~inf.agg.o_c+bfio_c+ (1|groupID), data=s1grp3soc)
summary(o.is)
o.ips<-lmer(status.t.gm~ope.t+inf.agg.o_c+bfio_c+ (1|groupID), data=s1grp3soc)
summary(o.ips)

#### Self-Other Agreement Moderated by Status ####
# for simplicity, just using pre-lab self-reports, B5

# Whole Sample
# these might be wrong; models wouldn't converge w/ rdn effects;
# issue is that DV isn't rep, but status item is?
e.inf<-lmer(bfie~inf.agg.e_c*status.t + (1|groupID),data=s1grp3)
summary(e.inf)
a.inf<-lmer(bfia~inf.agg.a_c*status.t+ (1|groupID),data=s1grp3)
summary(a.inf)
c.inf<-lmer(bfic~inf.agg.c_c*status.t +(1|groupID),data=s1grp3)
summary(c.inf)
n.inf<-lmer(bfin~inf.agg.n_c*status.t +(1|groupID),data=s1grp3)
summary(n.inf)
o.inf<-lmer(bfio~inf.agg.o_c*status.t + (1|groupID),data=s1grp3)
summary(o.inf)

e.p<-lmer(bfie~ext.t*status.t+(1|groupID),data=s1grp3)
summary(e.p)
a.p<-lmer(bfia~agr.t*status.t+(1|groupID),data=s1grp3)
summary(a.p)
c.p<-lmer(bfic~con.t*status.t + (1|groupID),data=s1grp3)
summary(c.p)
n.p<-lmer(bfin~neu.t*status.t + (1|groupID),data=s1grp3)
summary(n.p)
o.p<-lmer(bfio~ope.t*status.t + (1|groupID),data=s1grp3)
summary(o.p)


# PS
e.inf<-lm(bfie~inf.agg.e_c*status.t,data=s1grp3ps)
summary(e.inf)
a.inf<-lm(bfia~inf.agg.a_c*status.t,data=s1grp3ps)
summary(a.inf)
c.inf<-lm(bfic~inf.agg.c_c*status.t,data=s1grp3ps)
summary(c.inf)
n.inf<-lm(bfin~inf.agg.n_c*status.t,data=s1grp3ps)
summary(n.inf)
o.inf<-lm(bfio~inf.agg.o_c*status.t,data=s1grp3ps)
summary(o.inf)

e.p<-lm(bfie~ext.t*status.t,data=s1grp3ps)
summary(e.p)
a.p<-lm(bfia~agr.t*status.t,data=s1grp3ps)
summary(a.p)
c.p<-lm(bfic~con.t*status.t,data=s1grp3ps)
summary(c.p)
n.p<-lm(bfin~neu.t*status.t,data=s1grp3ps)
summary(n.p)
o.p<-lm(bfio~ope.t*status.t,data=s1grp3ps)
summary(o.p)

# LGD
e.inf<-lm(bfie~inf.agg.e_c*status.t,data=s1grp3lgd)
summary(e.inf)
a.inf<-lm(bfia~inf.agg.a_c*status.t,data=s1grp3lgd)
summary(a.inf)
c.inf<-lm(bfic~inf.agg.c_c*status.t,data=s1grp3lgd)
summary(c.inf)
n.inf<-lm(bfin~inf.agg.n_c*status.t,data=s1grp3lgd)
summary(n.inf)
o.inf<-lm(bfio~inf.agg.o_c*status.t,data=s1grp3lgd)
summary(o.inf)

e.p<-lm(bfie~ext.t*status.t,data=s1grp3lgd)
summary(e.p)
a.p<-lm(bfia~agr.t*status.t,data=s1grp3lgd)
summary(a.p)
c.p<-lm(bfic~con.t*status.t,data=s1grp3lgd)
summary(c.p)
n.p<-lm(bfin~neu.t*status.t,data=s1grp3lgd)
summary(n.p)
o.p<-lm(bfio~ope.t*status.t,data=s1grp3lgd)
summary(o.p)

# LOM
e.inf<-lm(bfie~inf.agg.e_c*status.t,data=s1grp3lom)
summary(e.inf)
a.inf<-lm(bfia~inf.agg.a_c*status.t,data=s1grp3lom)
summary(a.inf)
c.inf<-lm(bfic~inf.agg.c_c*status.t,data=s1grp3lom)
summary(c.inf)
n.inf<-lm(bfin~inf.agg.n_c*status.t,data=s1grp3lom)
summary(n.inf)
o.inf<-lm(bfio~inf.agg.o_c*status.t,data=s1grp3lom)
summary(o.inf)

e.p<-lm(bfie~ext.t*status.t,data=s1grp3lom)
summary(e.p)
a.p<-lm(bfia~agr.t*status.t,data=s1grp3lom)
summary(a.p)
c.p<-lm(bfic~con.t*status.t,data=s1grp3lom)
summary(c.p)
n.p<-lm(bfin~neu.t*status.t,data=s1grp3lom)
summary(n.p)
o.p<-lm(bfio~ope.t*status.t,data=s1grp3lom)
summary(o.p)


# Social
e.inf<-lm(bfie~inf.agg.e_c*status.t,data=s1grp3soc)
summary(e.inf)
a.inf<-lm(bfia~inf.agg.a_c*status.t,data=s1grp3soc)
summary(a.inf)
c.inf<-lm(bfic~inf.agg.c_c*status.t,data=s1grp3soc)
summary(c.inf)
n.inf<-lm(bfin~inf.agg.n_c*status.t,data=s1grp3soc)
summary(n.inf)
o.inf<-lm(bfio~inf.agg.o_c*status.t,data=s1grp3soc)
summary(o.inf)

e.p<-lm(bfie~ext.t*status.t,data=s1grp3soc)
summary(e.p)
a.p<-lm(bfia~agr.t*status.t,data=s1grp3soc)
summary(a.p)
c.p<-lm(bfic~con.t*status.t,data=s1grp3soc)
summary(c.p)
n.p<-lm(bfin~neu.t*status.t,data=s1grp3soc)
summary(n.p)
o.p<-lm(bfio~ope.t*status.t,data=s1grp3soc)
summary(o.p)

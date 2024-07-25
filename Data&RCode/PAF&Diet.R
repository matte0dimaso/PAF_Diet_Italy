################################################################################
# DATA OF EACH PAIR OF DIETARY FACTOR AND CANCER SITE TO COMPUTE AF
################################################################################
DIET<-read.table("[YourPath]\\AfData.csv",
                 header=T,
                 dec=".",
                 sep=";"
                )

# Log RR for an increase of 1 g/day of dietary factor
DIET$logRR.1g[
              DIET$DF=="Dai"|
              DIET$DF=="Nsv"|
              DIET$DF=="Fru"|
              DIET$DF=="Fib"|
              DIET$DF=="Cfr"|
              DIET$DF=="Cof"
             ]<-log(1/DIET$RR[
                              DIET$DF=="Dai"|
                              DIET$DF=="Nsv"|
                              DIET$DF=="Fru"|
                              DIET$DF=="Fib"|
                              DIET$DF=="Cfr"|
                              DIET$DF=="Cof"
                             ])/
             DIET$RR.div[
                         DIET$DF=="Dai"|
                         DIET$DF=="Nsv"|
                         DIET$DF=="Fru"|
                         DIET$DF=="Fib"|
                         DIET$DF=="Cfr"|
                         DIET$DF=="Cof"
                        ]

DIET$logRR.1g[
              DIET$DF=="Pme"|
              DIET$DF=="Rme"
             ]<-log(DIET$RR[
                            DIET$DF=="Pme"|
                            DIET$DF=="Rme"
                           ])/
             DIET$RR.div[
                         DIET$DF=="Pme"|
                         DIET$DF=="Rme"
                        ]
# Log SE of risk estimate of 1 g/day of dietary factor
DIET$logSE.1g[
              DIET$DF=="Dai"|
              DIET$DF=="Nsv"|
              DIET$DF=="Fru"|
              DIET$DF=="Fib"|
              DIET$DF=="Cfr"|
              DIET$DF=="Cof"
             ]<-((log(1/DIET$RR.lowCI[
                                      DIET$DF=="Dai"|
                                      DIET$DF=="Nsv"|
                                      DIET$DF=="Fru"|
                                      DIET$DF=="Fib"|
                                      DIET$DF=="Cfr"|
                                      DIET$DF=="Cof"
                                     ])/
                DIET$RR.div[
                            DIET$DF=="Dai"|
                            DIET$DF=="Nsv"|
                            DIET$DF=="Fru"|
                            DIET$DF=="Fib"|
                            DIET$DF=="Cfr"|
                            DIET$DF=="Cof"
                           ])-
                (log(1/DIET$RR.highCI[
                                      DIET$DF=="Dai"|
                                      DIET$DF=="Nsv"|
                                      DIET$DF=="Fru"|
                                      DIET$DF=="Fib"|
                                      DIET$DF=="Cfr"|
                                      DIET$DF=="Cof"
                                     ])/
                DIET$RR.div[
                            DIET$DF=="Dai"|
                            DIET$DF=="Nsv"|
                            DIET$DF=="Fru"|
                            DIET$DF=="Fib"|
                            DIET$DF=="Cfr"|
                            DIET$DF=="Cof"
                           ]))/(1.96*2)

DIET$logSE.1g[DIET$DF=="Pme"|
              DIET$DF=="Rme"
             ]<-((log(DIET$RR.highCI[
                                     DIET$DF=="Pme"|
                                     DIET$DF=="Rme"
                                    ])/
                DIET$RR.div[
                            DIET$DF=="Pme"|
                            DIET$DF=="Rme"
                           ])-
                (log(DIET$RR.lowCI[
                                   DIET$DF=="Pme"|
                                   DIET$DF=="Rme"
                                  ])/
                DIET$RR.div[
                            DIET$DF=="Pme"|
                            DIET$DF=="Rme"
                           ]))/(1.96*2)

# Proportion of consumers and no consumers for each dietary factor among women
DIET$pro.con.w<-DIET$n.con.w/DIET$n.w
DIET$pro.ncon.w<-1-DIET$pro.con.w

# Proportion of consumers and no consumers for each dietary factor among men
DIET$pro.con.m<-DIET$n.con.m/DIET$n.m
DIET$pro.ncon.m<-1-DIET$pro.con.m

# Shape and scale parameters for gamma distribution among women
DIET$kappa.DF.w<-(DIET$mean.DF.w/DIET$sd.DF.w)^2
DIET$theta.DF.w<-DIET$sd.DF.w^2/DIET$mean.DF.w

# Shape and scale parameters for gamma distribution among men
DIET$kappa.DF.m<-(DIET$mean.DF.m/DIET$sd.DF.m)^2
DIET$theta.DF.m<-DIET$sd.DF.m^2/DIET$mean.DF.m

################################################################################
#
# COUNTERFACTUAL DISTRIBUTION FOR DIETARY FACTORS
#
################################################################################
# Processed meat
counter.pme<-0

# Red meat
counter.rme<-50
#counter.rme<-71.5

# Dairy products
counter.dai<-300

# Non-starchy vegetables
counter.nsv<-240

# Fruit
counter.fru<-160

# Fibre
counter.fib<-30

################################################################################
# QUANTILES OF DIETARY FACTORS AMONG CONSUMERS ACCORDING TO THE
# GAMMA DISTRIBUTION
################################################################################
# Matrix containing quantiles and prevalence of dietary factors
PREV.DF<-matrix(rep(NA,11*104),nrow=11,ncol=104)
colnames(PREV.DF)<-c(
                     "pme.col.w","mp.pme.col.w","p.pme.col.w","d.pme.col.w",
                     "pme.col.m","mp.pme.col.m","p.pme.col.m","d.pme.col.m",
                     "rme.col.w","mp.rme.col.w","p.rme.col.w","d.rme.col.w",
                     "rme.col.m","mp.rme.col.m","p.rme.col.m","d.rme.col.m",
                     "dai.col.w","mp.dai.col.w","p.dai.col.w","d.dai.col.w",
                     "dai.col.m","mp.dai.col.m","p.dai.col.m","d.dai.col.m",
                     "nsv.ora.w","mp.nsv.ora.w","p.nsv.ora.w","d.nsv.ora.w",
                     "nsv.ora.m","mp.nsv.ora.m","p.nsv.ora.m","d.nsv.ora.m",
                     "nsv.nas.w","mp.nsv.nas.w","p.nsv.nas.w","d.nsv.nas.w",
                     "nsv.nas.m","mp.nsv.nas.m","p.nsv.nas.m","d.nsv.nas.m",
                     "nsv.aes.w","mp.nsv.aes.w","p.nsv.aes.w","d.nsv.aes.w",
                     "nsv.aes.m","mp.nsv.aes.m","p.nsv.aes.m","d.nsv.aes.m",
                     "nsv.ses.w","mp.nsv.ses.w","p.nsv.ses.w","d.nsv.ses.w",
                     "nsv.ses.m","mp.nsv.ses.m","p.nsv.ses.m","d.nsv.ses.m",
                     "nsv.col.w","mp.nsv.col.w","p.nsv.col.w","d.nsv.col.w",
                     "nsv.col.m","mp.nsv.col.m","p.nsv.col.m","d.nsv.col.m",
                     "nsv.lun.w","mp.nsv.lun.w","p.nsv.lun.w","d.nsv.lun.w",
                     "nsv.lun.m","mp.nsv.lun.m","p.nsv.lun.m","d.nsv.lun.m",
                     "fru.ses.w","mp.fru.ses.w","p.fru.ses.w","d.fru.ses.w",
                     "fru.ses.m","mp.fru.ses.m","p.fru.ses.m","d.fru.ses.m",
                     "fru.sto.w","mp.fru.sto.w","p.fru.sto.w","d.fru.sto.w",
                     "fru.sto.m","mp.fru.sto.m","p.fru.sto.m","d.fru.sto.m",
                     "fru.lun.w","mp.fru.lun.w","p.fru.lun.w","d.fru.lun.w",
                     "fru.lun.m","mp.fru.lun.m","p.fru.lun.m","d.fru.lun.m",
                     "fib.col.w","mp.fib.col.w","p.fib.col.w","d.fib.col.w", 
                     "fib.col.m","mp.fib.col.m","p.fib.col.m","d.fib.col.m"
                    )

# PROCESSED MEAT - WOMEN
#############################
PREV.DF[,"pme.col.w"]<-c(
                         qgamma(
                                seq(0,.9,.1),
                                shape=DIET$kappa.DF.w[DIET$DF=="Pme"&DIET$Cancer=="Col"],
                                scale=DIET$theta.DF.w[DIET$DF=="Pme"&DIET$Cancer=="Col"]
                               ),
                         DIET$high.DF.w[DIET$DF=="Pme"&DIET$Cancer=="Col"]
                        )
# Mid-points for adjacent values of quantiles
for(i in 1:nrow(PREV.DF)-1)
{
 PREV.DF[i+1,"mp.pme.col.w"]<-mean(c(PREV.DF[i,"pme.col.w"],PREV.DF[i+1,"pme.col.w"]))
}
# Prevalence
PREV.DF[,"p.pme.col.w"]<-c(
                           DIET$pro.ncon.w[DIET$DF=="Pme"&DIET$Cancer=="Col"],
                           rep(DIET$pro.con.w[DIET$DF=="Pme"&DIET$Cancer=="Col"]/(nrow(PREV.DF)-1),(nrow(PREV.DF)-1))
                          )
# Deviation (excess or deficit) in consumption from the recommendation (counterfactual distribution)
PREV.DF[which(PREV.DF[,"mp.pme.col.w"]==counter.pme),"d.pme.col.w"]<-0
PREV.DF[which(PREV.DF[,"mp.pme.col.w"]>counter.pme),"d.pme.col.w"]<-PREV.DF[which(PREV.DF[,"mp.pme.col.w"]>counter.pme),"mp.pme.col.w"]-counter.pme

# PROCESSED MEAT - MEN
#############################
PREV.DF[,"pme.col.m"]<-c(
                         qgamma(
                                seq(0,.9,.1),
                                shape=DIET$kappa.DF.m[DIET$DF=="Pme"&DIET$Cancer=="Col"],
                                scale=DIET$theta.DF.m[DIET$DF=="Pme"&DIET$Cancer=="Col"]
                               ),
                         DIET$high.DF.m[DIET$DF=="Pme"&DIET$Cancer=="Col"]
                        )
# Mid-points for adjacent values of quantiles
for(i in 1:nrow(PREV.DF)-1)
{
 PREV.DF[i+1,"mp.pme.col.m"]<-mean(c(PREV.DF[i,"pme.col.m"],PREV.DF[i+1,"pme.col.m"]))
}
# Prevalence
PREV.DF[,"p.pme.col.m"]<-c(
                           DIET$pro.ncon.m[DIET$DF=="Pme"&DIET$Cancer=="Col"],
                           rep(DIET$pro.con.m[DIET$DF=="Pme"&DIET$Cancer=="Col"]/(nrow(PREV.DF)-1),(nrow(PREV.DF)-1))
                          )
# Deviation (excess or deficit) in consumption from the recommendation (counterfactual distribution)
PREV.DF[which(PREV.DF[,"mp.pme.col.m"]==counter.pme),"d.pme.col.m"]<-0
PREV.DF[which(PREV.DF[,"mp.pme.col.m"]>counter.pme),"d.pme.col.m"]<-PREV.DF[which(PREV.DF[,"mp.pme.col.m"]>counter.pme),"mp.pme.col.m"]-counter.pme

# RED MEAT - WOMEN
#############################
PREV.DF[,"rme.col.w"]<-c(
                         qgamma(
                                seq(0,.9,.1),
                                shape=DIET$kappa.DF.w[DIET$DF=="Rme"&DIET$Cancer=="Col"],
                                scale=DIET$theta.DF.w[DIET$DF=="Rme"&DIET$Cancer=="Col"]
                               ),
                         DIET$high.DF.w[DIET$DF=="Rme"&DIET$Cancer=="Col"]
                        )
# Mid-points for adjacent values of quantiles
for(i in 1:nrow(PREV.DF)-1)
{
 PREV.DF[i+1,"mp.rme.col.w"]<-mean(c(PREV.DF[i,"rme.col.w"],PREV.DF[i+1,"rme.col.w"]))
}
# Prevalence
PREV.DF[,"p.rme.col.w"]<-c(
                           DIET$pro.ncon.w[DIET$DF=="Rme"&DIET$Cancer=="Col"],
                           rep(DIET$pro.con.w[DIET$DF=="Rme"&DIET$Cancer=="Col"]/(nrow(PREV.DF)-1),(nrow(PREV.DF)-1))
                          )
# Deviation (excess or deficit) in consumption from the recommendation (conterfactual distribution)
PREV.DF[which(PREV.DF[,"mp.rme.col.w"]<counter.rme),"d.rme.col.w"]<-0
PREV.DF[which(PREV.DF[,"mp.rme.col.w"]>=counter.rme),"d.rme.col.w"]<-PREV.DF[which(PREV.DF[,"mp.rme.col.w"]>=counter.rme),"mp.rme.col.w"]-counter.rme

# RED MEAT - MEN
#############################
PREV.DF[,"rme.col.m"]<-c(
                         qgamma(
                                seq(0,.9,.1),
                                shape=DIET$kappa.DF.m[DIET$DF=="Rme"&DIET$Cancer=="Col"],
                                scale=DIET$theta.DF.m[DIET$DF=="Rme"&DIET$Cancer=="Col"]
                               ),
                         DIET$high.DF.m[DIET$DF=="Rme"&DIET$Cancer=="Col"]
                        )
# Mid-points for adjacent values of quantiles
for(i in 1:nrow(PREV.DF)-1)
{
 PREV.DF[i+1,"mp.rme.col.m"]<-mean(c(PREV.DF[i,"rme.col.m"],PREV.DF[i+1,"rme.col.m"]))
}
# Prevalence
PREV.DF[,"p.rme.col.m"]<-c(
                           DIET$pro.ncon.m[DIET$DF=="Rme"&DIET$Cancer=="Col"],
                           rep(DIET$pro.con.m[DIET$DF=="Rme"&DIET$Cancer=="Col"]/(nrow(PREV.DF)-1),(nrow(PREV.DF)-1))
                          )
# Deviation (excess or deficit) in consumption from the recommendation (counterfactual distribution)
PREV.DF[which(PREV.DF[,"mp.rme.col.m"]<counter.rme),"d.rme.col.m"]<-0
PREV.DF[which(PREV.DF[,"mp.rme.col.m"]>=counter.rme),"d.rme.col.m"]<-PREV.DF[which(PREV.DF[,"mp.rme.col.m"]>=counter.rme),"mp.rme.col.m"]-counter.rme

# DAIRY PRODUCTS - WOMEN
#############################
PREV.DF[,"dai.col.w"]<-c(
                         qgamma(
                                seq(0,.9,.1),
                                shape=DIET$kappa.DF.w[DIET$DF=="Dai"&DIET$Cancer=="Col"],
                                scale=DIET$theta.DF.w[DIET$DF=="Dai"&DIET$Cancer=="Col"]
                               ),
                         DIET$high.DF.w[DIET$DF=="Dai"&DIET$Cancer=="Col"]
                        )
# Mid-points for adjacent values of quantiles
for(i in 1:nrow(PREV.DF)-1)
{
 PREV.DF[i+1,"mp.dai.col.w"]<-mean(c(PREV.DF[i,"dai.col.w"],PREV.DF[i+1,"dai.col.w"]))
}
# Prevalence
PREV.DF[,"p.dai.col.w"]<-c(
                           DIET$pro.ncon.w[DIET$DF=="Dai"&DIET$Cancer=="Col"],
                           rep(DIET$pro.con.w[DIET$DF=="Dai"&DIET$Cancer=="Col"]/(nrow(PREV.DF)-1),(nrow(PREV.DF)-1))
                          )
# Deviation (excess or deficit) in consumption from the recommendation (conterfactual distribution)
PREV.DF[which(PREV.DF[,"mp.dai.col.w"]>=counter.dai),"d.dai.col.w"]<-0
PREV.DF[which(PREV.DF[,"mp.dai.col.w"]<counter.dai),"d.dai.col.w"]<-counter.dai-PREV.DF[which(PREV.DF[,"mp.dai.col.w"]<counter.dai),"mp.dai.col.w"]

# DAIRY PRODUCTS - MEN
#############################
PREV.DF[,"dai.col.m"]<-c(
                         qgamma(
                                seq(0,.9,.1),
                                shape=DIET$kappa.DF.m[DIET$DF=="Dai"&DIET$Cancer=="Col"],
                                scale=DIET$theta.DF.m[DIET$DF=="Dai"&DIET$Cancer=="Col"]
                               ),
                         DIET$high.DF.m[DIET$DF=="Dai"&DIET$Cancer=="Col"]
                        )
# Mid-points for adjacent values of quantiles
for(i in 1:nrow(PREV.DF)-1)
{
 PREV.DF[i+1,"mp.dai.col.m"]<-mean(c(PREV.DF[i,"dai.col.m"],PREV.DF[i+1,"dai.col.m"]))
}
# Prevalence
PREV.DF[,"p.dai.col.m"]<-c(
                           DIET$pro.ncon.m[DIET$DF=="Dai"&DIET$Cancer=="Col"],
                           rep(DIET$pro.con.m[DIET$DF=="Dai"&DIET$Cancer=="Col"]/(nrow(PREV.DF)-1),(nrow(PREV.DF)-1))
                          )
# Deviation (excess or deficit) in consumption from the recommendation (counterfactual distribution)
PREV.DF[which(PREV.DF[,"mp.dai.col.m"]>=counter.dai),"d.dai.col.m"]<-0
PREV.DF[which(PREV.DF[,"mp.dai.col.m"]<counter.dai),"d.dai.col.m"]<-counter.dai-PREV.DF[which(PREV.DF[,"mp.dai.col.m"]<counter.dai),"mp.dai.col.m"]

# NON-STARCHY VEGETABLES - WOMEN
#############################
PREV.DF[,seq(25,72,8)]<-c(
                         qgamma(
                                seq(0,.9,.1),
                                shape=DIET$kappa.DF.w[DIET$DF=="Nsv"&DIET$Cancer=="Ora"],
                                scale=DIET$theta.DF.w[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]
                               ),
                         DIET$high.DF.w[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]
                        )
# Mid-points for adjacent values of quantiles
for(i in 1:nrow(PREV.DF)-1)
{
 PREV.DF[i+1,seq(25,72,8)+1]<-mean(c(PREV.DF[i,"nsv.ora.w"],PREV.DF[i+1,"nsv.ora.w"]))
}
# Prevalence
PREV.DF[,seq(25,72,8)+2]<-c(
                           DIET$pro.ncon.w[DIET$DF=="Nsv"&DIET$Cancer=="Ora"],
                           rep(DIET$pro.con.w[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]/(nrow(PREV.DF)-1),(nrow(PREV.DF)-1))
                          )
# Deviation (excess or deficit) in consumption from the recommendation (couterfactual distribution)
PREV.DF[which(PREV.DF[,"mp.nsv.ora.w"]<counter.nsv),seq(25,72,8)+3]<-counter.nsv-PREV.DF[which(PREV.DF[,"mp.nsv.ora.w"]<counter.nsv),"mp.nsv.ora.w"]
PREV.DF[which(PREV.DF[,"mp.nsv.ora.w"]>=counter.nsv),seq(25,72,8)+3]<-0

# NON-STARCHY VEGETABLES - MEN
#############################
PREV.DF[,seq(29,72,8)]<-c(
                         qgamma(
                                seq(0,.9,.1),
                                shape=DIET$kappa.DF.m[DIET$DF=="Nsv"&DIET$Cancer=="Ora"],
                                scale=DIET$theta.DF.m[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]
                               ),
                         DIET$high.DF.m[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]
                        )
# Mid-points for adjacent values of quantiles
for(i in 1:nrow(PREV.DF)-1)
{
 PREV.DF[i+1,seq(29,72,8)+1]<-mean(c(PREV.DF[i,"nsv.ora.m"],PREV.DF[i+1,"nsv.ora.m"]))
}
# Prevalence
PREV.DF[,seq(29,72,8)+2]<-c(
                           DIET$pro.ncon.m[DIET$DF=="Nsv"&DIET$Cancer=="Ora"],
                           rep(DIET$pro.con.m[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]/(nrow(PREV.DF)-1),(nrow(PREV.DF)-1))
                          )
# Deviation (excess or deficit) in consumption from the recommendation (counterfactual ditribution)
PREV.DF[which(PREV.DF[,"mp.nsv.ora.m"]<counter.nsv),seq(29,72,8)+3]<-counter.nsv-PREV.DF[which(PREV.DF[,"mp.nsv.ora.m"]<counter.nsv),"mp.nsv.ora.m"]
PREV.DF[which(PREV.DF[,"mp.nsv.ora.m"]>=counter.nsv),seq(29,72,8)+3]<-0

# FRUIT - WOMEN
#############################
PREV.DF[,seq(73,96,8)]<-c(
                          qgamma(
                                 seq(0,.9,.1),
                                 shape=DIET$kappa.DF.w[DIET$DF=="Fru"&DIET$Cancer=="Ses"],
                                 scale=DIET$theta.DF.w[DIET$DF=="Fru"&DIET$Cancer=="Ses"]
                                ),
                          DIET$high.DF.w[DIET$DF=="Fru"&DIET$Cancer=="Ses"]
                         )
# Mid-points for adjacent values of quantiles
for(i in 1:nrow(PREV.DF)-1)
{
 PREV.DF[i+1,seq(73,96,8)+1]<-mean(c(PREV.DF[i,"fru.ses.w"],PREV.DF[i+1,"fru.ses.w"]))
}
# Prevalence
PREV.DF[,seq(73,96,8)+2]<-c(
                            DIET$pro.ncon.w[DIET$DF=="Fru"&DIET$Cancer=="Ses"],
                            rep(DIET$pro.con.w[DIET$DF=="Fru"&DIET$Cancer=="Ses"]/(nrow(PREV.DF)-1),(nrow(PREV.DF)-1))
                           )
# Deviation (excess or deficit) in consumption from the recommendation (counterfactual distribution)
PREV.DF[which(PREV.DF[,"mp.fru.ses.w"]<counter.fru),seq(73,96,8)+3]<-counter.fru-PREV.DF[which(PREV.DF[,"mp.fru.ses.w"]<counter.fru),"mp.fru.ses.w"]
PREV.DF[which(PREV.DF[,"mp.fru.ses.w"]>=counter.fru),seq(73,96,8)+3]<-0

# FRUIT - MEN
#############################
PREV.DF[,seq(77,96,8)]<-c(
                          qgamma(
                                 seq(0,.9,.1),
                                 shape=DIET$kappa.DF.m[DIET$DF=="Fru"&DIET$Cancer=="Ses"],
                                 scale=DIET$theta.DF.m[DIET$DF=="Fru"&DIET$Cancer=="Ses"]
                                ),
                          DIET$high.DF.m[DIET$DF=="Fru"&DIET$Cancer=="Ses"]
                         )
# Mid-points for adjacent values of quantiles
for(i in 1:nrow(PREV.DF)-1)
{
 PREV.DF[i+1,seq(77,96,8)+1]<-mean(c(PREV.DF[i,"fru.ses.m"],PREV.DF[i+1,"fru.ses.m"]))
}
# Prevalence
PREV.DF[,seq(77,96,8)+2]<-c(
                            DIET$pro.ncon.m[DIET$DF=="Fru"&DIET$Cancer=="Ses"],
                            rep(DIET$pro.con.m[DIET$DF=="Fru"&DIET$Cancer=="Ses"]/(nrow(PREV.DF)-1),(nrow(PREV.DF)-1))
                           )
# Deviation (excess or deficit) in consumption from the recommendation (counterfactual distribution)
PREV.DF[which(PREV.DF[,"mp.fru.ses.m"]<counter.fru),seq(77,96,8)+3]<-counter.fru-PREV.DF[which(PREV.DF[,"mp.fru.ses.m"]<counter.fru),"mp.fru.ses.m"]
PREV.DF[which(PREV.DF[,"mp.fru.ses.m"]>=counter.fru),seq(77,96,8)+3]<-0

# FIBRE - WOMEN
#############################
PREV.DF[,"fib.col.w"]<-c(
                         qgamma(
                                seq(0,.9,.1),
                                shape=DIET$kappa.DF.w[DIET$DF=="Fib"&DIET$Cancer=="Col"],
                                scale=DIET$theta.DF.w[DIET$DF=="Fib"&DIET$Cancer=="Col"]
                               ),
                         DIET$high.DF.w[DIET$DF=="Fib"&DIET$Cancer=="Col"]
                        )
# Mid-points for adjacent values of quantiles
for(i in 1:nrow(PREV.DF)-1)
{
 PREV.DF[i+1,"mp.fib.col.w"]<-mean(c(PREV.DF[i,"fib.col.w"],PREV.DF[i+1,"fib.col.w"]))
}
# Prevalence
PREV.DF[,"p.fib.col.w"]<-c(
                           DIET$pro.ncon.w[DIET$DF=="Fib"&DIET$Cancer=="Col"],
                           rep(DIET$pro.con.w[DIET$DF=="Fib"&DIET$Cancer=="Col"]/(nrow(PREV.DF)-1),(nrow(PREV.DF)-1))
                          )
# Deviation (excess or deficit) in consumption from the recommendation (counterfactual distribution)
PREV.DF[which(PREV.DF[,"mp.fib.col.w"]>=counter.fib),"d.fib.col.w"]<-0
PREV.DF[which(PREV.DF[,"mp.fib.col.w"]<counter.fib),"d.fib.col.w"]<-counter.fib-PREV.DF[which(PREV.DF[,"mp.fib.col.w"]<counter.fib),"mp.fib.col.w"]

# FIBRE - MEN
#############################
PREV.DF[,"fib.col.m"]<-c(
                         qgamma(
                                seq(0,.9,.1),
                                shape=DIET$kappa.DF.m[DIET$DF=="Fib"&DIET$Cancer=="Col"],
                                scale=DIET$theta.DF.m[DIET$DF=="Fib"&DIET$Cancer=="Col"]
                               ),
                         DIET$high.DF.m[DIET$DF=="Fib"&DIET$Cancer=="Col"]
                        )
# Mid-points for adjacent values of quantiles
for(i in 1:nrow(PREV.DF)-1)
{
 PREV.DF[i+1,"mp.fib.col.m"]<-mean(c(PREV.DF[i,"fib.col.m"],PREV.DF[i+1,"fib.col.m"]))
}
# Prevalence
PREV.DF[,"p.fib.col.m"]<-c(
                           DIET$pro.ncon.m[DIET$DF=="Fib"&DIET$Cancer=="Col"],
                           rep(DIET$pro.con.m[DIET$DF=="Fib"&DIET$Cancer=="Col"]/(nrow(PREV.DF)-1),(nrow(PREV.DF)-1))
                          )
# Deviation (excess or deficit) in consumption from the recommendation (counterfactual distribution)
PREV.DF[which(PREV.DF[,"mp.fib.col.m"]>=counter.fib),"d.fib.col.m"]<-0
PREV.DF[which(PREV.DF[,"mp.fib.col.m"]<counter.fib),"d.fib.col.m"]<-counter.fib-PREV.DF[which(PREV.DF[,"mp.fib.col.m"]<counter.fib),"mp.fib.col.m"]

################################################################################
#
# EXCESS RELATIVE RISK (ERR) ACCORDING TO THE CONSUMPTION OF DIETARY FACTORS
#
################################################################################
ERR<-matrix(rep(NA,11*26),nrow=11,ncol=26)
colnames(ERR)<-c(
                 "pme.col.w","pme.col.m",
                 "rme.col.w","rme.col.m",
                 "dai.col.w","dai.col.m",
                 "nsv.ora.w","nsv.ora.m",
                 "nsv.nas.w","nsv.nas.m",
                 "nsv.aes.w","nsv.aes.m",
                 "nsv.ses.w","nsv.ses.m",
                 "nsv.col.w","nsv.col.m",
                 "nsv.lun.w","nsv.lun.m",
                 "fru.ses.w","fru.ses.m",
                 "fru.sto.w","fru.sto.m",
                 "fru.lun.w","fru.lun.m",
                 "fib.col.w","fib.col.m"
                )

for(j in 1:ncol(ERR))
{
 ERR[,j]<-exp(PREV.DF[,(j*4)]*rep(DIET$logRR.1g[-c(14:16)],each=2)[j])-1
}

################################################################################
#
# ATTRIBUTABLE FRACTION (AF) - PUNCTUAL ESTIMATES
#
################################################################################
AF<-as.data.frame(matrix(rep(NA,32*14),nrow=32,ncol=14))
colnames(AF)<-c(
                "DF","Cancer","Sex",
                "AF","AF.lowCI","AF.highCI",
                "Obs.ca","AC","AC.lowCI","AC.highCI",
                "Obs.de","AD","AD.lowCI","AD.highCI"
               )
AF<-AF[-nrow(AF),]
AF[,"DF"]<-c(rep(c("Pme","Rme","Dai"),each=2),rep("Nsv",12),rep("Fru",6),rep(c("Fib","Cfr","Cof"),each=2),"Cof")
AF[,"Cancer"]<-c(rep(c("Col","Col","Col","Ora","Nas","Aes","Ses","Col","Lun","Ses","Sto","Lun","Col","Sca","Liv"),each=2),"End")
AF[,"Sex"]<-c(rep(c("W","M"),15),"W")

# AF FOR CONTINUOUS DIETARY FACTORS
#############################
for (i in 1:(nrow(AF)-5))
{
 AF[i,"AF"]<-as.numeric(round(sum(ERR[,i]*PREV.DF[,(i*4)-1])/(1+sum(ERR[,i]*PREV.DF[,(i*4)-1]))*100,1))
}

# AF FOR DICHOTOMOUS DIETARY FACTORS
#############################
# CITRUS FRUIT - WOMEN
AF[27,"AF"]<-round((exp(DIET$logRR.1g[DIET$DF=="Cfr"&DIET$Cancer=="Sca"])-1)*DIET$pro.con.w[DIET$DF=="Cfr"&DIET$Cancer=="Sca"]/
                   (1+(exp(DIET$logRR.1g[DIET$DF=="Cfr"&DIET$Cancer=="Sca"])-1)*DIET$pro.con.w[DIET$DF=="Cfr"&DIET$Cancer=="Sca"])
                   *100,1
                  )

# CITRUS - MEN
AF[28,"AF"]<-round((exp(DIET$logRR.1g[DIET$DF=="Cfr"&DIET$Cancer=="Sca"])-1)*DIET$pro.con.m[DIET$DF=="Cfr"&DIET$Cancer=="Sca"]/
                   (1+(exp(DIET$logRR.1g[DIET$DF=="Cfr"&DIET$Cancer=="Sca"])-1)*DIET$pro.con.m[DIET$DF=="Cfr"&DIET$Cancer=="Sca"])
                   *100,1
                  )

# COFFEE AND LIVER - WOMEN
AF[29,"AF"]<-round((exp(DIET$logRR.1g[DIET$DF=="Cof"&DIET$Cancer=="Liv"])-1)*DIET$pro.con.w[DIET$DF=="Cof"&DIET$Cancer=="Liv"]/
                   (1+(exp(DIET$logRR.1g[DIET$DF=="Cof"&DIET$Cancer=="Liv"])-1)*DIET$pro.con.w[DIET$DF=="Cof"&DIET$Cancer=="Liv"])
                   *100,1
                  )

# COFFEE AND LIVER - MEN
AF[30,"AF"]<-round((exp(DIET$logRR.1g[DIET$DF=="Cof"&DIET$Cancer=="Liv"])-1)*DIET$pro.con.m[DIET$DF=="Cof"&DIET$Cancer=="Liv"]/
                   (1+(exp(DIET$logRR.1g[DIET$DF=="Cof"&DIET$Cancer=="Liv"])-1)*DIET$pro.con.m[DIET$DF=="Cof"&DIET$Cancer=="Liv"])
                   *100,1
                  )

# COFFEE AND ENDOMETRIUM - WOMEN
AF[31,"AF"]<-round((exp(DIET$logRR.1g[DIET$DF=="Cof"&DIET$Cancer=="End"])-1)*DIET$pro.con.w[DIET$DF=="Cof"&DIET$Cancer=="End"]/
                   (1+(exp(DIET$logRR.1g[DIET$DF=="Cof"&DIET$Cancer=="End"])-1)*DIET$pro.con.w[DIET$DF=="Cof"&DIET$Cancer=="End"])
                   *100,1
                  )

################################################################################
#
# MONTECARLO SIMULATIONS 
#
################################################################################
#
# SIMULATED PREVALENCE FOR CONTINUOUS DIETARY FACTORS
#
################################################################################
# Generating 10000 means for each dietary factor from a normal distribution using 
# the observed means standard deviations
SIM.MI<-matrix(rep(NA,10000*48),nrow=10000,ncol=48)
colnames(SIM.MI)<-c(
                    "mi.pme.w","si.pme.w","kappa.pme.w","theta.pme.w",
                    "mi.pme.m","si.pme.m","kappa.pme.m","theta.pme.m",
                    "mi.rme.w","si.rme.w","kappa.rme.w","theta.rme.w",
                    "mi.rme.m","si.rme.m","kappa.rme.m","theta.rme.m",
                    "mi.dai.w","si.dai.w","kappa.dai.w","theta.dai.w",
                    "mi.dai.m","si.dai.m","kappa.dai.m","theta.dai.m",
                    "mi.nsv.w","si.nsv.w","kappa.nsv.w","theta.nsv.w",
                    "mi.nsv.m","si.nsv.m","kappa.nsv.m","theta.nsv.m",
                    "mi.fru.w","si.fru.w","kappa.fru.w","theta.fru.w",
                    "mi.fru.m","si.fru.m","kappa.fru.m","theta.fru.m",
                    "mi.fib.w","si.fib.w","kappa.fib.w","theta.fib.w",
                    "mi.fib.m","si.fib.m","kappa.fib.m","theta.fib.m"
                   )

# PROCESSED MEAT - WOMEN
#############################
set.seed(18071992)
SIM.MI[,1]<-rnorm(10000,DIET$mean.DF.w[1],DIET$sd.DF.w[1]/sqrt(DIET$n.w[1]))
SIM.MI[,2]<-rep(DIET$sd.DF.w[1],10000)
SIM.MI[,3]<-(SIM.MI[,1]/SIM.MI[,2])^2
SIM.MI[,4]<-SIM.MI[,2]^2/SIM.MI[,1]

# PROCESSED MEAT - MEN
#############################
set.seed(18071992)
SIM.MI[,5]<-rnorm(10000,DIET$mean.DF.m[1],DIET$sd.DF.m[1]/sqrt(DIET$n.m[1]))
SIM.MI[,6]<-rep(DIET$sd.DF.m[1],10000)
SIM.MI[,7]<-(SIM.MI[,5]/SIM.MI[,6])^2
SIM.MI[,8]<-SIM.MI[,6]^2/SIM.MI[,5]

# RED MEAT - WOMEN
#############################
set.seed(18071992)
SIM.MI[,9]<-rnorm(10000,DIET$mean.DF.w[2],DIET$sd.DF.w[2]/sqrt(DIET$n.w[2]))
SIM.MI[,10]<-rep(DIET$sd.DF.w[2],10000)
SIM.MI[,11]<-(SIM.MI[,9]/SIM.MI[,10])^2
SIM.MI[,12]<-SIM.MI[,10]^2/SIM.MI[,9]

# RED MEAT - MEN
#############################
set.seed(18071992)
SIM.MI[,13]<-rnorm(10000,DIET$mean.DF.m[2],DIET$sd.DF.m[2]/sqrt(DIET$n.m[2]))
SIM.MI[,14]<-rep(DIET$sd.DF.m[2],10000)
SIM.MI[,15]<-(SIM.MI[,13]/SIM.MI[,14])^2
SIM.MI[,16]<-SIM.MI[,14]^2/SIM.MI[,13]

# DAIRY PRODUCTS - WOMEN
#############################
set.seed(18071992)
SIM.MI[,17]<-rnorm(10000,DIET$mean.DF.w[3],DIET$sd.DF.w[3]/sqrt(DIET$n.w[3]))
SIM.MI[,18]<-rep(DIET$sd.DF.w[3],10000)
SIM.MI[,19]<-(SIM.MI[,17]/SIM.MI[,18])^2
SIM.MI[,20]<-SIM.MI[,18]^2/SIM.MI[,17]

# DAIRY PRODUCTS - MEN
#############################
set.seed(18071992)
SIM.MI[,21]<-rnorm(10000,DIET$mean.DF.m[3],DIET$sd.DF.m[3]/sqrt(DIET$n.m[3]))
SIM.MI[,22]<-rep(DIET$sd.DF.m[3],10000)
SIM.MI[,23]<-(SIM.MI[,21]/SIM.MI[,22])^2
SIM.MI[,24]<-SIM.MI[,22]^2/SIM.MI[,21]

# NON-STARCHY VEGETABLES - WOMEN
#############################
set.seed(18071992)
SIM.MI[,25]<-rnorm(10000,DIET$mean.DF.w[4],DIET$sd.DF.w[4]/sqrt(DIET$n.w[4]))
SIM.MI[,26]<-rep(DIET$sd.DF.w[4],10000)
SIM.MI[,27]<-(SIM.MI[,25]/SIM.MI[,26])^2
SIM.MI[,28]<-SIM.MI[,26]^2/SIM.MI[,25]

# NON-STARCHY VEGETABLES - MEN
#############################
set.seed(18071992)
SIM.MI[,29]<-rnorm(10000,DIET$mean.DF.m[4],DIET$sd.DF.m[4]/sqrt(DIET$n.m[4]))
SIM.MI[,30]<-rep(DIET$sd.DF.m[4],10000)
SIM.MI[,31]<-(SIM.MI[,29]/SIM.MI[,30])^2
SIM.MI[,32]<-SIM.MI[,30]^2/SIM.MI[,29]

# FRUIT - WOMEN
#############################
set.seed(18071992)
SIM.MI[,33]<-rnorm(10000,DIET$mean.DF.w[10],DIET$sd.DF.w[10]/sqrt(DIET$n.w[10]))
SIM.MI[,34]<-rep(DIET$sd.DF.w[10],10000)
SIM.MI[,35]<-(SIM.MI[,33]/SIM.MI[,34])^2
SIM.MI[,36]<-SIM.MI[,34]^2/SIM.MI[,33]

# FRUIT - MEN
#############################
set.seed(18071992)
SIM.MI[,37]<-rnorm(10000,DIET$mean.DF.m[10],DIET$sd.DF.m[10]/sqrt(DIET$n.m[10]))
SIM.MI[,38]<-rep(DIET$sd.DF.m[10],10000)
SIM.MI[,39]<-(SIM.MI[,37]/SIM.MI[,38])^2
SIM.MI[,40]<-SIM.MI[,38]^2/SIM.MI[,37]

# FIBRE - WOMEN
#############################
set.seed(18071992)
SIM.MI[,41]<-rnorm(10000,DIET$mean.DF.w[13],DIET$sd.DF.w[13]/sqrt(DIET$n.w[13]))
SIM.MI[,42]<-rep(DIET$sd.DF.w[13],10000)
SIM.MI[,43]<-(SIM.MI[,41]/SIM.MI[,42])^2
SIM.MI[,44]<-SIM.MI[,42]^2/SIM.MI[,41]

# FIBRE - MEN
#############################
set.seed(18071992)
SIM.MI[,45]<-rnorm(10000,DIET$mean.DF.m[13],DIET$sd.DF.m[13]/sqrt(DIET$n.m[13]))
SIM.MI[,46]<-rep(DIET$sd.DF.w[13],10000)
SIM.MI[,47]<-(SIM.MI[,45]/SIM.MI[,46])^2
SIM.MI[,48]<-SIM.MI[,46]^2/SIM.MI[,45]

################################################################################
# QUANTILES OF DIETARY FACTORS AMONG CONSUMERS ACCORDING TO THE
# GAMMA DISTRIBUTION USING SIMULATED MEANS AND OBSERVED STANDARD DEVIATION
################################################################################
# PROCESSED MEAT - WOMEN
#############################
# Matrix containing quantiles and prevalence of dietary factors of 10000 gamma distribution
Q.PME.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(Q.PME.SIM.W)<-c(paste0("pme.w",1:10000))

for (j in 1:ncol(Q.PME.SIM.W))
{
 Q.PME.SIM.W[,j]<-c(
                    qgamma(
                           seq(0,.9,.1),
                           shape=SIM.MI[j,"kappa.pme.w"],
                           scale=SIM.MI[j,"theta.pme.w"]
                          ),
                    DIET$high.DF.w[DIET$DF=="Pme"&DIET$Cancer=="Col"]
                   )
}

# Matrix containing mid-points for adjacent values of quantiles
MP.PME.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(MP.PME.SIM.W)<-c(paste0("mp.pme.w",1:10000))

for (j in 1:ncol(MP.PME.SIM.W))
{
 for(i in 1:nrow(MP.PME.SIM.W)-1)
 {
  MP.PME.SIM.W[i+1,j]<-mean(c(Q.PME.SIM.W[i,j],Q.PME.SIM.W[i+1,j]))
 }
}

# Matrix containing (excess or deficit) in consumption from the recommandation (counterfactual distribution)
D.PME.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(D.PME.SIM.W)<-c(paste0("d.pme.w",1:10000))
for (j in 1:ncol(D.PME.SIM.W))
{
 D.PME.SIM.W[which(MP.PME.SIM.W[,j]==counter.pme),j]<-0
 D.PME.SIM.W[which(MP.PME.SIM.W[,j]>counter.pme),j]<-MP.PME.SIM.W[which(MP.PME.SIM.W[,j]>counter.pme),j]-counter.pme
}

# PROCESSED MEAT - MEN
#############################
# Matrix containing quantiles and prevalence of dietary factors of 10000 gamma distribution
Q.PME.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(Q.PME.SIM.M)<-c(paste0("pme.m",1:10000))

for (j in 1:ncol(Q.PME.SIM.M))
{
 Q.PME.SIM.M[,j]<-c(
                    qgamma(
                           seq(0,.9,.1),
                           shape=SIM.MI[j,"kappa.pme.m"],
                           scale=SIM.MI[j,"theta.pme.m"]
                          ),
                    DIET$high.DF.m[DIET$DF=="Pme"&DIET$Cancer=="Col"]
                   )
}

# Matrix containing mid-points for adjacent values of quantiles
MP.PME.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(MP.PME.SIM.M)<-c(paste0("mp.pme.m",1:10000))

for (j in 1:ncol(MP.PME.SIM.M))
{
 for(i in 1:nrow(MP.PME.SIM.M)-1)
 {
  MP.PME.SIM.M[i+1,j]<-mean(c(Q.PME.SIM.M[i,j],Q.PME.SIM.M[i+1,j]))
 }
}

# Matrix containing (excess or deficit) in consumption from the recommandation (counterfactual distribution)
D.PME.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(D.PME.SIM.M)<-c(paste0("d.pme.m",1:10000))
for (j in 1:ncol(D.PME.SIM.M))
{
 D.PME.SIM.M[which(MP.PME.SIM.M[,j]==counter.pme),j]<-0
 D.PME.SIM.M[which(MP.PME.SIM.M[,j]>counter.pme),j]<-MP.PME.SIM.M[which(MP.PME.SIM.M[,j]>counter.pme),j]-counter.pme
}

# RED MEAT - WOMEN
#############################
# Matrix containing quantiles and prevalence of dietary factors of 10000 gamma distribution
Q.RME.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(Q.RME.SIM.W)<-c(paste0("rme.w",1:10000))

for (j in 1:ncol(Q.RME.SIM.W))
{
 Q.RME.SIM.W[,j]<-c(
                    qgamma(
                           seq(0,.9,.1),
                           shape=SIM.MI[j,"kappa.rme.w"],
                           scale=SIM.MI[j,"theta.rme.w"]
                          ),
                    DIET$high.DF.w[DIET$DF=="Rme"&DIET$Cancer=="Col"]
                   )
}

# Matrix containing mid-points for adjacent values of quantiles
MP.RME.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(MP.RME.SIM.W)<-c(paste0("mp.rme.w",1:10000))

for (j in 1:ncol(MP.RME.SIM.W))
{
 for(i in 1:nrow(MP.RME.SIM.W)-1)
 {
  MP.RME.SIM.W[i+1,j]<-mean(c(Q.RME.SIM.W[i,j],Q.RME.SIM.W[i+1,j]))
 }
}

# Matrix containing (excess or deficit) in consumption from the recommandation (counterfactual distribution)
D.RME.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(D.RME.SIM.W)<-c(paste0("d.rme.w",1:10000))

for (j in 1:ncol(D.RME.SIM.W))
{
 D.RME.SIM.W[which(MP.RME.SIM.W[,j]<counter.rme),j]<-0
 D.RME.SIM.W[which(MP.RME.SIM.W[,j]>=counter.rme),j]<-MP.RME.SIM.W[which(MP.RME.SIM.W[,j]>=counter.rme),j]-counter.rme
}

# RED MEAT - MEN
#############################
# Matrix containing quantiles and prevalence of dietary factors of 10000 gamma distribution
Q.RME.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(Q.RME.SIM.M)<-c(paste0("rme.m",1:10000))

for (j in 1:ncol(Q.RME.SIM.M))
{
 Q.RME.SIM.M[,j]<-c(
                    qgamma(
                           seq(0,.9,.1),
                           shape=SIM.MI[j,"kappa.rme.m"],
                           scale=SIM.MI[j,"theta.rme.m"]
                          ),
                    DIET$high.DF.m[DIET$DF=="Rme"&DIET$Cancer=="Col"]
                   )
}

# Matrix containing mid-points for adjacent values of quantiles
MP.RME.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(MP.RME.SIM.M)<-c(paste0("mp.rme.m",1:10000))

for (j in 1:ncol(MP.RME.SIM.M))
{
 for(i in 1:nrow(MP.RME.SIM.M)-1)
 {
  MP.RME.SIM.M[i+1,j]<-mean(c(Q.RME.SIM.M[i,j],Q.RME.SIM.M[i+1,j]))
 }
}

# Matrix containing (excess or deficit) in consumption from the recommandation (counterfactual distribution)
D.RME.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(D.RME.SIM.M)<-c(paste0("d.rme.m",1:10000))

for (j in 1:ncol(D.RME.SIM.M))
{
 D.RME.SIM.M[which(MP.RME.SIM.M[,j]<counter.rme),j]<-0
 D.RME.SIM.M[which(MP.RME.SIM.M[,j]>=counter.rme),j]<-MP.RME.SIM.M[which(MP.RME.SIM.M[,j]>=counter.rme),j]-counter.rme
}

# DAIRY PRODUCTS - WOMEN
#############################
# Matrix containing quantiles and prevalence of dietary factors of 10000 gamma distribution
Q.DAI.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(Q.DAI.SIM.W)<-c(paste0("dai.w",1:10000))

for (j in 1:ncol(Q.DAI.SIM.W))
{
 Q.DAI.SIM.W[,j]<-c(
                    qgamma(
                           seq(0,.9,.1),
                           shape=SIM.MI[j,"kappa.dai.w"],
                           scale=SIM.MI[j,"theta.dai.w"]
                          ),
                    DIET$high.DF.w[DIET$DF=="Dai"&DIET$Cancer=="Col"]
                   )
}

# Matrix containing mid-points for adjacent values of quantiles
MP.DAI.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(MP.DAI.SIM.W)<-c(paste0("mp.dai.w",1:10000))

for (j in 1:ncol(MP.DAI.SIM.W))
{
 for(i in 1:nrow(MP.DAI.SIM.W)-1)
 {
  MP.DAI.SIM.W[i+1,j]<-mean(c(Q.DAI.SIM.W[i,j],Q.DAI.SIM.W[i+1,j]))
 }
}

# Matrix containing (excess or deficit) in consumption from the recommandation (counterfactual distribution)
D.DAI.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(D.DAI.SIM.W)<-c(paste0("d.dai.w",1:10000))

for (j in 1:ncol(D.DAI.SIM.W))
{
 D.DAI.SIM.W[which(MP.DAI.SIM.W[,j]<counter.dai),j]<-counter.dai-MP.DAI.SIM.W[which(MP.DAI.SIM.W[,j]<counter.dai),j]
 D.DAI.SIM.W[which(MP.DAI.SIM.W[,j]>=counter.dai),j]<-0
}

# DAIRY PRODUCTS - MEN
#############################
# Matrix containing quantiles and prevalence of dietary factors of 10000 gamma distribution
Q.DAI.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(Q.DAI.SIM.M)<-c(paste0("dai.m",1:10000))

for (j in 1:ncol(Q.DAI.SIM.M))
{
 Q.DAI.SIM.M[,j]<-c(
                    qgamma(
                           seq(0,.9,.1),
                           shape=SIM.MI[j,"kappa.dai.m"],
                           scale=SIM.MI[j,"theta.dai.m"]
                          ),
                    DIET$high.DF.m[DIET$DF=="Dai"&DIET$Cancer=="Col"]
                   )
}

# Matrix containing mid-points for adjacent values of quantiles
MP.DAI.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(MP.DAI.SIM.M)<-c(paste0("mp.dai.m",1:10000))

for (j in 1:ncol(MP.DAI.SIM.M))
{
 for(i in 1:nrow(MP.DAI.SIM.M)-1)
 {
  MP.DAI.SIM.M[i+1,j]<-mean(c(Q.DAI.SIM.M[i,j],Q.DAI.SIM.M[i+1,j]))
 }
}

# Matrix containing (excess or deficit) in consumption from the recommandation (counterfactual distribution)
D.DAI.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(D.DAI.SIM.M)<-c(paste0("d.dai.m",1:10000))

for (j in 1:ncol(D.DAI.SIM.M))
{
 D.DAI.SIM.M[which(MP.DAI.SIM.M[,j]<counter.dai),j]<-counter.dai-MP.DAI.SIM.M[which(MP.DAI.SIM.M[,j]<counter.dai),j]
 D.DAI.SIM.M[which(MP.DAI.SIM.M[,j]>=counter.dai),j]<-0
}

# NON-STARCHY VEGETABLES - WOMEN
#############################
# Matrix containing quantiles and prevalence of dietary factors of 10000 gamma distribution
Q.NSV.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(Q.NSV.SIM.W)<-c(paste0("nsv.w",1:10000))

for (j in 1:ncol(Q.NSV.SIM.W))
{
 Q.NSV.SIM.W[,j]<-c(
                    qgamma(
                           seq(0,.9,.1),
                           shape=SIM.MI[j,"kappa.nsv.w"],
                           scale=SIM.MI[j,"theta.nsv.w"]
                          ),
                    DIET$high.DF.w[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]
                   )
}

# Matrix containing mid-points for adjacent values of quantiles
MP.NSV.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(MP.NSV.SIM.W)<-c(paste0("mp.nsv.w",1:10000))

for (j in 1:ncol(MP.NSV.SIM.W))
{
 for(i in 1:nrow(MP.NSV.SIM.W)-1)
 {
  MP.NSV.SIM.W[i+1,j]<-mean(c(Q.NSV.SIM.W[i,j],Q.NSV.SIM.W[i+1,j]))
 }
}

# Matrix containing (excess or deficit) in consumption from the recommandation (counterfactual distribution)
D.NSV.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(D.NSV.SIM.W)<-c(paste0("d.nsv.w",1:10000))

for (j in 1:ncol(D.NSV.SIM.W))
{
 D.NSV.SIM.W[which(MP.NSV.SIM.W[,j]<counter.nsv),j]<-counter.nsv-MP.NSV.SIM.W[which(MP.NSV.SIM.W[,j]<counter.nsv),j]
 D.NSV.SIM.W[which(MP.NSV.SIM.W[,j]>=counter.nsv),j]<-0
}

# NON-STARCHY VEGETABLES - MEN
#############################
# Matrix containing quantiles and prevalence of dietary factors of 10000 gamma distribution
Q.NSV.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(Q.NSV.SIM.M)<-c(paste0("nsv.m",1:10000))

for (i in 1:ncol(Q.NSV.SIM.M))
{
 Q.NSV.SIM.M[,i]<-c(
                    qgamma(
                           seq(0,.9,.1),
                           shape=SIM.MI[i,"kappa.nsv.m"],
                           scale=SIM.MI[i,"theta.nsv.m"]
                          ),
                    DIET$high.DF.m[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]
                   )
}

# Matrix containing mid-points for adjacent values of quantiles
MP.NSV.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(MP.NSV.SIM.M)<-c(paste0("mp.nsv.m",1:10000))

for (j in 1:ncol(MP.NSV.SIM.M))
{
 for(i in 1:nrow(MP.NSV.SIM.M)-1)
 {
  MP.NSV.SIM.M[i+1,j]<-mean(c(Q.NSV.SIM.M[i,j],Q.NSV.SIM.M[i+1,j]))
 }
}

# Matrix containing (excess or deficit) in consumption from the recommandation (counterfactual distribution)
D.NSV.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(D.NSV.SIM.M)<-c(paste0("d.nsv.m",1:10000))

for (j in 1:ncol(D.NSV.SIM.M))
{
 D.NSV.SIM.M[which(MP.NSV.SIM.M[,j]<counter.nsv),j]<-counter.nsv-MP.NSV.SIM.M[which(MP.NSV.SIM.M[,j]<counter.nsv),j]
 D.NSV.SIM.M[which(MP.NSV.SIM.M[,j]>=counter.nsv),j]<-0
}

# FRUIT - WOMEN
#############################
# Matrix containing quantiles and prevalence of dietary factors of 10000 gamma distribution
Q.FRU.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(Q.FRU.SIM.W)<-c(paste0("fru.w",1:10000))

for (j in 1:ncol(Q.FRU.SIM.W))
{
 Q.FRU.SIM.W[,j]<-c(
                    qgamma(
                           seq(0,.9,.1),
                           shape=SIM.MI[j,"kappa.fru.w"],
                           scale=SIM.MI[j,"theta.fru.w"]
                          ),
                    DIET$high.DF.w[DIET$DF=="Fru"&DIET$Cancer=="Ses"]
                   )
}

# Matrix containing mid-points for adjacent values of quantiles
MP.FRU.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(MP.FRU.SIM.W)<-c(paste0("mp.fru.w",1:10000))

for (j in 1:ncol(MP.FRU.SIM.W))
{
 for(i in 1:nrow(MP.FRU.SIM.W)-1)
 {
  MP.FRU.SIM.W[i+1,j]<-mean(c(Q.FRU.SIM.W[i,j],Q.FRU.SIM.W[i+1,j]))
 }
}

# Matrix containing (excess or deficit) in consumption from the recommandation (counterfactual distribution)
D.FRU.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(D.FRU.SIM.W)<-c(paste0("d.fru.w",1:10000))

for (j in 1:ncol(D.FRU.SIM.W))
{
 D.FRU.SIM.W[which(MP.FRU.SIM.W[,j]<counter.fru),j]<-counter.fru-MP.FRU.SIM.W[which(MP.FRU.SIM.W[,j]<counter.fru),j]
 D.FRU.SIM.W[which(MP.FRU.SIM.W[,j]>=counter.fru),j]<-0
}

# FRUIT - MEN
#############################
# Matrix containing quantiles and prevalence of dietary factors of 10000 gamma distribution
Q.FRU.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(Q.FRU.SIM.M)<-c(paste0("fru.m",1:10000))

for (j in 1:ncol(Q.FRU.SIM.M))
{
 Q.FRU.SIM.M[,j]<-c(
                    qgamma(
                           seq(0,.9,.1),
                           shape=SIM.MI[j,"kappa.fru.m"],
                           scale=SIM.MI[j,"theta.fru.m"]
                          ),
                    DIET$high.DF.m[DIET$DF=="Fru"&DIET$Cancer=="Ses"]
                   )
}

# Matrix containing mid-points for adjacent values of quantiles
MP.FRU.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(MP.FRU.SIM.M)<-c(paste0("mp.fru.m",1:10000))

for (j in 1:ncol(MP.FRU.SIM.M))
{
 for(i in 1:nrow(MP.FRU.SIM.M)-1)
 {
  MP.FRU.SIM.M[i+1,j]<-mean(c(Q.FRU.SIM.M[i,j],Q.FRU.SIM.M[i+1,j]))
 }
}

# Matrix containing (excess or deficit) in consumption from the recommandation (counterfactual distribution)
D.FRU.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(D.FRU.SIM.M)<-c(paste0("d.fru.m",1:10000))

for (j in 1:ncol(D.FRU.SIM.M))
{
 D.FRU.SIM.M[which(MP.FRU.SIM.M[,j]<counter.fru),j]<-counter.fru-MP.FRU.SIM.M[which(MP.FRU.SIM.M[,j]<counter.fru),j]
 D.FRU.SIM.M[which(MP.FRU.SIM.M[,j]>=counter.fru),j]<-0
}

# FIBRE - WOMEN
#############################
# Matrix containing quantiles and prevalence of dietary factors of 10000 gamma distribution
Q.FIB.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(Q.FIB.SIM.W)<-c(paste0("fib.w",1:10000))

for (j in 1:ncol(Q.FIB.SIM.W))
{
 Q.FIB.SIM.W[,j]<-c(
                    qgamma(
                           seq(0,.9,.1),
                           shape=SIM.MI[j,"kappa.fib.w"],
                           scale=SIM.MI[j,"theta.fib.w"]
                          ),
                    DIET$high.DF.w[DIET$DF=="Fib"&DIET$Cancer=="Col"]
                   )
}

# Matrix containing mid-points for adjacent values of quantiles
MP.FIB.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(MP.FIB.SIM.W)<-c(paste0("mp.fib.w",1:10000))

for (j in 1:ncol(MP.FIB.SIM.W))
{
 for(i in 1:nrow(MP.FIB.SIM.W)-1)
 {
  MP.FIB.SIM.W[i+1,j]<-mean(c(Q.FIB.SIM.W[i,j],Q.FIB.SIM.W[i+1,j]))
 }
}

# Matrix containing (excess or deficit) in consumption from the recommandation (counterfactual distribution)
D.FIB.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(D.FIB.SIM.W)<-c(paste0("d.fib.w",1:10000))

for (j in 1:ncol(D.FIB.SIM.W))
{
 D.FIB.SIM.W[which(MP.FIB.SIM.W[,j]<counter.fib),j]<-counter.fib-MP.FIB.SIM.W[which(MP.FIB.SIM.W[,j]<counter.fib),j]
 D.FIB.SIM.W[which(MP.FIB.SIM.W[,j]>=counter.fib),j]<-0
}

# FIBRE - MEN
#############################
# Matrix containing quantiles and prevalence of dietary factors of 10000 gamma distribution
Q.FIB.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(Q.FIB.SIM.M)<-c(paste0("fib.m",1:10000))

for (j in 1:ncol(Q.FIB.SIM.M))
{
 Q.FIB.SIM.M[,j]<-c(
                    qgamma(
                           seq(0,.9,.1),
                           shape=SIM.MI[j,"kappa.fib.m"],
                           scale=SIM.MI[j,"theta.fib.m"]
                          ),
                    DIET$high.DF.m[DIET$DF=="Fib"&DIET$Cancer=="Col"]
                   )
}

# Matrix containing mid-points for adjacent values of quantiles
MP.FIB.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(MP.FIB.SIM.M)<-c(paste0("mp.fib.m",1:10000))

for (j in 1:ncol(MP.FIB.SIM.M))
{
 for(i in 1:nrow(MP.FIB.SIM.M)-1)
 {
  MP.FIB.SIM.M[i+1,j]<-mean(c(Q.FIB.SIM.M[i,j],Q.FIB.SIM.M[i+1,j]))
 }
}

# Matrix containing (excess or deficit) in consumption from the recommandation (counterfactual distribution)
D.FIB.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(D.FIB.SIM.M)<-c(paste0("d.fib.m",1:10000))

for (j in 1:ncol(D.FIB.SIM.M))
{
 D.FIB.SIM.M[which(MP.FIB.SIM.M[,j]<counter.fib),j]<-counter.fib-MP.FIB.SIM.M[which(MP.FIB.SIM.M[,j]<counter.fib),j]
 D.FIB.SIM.M[which(MP.FIB.SIM.M[,j]>=counter.fib),j]<-0
}

################################################################################
#
# SIMULATED PREVALENCE FOR DICHOTOMOUS DIETARY FACTORS
#
################################################################################
# CITRUS FRUIT - WOMEN
#############################
set.seed(18071992)
P.CFR.SIM.W<-rnorm(
                   10000,
                   DIET$pro.con.w[DIET$DF=="Cfr"&DIET$Cancer=="Sca"],
                   sqrt(
                        (DIET$pro.con.w[DIET$DF=="Cfr"&DIET$Cancer=="Sca"]*
                        DIET$pro.ncon.w[DIET$DF=="Cfr"&DIET$Cancer=="Sca"])/
                        DIET$n.w[DIET$DF=="Cfr"&DIET$Cancer=="Sca"]
                       )
                  )

# CITRUS FRUIT - MEN
#############################
set.seed(18071992)
P.CFR.SIM.M<-rnorm(
                   10000,
                   DIET$pro.con.m[DIET$DF=="Cfr"&DIET$Cancer=="Sca"],
                   sqrt(
                        (DIET$pro.con.m[DIET$DF=="Cfr"&DIET$Cancer=="Sca"]*
                        DIET$pro.ncon.m[DIET$DF=="Cfr"&DIET$Cancer=="Sca"])/
                        DIET$n.m[DIET$DF=="Cfr"&DIET$Cancer=="Sca"]
                       )
                  )

# COFFEE - WOMEN
#############################
set.seed(18071992)
P.COF.SIM.W<-rnorm(
                   10000,
                   DIET$pro.con.w[DIET$DF=="Cof"&DIET$Cancer=="Liv"],
                   sqrt(
                        (DIET$pro.con.w[DIET$DF=="Cof"&DIET$Cancer=="Liv"]*
                        DIET$pro.ncon.w[DIET$DF=="Cof"&DIET$Cancer=="Liv"])/
                        DIET$n.w[DIET$DF=="Cof"&DIET$Cancer=="Liv"]
                       )
                  )
# COFFEE - MEN
#############################
set.seed(18071992)
P.COF.SIM.M<-rnorm(
                   10000,
                   DIET$pro.con.m[DIET$DF=="Cof"&DIET$Cancer=="Liv"],
                   sqrt(
                        (DIET$pro.con.m[DIET$DF=="Cof"&DIET$Cancer=="Liv"]*
                        DIET$pro.ncon.m[DIET$DF=="Cof"&DIET$Cancer=="Liv"])/
                        DIET$n.m[DIET$DF=="Cof"&DIET$Cancer=="Liv"]
                       )
                  )

################################################################################
#
# SIMULATED LOG RR
# 
################################################################################
# PROCESSED MEAT AND COLORECTAL CANCER
#############################
set.seed(18071992)
logRR.PME.COL.SIM<-rnorm(
                         10000,
                         DIET$logRR.1g[DIET$DF=="Pme"&DIET$Cancer=="Col"],
                         DIET$logSE.1g[DIET$DF=="Pme"&DIET$Cancer=="Col"]
                        )

# RED MEAT AND COLORECTAL CANCER
#############################
set.seed(18071992)
logRR.RME.COL.SIM<-rnorm(
                         10000,
                         DIET$logRR.1g[DIET$DF=="Rme"&DIET$Cancer=="Col"],
                         DIET$logSE.1g[DIET$DF=="Rme"&DIET$Cancer=="Col"]
                        )

# DAIRY PRODUCTS AND COLORECTAL CANCER
#############################
set.seed(18071992)
logRR.DAI.COL.SIM<-rnorm(
                         10000,
                         DIET$logRR.1g[DIET$DF=="Dai"&DIET$Cancer=="Col"],
                         DIET$logSE.1g[DIET$DF=="Dai"&DIET$Cancer=="Col"]
                        )
# NON-STARCHY VEGETABLES AND ORAL CANCER
#############################
set.seed(18071992)
logRR.NSV.ORA.SIM<-rnorm(
                         10000,
                         DIET$logRR.1g[DIET$DF=="Nsv"&DIET$Cancer=="Ora"],
                         DIET$logSE.1g[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]
                        )
# NON-STARCHY VEGETABLES AND NASOPHARYNGEAL CANCER
#############################
set.seed(18071992)
logRR.NSV.NAS.SIM<-rnorm(
                         10000,
                         DIET$logRR.1g[DIET$DF=="Nsv"&DIET$Cancer=="Nas"],
                         DIET$logSE.1g[DIET$DF=="Nsv"&DIET$Cancer=="Nas"]
                        )
# NON-STARCHY VEGETABLES AND ADENOCARCINOMA OF THE ESOPHAGUS
#############################
set.seed(18071992)
logRR.NSV.AES.SIM<-rnorm(
                         10000,
                         DIET$logRR.1g[DIET$DF=="Nsv"&DIET$Cancer=="Aes"],
                         DIET$logSE.1g[DIET$DF=="Nsv"&DIET$Cancer=="Aes"]
                        )
# NON-STARCHY VEGETABLES AND SQUAMOUS CELL CARCINOMA OF THE ESOPHAGUS
#############################
set.seed(18071992)
logRR.NSV.SES.SIM<-rnorm(
                         10000,
                         DIET$logRR.1g[DIET$DF=="Nsv"&DIET$Cancer=="Ses"],
                         DIET$logSE.1g[DIET$DF=="Nsv"&DIET$Cancer=="Ses"]
                        )
# NON-STARCHY VEGETABLES AND COLORECTAL CANCER
#############################
set.seed(18071992)
logRR.NSV.COL.SIM<-rnorm(
                         10000,
                         DIET$logRR.1g[DIET$DF=="Nsv"&DIET$Cancer=="Col"],
                         DIET$logSE.1g[DIET$DF=="Nsv"&DIET$Cancer=="Col"]
                        )
# NON-STARCHY VEGETABLES AND LUNG CANCER
#############################
set.seed(18071992)
logRR.NSV.LUN.SIM<-rnorm(
                         10000,
                         DIET$logRR.1g[DIET$DF=="Nsv"&DIET$Cancer=="Lun"],
                         DIET$logSE.1g[DIET$DF=="Nsv"&DIET$Cancer=="Lun"]
                        )

# FRUIT AND SQUAMOUS CELL CARCINOMA OF THE ESOPHAGUS
#############################
set.seed(18071992)
logRR.FRU.SES.SIM<-rnorm(
                         10000,
                         DIET$logRR.1g[DIET$DF=="Fru"&DIET$Cancer=="Ses"],
                         DIET$logSE.1g[DIET$DF=="Fru"&DIET$Cancer=="Ses"]
                        )

# FRUIT AND STOMACH CANCER
#############################
set.seed(18071992)
logRR.FRU.STO.SIM<-rnorm(
                         10000,
                         DIET$logRR.1g[DIET$DF=="Fru"&DIET$Cancer=="Sto"],
                         DIET$logSE.1g[DIET$DF=="Fru"&DIET$Cancer=="Sto"]
                        )

# FRUIT AND LUNG CANCER
#############################
set.seed(18071992)
logRR.FRU.LUN.SIM<-rnorm(
                         10000,
                         DIET$logRR.1g[DIET$DF=="Fru"&DIET$Cancer=="Lun"],
                         DIET$logSE.1g[DIET$DF=="Fru"&DIET$Cancer=="Lun"]
                        )
# FIBRE MEAT AND COLORECTAL CANCER
#############################
set.seed(18071992)
logRR.FIB.COL.SIM<-rnorm(
                         10000,
                         DIET$logRR.1g[DIET$DF=="Fib"&DIET$Cancer=="Col"],
                         DIET$logSE.1g[DIET$DF=="Fib"&DIET$Cancer=="Col"]
                        )
# CITRUS FRUIT AND STOMACH CANCER (CARDIA)
#############################
set.seed(18071992)
logRR.CFR.SCA.SIM<-rnorm(
                         10000,
                         DIET$logRR.1g[DIET$DF=="Cfr"&DIET$Cancer=="Sca"],
                         DIET$logSE.1g[DIET$DF=="Cfr"&DIET$Cancer=="Sca"]
                        )
# COFFEE AND LIVER CANCER
#############################
set.seed(18071992)
logRR.COF.LIV.SIM<-rnorm(
                         10000,
                         DIET$logRR.1g[DIET$DF=="Cof"&DIET$Cancer=="Liv"],
                         DIET$logSE.1g[DIET$DF=="Cof"&DIET$Cancer=="Liv"]
                        )

# COFFEE AND ENDOMETRIAL CANCER
#############################
set.seed(18071992)
logRR.COF.END.SIM<-rnorm(
                         10000,
                         DIET$logRR.1g[DIET$DF=="Cof"&DIET$Cancer=="End"],
                         DIET$logSE.1g[DIET$DF=="Cof"&DIET$Cancer=="End"]
                        )

################################################################################
#
# EXCESS RELATIVE RISK (ERR) ACCORDING TO THE CONSUMPTION OF CONTINUOUS 
# DIETARY FACTORS FOR EACH SIMULATION
# 
################################################################################
# PROCESSED MEAT AND COLORECTAL CANCER - WOMEN
#############################
ERR.PME.COL.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.PME.COL.SIM.W)<-c(paste0("pme.col.w",1:10000))

for(j in 1:ncol(ERR.PME.COL.SIM.W))
{
 ERR.PME.COL.SIM.W[,j]<-exp(D.PME.SIM.W[,j]*logRR.PME.COL.SIM[j])-1
}

# PROCESSED MEAT AND COLORECTAL CANCER - MEN
#############################
ERR.PME.COL.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.PME.COL.SIM.M)<-c(paste0("pme.col.m",1:10000))

for(j in 1:ncol(ERR.PME.COL.SIM.M))
{
 ERR.PME.COL.SIM.M[,j]<-exp(D.PME.SIM.M[,j]*logRR.PME.COL.SIM[j])-1
}

# RED MEAT AND COLORECTAL CANCER - WOMEN
#############################
ERR.RME.COL.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.RME.COL.SIM.W)<-c(paste0("rme.col.w",1:10000))

for(j in 1:ncol(ERR.RME.COL.SIM.W))
{
 ERR.RME.COL.SIM.W[,j]<-exp(D.RME.SIM.W[,j]*logRR.RME.COL.SIM[j])-1
}

# RED MEAT AND COLORECTAL CANCER - MEN
#############################
ERR.RME.COL.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.RME.COL.SIM.M)<-c(paste0("rme.col.m",1:10000))

for(j in 1:ncol(ERR.RME.COL.SIM.M))
{
 ERR.RME.COL.SIM.M[,j]<-exp(D.RME.SIM.M[,j]*logRR.RME.COL.SIM[j])-1
}

# DAIRY PRODUCTS AND COLORECTAL CANCER - WOMEN
#############################
ERR.DAI.COL.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.DAI.COL.SIM.W)<-c(paste0("dai.col.w",1:10000))

for(j in 1:ncol(ERR.DAI.COL.SIM.W))
{
 ERR.DAI.COL.SIM.W[,j]<-exp(D.DAI.SIM.W[,j]*logRR.DAI.COL.SIM[j])-1
}

# DAIRY PRODUCTS AND COLORECTAL CANCER - MEN
#############################
ERR.DAI.COL.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.DAI.COL.SIM.M)<-c(paste0("dai.col.m",1:10000))

for(j in 1:ncol(ERR.DAI.COL.SIM.M))
{
 ERR.DAI.COL.SIM.M[,j]<-exp(D.DAI.SIM.M[,j]*logRR.DAI.COL.SIM[j])-1
}

# NON-STARCHY VEGETABLES AND ORAL CANCER - WOMEN
#############################
ERR.NSV.ORA.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.NSV.ORA.SIM.W)<-c(paste0("nsv.ora.w",1:10000))

for(j in 1:ncol(ERR.NSV.ORA.SIM.W))
{
 ERR.NSV.ORA.SIM.W[,j]<-exp(D.NSV.SIM.W[,j]*logRR.NSV.ORA.SIM[j])-1
}

# NON-STARCHY VEGETABLES AND ORAL CANCER - MEN
#############################
ERR.NSV.ORA.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.NSV.ORA.SIM.M)<-c(paste0("nsv.ora.m",1:10000))

for(j in 1:ncol(ERR.NSV.ORA.SIM.M))
{
 ERR.NSV.ORA.SIM.M[,j]<-exp(D.NSV.SIM.M[,j]*logRR.NSV.ORA.SIM[j])-1
}

# NON-STARCHY VEGETABLES AND NASOPHARYNGEAL CANCER - WOMEN
#############################
ERR.NSV.NAS.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.NSV.NAS.SIM.W)<-c(paste0("nsv.nas.w",1:10000))

for(j in 1:ncol(ERR.NSV.NAS.SIM.W))
{
 ERR.NSV.NAS.SIM.W[,j]<-exp(D.NSV.SIM.W[,j]*logRR.NSV.NAS.SIM[j])-1
}

# NON-STARCHY VEGETABLES AND NASOPHARYNGEAL CANCER - MEN
#############################
ERR.NSV.NAS.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.NSV.NAS.SIM.M)<-c(paste0("nsv.nas.m",1:10000))

for(j in 1:ncol(ERR.NSV.NAS.SIM.M))
{
 ERR.NSV.NAS.SIM.M[,j]<-exp(D.NSV.SIM.M[,j]*logRR.NSV.NAS.SIM[j])-1
}

# NON-STARCHY VEGETABLES AND ADENOCARCINOMA OF THE ESOPHAGUS - WOMEN
#############################
ERR.NSV.AES.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.NSV.AES.SIM.W)<-c(paste0("nsv.aes.w",1:10000))

for(j in 1:ncol(ERR.NSV.AES.SIM.W))
{
 ERR.NSV.AES.SIM.W[,j]<-exp(D.NSV.SIM.W[,j]*logRR.NSV.AES.SIM[j])-1
}

# NON-STARCHY VEGETABLES AND ADENOCARCINOMA OF THE ESOPHAGUS - MEN
#############################
ERR.NSV.AES.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.NSV.AES.SIM.M)<-c(paste0("nsv.aes.m",1:10000))

for(j in 1:ncol(ERR.NSV.AES.SIM.M))
{
 ERR.NSV.AES.SIM.M[,j]<-exp(D.NSV.SIM.M[,j]*logRR.NSV.AES.SIM[j])-1
}

# NON-STARCHY VEGETABLES AND SQUAMOUS CELL CARCINOMA OF THE ESOPHAGUS - WOMEN
#############################
ERR.NSV.SES.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.NSV.SES.SIM.W)<-c(paste0("nsv.ses.w",1:10000))

for(j in 1:ncol(ERR.NSV.SES.SIM.W))
{
 ERR.NSV.SES.SIM.W[,j]<-exp(D.NSV.SIM.W[,j]*logRR.NSV.SES.SIM[j])-1
}

# NON-STARCHY VEGETABLES AND SQUAMOUS CELL CARCINOMA OF THE ESOPHAGUS - MEN
#############################
ERR.NSV.SES.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.NSV.SES.SIM.M)<-c(paste0("nsv.ses.m",1:10000))

for(j in 1:ncol(ERR.NSV.SES.SIM.M))
{
 ERR.NSV.SES.SIM.M[,j]<-exp(D.NSV.SIM.M[,j]*logRR.NSV.SES.SIM[j])-1
}

# NON-STARCHY VEGETABLES AND COLORECTAL CANCER - WOMEN
#############################
ERR.NSV.COL.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.NSV.COL.SIM.W)<-c(paste0("nsv.col.w",1:10000))

for(j in 1:ncol(ERR.NSV.COL.SIM.W))
{
 ERR.NSV.COL.SIM.W[,j]<-exp(D.NSV.SIM.W[,j]*logRR.NSV.COL.SIM[j])-1
}

# NON-STARCHY VEGETABLES AND COLORECTAL CANCER - MEN
#############################
ERR.NSV.COL.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.NSV.COL.SIM.M)<-c(paste0("nsv.col.m",1:10000))

for(j in 1:ncol(ERR.NSV.COL.SIM.M))
{
 ERR.NSV.COL.SIM.M[,j]<-exp(D.NSV.SIM.M[,j]*logRR.NSV.COL.SIM[j])-1
}

# NON-STARCHY VEGETABLES AND LUNG CANCER - WOMEN
#############################
ERR.NSV.LUN.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.NSV.LUN.SIM.W)<-c(paste0("nsv.lun.w",1:10000))

for(j in 1:ncol(ERR.NSV.LUN.SIM.W))
{
 ERR.NSV.LUN.SIM.W[,j]<-exp(D.NSV.SIM.W[,j]*logRR.NSV.LUN.SIM[j])-1
}

# NON-STARCHY VEGETABLES AND LUNG CANCER - MEN
#############################
ERR.NSV.LUN.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.NSV.LUN.SIM.M)<-c(paste0("nsv.lun.m",1:10000))

for(j in 1:ncol(ERR.NSV.LUN.SIM.M))
{
 ERR.NSV.LUN.SIM.M[,j]<-exp(D.NSV.SIM.M[,j]*logRR.NSV.LUN.SIM[j])-1
}

# FRUIT AND SQUAMOUS CELL CARCINOMA OF THE ESOPHAGUS - WOMEN
#############################
ERR.FRU.SES.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.FRU.SES.SIM.W)<-c(paste0("fru.ses.w",1:10000))

for(j in 1:ncol(ERR.FRU.SES.SIM.W))
{
 ERR.FRU.SES.SIM.W[,j]<-exp(D.FRU.SIM.W[,j]*logRR.FRU.SES.SIM[j])-1
}

# FRUIT AND SQUAMOUS CELL CARCINOMA OF THE ESOPHAGUS - MEN
#############################
ERR.FRU.SES.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.FRU.SES.SIM.M)<-c(paste0("fru.ses.m",1:10000))

for(j in 1:ncol(ERR.FRU.SES.SIM.M))
{
 ERR.FRU.SES.SIM.M[,j]<-exp(D.FRU.SIM.M[,j]*logRR.FRU.SES.SIM[j])-1
}

# FRUIT AND STOMACH CANCER - WOMEN
#############################
ERR.FRU.STO.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.FRU.STO.SIM.W)<-c(paste0("fru.sto.w",1:10000))

for(j in 1:ncol(ERR.FRU.STO.SIM.W))
{
 ERR.FRU.STO.SIM.W[,j]<-exp(D.FRU.SIM.W[,j]*logRR.FRU.STO.SIM[j])-1
}

# FRUIT AND STOMACH CANCER - MEN
#############################
ERR.FRU.STO.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.FRU.STO.SIM.M)<-c(paste0("fru.sto.m",1:10000))

for(j in 1:ncol(ERR.FRU.STO.SIM.M))
{
 ERR.FRU.STO.SIM.M[,j]<-exp(D.FRU.SIM.M[,j]*logRR.FRU.STO.SIM[j])-1
}

# FRUIT AND LUNG CANCER - WOMEN
#############################
ERR.FRU.LUN.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.FRU.LUN.SIM.W)<-c(paste0("fru.lun.w",1:10000))

for(j in 1:ncol(ERR.FRU.LUN.SIM.W))
{
 ERR.FRU.LUN.SIM.W[,j]<-exp(D.FRU.SIM.W[,j]*logRR.FRU.LUN.SIM[j])-1
}

# FRUIT AND LUNG CANCER - MEN
#############################
ERR.FRU.LUN.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.FRU.LUN.SIM.M)<-c(paste0("fru.lug.m",1:10000))

for(j in 1:ncol(ERR.FRU.LUN.SIM.M))
{
 ERR.FRU.LUN.SIM.M[,j]<-exp(D.FRU.SIM.M[,j]*logRR.FRU.LUN.SIM[j])-1
}

# FIBRE AND COLORECTAL CANCER - WOMEN
#############################
ERR.FIB.COL.SIM.W<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.FIB.COL.SIM.W)<-c(paste0("fib.col.w",1:10000))

for(j in 1:ncol(ERR.FIB.COL.SIM.W))
{
 ERR.FIB.COL.SIM.W[,j]<-exp(D.FIB.SIM.W[,j]*logRR.FIB.COL.SIM[j])-1
}

# FIBRE AND COLORECTAL CANCER - MEN
#############################
ERR.FIB.COL.SIM.M<-matrix(rep(NA,11*10000),nrow=11,ncol=10000)
colnames(ERR.FIB.COL.SIM.M)<-c(paste0("fib.col.m",1:10000))

for(j in 1:ncol(ERR.FIB.COL.SIM.M))
{
 ERR.FIB.COL.SIM.M[,j]<-exp(D.FIB.SIM.M[,j]*logRR.FIB.COL.SIM[j])-1
}

################################################################################
#
# ATTRIBUTABLE FRACTION (AF) FOR EACH SIMULATION (CONTINUOUS DIETARY FACTOR)
# 
################################################################################
# PROCESSED MEAT AND COLORECTAL CANCER - WOMEN
#############################
AF.PME.COL.SIM.W<-rep(NA,10000)
for (j in 1:length(AF.PME.COL.SIM.W))
{
 AF.PME.COL.SIM.W[j]<-sum(ERR.PME.COL.SIM.W[,j]*PREV.DF[,"p.pme.col.w"])/(1+sum(ERR.PME.COL.SIM.W[,j]*PREV.DF[,"p.pme.col.w"]))*100
}

# PROCESSED MEAT AND COLORECTAL CANCER - MEN
#############################
AF.PME.COL.SIM.M<-rep(NA,10000)
for (j in 1:length(AF.PME.COL.SIM.M))
{
 AF.PME.COL.SIM.M[j]<-sum(ERR.PME.COL.SIM.M[,j]*PREV.DF[,"p.pme.col.m"])/(1+sum(ERR.PME.COL.SIM.M[,j]*PREV.DF[,"p.pme.col.m"]))*100
}

# RED MEAT AND COLORECTAL CANCER - WOMEN
#############################
AF.RME.COL.SIM.W<-rep(NA,10000)
for (j in 1:length(AF.RME.COL.SIM.W))
{
 AF.RME.COL.SIM.W[j]<-sum(ERR.RME.COL.SIM.W[,j]*PREV.DF[,"p.rme.col.w"])/(1+sum(ERR.RME.COL.SIM.W[,j]*PREV.DF[,"p.rme.col.w"]))*100
}

# RED MEAT AND COLORECTAL CANCER - MEN
#############################
AF.RME.COL.SIM.M<-rep(NA,10000)
for (j in 1:length(AF.RME.COL.SIM.M))
{
 AF.RME.COL.SIM.M[j]<-sum(ERR.RME.COL.SIM.M[,j]*PREV.DF[,"p.rme.col.m"])/(1+sum(ERR.RME.COL.SIM.M[,j]*PREV.DF[,"p.rme.col.m"]))*100
}

# DAIRY PRODUCTS AND COLORECTAL CANCER - WOMEN
#############################
AF.DAI.COL.SIM.W<-rep(NA,10000)
for (j in 1:length(AF.DAI.COL.SIM.W))
{
 AF.DAI.COL.SIM.W[j]<-sum(ERR.DAI.COL.SIM.W[,j]*PREV.DF[,"p.dai.col.w"])/(1+sum(ERR.DAI.COL.SIM.W[,j]*PREV.DF[,"p.dai.col.w"]))*100
}

# DAIRY PRODUCTS AND COLORECTAL CANCER - MEN
#############################
AF.DAI.COL.SIM.M<-rep(NA,10000)
for (j in 1:length(AF.DAI.COL.SIM.M))
{
 AF.DAI.COL.SIM.M[j]<-sum(ERR.DAI.COL.SIM.M[,j]*PREV.DF[,"p.dai.col.m"])/(1+sum(ERR.DAI.COL.SIM.M[,j]*PREV.DF[,"p.dai.col.m"]))*100
}

# NON-STARCHY VEGETABLES AND ORAL CANCER - WOMEN
#############################
AF.NSV.ORA.SIM.W<-rep(NA,10000)
for (j in 1:length(AF.NSV.ORA.SIM.W))
{
 AF.NSV.ORA.SIM.W[j]<-sum(ERR.NSV.ORA.SIM.W[,j]*PREV.DF[,"p.nsv.ora.w"])/(1+sum(ERR.NSV.ORA.SIM.W[,j]*PREV.DF[,"p.nsv.ora.w"]))*100
}

# NON-STARCHY VEGETABLES AND ORAL CANCER - MEN
#############################
AF.NSV.ORA.SIM.M<-rep(NA,10000)
for (j in 1:length(AF.NSV.ORA.SIM.M))
{
 AF.NSV.ORA.SIM.M[j]<-sum(ERR.NSV.ORA.SIM.M[,j]*PREV.DF[,"p.nsv.ora.m"])/(1+sum(ERR.NSV.ORA.SIM.M[,j]*PREV.DF[,"p.nsv.ora.m"]))*100
}

# NON-STARCHY VEGETABLES AND NASOPHARYNGEAL CANCER - WOMEN
#############################
AF.NSV.NAS.SIM.W<-rep(NA,10000)
for (j in 1:length(AF.NSV.NAS.SIM.W))
{
 AF.NSV.NAS.SIM.W[j]<-sum(ERR.NSV.NAS.SIM.W[,j]*PREV.DF[,"p.nsv.nas.w"])/(1+sum(ERR.NSV.NAS.SIM.W[,j]*PREV.DF[,"p.nsv.nas.w"]))*100
}

# NON-STARCHY VEGETABLES AND NASOPHARYNGEAL CANCER - MEN
#############################
AF.NSV.NAS.SIM.M<-rep(NA,10000)
for (j in 1:length(AF.NSV.NAS.SIM.M))
{
 AF.NSV.NAS.SIM.M[j]<-sum(ERR.NSV.NAS.SIM.M[,j]*PREV.DF[,"p.nsv.nas.m"])/(1+sum(ERR.NSV.NAS.SIM.M[,j]*PREV.DF[,"p.nsv.nas.m"]))*100
}

# NON-STARCHY VEGETABLES AND ADENOCARCINOMA OF THE ESOPHAGUS - WOMEN
#############################
AF.NSV.AES.SIM.W<-rep(NA,10000)
for (j in 1:length(AF.NSV.AES.SIM.W))
{
 AF.NSV.AES.SIM.W[j]<-sum(ERR.NSV.AES.SIM.W[,j]*PREV.DF[,"p.nsv.aes.w"])/(1+sum(ERR.NSV.AES.SIM.W[,j]*PREV.DF[,"p.nsv.aes.w"]))*100
}

# NON-STARCHY VEGETABLES AND ADENOCARCINOMA OF THE ESOPHAGUS - MEN
#############################
AF.NSV.AES.SIM.M<-rep(NA,10000)
for (j in 1:length(AF.NSV.AES.SIM.M))
{
 AF.NSV.AES.SIM.M[j]<-sum(ERR.NSV.AES.SIM.M[,j]*PREV.DF[,"p.nsv.aes.m"])/(1+sum(ERR.NSV.AES.SIM.M[,j]*PREV.DF[,"p.nsv.aes.m"]))*100
}

# NON-STARCHY VEGETABLES AND SQUAMOUS CELL CARCINOMA OF THE ESOPHAGUS - WOMEN
#############################
AF.NSV.SES.SIM.W<-rep(NA,10000)
for (j in 1:length(AF.NSV.SES.SIM.W))
{
 AF.NSV.SES.SIM.W[j]<-sum(ERR.NSV.SES.SIM.W[,j]*PREV.DF[,"p.nsv.ses.w"])/(1+sum(ERR.NSV.SES.SIM.W[,j]*PREV.DF[,"p.nsv.ses.w"]))*100
}

# NON-STARCHY VEGETABLES AND SQUAMOUS CELL CARCINOMA OF THE ESOPHAGUS - MEN
#############################
AF.NSV.SES.SIM.M<-rep(NA,10000)
for (j in 1:length(AF.NSV.SES.SIM.M))
{
 AF.NSV.SES.SIM.M[j]<-sum(ERR.NSV.SES.SIM.M[,j]*PREV.DF[,"p.nsv.ses.m"])/(1+sum(ERR.NSV.SES.SIM.M[,j]*PREV.DF[,"p.nsv.ses.m"]))*100
}

# NON-STARCHY VEGETABLES AND COLORECTAL CANCER - WOMEN
#############################
AF.NSV.COL.SIM.W<-rep(NA,10000)
for (j in 1:length(AF.NSV.COL.SIM.W))
{
 AF.NSV.COL.SIM.W[j]<-sum(ERR.NSV.COL.SIM.W[,j]*PREV.DF[,"p.nsv.col.w"])/(1+sum(ERR.NSV.COL.SIM.W[,j]*PREV.DF[,"p.nsv.col.w"]))*100
}

# NON-STARCHY VEGETABLES AND COLORECTAL CANCER - MEN
#############################
AF.NSV.COL.SIM.M<-rep(NA,10000)
for (j in 1:length(AF.NSV.COL.SIM.M))
{
 AF.NSV.COL.SIM.M[j]<-sum(ERR.NSV.COL.SIM.M[,j]*PREV.DF[,"p.nsv.col.m"])/(1+sum(ERR.NSV.COL.SIM.M[,j]*PREV.DF[,"p.nsv.col.m"]))*100
}

# NON-STARCHY VEGETABLES AND LUNG CANCER - WOMEN
#############################
AF.NSV.LUN.SIM.W<-rep(NA,10000)
for (j in 1:length(AF.NSV.LUN.SIM.W))
{
 AF.NSV.LUN.SIM.W[j]<-sum(ERR.NSV.LUN.SIM.W[,j]*PREV.DF[,"p.nsv.lun.w"])/(1+sum(ERR.NSV.LUN.SIM.W[,j]*PREV.DF[,"p.nsv.lun.w"]))*100
}

# NON-STARCHY VEGETABLES AND LUNG CANCER - MEN
#############################
AF.NSV.LUN.SIM.M<-rep(NA,10000)
for (j in 1:length(AF.NSV.LUN.SIM.M))
{
 AF.NSV.LUN.SIM.M[j]<-sum(ERR.NSV.LUN.SIM.M[,j]*PREV.DF[,"p.nsv.lun.m"])/(1+sum(ERR.NSV.LUN.SIM.M[,j]*PREV.DF[,"p.nsv.lun.m"]))*100
}

# FRUIT AND SQUAMOUS CELL CARCINOMA OF THE ESOPHAGUS - WOMEN
#############################
AF.FRU.SES.SIM.W<-rep(NA,10000)
for (j in 1:length(AF.FRU.SES.SIM.W))
{
 AF.FRU.SES.SIM.W[j]<-sum(ERR.FRU.SES.SIM.W[,j]*PREV.DF[,"p.fru.ses.w"])/(1+sum(ERR.FRU.SES.SIM.W[,j]*PREV.DF[,"p.fru.ses.w"]))*100
}

# FRUIT AND SQUAMOUS CELL CARCINOMA OF THE ESOPHAGUS - MEN
#############################
AF.FRU.SES.SIM.M<-rep(NA,10000)
for (j in 1:length(AF.FRU.SES.SIM.M))
{
 AF.FRU.SES.SIM.M[j]<-sum(ERR.FRU.SES.SIM.M[,j]*PREV.DF[,"p.fru.ses.m"])/(1+sum(ERR.FRU.SES.SIM.M[,j]*PREV.DF[,"p.fru.ses.m"]))*100
}

# FRUIT AND STOMACH CANCER - WOMEN
#############################
AF.FRU.STO.SIM.W<-rep(NA,10000)
for (j in 1:length(AF.FRU.STO.SIM.W))
{
 AF.FRU.STO.SIM.W[j]<-sum(ERR.FRU.STO.SIM.W[,j]*PREV.DF[,"p.fru.sto.w"])/(1+sum(ERR.FRU.STO.SIM.W[,j]*PREV.DF[,"p.fru.sto.w"]))*100
}

# FRUIT AND STOMACH CANCER - MEN
#############################
AF.FRU.STO.SIM.M<-rep(NA,10000)
for (j in 1:length(AF.FRU.STO.SIM.M))
{
 AF.FRU.STO.SIM.M[j]<-sum(ERR.FRU.STO.SIM.M[,j]*PREV.DF[,"p.fru.sto.m"])/(1+sum(ERR.FRU.STO.SIM.M[,j]*PREV.DF[,"p.fru.sto.m"]))*100
}

# FRUIT AND LUNG CANCER - WOMEN
#############################
AF.FRU.LUN.SIM.W<-rep(NA,10000)
for (j in 1:length(AF.FRU.LUN.SIM.W))
{
 AF.FRU.LUN.SIM.W[j]<-sum(ERR.FRU.LUN.SIM.W[,j]*PREV.DF[,"p.fru.lun.w"])/(1+sum(ERR.FRU.LUN.SIM.W[,j]*PREV.DF[,"p.fru.lun.w"]))*100
}

# FRUIT AND LUNG CANCER - MEN
#############################
AF.FRU.LUN.SIM.M<-rep(NA,10000)
for (j in 1:length(AF.FRU.LUN.SIM.M))
{
 AF.FRU.LUN.SIM.M[j]<-sum(ERR.FRU.LUN.SIM.M[,j]*PREV.DF[,"p.fru.lun.m"])/(1+sum(ERR.FRU.LUN.SIM.M[,j]*PREV.DF[,"p.fru.lun.m"]))*100
}

# FIBRE AND COLORECTAL CANCER - WOMEN
#############################
AF.FIB.COL.SIM.W<-rep(NA,10000)
for (j in 1:length(AF.FIB.COL.SIM.W))
{
 AF.FIB.COL.SIM.W[j]<-sum(ERR.FIB.COL.SIM.W[,j]*PREV.DF[,"p.fib.col.w"])/(1+sum(ERR.FIB.COL.SIM.W[,j]*PREV.DF[,"p.fib.col.w"]))*100
}

# FIBRE AND COLORECTAL CANCER - MEN
#############################
AF.FIB.COL.SIM.M<-rep(NA,10000)
for (j in 1:length(AF.FIB.COL.SIM.M))
{
 AF.FIB.COL.SIM.M[j]<-sum(ERR.FIB.COL.SIM.M[,j]*PREV.DF[,"p.fib.col.m"])/(1+sum(ERR.FIB.COL.SIM.M[,j]*PREV.DF[,"p.fib.col.m"]))*100
}

# CITRUS FRUIT AND STOMACH CANCER (CARDIA) - WOMEN
#############################
AF.CFR.SCA.SIM.W<-round((exp(logRR.CFR.SCA.SIM)-1)*P.CFR.SIM.W/(1+(exp(logRR.CFR.SCA.SIM)-1)*P.CFR.SIM.W)*100,1)

# CITRUS FRUIT AND STOMACH CANCER (CARDIA) - MEN
#############################
AF.CFR.SCA.SIM.M<-round((exp(logRR.CFR.SCA.SIM)-1)*P.CFR.SIM.M/(1+(exp(logRR.CFR.SCA.SIM)-1)*P.CFR.SIM.M)*100,1)

# COFFEE AND LIVER CANCER - WOMEN
#############################
AF.COF.LIV.SIM.W<-round((exp(logRR.COF.LIV.SIM)-1)*P.COF.SIM.W/(1+(exp(logRR.COF.LIV.SIM)-1)*P.COF.SIM.W)*100,1)

# COFFEE AND LIVER CANCER - MEN
#############################
AF.COF.LIV.SIM.M<-round((exp(logRR.COF.LIV.SIM)-1)*P.COF.SIM.M/(1+(exp(logRR.COF.LIV.SIM)-1)*P.COF.SIM.M)*100,1)

# COFFEE AND ENDOMETRIAL CANCER - WOMEN
#############################
AF.COF.END.SIM.W<-round((exp(logRR.COF.END.SIM)-1)*P.COF.SIM.W/(1+(exp(logRR.COF.END.SIM)-1)*P.COF.SIM.W)*100,1)

################################################################################
#
# ATTRIBUTABLE FRACTION (AF) - INTERVAL ESTIMATES
#
################################################################################
AF[1,c(5,6)]<-c(round(quantile(AF.PME.COL.SIM.W,.025),1),round(quantile(AF.PME.COL.SIM.W,.975),1))
AF[2,c(5,6)]<-c(round(quantile(AF.PME.COL.SIM.M,.025),1),round(quantile(AF.PME.COL.SIM.M,.975),1))
AF[3,c(5,6)]<-c(round(quantile(AF.RME.COL.SIM.W,.025),1),round(quantile(AF.RME.COL.SIM.W,.975),1))
AF[4,c(5,6)]<-c(round(quantile(AF.RME.COL.SIM.M,.025),1),round(quantile(AF.RME.COL.SIM.M,.975),1))
AF[5,c(5,6)]<-c(round(quantile(AF.DAI.COL.SIM.W,.025),1),round(quantile(AF.DAI.COL.SIM.W,.975),1))
AF[6,c(5,6)]<-c(round(quantile(AF.DAI.COL.SIM.M,.025),1),round(quantile(AF.DAI.COL.SIM.M,.975),1))
AF[7,c(5,6)]<-c(round(quantile(AF.NSV.ORA.SIM.W,.025),1),round(quantile(AF.NSV.ORA.SIM.W,.975),1))
AF[8,c(5,6)]<-c(round(quantile(AF.NSV.ORA.SIM.M,.025),1),round(quantile(AF.NSV.ORA.SIM.M,.975),1))
AF[9,c(5,6)]<-c(round(quantile(AF.NSV.NAS.SIM.W,.025),1),round(quantile(AF.NSV.NAS.SIM.W,.975),1))
AF[10,c(5,6)]<-c(round(quantile(AF.NSV.NAS.SIM.M,.025),1),round(quantile(AF.NSV.NAS.SIM.M,.975),1))
AF[11,c(5,6)]<-c(round(quantile(AF.NSV.AES.SIM.W,.025),1),round(quantile(AF.NSV.AES.SIM.W,.975),1))
AF[12,c(5,6)]<-c(round(quantile(AF.NSV.AES.SIM.M,.025),1),round(quantile(AF.NSV.AES.SIM.M,.975),1))
AF[13,c(5,6)]<-c(round(quantile(AF.NSV.SES.SIM.W,.025),1),round(quantile(AF.NSV.SES.SIM.W,.975),1))
AF[14,c(5,6)]<-c(round(quantile(AF.NSV.SES.SIM.M,.025),1),round(quantile(AF.NSV.SES.SIM.M,.975),1))
AF[15,c(5,6)]<-c(round(quantile(AF.NSV.COL.SIM.W,.025),1),round(quantile(AF.NSV.COL.SIM.W,.975),1))
AF[16,c(5,6)]<-c(round(quantile(AF.NSV.COL.SIM.M,.025),1),round(quantile(AF.NSV.COL.SIM.M,.975),1))
AF[17,c(5,6)]<-c(round(quantile(AF.NSV.LUN.SIM.W,.025),1),round(quantile(AF.NSV.LUN.SIM.W,.975),1))
AF[18,c(5,6)]<-c(round(quantile(AF.NSV.LUN.SIM.M,.025),1),round(quantile(AF.NSV.LUN.SIM.M,.975),1))
AF[19,c(5,6)]<-c(round(quantile(AF.FRU.SES.SIM.W,.025),1),round(quantile(AF.FRU.SES.SIM.W,.975),1))
AF[20,c(5,6)]<-c(round(quantile(AF.FRU.SES.SIM.M,.025),1),round(quantile(AF.FRU.SES.SIM.M,.975),1))
AF[21,c(5,6)]<-c(round(quantile(AF.FRU.STO.SIM.W,.025),1),round(quantile(AF.FRU.STO.SIM.W,.975),1))
AF[22,c(5,6)]<-c(round(quantile(AF.FRU.STO.SIM.M,.025),1),round(quantile(AF.FRU.STO.SIM.M,.975),1))
AF[23,c(5,6)]<-c(round(quantile(AF.FRU.LUN.SIM.W,.025),1),round(quantile(AF.FRU.LUN.SIM.W,.975),1))
AF[24,c(5,6)]<-c(round(quantile(AF.FRU.LUN.SIM.M,.025),1),round(quantile(AF.FRU.LUN.SIM.M,.975),1))
AF[25,c(5,6)]<-c(round(quantile(AF.FIB.COL.SIM.W,.025),1),round(quantile(AF.FIB.COL.SIM.W,.975),1))
AF[26,c(5,6)]<-c(round(quantile(AF.FIB.COL.SIM.M,.025),1),round(quantile(AF.FIB.COL.SIM.M,.975),1))
AF[27,c(5,6)]<-c(round(quantile(AF.CFR.SCA.SIM.W,.025),1),round(quantile(AF.CFR.SCA.SIM.W,.975),1))
AF[28,c(5,6)]<-c(round(quantile(AF.CFR.SCA.SIM.M,.025),1),round(quantile(AF.CFR.SCA.SIM.M,.975),1))
AF[29,c(5,6)]<-c(round(quantile(AF.COF.LIV.SIM.W,.025),1),round(quantile(AF.COF.LIV.SIM.W,.975),1))
AF[30,c(5,6)]<-c(round(quantile(AF.COF.LIV.SIM.M,.025),1),round(quantile(AF.COF.LIV.SIM.M,.975),1))
AF[31,c(5,6)]<-c(round(quantile(AF.COF.END.SIM.W,.025),1),round(quantile(AF.COF.END.SIM.W,.975),1))

# IMPUTING TO ZERO THE LOWER LIMIT OF CI WHEN IT IS < 0
AF.lowCI<-AF[,"AF.lowCI"]
AF.lowCI[AF.lowCI<0]<-0
AF.mod<-AF
AF.mod[,"AF.lowCI"]<-AF.lowCI

# ATTRIBUTABLE CASES AND CORRESPONDING 95% CI
AF.mod[,"Obs.ca"]<-c(
                     rep(c(20282,23420),3),
                     c(2462,6855),
                     c(118,421),
                     c(226,564),
                     c(458,1146),
                     c(20282,23420),
                     c(13328,27554),
                     c(458,1146),
                     c(6098,8458),
                     c(13328,27554),
                     c(20282,23420),
                     c(915,1269),
                     c(4034,8978),
                     8335
                     )
AF.mod[,"AC"]<-round(AF.mod[,"AF"]*AF.mod[,"Obs.ca"]/100,0)
AF.mod[,"AC.lowCI"]<-round(AF.mod[,"AF.lowCI"]*AF.mod[,"Obs.ca"]/100,0)
AF.mod[,"AC.highCI"]<-round(AF.mod[,"AF.highCI"]*AF.mod[,"Obs.ca"]/100,0)

# WOMEN
AF.mod[AF.mod$Sex=="W",1:10]
sum(AF.mod$AC[AF.mod$Sex=="W"])
sum(AF.mod$AC.lowCI[AF.mod$Sex=="W"])
sum(AF.mod$AC.highCI[AF.mod$Sex=="W"])

t.obs.ca.w<-20282+2462+118+226+458+13328+6098+4034+8335
round(sum(AF.mod$AC[AF.mod$Sex=="W"])/t.obs.ca.w*100,1)
round(sum(AF.mod$AC.lowCI[AF.mod$Sex=="W"])/t.obs.ca.w*100,1)
round(sum(AF.mod$AC.highCI[AF.mod$Sex=="W"])/t.obs.ca.w*100,1)

round(sum(AF.mod$AC[AF.mod$Sex=="W"])/181857*100,1)
round(sum(AF.mod$AC.lowCI[AF.mod$Sex=="W"])/181857*100,1)
round(sum(AF.mod$AC.highCI[AF.mod$Sex=="W"])/181857*100,1)

# MEN
AF.mod[AF.mod$Sex=="M",1:10]
sum(AF.mod$AC[AF.mod$Sex=="M"])
sum(AF.mod$AC.lowCI[AF.mod$Sex=="M"])
sum(AF.mod$AC.highCI[AF.mod$Sex=="M"])

t.obs.ca.m<-23420+6855+421+564+1146+27554+8458+8978
round(sum(AF.mod$AC[AF.mod$Sex=="M"])/t.obs.ca.m*100,1)
round(sum(AF.mod$AC.lowCI[AF.mod$Sex=="M"])/t.obs.ca.m*100,1)
round(sum(AF.mod$AC.highCI[AF.mod$Sex=="M"])/t.obs.ca.m*100,1)

round(sum(AF.mod$AC[AF.mod$Sex=="M"])/194754*100,1)
round(sum(AF.mod$AC.lowCI[AF.mod$Sex=="M"])/194754*100,1)
round(sum(AF.mod$AC.highCI[AF.mod$Sex=="M"])/194754*100,1)

# ATTRIBUTABLE DEATHS AND CORRESPONDING 95% CI
AF.mod[,"Obs.de"]<-c(
                     rep(c(8716,10246),3),
                     c(1176,3160),
                     c(73,178),
                     c(181,447),
                     c(367,907),
                     c(8716,10246),
                     c(10110,22188),
                     c(367,907),
                     c(3550,5082),
                     c(10110,22188),
                     c(8716,10246),
                     c(533,762),
                     c(2882,5708),
                     2152 
                     )
AF.mod[,"AD"]<-round(AF.mod[,"AF"]*AF.mod[,"Obs.de"]/100,0)
AF.mod[,"AD.lowCI"]<-round(AF.mod[,"AF.lowCI"]*AF.mod[,"Obs.de"]/100,0)
AF.mod[,"AD.highCI"]<-round(AF.mod[,"AF.highCI"]*AF.mod[,"Obs.de"]/100,0)

# WOMEN
AF.mod[AF.mod$Sex=="W",c(1:6,11:14)]
sum(AF.mod$AD[AF.mod$Sex=="W"])
sum(AF.mod$AD.lowCI[AF.mod$Sex=="W"])
sum(AF.mod$AD.highCI[AF.mod$Sex=="W"])

t.obs.de.w<-8716+1176+73+181+367+10110+3550+2882+2152
round(sum(AF.mod$AD[AF.mod$Sex=="W"])/t.obs.de.w*100,1)
round(sum(AF.mod$AD.lowCI[AF.mod$Sex=="W"])/t.obs.de.w*100,1)
round(sum(AF.mod$AD.highCI[AF.mod$Sex=="W"])/t.obs.de.w*100,1)

round(sum(AF.mod$AD[AF.mod$Sex=="W"])/79991*100,1)
round(sum(AF.mod$AD.lowCI[AF.mod$Sex=="W"])/79991*100,1)
round(sum(AF.mod$AD.highCI[AF.mod$Sex=="W"])/79991*100,1)

# MEN
AF.mod[AF.mod$Sex=="M",c(1:6,11:14)]
sum(AF.mod$AD[AF.mod$Sex=="M"])
sum(AF.mod$AD.lowCI[AF.mod$Sex=="M"])
sum(AF.mod$AD.highCI[AF.mod$Sex=="M"])

t.obs.de.m<-10246+3160+178+447+907+22188+5082+5708
round(sum(AF.mod$AD[AF.mod$Sex=="M"])/t.obs.de.m*100,1)
round(sum(AF.mod$AD.lowCI[AF.mod$Sex=="M"])/t.obs.de.m*100,1)
round(sum(AF.mod$AD.highCI[AF.mod$Sex=="M"])/t.obs.de.m*100,1)

round(sum(AF.mod$AD[AF.mod$Sex=="M"])/97867*100,1)
round(sum(AF.mod$AD.lowCI[AF.mod$Sex=="M"])/97867*100,1)
round(sum(AF.mod$AD.highCI[AF.mod$Sex=="M"])/97867*100,1)

################################################################################
#
# DISTRIBUTION PLOT OF DIETARY FACTORS 
#
################################################################################
# X AND Y VALUES FOR WOMEN 
#############################
# PROCESSED MEAT - WOMEN
x.pme.w<-qgamma(
                seq(0,.99,.00001),
                shape=DIET$kappa.DF.w[DIET$DF=="Pme"&DIET$Cancer=="Col"],
                scale=DIET$theta.DF.w[DIET$DF=="Pme"&DIET$Cancer=="Col"]
               )
y.pme.w.tmp<-dgamma(
                    x.pme.w,
                    shape=DIET$kappa.DF.w[DIET$DF=="Pme"&DIET$Cancer=="Col"],
                    scale=DIET$theta.DF.w[DIET$DF=="Pme"&DIET$Cancer=="Col"]
                   )
y.pme.w<-y.pme.w.tmp*DIET$pro.con.w[DIET$DF=="Pme"&DIET$Cancer=="Col"]

# PROCESSED MEAT - MEN
x.pme.m<-qgamma(
                seq(0,.99,.00001),
                shape=DIET$kappa.DF.m[DIET$DF=="Pme"&DIET$Cancer=="Col"],
                scale=DIET$theta.DF.m[DIET$DF=="Pme"&DIET$Cancer=="Col"]
               )
y.pme.m.tmp<-dgamma(
                    x.pme.m,
                    shape=DIET$kappa.DF.m[DIET$DF=="Pme"&DIET$Cancer=="Col"],
                    scale=DIET$theta.DF.m[DIET$DF=="Pme"&DIET$Cancer=="Col"]
                   )
y.pme.m<-y.pme.m.tmp*DIET$pro.con.m[DIET$DF=="Pme"&DIET$Cancer=="Col"]

# RED MEAT - WOMEN
x.rme.w<-qgamma(
                seq(0,.99,.00001),
                shape=DIET$kappa.DF.w[DIET$DF=="Rme"&DIET$Cancer=="Col"],
                scale=DIET$theta.DF.w[DIET$DF=="Rme"&DIET$Cancer=="Col"]
               )
y.rme.w.tmp<-dgamma(
                    x.rme.w,
                    shape=DIET$kappa.DF.w[DIET$DF=="Rme"&DIET$Cancer=="Col"],
                    scale=DIET$theta.DF.w[DIET$DF=="Rme"&DIET$Cancer=="Col"]
                   )
y.rme.w<-y.rme.w.tmp*DIET$pro.con.w[DIET$DF=="Rme"&DIET$Cancer=="Col"]

# RED MEAT - MEN
x.rme.m<-qgamma(
                seq(0,.99,.00001),
                shape=DIET$kappa.DF.m[DIET$DF=="Rme"&DIET$Cancer=="Col"],
                scale=DIET$theta.DF.m[DIET$DF=="Rme"&DIET$Cancer=="Col"]
               )
y.rme.m.tmp<-dgamma(
                    x.rme.m,
                    shape=DIET$kappa.DF.m[DIET$DF=="Rme"&DIET$Cancer=="Col"],
                    scale=DIET$theta.DF.m[DIET$DF=="Rme"&DIET$Cancer=="Col"]
                   )
y.rme.m<-y.rme.m.tmp*DIET$pro.con.m[DIET$DF=="Rme"&DIET$Cancer=="Col"]

# DAIRY PRODUCTS - WOMEN
x.dai.w<-qgamma(
                seq(0,.99,.00001),
                shape=DIET$kappa.DF.w[DIET$DF=="Dai"&DIET$Cancer=="Col"],
                scale=DIET$theta.DF.w[DIET$DF=="Dai"&DIET$Cancer=="Col"]
               )
y.dai.w.tmp<-dgamma(
                    x.dai.w,
                    shape=DIET$kappa.DF.w[DIET$DF=="Dai"&DIET$Cancer=="Col"],
                    scale=DIET$theta.DF.w[DIET$DF=="Dai"&DIET$Cancer=="Col"]
                   )
y.dai.w<-y.dai.w.tmp*DIET$pro.con.w[DIET$DF=="Dai"&DIET$Cancer=="Col"]

# DAIRY PRODUCTS - MEN
x.dai.m<-qgamma(
                seq(0,.99,.00001),
                shape=DIET$kappa.DF.m[DIET$DF=="Dai"&DIET$Cancer=="Col"],
                scale=DIET$theta.DF.m[DIET$DF=="Dai"&DIET$Cancer=="Col"]
               )
y.dai.m.tmp<-dgamma(
                    x.dai.m,
                    shape=DIET$kappa.DF.m[DIET$DF=="Dai"&DIET$Cancer=="Col"],
                    scale=DIET$theta.DF.m[DIET$DF=="Dai"&DIET$Cancer=="Col"]
                   )
y.dai.m<-y.dai.m.tmp*DIET$pro.con.m[DIET$DF=="Dai"&DIET$Cancer=="Col"]

# NON-STARCHY VEGETABLES - WOMEN
x.nsv.w<-qgamma(
                seq(0,.99,.00001),
                shape=DIET$kappa.DF.w[DIET$DF=="Nsv"&DIET$Cancer=="Ora"],
                scale=DIET$theta.DF.w[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]
               )
y.nsv.w.tmp<-dgamma(
                    x.nsv.w,
                    shape=DIET$kappa.DF.w[DIET$DF=="Nsv"&DIET$Cancer=="Ora"],
                    scale=DIET$theta.DF.w[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]
                   )
y.nsv.w<-y.nsv.w.tmp*DIET$pro.con.w[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]

# NON-STARCHY VEGETABLES - MEN
x.nsv.m<-qgamma(
                seq(0,.99,.00001),
                shape=DIET$kappa.DF.m[DIET$DF=="Nsv"&DIET$Cancer=="Ora"],
                scale=DIET$theta.DF.m[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]
               )
y.nsv.m.tmp<-dgamma(
                    x.nsv.m,
                    shape=DIET$kappa.DF.m[DIET$DF=="Nsv"&DIET$Cancer=="Ora"],
                    scale=DIET$theta.DF.m[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]
                   )
y.nsv.m<-y.nsv.m.tmp*DIET$pro.con.m[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]

# FRUIT - WOMEN
x.fru.w<-qgamma(
                seq(0,.99,.00001),
                shape=DIET$kappa.DF.w[DIET$DF=="Fru"&DIET$Cancer=="Ses"],
                scale=DIET$theta.DF.w[DIET$DF=="Fru"&DIET$Cancer=="Ses"]
               )
y.fru.w.tmp<-dgamma(
                    x.fru.w,
                    shape=DIET$kappa.DF.w[DIET$DF=="Fru"&DIET$Cancer=="Ses"],
                    scale=DIET$theta.DF.w[DIET$DF=="Fru"&DIET$Cancer=="Ses"]
                   )
y.fru.w<-y.fru.w.tmp*DIET$pro.con.w[DIET$DF=="Fru"&DIET$Cancer=="Ses"]

# FRUIT - MEN
x.fru.m<-qgamma(
                seq(0,.99,.00001),
                shape=DIET$kappa.DF.m[DIET$DF=="Fru"&DIET$Cancer=="Ses"],
                scale=DIET$theta.DF.m[DIET$DF=="Fru"&DIET$Cancer=="Ses"]
               )
y.fru.m.tmp<-dgamma(
                    x.fru.m,
                    shape=DIET$kappa.DF.m[DIET$DF=="Fru"&DIET$Cancer=="Ses"],
                    scale=DIET$theta.DF.m[DIET$DF=="Fru"&DIET$Cancer=="Ses"]
                   )
y.fru.m<-y.fru.m.tmp*DIET$pro.con.m[DIET$DF=="Fru"&DIET$Cancer=="Ses"]

# FIBRE - WOMEN
x.fib.w<-qgamma(
                seq(0,.99,.00001),
                shape=DIET$kappa.DF.w[DIET$DF=="Fib"&DIET$Cancer=="Col"],
                scale=DIET$theta.DF.w[DIET$DF=="Fib"&DIET$Cancer=="Col"]
               )
y.fib.w.tmp<-dgamma(
                    x.fib.w,
                    shape=DIET$kappa.DF.w[DIET$DF=="Fib"&DIET$Cancer=="Col"],
                    scale=DIET$theta.DF.w[DIET$DF=="Fib"&DIET$Cancer=="Col"]
                   )
y.fib.w<-y.fib.w.tmp*DIET$pro.con.w[DIET$DF=="Fib"&DIET$Cancer=="Col"]

# FIBRE - MEN
x.fib.m<-qgamma(
                seq(0,.99,.00001),
                shape=DIET$kappa.DF.m[DIET$DF=="Fib"&DIET$Cancer=="Col"],
                scale=DIET$theta.DF.m[DIET$DF=="Fib"&DIET$Cancer=="Col"]
               )
y.fib.m.tmp<-dgamma(
                    x.fib.m,
                    shape=DIET$kappa.DF.m[DIET$DF=="Fib"&DIET$Cancer=="Col"],
                    scale=DIET$theta.DF.m[DIET$DF=="Fib"&DIET$Cancer=="Col"]
                   )
y.fib.m<-y.fib.m.tmp*DIET$pro.con.m[DIET$DF=="Fib"&DIET$Cancer=="Col"]

################################################################################
#
# PROPORTION OF PEOPLE WHO WERE NOT ADHERENT TO THE OPTIMAL INTAKE
#
################################################################################
# PROCESSED MEAT - WOMEN
#############################
round(DIET$pro.con.w[DIET$DF=="Pme"&DIET$Cancer=="Col"]*100,0)

# PROCESSED MEAT - MEN
#############################
round(DIET$pro.con.m[DIET$DF=="Pme"&DIET$Cancer=="Col"]*100,0)

# RED MEAT - WOMEN
#############################
round((DIET$pro.con.w[DIET$DF=="Rme"&DIET$Cancer=="Col"]-
(pgamma(counter.rme,
       shape=DIET$kappa.DF.w[DIET$DF=="Rme"&DIET$Cancer=="Col"],
       scale=DIET$theta.DF.w[DIET$DF=="Rme"&DIET$Cancer=="Col"]
      )*DIET$pro.con.w[DIET$DF=="Rme"&DIET$Cancer=="Col"]))*100,0)

# RED MEAT - MEN
#############################
round((DIET$pro.con.m[DIET$DF=="Rme"&DIET$Cancer=="Col"]-
(pgamma(counter.rme,
       shape=DIET$kappa.DF.m[DIET$DF=="Rme"&DIET$Cancer=="Col"],
       scale=DIET$theta.DF.m[DIET$DF=="Rme"&DIET$Cancer=="Col"]
      )*DIET$pro.con.m[DIET$DF=="Rme"&DIET$Cancer=="Col"]))*100,0)

# DAIRY PRODUCTS - WOMEN
#############################
round((DIET$pro.ncon.w[DIET$DF=="Dai"&DIET$Cancer=="Col"]+
(pgamma(counter.dai,
       shape=DIET$kappa.DF.w[DIET$DF=="Dai"&DIET$Cancer=="Col"],
       scale=DIET$theta.DF.w[DIET$DF=="Dai"&DIET$Cancer=="Col"]
      )*DIET$pro.con.w[DIET$DF=="Dai"&DIET$Cancer=="Col"]))*100,0)

# DAIRY PRODUCTS - MEN
#############################
round((DIET$pro.ncon.m[DIET$DF=="Dai"&DIET$Cancer=="Col"]+
(pgamma(counter.dai,
       shape=DIET$kappa.DF.m[DIET$DF=="Dai"&DIET$Cancer=="Col"],
       scale=DIET$theta.DF.m[DIET$DF=="Dai"&DIET$Cancer=="Col"]
      )*DIET$pro.con.m[DIET$DF=="Dai"&DIET$Cancer=="Col"]))*100,0)

# NON-STARCHY VEGETABLES - WOMEN
#############################
round((DIET$pro.ncon.w[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]+
(pgamma(counter.nsv,
       shape=DIET$kappa.DF.w[DIET$DF=="Nsv"&DIET$Cancer=="Ora"],
       scale=DIET$theta.DF.w[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]
      )*DIET$pro.con.w[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]))*100,0)

# NON-STARCHY VEGETABLES - MEN
#############################
round((DIET$pro.ncon.m[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]+
(pgamma(counter.nsv,
       shape=DIET$kappa.DF.m[DIET$DF=="Nsv"&DIET$Cancer=="Ora"],
       scale=DIET$theta.DF.m[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]
      )*DIET$pro.con.m[DIET$DF=="Nsv"&DIET$Cancer=="Ora"]))*100,0)

# FRUIT - WOMEN
#############################
round((DIET$pro.ncon.w[DIET$DF=="Fru"&DIET$Cancer=="Ses"]+
(pgamma(counter.fru,
       shape=DIET$kappa.DF.w[DIET$DF=="Fru"&DIET$Cancer=="Ses"],
       scale=DIET$theta.DF.w[DIET$DF=="Fru"&DIET$Cancer=="Ses"]
      )*DIET$pro.con.w[DIET$DF=="Fru"&DIET$Cancer=="Ses"]))*100,0)

# FRUIT - MEN
#############################
round((DIET$pro.ncon.m[DIET$DF=="Fru"&DIET$Cancer=="Ses"]+
(pgamma(counter.fru,
       shape=DIET$kappa.DF.m[DIET$DF=="Fru"&DIET$Cancer=="Ses"],
       scale=DIET$theta.DF.m[DIET$DF=="Fru"&DIET$Cancer=="Ses"]
      )*DIET$pro.con.m[DIET$DF=="Fru"&DIET$Cancer=="Ses"]))*100,0)

# FIBRE - WOMEN
#############################
round((DIET$pro.ncon.w[DIET$DF=="Fib"&DIET$Cancer=="Col"]+
(pgamma(counter.fib,
       shape=DIET$kappa.DF.w[DIET$DF=="Fib"&DIET$Cancer=="Col"],
       scale=DIET$theta.DF.w[DIET$DF=="Fib"&DIET$Cancer=="Col"]
      )*DIET$pro.con.w[DIET$DF=="Fib"&DIET$Cancer=="Col"]))*100,0)

# FIBRE - MEN
#############################
round((DIET$pro.ncon.m[DIET$DF=="Fib"&DIET$Cancer=="Col"]+
(pgamma(counter.fib,
       shape=DIET$kappa.DF.m[DIET$DF=="Fib"&DIET$Cancer=="Col"],
       scale=DIET$theta.DF.m[DIET$DF=="Fib"&DIET$Cancer=="Col"]
      )*DIET$pro.con.m[DIET$DF=="Fib"&DIET$Cancer=="Col"]))*100,0)

# CITRUS FRUIT - WOMEN
#############################
round(DIET$pro.con.w[DIET$DF=="Cfr"&DIET$Cancer=="Sca"]*100,0)

# CITRUS FRUIT - MEN
#############################
round(DIET$pro.con.m[DIET$DF=="Cfr"&DIET$Cancer=="Sca"]*100,0)

# COFFEE - WOMEN
#############################
round(DIET$pro.con.w[DIET$DF=="Cof"&DIET$Cancer=="Liv"]*100,0)

# COFFEE - MEN
#############################
round(DIET$pro.con.m[DIET$DF=="Cof"&DIET$Cancer=="Liv"]*100,0)

################################################################################
#
# PLOT FOR WOMEN
#
################################################################################
require(plotrix)
require(extrafont)

cairo_pdf(
          "[YourPath]\\SupFig2.DFDistrWom.pdf",
          width=7,
          height=11.5,
          family="Times"
         )
par(mfrow=c(4,2),
    mar=c(3,3,1.5,0.5),
    family="Times New Roman",
    cex.lab=.95,
    cex.axis=.75,
    tck=-.015,
    mgp=c(1.3,.32,0),
    lwd=1.5
   )

# PROCESSED MEAT
gap.plot(x.pme.w[x.pme.w>.55],
         y.pme.w[x.pme.w>.55]*100,
         xlim=c(0,110),
         ylim=c(0,3),
         gap=c(2.3,2.4),
         type="l",
         lwd=1.5,
         xlab="",
         ytics=c(seq(0,2,.5),2.75),
         yticlab=formatC(c(seq(0,2,.5),20),1,format="f"),
         ylab="",
         yaxs="i",
         xtics=F
        )

# Adding histogram for non consumers
rect(xleft=-.55,ybottom=0,xright=.55,ytop=2.3,col="white")
rect(xleft=-.55,ybottom=2.358,xright=.55,ytop=2.65,col="white")

# Shadowing consumption higher than the recommendation
polygon(
        c(
          min(x.pme.w[x.pme.w>.55]),
          x.pme.w[x.pme.w>.55&x.pme.w<=max(x.pme.w)],
          max(x.pme.w)
         ),
        c(
          0,
          y.pme.w[x.pme.w>.55&x.pme.w<=max(x.pme.w)]*100,
          0
          ),
        col="grey"
        )


# Adding x-axis
axis(1,at=c(PREV.DF[seq(2,10,2),"pme.col.w"]),labels=F)
text(c(0,PREV.DF[seq(2,10,2),"pme.col.w"]),
     -.08,
     labels=formatC(round(c(0,PREV.DF[seq(2,10,2),"pme.col.w"]),1),1,format="f"),
     srt=60,
     cex=.6,
     adj=1,
     xpd=TRUE)

axis(1,at=PREV.DF[seq(1,10,2),"pme.col.w"],labels=F)
title(xlab="Processed meat intake (gr/day)",line=1.5)

# Adding label to y-axis
title(ylab="Density (%)")

# Adding legend
rect(xleft=5,ybottom=2.12,xright=9,ytop=2.205,col="grey")
text(10,2.15,"Proportion of women with any intake: 80%",cex=.85,adj=0)

# RED MEAT
gap.plot(x.rme.w[x.rme.w>.95],
         y.rme.w[x.rme.w>.95]*100,
         xlim=c(0,190),
         ylim=c(0,1.75),
         gap=c(1.3,1.4),
         type="l",
         lwd=1.5,
         xlab="",
         ytics=c(seq(0,1,.5),1.6),
         yticlab=formatC(c(seq(0,1,.5),20),1,format="f"),
         ylab="",
         yaxs="i",
         xtics=F
        )

# Shadowing consumption lower than the recommendation
polygon(
        c(
          counter.rme,
          x.rme.w[x.rme.w>counter.rme&x.rme.w<max(x.rme.w)],
          max(x.rme.w)
         ),
        c(
          0,
          y.rme.w[x.rme.w>counter.rme&x.rme.w<max(x.rme.w)]*100,
          0
          ),
        col="grey"
        )

# Adding histogram for non consumers
rect(xleft=-.95,ybottom=0,xright=.95,ytop=1.3,col="white")
rect(xleft=-.95,ybottom=1.334,xright=.95,ytop=1.5,col="white")

# Adding x-axis
axis(1,at=c(0,PREV.DF[seq(2,10,2),"rme.col.w"]),labels=F)
text(c(0,PREV.DF[seq(2,10,2),"rme.col.w"]),
     -.04,
     labels=formatC(round(c(0,PREV.DF[seq(2,10,2),"rme.col.w"]),1),1,format="f"),
     srt=60,
     cex=.6,
     adj=1,
     xpd=TRUE)

axis(1,at=PREV.DF[seq(1,10,2),"rme.col.w"],labels=F)
title(xlab="Red meat intake (gr/day)",line=1.5)

# Adding label to y-axis
title(ylab="Density (%)")

# Adding text in the plot
rect(xleft=8,ybottom=1.207,xright=15,ytop=1.254,col="grey")
text(17,1.223,"Proportion of women with an intake of \u226550 gr/day: 42%",cex=.85,adj=0)

# DAIRY PRODUCTS
gap.plot(x.dai.w[x.dai.w>2.3],
         y.dai.w[x.dai.w>2.3]*100,
         xlim=c(0,570),
         ylim=c(0,.6),
         gap=c(.45,.48),
         type="l",
         lwd=1.5,
         xlab="",
         ytics=c(seq(0,.4,.1),.552),
         yticlab=formatC(c(seq(0,.4,.1),1),1,format="f"),
         ylab="",
         yaxs="i",
         xtics=F
        )

# Shadowing consumption lower than the recommendation
polygon(
        c(
          min(x.dai.w[x.dai.w>2.3]),
          x.dai.w[x.dai.w>2.3&x.dai.w<=counter.dai&x.dai.w<=max(x.dai.w)],
          counter.dai
         ),
        c(
          0,
          y.dai.w[x.dai.w>2.3&x.dai.w<=counter.dai&x.dai.w<=max(x.dai.w)]*100,
          0
          ),
        col="grey"
        )

# Adding histogram for non consumers
rect(xleft=-2.3,ybottom=0,xright=2.3,ytop=.45,col="grey")
rect(xleft=-2.3,ybottom=.4617,xright=2.3,ytop=.52,col="grey")

# Adding x-axis
axis(1,at=c(0,PREV.DF[seq(2,10,2),"dai.col.w"]),labels=F)
text(c(0,PREV.DF[seq(2,10,2),"dai.col.w"]),
     -.015,
     labels=formatC(round(c(0,PREV.DF[seq(2,10,2),"dai.col.w"]),1),1,format="f"),
     srt=60,
     cex=.6,
     adj=1,
     xpd=TRUE)

axis(1,at=PREV.DF[seq(1,10,2),"dai.col.w"],labels=F)
title(xlab="Dairy products intake (gr/day)",line=1.5)

# Adding label to y-axis
title(ylab="Density (%)")

# Adding legend
rect(xleft=26,ybottom=.4135,xright=47,ytop=.431,col="grey")
text(52,.42,"Proportion of women with an intake of <300 gr/day: 84%",cex=.85,adj=0)

# FIBRE
gap.plot(x.fib.w,
         y.fib.w*100,
         xlim=c(0,37),
         ylim=c(0,8.5),
         gap=c(10,10.5),
         type="l",
         lwd=1.5,
         xlab="",
         ytics=seq(0,8,1),
         yticlab=formatC(seq(0,8,1),1,format="f"),
         ylab="",
         yaxs="i",
         xtics=F
        )

# Shadowing consumption lower than the recommendation
polygon(
        c(
          min(x.fib.w),
          x.fib.w[x.fib.w>min(x.fib.w)&x.fib.w<=counter.fib],
          counter.fib
         ),
        c(
          0,
          y.fib.w[x.fib.w>min(x.fib.w)&x.fib.w<=counter.fib]*100,
          0
          ),
        col="grey"
        )

# Adding x-axis
axis(1,at=c(0,PREV.DF[seq(2,10,2),"fib.col.w"]),labels=F)
text(c(0,PREV.DF[seq(2,10,2),"fib.col.w"]),
     -.195,
     labels=formatC(round(c(0,PREV.DF[seq(2,10,2),"fib.col.w"]),1),1,format="f"),
     srt=60,
     cex=.65,
     adj=1,
     xpd=TRUE)

axis(1,at=PREV.DF[seq(1,10,2),"fib.col.w"],labels=F)
title(xlab="Fibre intake (gr/day)",line=1.5)

# Adding label to y-axis
title(ylab="Density (%)")

# Adding legend
rect(xleft=1.473,ybottom=7.45,xright=2.923,ytop=7.675,col="grey")
text(3.36,7.542,"Proportion of women with an intake of <30 gr/day: 96%",cex=.85,adj=0)

# NON-STARCHY VEGETABLES
gap.plot(x.nsv.w,
         y.nsv.w*100,
         xlim=c(0,530),
         ylim=c(0,.6),
         gap=c(.7,.8),
         type="l",
         lwd=1.5,
         xlab="",
         ytics=seq(0,.6,.1),
         yticlab=formatC(seq(0,.6,.1),1,format="f"),
         ylab="",
         yaxs="i",
         xtics=F
        )

# Shadowing consumption lower than the recommendation
polygon(
        c(
          min(x.nsv.w),
          x.nsv.w[x.nsv.w>min(x.nsv.w)&x.nsv.w<=counter.nsv],
          counter.nsv
         ),
        c(
          0,
          y.nsv.w[x.nsv.w>min(x.nsv.w)&x.nsv.w<=counter.nsv]*100,
          0
          ),
        col="grey"
        )

# Adding histogram for non consumers
rect(xleft=-2.9,ybottom=0,xright=2.9,ytop=.09,col="grey")

# Adding x-axis
axis(1,at=c(0,PREV.DF[seq(2,10,2),"nsv.ora.w"]),labels=F)
text(c(0,PREV.DF[seq(2,10,2),"nsv.ora.w"]),
     -.015,
     labels=formatC(round(c(0,PREV.DF[seq(2,10,2),"nsv.ora.w"]),1),1,format="f"),
     srt=60,
     cex=.6,
     adj=1,
     xpd=TRUE)

axis(1,at=PREV.DF[seq(1,10,2),"nsv.ora.w"],labels=F)
title(xlab="Non-starchy vegetables intake (gr/day)",line=1.5)

# Adding legend
rect(xleft=22.45,ybottom=.465,xright=43.45,ytop=.479,col="grey")
text(48.45,.47,"Proportion of women with an intake of <240 gr/day: 66%",cex=.85,adj=0)

# Adding label to y-axis
title(ylab="Density (%)")

# FRUIT
gap.plot(x.fru.w[x.fru.w>3],
         y.fru.w[x.fru.w>3]*100,
         xlim=c(0,690),
         ylim=c(0,.6),
         gap=c(.45,.4825),
         type="l",
         lwd=1.5,
         xlab="",
         ytics=c(seq(0,.4,.1),.55),
         yticlab=formatC(c(seq(0,.4,.1),5),1,format="f"),
         ylab="",
         yaxs="i",
         xtics=F
        )

# Shadowing consumption lower than the recommendation
polygon(
        c(
          min(x.fru.w[x.fru.w>3]),
          x.fru.w[x.fru.w>3&x.fru.w<=counter.fru],
          counter.fru
         ),
        c(
          0,
          y.fru.w[x.fru.w>3&x.fru.w<=counter.fru]*100,
          0
          ),
        col="grey"
        )

# Adding histogram for non consumers
rect(xleft=-3,ybottom=0,xright=3,ytop=.45,col="grey")
rect(xleft=-3,ybottom=.46135,xright=3,ytop=.5175,col="grey")

# Adding x-axis
axis(1,at=c(0,PREV.DF[seq(2,10,2),"fru.ses.w"]),labels=F)
text(c(0,PREV.DF[seq(2,10,2),"fru.ses.w"]),
     -.015,
     labels=formatC(round(c(0,PREV.DF[seq(2,10,2),"fru.ses.w"]),1),1,format="f"),
     srt=60,
     cex=.6,
     adj=1,
     xpd=TRUE)

axis(1,at=PREV.DF[seq(1,10,2),"fru.ses.w"],labels=F)
title(xlab="Fruit intake (gr/day)",line=1.5)

# Adding label to y-axis
title(ylab="Density (%)")

# Adding legend
rect(xleft=29.505,ybottom=.41411,xright=56.519,ytop=.43,col="grey")
text(62,.42,"Proportion of women with an intake of <160 gr/day: 41%",cex=.85,adj=0)

# CITRUS FRUIT
barplot(c(round(DIET$pro.ncon.w[DIET$DF=="Cfr"]*100,0),round(DIET$pro.con.w[DIET$DF=="Cfr"]*100,0)),
        ylim=c(0,100),
        xlim=c(0,1),
        width=.2,
        space=1,
        col=c("white","grey"),
        names.arg=c("No consumption","Any consumption"),
        axes=F
       )
box()

# Adding y-axis
axis(2,at=seq(0,100,25),labels=formatC(seq(0,100,25),1,format="f"),cex=.75)
title(ylab="Frequency (%)")

# Adding label to y-axis
title(xlab="Citrus fruit",line=1.5)

# Adding legend
rect(xleft=.0435,ybottom=93.1,xright=.082,ytop=95.9,col="grey")
text(.092,93.85,"Proportion of women with any intake: 51%",cex=.85,adj=0)

# COFFEE
barplot(c(round(DIET$pro.ncon.w[DIET$DF=="Cof"&DIET$Cancer=="Liv"]*100,0),round(DIET$pro.con.w[DIET$DF=="Cof"&DIET$Cancer=="Liv"]*100,0)),
        ylim=c(0,100),
        xlim=c(0,1),
        width=.2,
        space=1,
        col=c("white","grey"),
        names.arg=c("No consumption","Any consumption"),
        axes=F
       )
box()

# Adding y-axis
axis(2,at=seq(0,100,25),labels=formatC(seq(0,100,25),1,format="f"),cex=.75)
title(ylab="Frequency (%)")

# Adding label to y-axis
title(xlab="Coffee",line=1.5)

# Adding legend
rect(xleft=.0435,ybottom=93.1,xright=.082,ytop=95.9,col="grey")
text(.092,93.85,"Proportion of women with any intake: 88%",cex=.85,adj=0)
dev.off()

################################################################################
#
# PLOT FOR MEN
#
################################################################################
cairo_pdf(
          "[YourPath]\\SupFig1.DFDistrMen.pdf",
          width=7,
          height=11.5,
          family="Times"
         )
par(
    mfrow=c(4,2),
    mar=c(3,3,1.5,0.5),
    family="Times New Roman",
    cex.lab=.95,
    cex.axis=.75,
    tck=-.015,
    mgp=c(1.3,.32,0),
    lwd=1.5
   )

# PROCESSED MEAT
gap.plot(
         x.pme.m[x.pme.m>.55&x.pme.m<=105],
         y.pme.m[x.pme.m>.55&x.pme.m<=105]*100,
         xlim=c(0,110),
         ylim=c(0,3),
         gap=c(2.3,2.4),
         type="l",
         lwd=1.5,
         xlab="",
         ytics=c(seq(0,2,.5),2.75),
         yticlab=formatC(c(seq(0,2,.5),13),1,format="f"),
         ylab="",
         yaxs="i",
         xtics=F
        )

# Adding histogram for non consumers
rect(xleft=-.55,ybottom=0,xright=.55,ytop=2.3,col="white")
rect(xleft=-.55,ybottom=2.358,xright=.55,ytop=2.65,col="white")

# Shadowing consumption higher than the recommendation
polygon(
        c(
          min(x.pme.m[x.pme.m>.55]),
          x.pme.m[x.pme.m>.55&x.pme.m<=105],
          105
         ),
        c(
          0,
          y.pme.m[x.pme.m>.55&x.pme.m<=105]*100,
          0
          ),
        col="grey"
        )


# Adding x-axis
axis(1,at=c(PREV.DF[seq(2,10,2),"pme.col.m"]),labels=F)
text(c(0,PREV.DF[seq(2,10,2),"pme.col.m"]),
     -.08,
     labels=formatC(round(c(0,PREV.DF[seq(2,10,2),"pme.col.m"]),1),1,format="f"),
     srt=60,
     cex=.6,
     adj=1,
     xpd=TRUE)

axis(1,at=PREV.DF[seq(1,10,2),"pme.col.m"],labels=F)
title(xlab="Processed meat intake (gr/day)",line=1.5)

# Adding label to y-axis
title(ylab="Density (%)")

# Adding legend
rect(xleft=5,ybottom=2.12,xright=9,ytop=2.205,col="grey")
text(10,2.15,"Proportion of men with any intake: 87%",cex=.85,adj=0)

# RED MEAT
gap.plot(x.rme.m[x.rme.m>.95],
         y.rme.m[x.rme.m>.95]*100,
         xlim=c(0,250),
         ylim=c(0,1.75),
         gap=c(1.3,1.4),
         type="l",
         lwd=1.5,
         xlab="",
         ytics=c(seq(0,1,.5),1.6),
         yticlab=formatC(c(seq(0,1,.5),17),1,format="f"),
         ylab="",
         yaxs="i",
         xtics=F
        )

# Shadowing consumption lower than the recommendation
polygon(
        c(
          counter.rme,
          x.rme.m[x.rme.m>counter.rme&x.rme.m<max(x.rme.m)],
          max(x.rme.m)
         ),
        c(
          0,
          y.rme.m[x.rme.m>counter.rme&x.rme.m<max(x.rme.m)]*100,
          0
          ),
        col="grey"
        )

# Adding histogram for non consumers
rect(xleft=-.95,ybottom=0,xright=.95,ytop=1.3,col="white")
rect(xleft=-.95,ybottom=1.334,xright=.95,ytop=1.5,col="white")

# Adding x-axis
axis(1,at=c(0,PREV.DF[seq(2,10,2),"rme.col.m"]),labels=F)
text(c(0,PREV.DF[seq(2,10,2),"rme.col.m"]),
     -.04,
     labels=formatC(round(c(0,PREV.DF[seq(2,10,2),"rme.col.m"]),1),1,format="f"),
     srt=60,
     cex=.6,
     adj=1,
     xpd=TRUE)

axis(1,at=PREV.DF[seq(1,10,2),"rme.col.m"],labels=F)
title(xlab="Red meat intake (gr/day)",line=1.5)

# Adding label to y-axis
title(ylab="Density (%)")

# Adding text in the plot
rect(xleft=8,ybottom=1.207,xright=15,ytop=1.254,col="grey")
text(17,1.223,"Proportion of men with an intake of \u226550 gr/day: 52%",cex=.85,adj=0)

# DAIRY PRODUCTS
gap.plot(x.dai.m[x.dai.m>2.3],
         y.dai.m[x.dai.m>2.3]*100,
         xlim=c(0,600),
         ylim=c(0,.6),
         gap=c(.45,.48),
         type="l",
         lwd=1.5,
         xlab="",
         ytics=c(seq(0,.4,.1),.552),
         yticlab=formatC(c(seq(0,.4,.1),1),1,format="f"),
         ylab="",
         yaxs="i",
         xtics=F
        )

# Shadowing consumption lower than the recommendation
polygon(
        c(
          min(x.dai.m[x.dai.m>2.3]),
          x.dai.m[x.dai.m>2.3&x.dai.m<=counter.dai&x.dai.m<=max(x.dai.m)],
          counter.dai
         ),
        c(
          0,
          y.dai.m[x.dai.m>2.3&x.dai.m<=counter.dai&x.dai.m<=max(x.dai.m)]*100,
          0
          ),
        col="grey"
        )

# Adding histogram for non consumers
rect(xleft=-2.3,ybottom=0,xright=2.3,ytop=.45,col="grey")
rect(xleft=-2.3,ybottom=.4617,xright=2.3,ytop=.52,col="grey")

# Adding x-axis
axis(1,at=c(0,PREV.DF[seq(2,10,2),"dai.col.m"]),labels=F)
text(c(0,PREV.DF[seq(2,10,2),"dai.col.m"]),
     -.015,
     labels=formatC(round(c(0,PREV.DF[seq(2,10,2),"dai.col.m"]),1),1,format="f"),
     srt=60,
     cex=.6,
     adj=1,
     xpd=TRUE)

axis(1,at=PREV.DF[seq(1,10,2),"dai.col.m"],labels=F)
title(xlab="Dairy products intake (gr/day)",line=1.5)

# Adding label to y-axis
title(ylab="Density (%)")

# Adding legend
rect(xleft=26,ybottom=.4135,xright=47,ytop=.431,col="grey")
text(52,.42,"Proportion of men with an intake of <300 gr/day: 85%",cex=.85,adj=0)

# FIBRE
gap.plot(x.fib.m,
         y.fib.m*100,
         xlim=c(0,42),
         ylim=c(0,8.5),
         gap=c(10,10.5),
         type="l",
         lwd=1.5,
         xlab="",
         ytics=seq(0,8,1),
         yticlab=formatC(seq(0,8,1),1,format="f"),
         ylab="",
         yaxs="i",
         xtics=F
        )

# Shadowing consumption lower than the recommendation
polygon(
        c(
          min(x.fib.m),
          x.fib.m[x.fib.m>min(x.fib.m)&x.fib.m<=counter.fib],
          counter.fib
         ),
        c(
          0,
          y.fib.m[x.fib.m>min(x.fib.m)&x.fib.m<=counter.fib]*100,
          0
          ),
        col="grey"
        )

# Adding x-axis
axis(1,at=c(0,PREV.DF[seq(2,10,2),"fib.col.m"]),labels=F)
text(c(0,PREV.DF[seq(2,10,2),"fib.col.m"]),
     -.195,
     labels=formatC(round(c(0,PREV.DF[seq(2,10,2),"fib.col.m"]),1),1,format="f"),
     srt=60,
     cex=.65,
     adj=1,
     xpd=TRUE)

axis(1,at=PREV.DF[seq(1,10,2),"fib.col.m"],labels=F)
title(xlab="Fibre intake (gr/day)",line=1.5)

# Adding label to y-axis
title(ylab="Density (%)")

# Adding legend
rect(xleft=1.473,ybottom=7.45,xright=2.923,ytop=7.675,col="grey")
text(3.36,7.542,"Proportion of men with an intake of <30 gr/day: 91%",cex=.85,adj=0)

# NON-STARCHY VEGETABLES
gap.plot(x.nsv.m,
         y.nsv.m*100,
         xlim=c(0,600),
         ylim=c(0,.6),
         gap=c(.7,.8),
         type="l",
         lwd=1.5,
         xlab="",
         ytics=seq(0,.6,.1),
         yticlab=formatC(seq(0,.6,.1),1,format="f"),
         ylab="",
         yaxs="i",
         xtics=F
        )

# Shadowing consumption lower than the recommendation
polygon(
        c(
          min(x.nsv.m),
          x.nsv.m[x.nsv.m>min(x.nsv.m)&x.nsv.m<=counter.nsv],
          counter.nsv
         ),
        c(
          0,
          y.nsv.m[x.nsv.m>min(x.nsv.m)&x.nsv.m<=counter.nsv]*100,
          0
          ),
        col="grey"
        )

# Adding histogram for non consumers
rect(xleft=-2.9,ybottom=0,xright=2.9,ytop=.095,col="grey")

# Adding x-axis
axis(1,at=c(0,PREV.DF[seq(2,10,2),"nsv.ora.m"]),labels=F)
text(c(0,PREV.DF[seq(2,10,2),"nsv.ora.m"]),
     -.015,
     labels=formatC(round(c(0,PREV.DF[seq(2,10,2),"nsv.ora.m"]),1),1,format="f"),
     srt=60,
     cex=.6,
     adj=1,
     xpd=TRUE)

axis(1,at=PREV.DF[seq(1,10,2),"nsv.ora.m"],labels=F)
title(xlab="Non-starchy vegetables intake (gr/day)",line=1.5)

# Adding legend
rect(xleft=22.45,ybottom=.465,xright=43.45,ytop=.479,col="grey")
text(48.45,.47,"Proportion of men with an intake of <240 gr/day: 59%",cex=.85,adj=0)

# Adding label to y-axis
title(ylab="Density (%)")

# FRUIT
gap.plot(x.fru.m[x.fru.m>3],
         y.fru.m[x.fru.m>3]*100,
         xlim=c(0,750),
         ylim=c(0,.6),
         gap=c(.45,.4825),
         type="l",
         lwd=1.5,
         xlab="",
         ytics=c(seq(0,.4,.1),.55),
         yticlab=formatC(c(seq(0,.4,.1),7),1,format="f"),
         ylab="",
         yaxs="i",
         xtics=F
        )

# Shadowing consumption lower than the recommendation
polygon(
        c(
          min(x.fru.m[x.fru.m>3]),
          x.fru.m[x.fru.m>3&x.fru.m<=counter.fru],
          counter.fru
         ),
        c(
          0,
          y.fru.m[x.fru.m>3&x.fru.m<=counter.fru]*100,
          0
          ),
        col="grey"
        )

# Adding histogram for non consumers
rect(xleft=-3,ybottom=0,xright=3,ytop=.45,col="grey")
rect(xleft=-3,ybottom=.46135,xright=3,ytop=.5175,col="grey")

# Adding x-axis
axis(1,at=c(0,PREV.DF[seq(2,10,2),"fru.ses.m"]),labels=F)
text(c(0,PREV.DF[seq(2,10,2),"fru.ses.m"]),
     -.015,
     labels=formatC(round(c(0,PREV.DF[seq(2,10,2),"fru.ses.m"]),1),1,format="f"),
     srt=60,
     cex=.6,
     adj=1,
     xpd=TRUE)

axis(1,at=PREV.DF[seq(1,10,2),"fru.ses.m"],labels=F)
title(xlab="Fruit intake (gr/day)",line=1.5)

# Adding label to y-axis
title(ylab="Density (%)")

# Adding legend
rect(xleft=29.505,ybottom=.41411,xright=56.519,ytop=.43,col="grey")
text(62,.42,"Proportion of men with an intake of <160 gr/day: 49%",cex=.85,adj=0)

# CITRUS FRUIT
barplot(c(round(DIET$pro.ncon.m[DIET$DF=="Cfr"]*100,0),round(DIET$pro.con.m[DIET$DF=="Cfr"]*100,0)),
        ylim=c(0,100),
        xlim=c(0,1),
        width=.2,
        space=1,
        col=c("white","grey"),
        names.arg=c("No consumption","Any consumption"),
        axes=F
       )
box()

# Adding y-axis
axis(2,at=seq(0,100,25),labels=formatC(seq(0,100,25),1,format="f"),cex=.75)
title(ylab="Frequency (%)")

# Adding label to y-axis
title(xlab="Citrus fruit",line=1.5)

# Adding legend
rect(xleft=.0435,ybottom=93.1,xright=.082,ytop=95.9,col="grey")
text(.092,93.85,"Proportion of men with any intake: 45%",cex=.85,adj=0)

# COFFEE
barplot(c(round(DIET$pro.ncon.m[DIET$DF=="Cof"&DIET$Cancer=="Liv"]*100,0),round(DIET$pro.con.m[DIET$DF=="Cof"&DIET$Cancer=="Liv"]*100,0)),
        ylim=c(0,100),
        xlim=c(0,1),
        width=.2,
        space=1,
        col=c("white","grey"),
        names.arg=c("No consumption","Any consumption"),
        axes=F
       )
box()

# Adding y-axis
axis(2,at=seq(0,100,25),labels=formatC(seq(0,100,25),1,format="f"),cex=.75)
title(ylab="Frequency (%)")

# Adding label to y-axis
title(xlab="Coffee",line=1.5)

# Adding legend
rect(xleft=.0435,ybottom=93.1,xright=.082,ytop=95.9,col="grey")
text(.092,93.85,"Proportion of men with any intake: 91%",cex=.85,adj=0)
dev.off()

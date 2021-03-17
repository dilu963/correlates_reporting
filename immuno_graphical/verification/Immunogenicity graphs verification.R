#example pull request1
library(dplyr)
dat<-read.csv("D:/Verification_Di/correlates_reporting/immuno_graphical/verification/practice_data.csv")

dat<-read.csv("D:/Verification_Di/correlates_reporting/immuno_graphical/verification/practice_data.csv")



data.variables_needed<-dat%>%select(Ptid, Trt, MinorityInd, HighRiskInd, Age, Sex, Bserostatus, Fullvaccine, Perprotocol, EventIndPrimaryD29, EventIndPrimaryD57, 
                                    SubcohortInd, age.geq.65, TwophasesampInd, Bstratum, wt, wt.2, race, ethnicity, EthnicityHispanic, EthnicityNotreported, 
                                    EthnicityUnknown,WhiteNonHispanic,
                                    BbindSpike,BbindRBD,Bpseudoneutid50,Bpseudoneutid80,
                                    Day29bindSpike,Day29bindRBD,Day29pseudoneutid50,Day29pseudoneutid80,
                                    Day57bindSpike,Day57bindRBD,Day57pseudoneutid50,Day57pseudoneutid80,
                                    Delta29overBbindSpike,Delta29overBbindRBD,Delta29overBpseudoneutid50,Delta29overBpseudoneutid80,
                                    Delta57overBbindSpike,Delta57overBbindRBD,Delta57overBpseudoneutid50,Delta57overBpseudoneutid80,
                                    Delta57over29bindSpike,Delta57over29bindRBD,Delta57over29pseudoneutid50,Delta57over29pseudoneutid80)


data.long<-reshape(data.variables_needed,direction="long",
                   varying = c("BbindSpike","BbindRBD","Bpseudoneutid50","Bpseudoneutid80",
                               "Day29bindSpike","Day29bindRBD","Day29pseudoneutid50","Day29pseudoneutid80",
                               "Day57bindSpike","Day57bindRBD","Day57pseudoneutid50","Day57pseudoneutid80",
                               "Delta29overBbindSpike","Delta29overBbindRBD","Delta29overBpseudoneutid50","Delta29overBpseudoneutid80",
                               "Delta57overBbindSpike","Delta57overBbindRBD","Delta57overBpseudoneutid50","Delta57overBpseudoneutid80",
                               "Delta57over29bindSpike","Delta57over29bindRBD","Delta57over29pseudoneutid50","Delta57over29pseudoneutid80"),
                   timevar = "assay",
                   times = c("bindSpike","bindRBD","pseudoneutid50","pseudoneutid80"),
                   v.names = c("B","Day29","Day57","Delta29overB","Delta57overB","Delta57over29"),
                   idvar = "Ptid")


dat.twophase.sample<-dat%>%filter(SubcohortInd==1,TwophasesampInd==1,Perprotocol==1)

dat.long.twophase.sample<-data.long%>%filter(SubcohortInd==1,TwophasesampInd==1,Perprotocol==1)

dat.long.twophase.sample<-dat.long.twophase.sample%>%
  mutate(trt_bstatus_label=paste(dat.long.twophase.sample$Trt, dat.long.twophase.sample$Bserostatus, sep = "*"))%>%
  mutate(age_geq_65_label=ifelse(age.geq.65==1,">=65","<65"))%>%
  mutate(highrisk_label=ifelse(HighRiskInd ==1,"Highrisk","Lowrisk"))%>%
  #mutate(age_risk_label=paste(dat.long.twophase.sample$age_geq_65_label, dat.long.twophase.sample$highrisk_label, sep = "*"))%>%
  mutate(sex_label=ifelse(Sex  ==1,"Male","Female"))%>%
  #mutate(age_sex_label=paste(dat.long.twophase.sample$age_geq_65_label, dat.long.twophase.sample$sex_label, sep = "*"))%>%
  mutate(ethnicity_label=ifelse(EthnicityHispanic==1,"Hispanic or Latino",ifelse(EthnicityHispanic==0 & EthnicityNotreported==0 & EthnicityUnknown==0,"Not Hispanic or Latino","Not reported and unknown")))%>%
  mutate(minority_label=ifelse(MinorityInd   ==1,"Minority","Not minority"))

dat.long.twophase.sample<-dat.long.twophase.sample%>%
  mutate(age_risk_label=paste(dat.long.twophase.sample$age_geq_65_label, dat.long.twophase.sample$highrisk_label, sep = "*"))%>%
  mutate(age_sex_label=paste(dat.long.twophase.sample$age_geq_65_label, dat.long.twophase.sample$sex_label, sep = "*"))%>%
  mutate(age_minority_label=paste(dat.long.twophase.sample$age_geq_65_label, dat.long.twophase.sample$minority_label, sep = "*"))

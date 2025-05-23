## ---------------------------
## Script name: MAB_prebal.R
##
## Purpose of script: Run pre-balance diagnostics on the MAB Rpath model to
##                    identify key weaknesses of the system.
##
## Author: Brandon Beltz
##
## Last updated: 07 Sep 2021
##
## Email: brandon.beltz@stonybrook.edu
## ---------------------------

## Load libraries, packages and functions
library(Rpath); library(data.table);library(dplyr);library(here)

## Create/update data table

pb.groups<-as.data.frame(MAB.Params$RPATH)
pb.biomass<-as.data.frame(MAB.rpath.params$model$Biomass[1:49])
pb.pb<-as.data.frame(MAB.rpath.params$model$PB[1:49])
pb.qb<-as.data.frame(MAB.rpath.params$model$QB[1:49])
pb.class<-as.data.frame(MAB.Params$Classification)
pb.guild<-as.data.frame(MAB.Params$Diet)
pb.table<-as.data.frame(cbind(pb.groups,pb.biomass,pb.pb,pb.qb,pb.class,pb.guild))
colnames(pb.table)<-c("RPATH","Biomass","PB","QB","Classification","Guild")

## Trophic level/Log biomass
trophic.lvl<-as.data.frame(MAB.rpath$TL[1:49])
trophic.lvl<-trophic.lvl[,1]
log.biomass<-log10(pb.table$Biomass)
prebal<-as.data.frame(cbind(trophic.lvl,log.biomass))
row.names(prebal)<-pb.table$RPATH
colnames(prebal)<-c("TL","LogB")

## Select model groups only
prebal<-prebal[1:49,]

## Run linear model
linear.model<-lm(log.biomass~trophic.lvl,data=prebal)
summary(linear.model)
plot(log.biomass~trophic.lvl,data=prebal,pch=1,cex=0.001)
abline(linear.model)
text(log.biomass~trophic.lvl,data=prebal,labels=rownames(prebal),cex=0.5,font=2)

## Prediction intervals
ints<-data.frame(trophic.lvl=seq(min(trophic.lvl),max(trophic.lvl)))
prd.band<-predict(linear.model,ints,interval="predict")
lines(ints[,1],prd.band[,2],col=3)
lines(ints[,1],prd.band[,3],col=3)

## Plot SDs
sd2<-sd(abs(linear.model$residuals))*2
abline(linear.model$coefficients[1],linear.model$coefficients[2])
abline(linear.model$coefficients[1]+sd2,linear.model$coefficients[2])
abline(linear.model$coefficients[1]-sd2,linear.model$coefficients[2])

## Sum quarter trophic level
prebal<-as.data.frame(MAB.rpath$TL)
prebal<-cbind(prebal,MAB.rpath.params$model$Biomass)
prebal<-prebal[1:49,]
row.names(prebal)<-pb.table$RPATH
colnames(prebal)<-c("TL","Biomass")
prebal$rounded<-round(prebal$TL*4)/4
prebal<-aggregate(x=prebal$Biomass,by=list(prebal$rounded),FUN=sum)
colnames(prebal)<-c("TL","Biomass")
prebal$log_biomass<-log10(prebal$Biomass)

## Run linear model
linear.model<-lm(log.biomass~trophic.lvl,data=prebal)
summary(linear.model)
plot(log.biomass~trophic.lvl,data=prebal,pch=19,cex=1)
abline(linear.model)
text(log.biomass~trophic.lvl,data=prebal,labels=(pb.table$RPATH),cex=0.5,font=2)

## Biomass ratios
## Sum by class
class<-pb.table[,c(1,2,5)]
class<-aggregate(x=class$Biomass,by=list(class$Classification),FUN=sum)
colnames(class)<-c("Taxa","Biomass")

## Demersal/medium pelagics:small pelagics
DM<-sum(class$Biomass[which(class$Taxa == "Demersal (Round)" | class$Taxa == "Demersal (Flat)")])
MP<-class$Biomass[which(class$Taxa == "Pelagic (Medium; Round)")]
SP<-sum(class$Biomass[which(class$Taxa == "Pelagic (Small; Round)" | class$Taxa == "Pelagic (Small)")])
DMP_SP<-(DM+MP)/SP

## Small pelagics:zooplankton
ZP<-class$Biomass[which(class$Taxa == "Zooplankton")]
SP_ZP<-SP/ZP

## Zooplankton:phytoplankton
PP<-class$Biomass[which(class$Taxa == "Primary Producer")]
ZP_PP<-ZP/PP

## Small pelagics:phytoplankton
SP_PP<-SP/PP

## Demersal:benthic invertebrates
BI<-class$Biomass[which(class$Taxa == "Invertebrate (Benthic)")]
DM_BI<-DM/BI

## Sharks/HMS:small pelagics
SH<-class$Biomass[which(class$Taxa == "Shark")]
HMS<-class$Biomass[which(class$Taxa == "HMS")]
SHMS_SP<-(SH+HMS)/SP

## Marine mammals/birds:small pelagics
MB<-sum(class$Biomass[which(class$Taxa == "Mammal" | class$Taxa == "Whale" | class$Taxa == "Bird")])
MB_SP<-MB/SP

## Whales:zooplankton
W<-class$Biomass[which(class$Taxa == "Whale")]
W_ZP<-W/ZP

## Ratio between fish aggregate groups
## Demersal:pelagics
PL<-MP+SP
DM_PL<-DM/PL

## Flatfish:roundfish
FF<-class$Biomass[which(class$Taxa == "Demersal (Flat)")]
RF<-class$Biomass[which(class$Taxa == "Demersal (Round)")]+MP+SP
FF_RF<-FF/RF

## Small pelagics: all fish
AF<-DM+MP+SP+HMS+SH
SP_AF<-SP/AF

## Medium pelagics:all fish
MP_AF<-MP/AF

## HMS:all fish
HMS_AF<-HMS/AF

## Sharks:all fish
SH_AF<-SH/AF

## Demersals:all fish
DM_AF<-DM/AF

## Ratio between invertebrate aggregate groups
PI<-class$Biomass[which(class$Taxa == "Invertebrate (Pelagic)")]
AI<-BI+ZP+PI

## Benthic invertebrate: all invertebrate
BI_AI<-BI/AI

## Zooplankton: all invertebrate
ZP_AI<-ZP/AI

## Pelagic invertebrate:all invertebrate
PI_AI<-PI/AI

## Other trophic ratios
## Zooplankton:benthos
ZP_BI<-ZP/BI

## Feeding guild ratios
class<-pb.table[,c(1,2,6)]

## Select model groups only
prebal<-prebal[1:49,]

## Sum biomass by class
class<-aggregate(x=class$Biomass,by=list(class$Guild),FUN=sum)
colnames(class)<-c("Diet","Biomass")

## Benth:Pisc
Benth<-class$Biomass[which(class$Diet == "Benth")]
Pisc<-class$Biomass[which(class$Diet == "Pisc")]
Benth_Pisc<-Benth/Pisc

## Benth:Plank
Plank<-class$Biomass[which(class$Diet == "Plank")]
Benth_Plank<-Benth/Plank

## Plank:Pisc
Plank_Pisc<-Plank/Pisc

## Taxa vital rates
## Trophic level v log P/B
trophic.lvl<-as.data.frame(MAB.rpath$TL[1:49])
trophic.lvl<-trophic.lvl[,1]
log.pb<-log10(pb.table[,3])
prebal<-as.data.frame(cbind(trophic.lvl,log.pb))
prebal<-prebal[1:49,]
row.names(prebal)<-pb.table$RPATH
colnames(prebal)<-c("TL","LogPB")

## Linear regression
linear.model<-lm(log.pb~trophic.lvl,data=prebal)
summary(linear.model)
plot(log.pb~trophic.lvl,data=prebal,pch=1,cex=0.1)
abline(linear.model)
text(log.pb~trophic.lvl,data=prebal,labels=rownames(prebal),cex=0.5,font=2)

## Taxa vital rates
## Trophic level v log Q/B
trophic.lvl<-as.data.frame(MAB.rpath$TL[1:49])
trophic.lvl<-trophic.lvl[,1]
log.qb<-log10(pb.table[,4])
log.qb[32]<-0
prebal<-as.data.frame(cbind(trophic.lvl,log.qb))
prebal<-prebal[1:49,]
row.names(prebal)<-pb.table$RPATH
colnames(prebal)<-c("TL","LogQB")

## Linear regression
linear.model<-lm(log.qb~trophic.lvl,data=prebal)
summary(linear.model)
plot(log.qb~trophic.lvl,data=prebal,pch=19,cex=1)
abline(linear.model)
text(log.qb~trophic.lvl,data=prebal,labels=rownames(prebal),cex=0.5,font=2)
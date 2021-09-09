## ---------------------------
## Script name: MAB_RPath.R
##
## Purpose of script: Compile all data to create functional RPath model.
##                    
##
## Author: Brandon Beltz
##
## Last updated: 09 Sep 2021
##
## Email: brandon.beltz@stonybrook.edu
## ---------------------------
##
## Notes: This version includes all original data for the purpose of
##        identifying and resolving issues.
##
## ---------------------------
## Set working directory

setwd("C:/Users/beven/Desktop/MAB-Rpath")

## Load libraries, packages and functions
library(Rpath); library(data.table); library(dplyr); library(here)

## Add functional groups to model and generate rpath params
source("MAB_fleets.R")
groups<-as.vector(groups_fleets$RPATH)
types<-c(rep(0,31),1,rep(0,18),rep(2,2),rep(3,11))
MAB.rpath.params<-create.rpath.params(group = groups, type = types)

## Add biomass estimates
source("MAB_biomass_estimates.R")
biomass<-left_join(groups_fleets,MAB.biomass.80s,by = "RPATH")
biomass<-as.vector(biomass$Biomass)
MAB.rpath.params$model[,Biomass:=biomass]

## Add PB parameters
source("MAB_params.R")
pb<-cbind(MAB.groups,MAB.PB)
pb<-left_join(groups_fleets,pb,by = "RPATH")
pb<-as.vector(pb$PB)
MAB.rpath.params$model[,PB:=pb]

## Add QB parameters
qb<-cbind(MAB.groups,MAB.QB)
qb<-left_join(groups_fleets,qb,by = "RPATH")
qb<-as.vector(qb$QB)
MAB.rpath.params$model[,QB:=qb]

## Add biomass accumulation
ba<-cbind(MAB.groups,MAB.Params$BA)
colnames(ba)[2]<-"BA"
ba<-left_join(groups_fleets,ba,by = "RPATH")
ba<-as.vector(ba$BA)
MAB.rpath.params$model[,BioAcc:=ba]

## Add unassimilated consumption


## Add detrital fate and set discards to 0
MAB.rpath.params$model[,Detritus:=c(rep(1,50),rep(0,13))]
MAB.rpath.params$model[,Discards:=c(rep(0,52),rep(1,11))]

## Add landings by gear type
source("MAB_discards.R")

## Fixed Gear
fixed<-left_join(groups_fleets,fixed,by="RPATH")
fixed<-as.vector(fixed$landings)
fixed[51:52]<-0
MAB.rpath.params$model[,"Fixed Gear":=fixed]

## Large Mesh
lg_mesh<-left_join(groups_fleets,lg_mesh,by="RPATH")
lg_mesh<-as.vector(lg_mesh$landings)
lg_mesh[51:52]<-0
MAB.rpath.params$model[, "LG Mesh" := lg_mesh]

## Other
other<-left_join(groups_fleets,other,by="RPATH")
other<-as.vector(other$landings)
other[51:52]<-0
MAB.rpath.params$model[, "Other" := other]

## Small Mesh
sm_mesh<-left_join(groups_fleets,sm_mesh,by="RPATH")
sm_mesh<-as.vector(sm_mesh$landings)
sm_mesh[51:52]<-0
MAB.rpath.params$model[, "SM Mesh" := sm_mesh]

## Scallop Dredge
scallop<-left_join(groups_fleets,scallop,by="RPATH")
scallop<-as.vector(scallop$landings)
scallop[51:52]<-0
MAB.rpath.params$model[, "Scallop Dredge" := scallop]

## Trap
trap<-left_join(groups_fleets,trap,by="RPATH")
trap<-as.vector(trap$landings)
trap[51:52]<-0
MAB.rpath.params$model[, "Trap" := trap]

## HMS Fleet
hms<-left_join(groups_fleets,hms,by="RPATH")
hms<-as.vector(hms$landings)
hms[51:52]<-0
MAB.rpath.params$model[, "HMS Fleet" := hms]

## Pelagic
pelagic<-left_join(groups_fleets,pelagic,by="RPATH")
pelagic<-as.vector(pelagic$landings)
pelagic[51:52]<-0
MAB.rpath.params$model[, "Pelagic" := pelagic]

## Other Dredge
other_dredge<-left_join(groups_fleets,other_dredge,by="RPATH")
other_dredge<-as.vector(other_dredge$landings)
other_dredge[51:52]<-0
MAB.rpath.params$model[, "Other Dredge" := other_dredge]

## Clam
clam<-left_join(groups_fleets,clam,by="RPATH")
clam<-as.vector(clam$landings)
clam[51:52]<-0
MAB.rpath.params$model[, "Clam Dredge" := clam]

## Recreational
source("MAB_rec_catch.R")
rec_catch<-left_join(groups_fleets,MAB.mrip,by="RPATH")
rec_catch<-as.vector(rec_catch$Per_Area)
rec_catch[is.na(rec_catch)]<-0
rec_catch[51:52]<-0
MAB.rpath.params$model[,"Recreational":=rec_catch]

## Add discards by gear type
## Fixed Gear
fixed.d<-left_join(groups_fleets,fixed.d,by="RPATH")
fixed.d<-as.vector(fixed.d$discards)
fixed.d[51:52]<-0
MAB.rpath.params$model[, "Fixed Gear.disc" := fixed.d]

## Lg Mesh
lg_mesh.d<-left_join(groups_fleets,lg_mesh.d,by="RPATH")
lg_mesh.d<-as.vector(lg_mesh.d$discards)
lg_mesh.d[51:52]<-0
MAB.rpath.params$model[, "LG Mesh.disc" := lg_mesh.d]

## Other
other.d<-left_join(groups_fleets,other.d,by="RPATH")
other.d<-as.vector(other.d$discards)
other.d[51:52]<-0
MAB.rpath.params$model[, "Other.disc" := other.d]

## SM Mesh
sm_mesh.d<-left_join(groups_fleets,sm_mesh.d,by="RPATH")
sm_mesh.d<-as.vector(sm_mesh.d$discards)
sm_mesh.d[51:52]<-0
MAB.rpath.params$model[, "SM Mesh.disc" := sm_mesh.d]

## Scallop Dredge
scallop.d<-left_join(groups_fleets,scallop.d,by="RPATH")
scallop.d<-as.vector(scallop.d$discards)
scallop.d[51:52]<-0
MAB.rpath.params$model[, "Scallop Dredge" := scallop.d]

## Trap
trap.d<-left_join(groups_fleets,trap.d,by="RPATH")
trap.d<-as.vector(trap.d$discards)
trap.d[51:52]<-0
MAB.rpath.params$model[, "Trap.disc" := trap.d]

## HMS
hms.d<-left_join(groups_fleets,hms.d,by="RPATH")
hms.d<-as.vector(hms.d$discards)
hms.d[51:52]<-0
MAB.rpath.params$model[, "HMS Fleet.disc" := hms.d]

## Pelagic
pelagic.d<-left_join(groups_fleets,pelagic.d,by="RPATH")
pelagic.d<-as.vector(pelagic.d$discards)
pelagic.d[51:52]<-0
MAB.rpath.params$model[, "Pelagic.disc" := pelagic.d]

## Other Dredge
other_dredge.d<-left_join(groups_fleets,other_dredge.d,by="RPATH")
other_dredge.d<-as.vector(other_dredge.d$discards)
other_dredge.d[51:52]<-0
MAB.rpath.params$model[, "Other Dredge.disc" := other_dredge.d]

## Clam Dredge
clam.d<-c(rep(0,50),rep(0,2),rep(NA,11))
MAB.rpath.params$model[, "Clam Dredge.disc" := clam.d]

## Add diet matrix
source("MAB_diet.R")
source("MAB_diet_fill.R")

## Run model
MAB.rpath<-rpath(MAB.rpath.params,eco.name='Mid-Atlantic Bight')
MAB.rpath
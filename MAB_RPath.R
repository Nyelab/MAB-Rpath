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
MAB.rpath.params$model[, Unassim := c(rep(0.2,5),0.4,rep(0.2,6),0.4,rep(0.2,3),0.4,rep(0.2,6),0.4,rep(0.2,7),0,rep(0.2,5),0.4,rep(0.2,12),rep(0,2),rep(NA,11))]

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

## Save .csv file with model information
write.csv(MAB.rpath.params$model, file="MAB_model.csv")

## Balance adjustments

## OtherPelagics
## Increase biomass by 300x (Lucey, Link, Buccheister et al.)
MAB.rpath.params$model$Biomass[29]<-MAB.rpath.params$model$Biomass[29]*300
## Increase PB by 3x
MAB.rpath.params$model$PB[29]<-MAB.rpath.params$model$PB[29]*3
## Decrease Trap fishing by 0.7x
MAB.rpath.params$model$Trap[29]<-MAB.rpath.params$model$Trap[29]*.7
## Decrease Recreational fishing by 0.7x
MAB.rpath.params$model$Recreational[29]<-MAB.rpath.params$model$Recreational[29]*.7

## OtherDemersals
## Increase biomass by 500x (Lucey, Link, Buccheister et al.)
MAB.rpath.params$model$Biomass[28]<-MAB.rpath.params$model$Biomass[28]*500
## Increase PB by 1.5x (Lucey, Okey)
MAB.rpath.params$model$PB[28]<-MAB.rpath.params$model$PB[28]*1.5

## OtherCephalopods
## Increase biomass by 1000x (Lucey)
MAB.rpath.params$model$Biomass[27]<-MAB.rpath.params$model$Biomass[27]*1000

## BlackSeaBass
## Increase biomass by 10x (Lucey)
MAB.rpath.params$model$Biomass[8]<-MAB.rpath.params$model$Biomass[8]*10
## Increase PB by 3x (Buccheister et al., Link, Okey)
MAB.rpath.params$model$PB[8]<-MAB.rpath.params$model$PB[8]*3

## OtherShrimps
## Increase biomass by 100x (Link)
MAB.rpath.params$model$Biomass[30]<-MAB.rpath.params$model$Biomass[30]*150
## Increase PB by 2x (Lucey, Okey)
MAB.rpath.params$model$PB[30]<-MAB.rpath.params$model$PB[30]*2

## Sharks
## Increase biomass by 3x (Okey)
MAB.rpath.params$model$Biomass[36]<-MAB.rpath.params$model$Biomass[36]*3
## Increase PB by 5x (Okey)
MAB.rpath.params$model$PB[36]<-MAB.rpath.params$model$PB[36]*5
## Decrease Recreational fishing by 0.1x
MAB.rpath.params$model$Recreational[36]<-MAB.rpath.params$model$Recreational[36]*.1

## SouthernDemersals *NEED MORE REFERENCE FOR THESE SPECIES*
## Increase biomass by 10x
MAB.rpath.params$model$Biomass[42]<-MAB.rpath.params$model$Biomass[42]*10
## Increase PB by 1.2x (Lucey)
MAB.rpath.params$model$PB[42]<-MAB.rpath.params$model$PB[42]*1.2

## AtlMackerel
## Increase biomass by 75x (Lucey, Buccheister)
MAB.rpath.params$model$Biomass[4]<-MAB.rpath.params$model$Biomass[4]*75
## Increase PB by 2x (Lucey)
MAB.rpath.params$model$PB[4]<-MAB.rpath.params$model$PB[4]*2

## Megabenthos
## Increase biomass by 150x (Buccheister)
MAB.rpath.params$model$Biomass[21]<-MAB.rpath.params$model$Biomass[21]*150

## Cod
## Increase biomass by 20x (Lucey)
MAB.rpath.params$model$Biomass[11]<-MAB.rpath.params$model$Biomass[11]*20

## SmFlatfishes
## Increase biomass by 100x (Lucey)
MAB.rpath.params$model$Biomass[39]<-MAB.rpath.params$model$Biomass[39]*100

## Bluefish
## Increase biomass by 3x (Buccheister)
MAB.rpath.params$model$Biomass[9]<-MAB.rpath.params$model$Biomass[9]*3
## Increase PB by 1.2x (Okey)
MAB.rpath.params$model$PB[9]<-MAB.rpath.params$model$PB[9]*1.2

## SummerFlounder
## Increase biomass by 1.5x (Lucey)
MAB.rpath.params$model$Biomass[44]<-MAB.rpath.params$model$Biomass[44]*1.5

## SmPelagics
## Increase biomass by 10x (Link, Buccheister)
MAB.rpath.params$model$Biomass[41]<-MAB.rpath.params$model$Biomass[41]*10
## Increase PB by 1.25x (Lucey)
MAB.rpath.params$model$PB[41]<-MAB.rpath.params$model$PB[41]*1.25

## Windowpane
## Increase biomass by 20x (Lucey)
MAB.rpath.params$model$Biomass[47]<-MAB.rpath.params$model$Biomass[47]*20
## Increase PB by 3x (Buccheister, Link, Okey)
MAB.rpath.params$model$PB[47]<-MAB.rpath.params$model$PB[47]*2

## WinterFlounder
## Increase biomass by 20x (Lucey)
MAB.rpath.params$model$Biomass[48]<-MAB.rpath.params$model$Biomass[48]*20

## WinterSkate
## Increase biomass by 10x (Lucey)
MAB.rpath.params$model$Biomass[49]<-MAB.rpath.params$model$Biomass[49]*10

## LittleSkate
## Increase biomass by 8x (Lucey)
MAB.rpath.params$model$Biomass[18]<-MAB.rpath.params$model$Biomass[18]*8

## OceanPout
## Increase biomass by 20x (Lucey)
MAB.rpath.params$model$Biomass[25]<-MAB.rpath.params$model$Biomass[25]*20

## SilverHake
## Increase biomass by 10x (Lucey, Okey)
MAB.rpath.params$model$Biomass[37]<-MAB.rpath.params$model$Biomass[37]*10
## Increase PB by 1.5x (Okey)
MAB.rpath.params$model$PB[37]<-MAB.rpath.params$model$PB[37]*1.5

## Scup
## Increase biomass by 5x (Lucey)
MAB.rpath.params$model$Biomass[34]<-MAB.rpath.params$model$Biomass[34]*5
## Increase PB by 2x (Okey)
MAB.rpath.params$model$PB[34]<-MAB.rpath.params$model$PB[34]*2

## Mesopelagics
## Increase PB by 1.5x (Lucey)
MAB.rpath.params$model$PB[22]<-MAB.rpath.params$model$PB[22]*1.5

## Odontocetes
## Increase biomass by 5x (Lucey)
MAB.rpath.params$model$Biomass[26]<-MAB.rpath.params$model$Biomass[26]*5
## Increase PB by 1.25x (Okey)
MAB.rpath.params$model$PB[26]<-MAB.rpath.params$model$PB[26]*1.25

## OtherSkates
## Increase biomass by 5x (Buccheister, Okey)
MAB.rpath.params$model$Biomass[31]<-MAB.rpath.params$model$Biomass[31]*5

## AtlScallop
## Increase biomass by 10x (Lucey)
MAB.rpath.params$model$Biomass[5]<-MAB.rpath.params$model$Biomass[5]*10

## Butterfish
## Increase biomass by 1.5x (Lucey)
MAB.rpath.params$model$Biomass[10]<-MAB.rpath.params$model$Biomass[10]*1.5
## Increase PB by 1.5x (Okey)
MAB.rpath.params$model$PB[10]<-MAB.rpath.params$model$PB[10]*1.5

## WhiteHake
## Increase biomass by 10x (Lucey)
MAB.rpath.params$model$Biomass[46]<-MAB.rpath.params$model$Biomass[46]*10
## Increase PB by 2x (Buccheister)
MAB.rpath.params$model$PB[46]<-MAB.rpath.params$model$PB[46]*2

## Weakfish
## Increase PB by 2.5x (Buccheister)
MAB.rpath.params$model$PB[45]<-MAB.rpath.params$model$PB[45]*2.5

## AmShad
## Increase biomass by 5x
MAB.rpath.params$model$Biomass[2]<-MAB.rpath.params$model$Biomass[2]*5

## Goosefish
## Increase biomass by 2.5x (Lucey, Okey)
MAB.rpath.params$model$Biomass[14]<-MAB.rpath.params$model$Biomass[14]*2.5

## Fourspot
## Increase biomass by 2x (Lucey)
MAB.rpath.params$model$Biomass[12]<-MAB.rpath.params$model$Biomass[12]*2

## YTFlounder
## Increase biomass by 2x (Buccheister)
MAB.rpath.params$model$Biomass[50]<-MAB.rpath.params$model$Biomass[50]*2
## Increase PB by 2x (Buccheister)
MAB.rpath.params$model$PB[50]<-MAB.rpath.params$model$PB[50]*2

## SpinyDogfish
## Increase PB by 1.5x (Lucey)
MAB.rpath.params$model$PB[43]<-MAB.rpath.params$model$PB[43]*1.5
## Decrease QB by 0.5x
MAB.rpath.params$model$QB[43]<-MAB.rpath.params$model$QB[43]*.5

## RedHake
## Increase biomass by 5x (Lucey)
MAB.rpath.params$model$Biomass[33]<-MAB.rpath.params$model$Biomass[33]*5

## Micronekton
## Increase PB by 1.5x (Lucey)
MAB.rpath.params$model$PB[23]<-MAB.rpath.params$model$PB[23]*1.5

## AmLobster
## Increase biomass by 2x
MAB.rpath.params$model$Biomass[1]<-MAB.rpath.params$model$Biomass[1]*2

## HMS
## Decrease QB by 0.75x (Lucey)
MAB.rpath.params$model$QB[15]<-MAB.rpath.params$model$QB[15]*.75

## Reallocate predation of Sharks
## Sharks: Sharks -0.1%, AmShad +0.1%
MAB.rpath.params$diet[36,37]<-MAB.rpath.params$diet[36,37]-0.001
MAB.rpath.params$diet[2,37]<-MAB.rpath.params$diet[2,37]+0.001
## Sharks: Sharks -0.1%, HMS +0.1%
MAB.rpath.params$diet[36,37]<-MAB.rpath.params$diet[36,37]-0.001
MAB.rpath.params$diet[15,37]<-MAB.rpath.params$diet[15,37]+0.001
## Sharks: Sharks -0.1%, Illex +0.1%
MAB.rpath.params$diet[36,37]<-MAB.rpath.params$diet[36,37]-0.001
MAB.rpath.params$diet[16,37]<-MAB.rpath.params$diet[16,37]+0.001
## Sharks: Sharks -0.1%, OceanPout +0.1%
MAB.rpath.params$diet[36,37]<-MAB.rpath.params$diet[36,37]-0.001
MAB.rpath.params$diet[25,37]<-MAB.rpath.params$diet[25,37]+0.001
## Sharks: Sharks -0.1%, RedHake +0.1%
MAB.rpath.params$diet[36,37]<-MAB.rpath.params$diet[36,37]-0.001
MAB.rpath.params$diet[33,37]<-MAB.rpath.params$diet[33,37]+0.001
## Sharks: Sharks -0.1%, Scup +0.1%
MAB.rpath.params$diet[36,37]<-MAB.rpath.params$diet[36,37]-0.001
MAB.rpath.params$diet[34,37]<-MAB.rpath.params$diet[34,37]+0.001
## Sharks: Sharks -0.1%, WhiteHake +0.1%
MAB.rpath.params$diet[36,37]<-MAB.rpath.params$diet[36,37]-0.001
MAB.rpath.params$diet[46,37]<-MAB.rpath.params$diet[46,37]+0.001
## Sharks: Sharks -0.1%, Windowpane +0.1%
MAB.rpath.params$diet[36,37]<-MAB.rpath.params$diet[36,37]-0.001
MAB.rpath.params$diet[47,37]<-MAB.rpath.params$diet[47,37]+0.001
## Sharks: Sharks -0.1%, WinterFlounder +0.1%
MAB.rpath.params$diet[36,37]<-MAB.rpath.params$diet[36,37]-0.001
MAB.rpath.params$diet[48,37]<-MAB.rpath.params$diet[48,37]+0.001
## Sharks: Sharks -0.1%, YTFlounder +0.1%
MAB.rpath.params$diet[36,37]<-MAB.rpath.params$diet[36,37]-0.001
MAB.rpath.params$diet[50,37]<-MAB.rpath.params$diet[50,37]+0.001

## Reallocate predation on OtherPelagics
## Bluefish: OtherPelagics -0.4%, Illex +0.4%
MAB.rpath.params$diet[29,10]<-MAB.rpath.params$diet[29,10]-0.004
MAB.rpath.params$diet[16,10]<-MAB.rpath.params$diet[16,10]+0.004
## Bluefish: OtherPelagics -0.4%, Loligo +0.4%
MAB.rpath.params$diet[29,10]<-MAB.rpath.params$diet[29,10]-0.004
MAB.rpath.params$diet[19,10]<-MAB.rpath.params$diet[19,10]+0.004
## Bluefish: OtherPelagics -0.4%, Macrobenthos +0.4%
MAB.rpath.params$diet[29,10]<-MAB.rpath.params$diet[29,10]-0.004
MAB.rpath.params$diet[20,10]<-MAB.rpath.params$diet[20,10]+0.004
## Bluefish: OtherPelagics -0.4%, Micronekton +0.4%
MAB.rpath.params$diet[29,10]<-MAB.rpath.params$diet[29,10]-0.004
MAB.rpath.params$diet[23,10]<-MAB.rpath.params$diet[23,10]+0.004
## Bluefish: OtherPelagics -0.4%, OceanPout +0.4%
MAB.rpath.params$diet[29,10]<-MAB.rpath.params$diet[29,10]-0.004
MAB.rpath.params$diet[25,10]<-MAB.rpath.params$diet[25,10]+0.004
## Bluefish: OtherPelagics -0.4%, OtherSkates +0.4%
MAB.rpath.params$diet[29,10]<-MAB.rpath.params$diet[29,10]-0.004
MAB.rpath.params$diet[31,10]<-MAB.rpath.params$diet[31,10]+0.004
## Bluefish: OtherPelagics -0.4%, RedHake +0.4%
MAB.rpath.params$diet[29,10]<-MAB.rpath.params$diet[29,10]-0.004
MAB.rpath.params$diet[33,10]<-MAB.rpath.params$diet[33,10]+0.004
## Bluefish: OtherPelagics -0.4%, Windowpane +0.4%
MAB.rpath.params$diet[29,10]<-MAB.rpath.params$diet[29,10]-0.004
MAB.rpath.params$diet[47,10]<-MAB.rpath.params$diet[47,10]+0.004
## Bluefish: OtherPelagics -0.4%, WinterFlounder +0.4%
MAB.rpath.params$diet[29,10]<-MAB.rpath.params$diet[29,10]-0.004
MAB.rpath.params$diet[48,10]<-MAB.rpath.params$diet[48,10]+0.004
## Bluefish: OtherPelagics -0.4%, YTFlounder +0.4%
MAB.rpath.params$diet[29,10]<-MAB.rpath.params$diet[29,10]-0.004
MAB.rpath.params$diet[50,10]<-MAB.rpath.params$diet[50,10]+0.004

## Cod: OtherPelagics -1%, AmLobster +1%
MAB.rpath.params$diet[29,12]<-MAB.rpath.params$diet[29,12]-0.01
MAB.rpath.params$diet[1,12]<-MAB.rpath.params$diet[1,12]+0.01
## Cod: OtherPelagics -1%, Macrobenthos +1%
MAB.rpath.params$diet[29,12]<-MAB.rpath.params$diet[29,12]-0.01
MAB.rpath.params$diet[20,12]<-MAB.rpath.params$diet[20,12]+0.01
## Cod: OtherPelagics -1%, Micronekton +1%
MAB.rpath.params$diet[29,12]<-MAB.rpath.params$diet[29,12]-0.01
MAB.rpath.params$diet[23,12]<-MAB.rpath.params$diet[23,12]+0.01
## Cod: OtherPelagics -1%, OceanPout +1%
MAB.rpath.params$diet[29,12]<-MAB.rpath.params$diet[29,12]-0.01
MAB.rpath.params$diet[25,12]<-MAB.rpath.params$diet[25,12]+0.01
## Cod: OtherPelagics -1%, OtherSkates +1%
MAB.rpath.params$diet[29,12]<-MAB.rpath.params$diet[29,12]-0.01
MAB.rpath.params$diet[31,12]<-MAB.rpath.params$diet[31,12]+0.01
## Cod: OtherPelagics -1%, RedHake +1%
MAB.rpath.params$diet[29,12]<-MAB.rpath.params$diet[29,12]-0.01
MAB.rpath.params$diet[33,12]<-MAB.rpath.params$diet[33,12]+0.01
## Cod: OtherPelagics -1%, Windowpane +1%
MAB.rpath.params$diet[29,12]<-MAB.rpath.params$diet[29,12]-0.01
MAB.rpath.params$diet[47,12]<-MAB.rpath.params$diet[47,12]+0.01
## Cod: OtherPelagics -1%, WinterFlounder +1%
MAB.rpath.params$diet[29,12]<-MAB.rpath.params$diet[29,12]-0.01
MAB.rpath.params$diet[48,12]<-MAB.rpath.params$diet[48,12]+0.01

## Rerun model, check prebal diagnostics and save EEs
MAB.rpath<-rpath(MAB.rpath.params,eco.name='Mid-Atlantic Bight')
MAB.rpath
source("MAB_prebal.R")
#write.csv(MAB.rpath$EE,file="MAB_EE_temp.csv")
#write.Rpath(MAB.rpath,morts=T,file="MAB.rpath_morts.csv")
EE<-MAB.rpath$EE
EE[order(EE)]

write.csv(MAB.rpath.params$diet,file="MAB.rpath.diet.csv")
write.csv(MAB.rpath.params$model,file="MAB.rpath.model.csv")
MAB.rpath$PB[4]
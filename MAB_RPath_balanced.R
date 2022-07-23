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
## Increase biomass by 1000x (Prebal diagnostics)
MAB.rpath.params$model$Biomass[29]<-MAB.rpath.params$model$Biomass[29]*1000
## Increase PB by 3x
MAB.rpath.params$model$PB[29]<-MAB.rpath.params$model$PB[29]*3
## Decrease Trap fishing by 0.25x
MAB.rpath.params$model$Trap[29]<-MAB.rpath.params$model$Trap[29]*.25
## Decrease Recreational fishing by 0.25x
MAB.rpath.params$model$Recreational[29]<-MAB.rpath.params$model$Recreational[29]*.25

## OtherDemersals
## Increase biomass by 1000x (Lucey, Link, Buccheister et al.)
MAB.rpath.params$model$Biomass[28]<-MAB.rpath.params$model$Biomass[28]*1000
## Increase PB by 2.75x (Okey)
MAB.rpath.params$model$PB[28]<-MAB.rpath.params$model$PB[28]*2.75
## Decrease QB by 0.5x (Buccheister, Link, Lucey)
MAB.rpath.params$model$QB[28]<-MAB.rpath.params$model$QB[28]*.5

## OtherCephalopods
## Increase biomass by 2000x (Prebal diagnostics)
MAB.rpath.params$model$Biomass[27]<-MAB.rpath.params$model$Biomass[27]*2000
## Increase PB by 2x (Prebal diagnostics)
MAB.rpath.params$model$PB[27]<-MAB.rpath.params$model$PB[27]*2

## BlackSeaBass
## Increase biomass by 40x (Prebal diagnostics)
MAB.rpath.params$model$Biomass[8]<-MAB.rpath.params$model$Biomass[8]*40
## Increase PB by 5x (Okey)
MAB.rpath.params$model$PB[8]<-MAB.rpath.params$model$PB[8]*5
## Increase QB by 2x (Okey)
MAB.rpath.params$model$QB[8]<-MAB.rpath.params$model$QB[8]*2
## Decrease Recreational fishing by 0.25x
MAB.rpath.params$model$Recreational[8]<-MAB.rpath.params$model$Recreational[8]*.3

## OtherShrimps
## Increase biomass by 700x (Lucey)
MAB.rpath.params$model$Biomass[30]<-MAB.rpath.params$model$Biomass[30]*700
## Increase PB by 2x (Lucey, Okey)
MAB.rpath.params$model$PB[30]<-MAB.rpath.params$model$PB[30]*2

## Sharks
## Increase biomass by 12x (Okey)
MAB.rpath.params$model$Biomass[36]<-MAB.rpath.params$model$Biomass[36]*12
## Increase PB by 5x (Okey)
MAB.rpath.params$model$PB[36]<-MAB.rpath.params$model$PB[36]*5
## Decrease QB by 0.8
MAB.rpath.params$model$QB[36]<-MAB.rpath.params$model$QB[36]*.8
## Decrease Recreational fishing by 0.25x
MAB.rpath.params$model$Recreational[36]<-MAB.rpath.params$model$Recreational[36]*.25
## Decrease Trap fishing by 0.8x
MAB.rpath.params$model$Trap[36]<-MAB.rpath.params$model$Trap[36]*.8

## SouthernDemersals *NEED MORE REFERENCE FOR THESE SPECIES*
## Increase biomass by 125x
MAB.rpath.params$model$Biomass[42]<-MAB.rpath.params$model$Biomass[42]*125
## Increase PB by 1.5x (Lucey)
MAB.rpath.params$model$PB[42]<-MAB.rpath.params$model$PB[42]*1.5
## Decrease Recreational fishing by 0.5x
MAB.rpath.params$model$Recreational[42]<-MAB.rpath.params$model$Recreational[42]*.5

## AtlMackerel
## Increase biomass by 200x (Lucey, Buccheister)
MAB.rpath.params$model$Biomass[4]<-MAB.rpath.params$model$Biomass[4]*200
## Increase PB by 3x (Lucey)
MAB.rpath.params$model$PB[4]<-MAB.rpath.params$model$PB[4]*3

## Megabenthos
## Increase biomass by 225x (Lucey, Link, Buccheister)
MAB.rpath.params$model$Biomass[21]<-MAB.rpath.params$model$Biomass[21]*225

## Cod
## Increase biomass by 25x (Lucey)
MAB.rpath.params$model$Biomass[11]<-MAB.rpath.params$model$Biomass[11]*25
## Increase PB by 2x (Buccheister)
MAB.rpath.params$model$PB[11]<-MAB.rpath.params$model$PB[11]*2
## Decrease Recreational fishing by 0.25x
MAB.rpath.params$model$Recreational[11]<-MAB.rpath.params$model$Recreational[11]*.75

## SmFlatfishes
## Increase biomass by 150x (Lucey)
MAB.rpath.params$model$Biomass[39]<-MAB.rpath.params$model$Biomass[39]*150

## Bluefish
## Increase biomass by 10x (Prebal diagnostics)
MAB.rpath.params$model$Biomass[9]<-MAB.rpath.params$model$Biomass[9]*10
## Increase PB by 3x (Buccheister)
MAB.rpath.params$model$PB[9]<-MAB.rpath.params$model$PB[9]*3
## Decrease Recreational fishing by 0.5x
MAB.rpath.params$model$Recreational[9]<-MAB.rpath.params$model$Recreational[9]*.5

## SummerFlounder
## Increase biomass by 10x (Buccheister)
MAB.rpath.params$model$Biomass[44]<-MAB.rpath.params$model$Biomass[44]*10
## Increase PB by 1.5x (Buccheister)
MAB.rpath.params$model$PB[44]<-MAB.rpath.params$model$PB[44]*1.75
## Decrease QB by .75x (Lucey)
MAB.rpath.params$model$QB[44]<-MAB.rpath.params$model$QB[44]*.75

## SmPelagics
## Increase biomass by 50x (Link, Lucey)
MAB.rpath.params$model$Biomass[41]<-MAB.rpath.params$model$Biomass[41]*50
## Increase PB by 1.25x (Lucey)
MAB.rpath.params$model$PB[41]<-MAB.rpath.params$model$PB[41]*1.25

## Windowpane
## Increase biomass by 20x (Lucey)
MAB.rpath.params$model$Biomass[47]<-MAB.rpath.params$model$Biomass[47]*20
## Increase PB by 1.5x (Prebal diagnostics)
MAB.rpath.params$model$PB[47]<-MAB.rpath.params$model$PB[47]*1.5

## WinterFlounder
## Increase biomass by 30x (Lucey)
MAB.rpath.params$model$Biomass[48]<-MAB.rpath.params$model$Biomass[48]*30

## WinterSkate
## Increase biomass by 50x (Lucey)
MAB.rpath.params$model$Biomass[49]<-MAB.rpath.params$model$Biomass[49]*50

## LittleSkate
## Increase biomass by 10x (Lucey)
MAB.rpath.params$model$Biomass[18]<-MAB.rpath.params$model$Biomass[18]*10
## Increase PB by 1.5x (Prebal diagnostics)
MAB.rpath.params$model$PB[18]<-MAB.rpath.params$model$PB[18]*1.5

## OceanPout
## Increase biomass by 25x (Lucey)
MAB.rpath.params$model$Biomass[25]<-MAB.rpath.params$model$Biomass[25]*25
## Decrease QB by 0.25x (Okey)
MAB.rpath.params$model$QB[25]<-MAB.rpath.params$model$QB[25]*.25

## SilverHake
## Increase biomass by 25x (Lucey, Okey)
MAB.rpath.params$model$Biomass[37]<-MAB.rpath.params$model$Biomass[37]*25

## Scup
## Increase biomass by 10x (Lucey)
MAB.rpath.params$model$Biomass[34]<-MAB.rpath.params$model$Biomass[34]*10
## Increase PB by 2.5x (Okey)
MAB.rpath.params$model$PB[34]<-MAB.rpath.params$model$PB[34]*2.5

## Mesopelagics
## Increase biomass by 1.5x (Prebal diagnostics)
MAB.rpath.params$model$Biomass[22]<-MAB.rpath.params$model$Biomass[22]*1.75
## Increase PB by 2x (Lucey)
MAB.rpath.params$model$PB[22]<-MAB.rpath.params$model$PB[22]*2
## Decrease QB by 0.5x (Link)
MAB.rpath.params$model$QB[22]<-MAB.rpath.params$model$QB[22]*.5
## Decrease Trap fishing by 0.5x
MAB.rpath.params$model$Trap[22]<-MAB.rpath.params$model$Trap[22]*.5

## Odontocetes
## Increase biomass by 5x (Lucey)
MAB.rpath.params$model$Biomass[26]<-MAB.rpath.params$model$Biomass[26]*5
## Increase PB by 2.5x (Okey)
MAB.rpath.params$model$PB[26]<-MAB.rpath.params$model$PB[26]*2.75
## Decrease QB by 0.5x (Lucey)
MAB.rpath.params$model$QB[26]<-MAB.rpath.params$model$QB[26]*0.5

## OtherSkates
## Increase biomass by 6x (Buccheister, Okey)
MAB.rpath.params$model$Biomass[31]<-MAB.rpath.params$model$Biomass[31]*6

## AtlScallop
## Increase biomass by 15x (Lucey)
MAB.rpath.params$model$Biomass[5]<-MAB.rpath.params$model$Biomass[5]*15

## Butterfish
## Increase biomass by 1.5x (Lucey)
MAB.rpath.params$model$Biomass[10]<-MAB.rpath.params$model$Biomass[10]*1.5
## Increase PB by 1.5x (Okey)
MAB.rpath.params$model$PB[10]<-MAB.rpath.params$model$PB[10]*1.5

## WhiteHake
## Increase biomass by 20x (Lucey)
MAB.rpath.params$model$Biomass[46]<-MAB.rpath.params$model$Biomass[46]*20
## Increase PB by 1.5x (Prebal diagnostics)
MAB.rpath.params$model$PB[46]<-MAB.rpath.params$model$PB[46]*1.5

## Weakfish
## Increase biomass by 2x (Prebal diagnostics)
MAB.rpath.params$model$Biomass[45]<-MAB.rpath.params$model$Biomass[45]*2
## Increase PB by 1.5x (Buccheister)
MAB.rpath.params$model$PB[45]<-MAB.rpath.params$model$PB[45]*1.5

## AmShad
## Increase biomass by 5x
MAB.rpath.params$model$Biomass[2]<-MAB.rpath.params$model$Biomass[2]*5

## Goosefish
## Increase biomass by 7x (Lucey, Okey)
MAB.rpath.params$model$Biomass[14]<-MAB.rpath.params$model$Biomass[14]*7

## Fourspot
## Increase biomass by 8x (Lucey)
MAB.rpath.params$model$Biomass[12]<-MAB.rpath.params$model$Biomass[12]*8

## YTFlounder
## Increase biomass by 7x (Lucey)
MAB.rpath.params$model$Biomass[50]<-MAB.rpath.params$model$Biomass[50]*7
## Increase PB by 2x (Buccheister)
MAB.rpath.params$model$PB[50]<-MAB.rpath.params$model$PB[50]*2

## SpinyDogfish
## Decrease biomass by 0.5x (Buccheister, Link)
MAB.rpath.params$model$Biomass[43]<-MAB.rpath.params$model$Biomass[43]*.6
## Increase PB by 2x (Lucey)
MAB.rpath.params$model$PB[43]<-MAB.rpath.params$model$PB[43]*2
## Decrease QB by 0.5x (Lucey)
MAB.rpath.params$model$QB[43]<-MAB.rpath.params$model$QB[43]*.5

## RedHake
## Increase biomass by 10x (Lucey)
MAB.rpath.params$model$Biomass[33]<-MAB.rpath.params$model$Biomass[33]*10
## Decrease QB by 0.25x (Lucey)
MAB.rpath.params$model$QB[33]<-MAB.rpath.params$model$QB[33]*.25

## Micronekton
## Increase biomass by 2x (Buccheister)
MAB.rpath.params$model$Biomass[23]<-MAB.rpath.params$model$Biomass[23]*2
## Increase PB by 2x (Lucey)
MAB.rpath.params$model$PB[23]<-MAB.rpath.params$model$PB[23]*2
## Decrease QB by 0.5x (Lucey)
MAB.rpath.params$model$QB[23]<-MAB.rpath.params$model$QB[23]*0.5

## AmLobster
## Increase biomass by 25x (Okey)
MAB.rpath.params$model$Biomass[1]<-MAB.rpath.params$model$Biomass[1]*25
## Decrease QB by 0.5x (Okey)
MAB.rpath.params$model$QB[1]<-MAB.rpath.params$model$QB[1]*.5

## HMS
## Increase biomass by 2.5x (Lucey)
MAB.rpath.params$model$Biomass[15]<-MAB.rpath.params$model$Biomass[15]*2.5
## Increase PB by 1.1x (Lucey)
MAB.rpath.params$model$Biomass[15]<-MAB.rpath.params$model$Biomass[15]*1.1
## Decrease QB by 0.75x (Lucey)
MAB.rpath.params$model$QB[15]<-MAB.rpath.params$model$QB[15]*.75

## SeaBirds
## Increase biomass by 5x (Lucey, Okey)
MAB.rpath.params$model$Biomass[35]<-MAB.rpath.params$model$Biomass[35]*5

## GelZooplankton
## Increase biomass by 5x (Buccheister)
MAB.rpath.params$model$Biomass[13]<-MAB.rpath.params$model$Biomass[13]*2
## Decrease QB by 0.7x (Lucey)
MAB.rpath.params$model$Biomass[13]<-MAB.rpath.params$model$Biomass[13]*.7

## AtlCroaker
## Increase biomass by 1.5x (Prebal diagnostics)
MAB.rpath.params$model$Biomass[3]<-MAB.rpath.params$model$Biomass[3]*1.5

## Loligo
## Increase biomass by 2x (Lucey)
MAB.rpath.params$model$Biomass[19]<-MAB.rpath.params$model$Biomass[19]*2
## Decrease QB by .75x (Okey)
MAB.rpath.params$model$QB[19]<-MAB.rpath.params$model$QB[19]*.5

## BaleenWhales
## Increase PB by 1.3x (Buccheister, Link, Okey)
MAB.rpath.params$model$PB[7]<-MAB.rpath.params$model$PB[7]*1.3

## SmoothDogfish
## Increase biomass by 1.2x (Prebal diagnostics)
MAB.rpath.params$model$Biomass[40]<-MAB.rpath.params$model$Biomass[40]*1.2

## LgCopepods
## Increase PB by 1.1x (Link)
MAB.rpath.params$model$PB[17]<-MAB.rpath.params$model$PB[17]*1.1

## Reallocate predation on OtherPelagics
## Bluefish: OtherPelagics -1%, AtlMackerel +1%
MAB.rpath.params$diet[29,10]<-MAB.rpath.params$diet[29,10]-0.01
MAB.rpath.params$diet[4,10]<-MAB.rpath.params$diet[4,10]+0.01
## Bluefish: OtherPelagics -1%, GelZooplankton +1%
MAB.rpath.params$diet[29,10]<-MAB.rpath.params$diet[29,10]-0.01
MAB.rpath.params$diet[13,10]<-MAB.rpath.params$diet[13,10]+0.01
## Bluefish: OtherPelagics -1%, OceanPout +1%
MAB.rpath.params$diet[29,10]<-MAB.rpath.params$diet[29,10]-0.01
MAB.rpath.params$diet[25,10]<-MAB.rpath.params$diet[25,10]+0.01
## Bluefish: OtherPelagics -1%, RedHake +1%
MAB.rpath.params$diet[29,10]<-MAB.rpath.params$diet[29,10]-0.01
MAB.rpath.params$diet[33,10]<-MAB.rpath.params$diet[33,10]+0.01
## Bluefish: OtherPelagics -1%, WinterFlounder +1%
MAB.rpath.params$diet[29,10]<-MAB.rpath.params$diet[29,10]-0.01
MAB.rpath.params$diet[48,10]<-MAB.rpath.params$diet[48,10]+0.01
## Bluefish: OtherPelagics -1%, YTFlounder +1%
MAB.rpath.params$diet[29,10]<-MAB.rpath.params$diet[29,10]-0.01
MAB.rpath.params$diet[50,10]<-MAB.rpath.params$diet[50,10]+0.01
## SpinyDogfish: OtherPelagics -1%, AmLobster +1%
MAB.rpath.params$diet[29,44]<-MAB.rpath.params$diet[29,44]-0.01
MAB.rpath.params$diet[1,44]<-MAB.rpath.params$diet[1,44]+0.01
## SpinyDogfish: OtherPelagics -1%, AtlMackerel +1%
MAB.rpath.params$diet[29,44]<-MAB.rpath.params$diet[29,44]-0.01
MAB.rpath.params$diet[4,44]<-MAB.rpath.params$diet[4,44]+0.01
## SpinyDogfish: OtherPelagics -1%, Fourspot +1%
MAB.rpath.params$diet[29,44]<-MAB.rpath.params$diet[29,44]-0.01
MAB.rpath.params$diet[12,44]<-MAB.rpath.params$diet[12,44]+0.01
## SpinyDogfish: OtherPelagics -1%, GelZooplankton +1%
MAB.rpath.params$diet[29,44]<-MAB.rpath.params$diet[29,44]-0.01
MAB.rpath.params$diet[13,44]<-MAB.rpath.params$diet[13,44]+0.01
## SpinyDogfish: OtherPelagics -1%, Goosefish +1%
MAB.rpath.params$diet[29,44]<-MAB.rpath.params$diet[29,44]-0.01
MAB.rpath.params$diet[14,44]<-MAB.rpath.params$diet[14,44]+0.01
## SpinyDogfish: OtherPelagics -1%, Micronekton +1%
MAB.rpath.params$diet[29,44]<-MAB.rpath.params$diet[29,44]-0.01
MAB.rpath.params$diet[23,44]<-MAB.rpath.params$diet[23,44]+0.01
## SpinyDogfish: OtherPelagics -1%, OceanPout +1%
MAB.rpath.params$diet[29,44]<-MAB.rpath.params$diet[29,44]-0.01
MAB.rpath.params$diet[25,44]<-MAB.rpath.params$diet[25,44]+0.01
## SpinyDogfish: OtherPelagics -1%, Odontocetes +1%
MAB.rpath.params$diet[29,44]<-MAB.rpath.params$diet[29,44]-0.01
MAB.rpath.params$diet[26,44]<-MAB.rpath.params$diet[26,44]+0.01
## SpinyDogfish: OtherPelagics -1%, RedHake +1%
MAB.rpath.params$diet[29,44]<-MAB.rpath.params$diet[29,44]-0.01
MAB.rpath.params$diet[33,44]<-MAB.rpath.params$diet[33,44]+0.01
## SpinyDogfish: OtherPelagics -1%, WinterFlounder +1%
MAB.rpath.params$diet[29,44]<-MAB.rpath.params$diet[29,44]-0.01
MAB.rpath.params$diet[48,44]<-MAB.rpath.params$diet[48,44]+0.01
## SpinyDogfish: OtherPelagics -1%, YTFlounder +1%
MAB.rpath.params$diet[29,44]<-MAB.rpath.params$diet[29,44]-0.01
MAB.rpath.params$diet[50,44]<-MAB.rpath.params$diet[50,44]+0.01

## Shift predation on OtherCephalopods
## AtlMackerel: OtherCephalopods -1%, GelZooplankton +1%
MAB.rpath.params$diet[27,5]<-MAB.rpath.params$diet[27,5]-0.01
MAB.rpath.params$diet[13,5]<-MAB.rpath.params$diet[13,5]+0.01
## SilverHake: OtherCephalopods -1.2%, RedHake +1.2%
MAB.rpath.params$diet[27,38]<-MAB.rpath.params$diet[27,38]-0.012
MAB.rpath.params$diet[33,38]<-MAB.rpath.params$diet[33,38]+0.012
## SilverHake: OtherCephalopods -1.2%, Fourspot +1.2%
MAB.rpath.params$diet[27,38]<-MAB.rpath.params$diet[27,38]-0.012
MAB.rpath.params$diet[12,38]<-MAB.rpath.params$diet[12,38]+0.012
## SilverHake: OtherCephalopods -1.2%, WhiteHake +1.2%
MAB.rpath.params$diet[27,38]<-MAB.rpath.params$diet[27,38]-0.012
MAB.rpath.params$diet[46,38]<-MAB.rpath.params$diet[46,38]+0.012
## Bluefish: OtherCephalopods -1%, GelZooplankton +1%
MAB.rpath.params$diet[27,10]<-MAB.rpath.params$diet[27,10]-0.01
MAB.rpath.params$diet[13,10]<-MAB.rpath.params$diet[13,10]+0.01
## Bluefish: OtherCephalopods -1.2%, AtlMackerel +1%
MAB.rpath.params$diet[27,10]<-MAB.rpath.params$diet[27,10]-0.01
MAB.rpath.params$diet[4,10]<-MAB.rpath.params$diet[4,10]+0.01
## Bluefish: OtherCephalopods -1.2%, Micronekton +1%
MAB.rpath.params$diet[27,10]<-MAB.rpath.params$diet[27,10]-0.01
MAB.rpath.params$diet[23,10]<-MAB.rpath.params$diet[23,10]+0.01
## Bluefish: OtherCephalopods -1.2%, OceanPout +1%
MAB.rpath.params$diet[27,10]<-MAB.rpath.params$diet[27,10]-0.01
MAB.rpath.params$diet[25,10]<-MAB.rpath.params$diet[25,10]+0.01

## Shift predation on Mesopelagics
## GelZooplankton: Mesopelagics -0.03%, AtlMackerel +0.03%
MAB.rpath.params$diet[22,14]<-MAB.rpath.params$diet[22,14]-0.0003
MAB.rpath.params$diet[4,14]<-MAB.rpath.params$diet[4,14]+0.0003
## GelZooplankton: Mesopelagics -0.03%, Micronekton +0.03%
MAB.rpath.params$diet[22,14]<-MAB.rpath.params$diet[22,14]-0.0003
MAB.rpath.params$diet[23,14]<-MAB.rpath.params$diet[23,14]+0.0003
## GelZooplankton: Mesopelagics -0.03%, SmCopepods +0.03%
MAB.rpath.params$diet[22,14]<-MAB.rpath.params$diet[22,14]-0.0003
MAB.rpath.params$diet[38,14]<-MAB.rpath.params$diet[38,14]+0.0003

## Shift predation on Butterfish
## OtherPelagics: Butterfish -5%, AmLobster +5%
MAB.rpath.params$diet[10,30]<-MAB.rpath.params$diet[10,30]-0.05
MAB.rpath.params$diet[1,30]<-MAB.rpath.params$diet[1,30]+0.05
## OtherPelagics: Butterfish -5%, AtlScallop +5%
MAB.rpath.params$diet[10,30]<-MAB.rpath.params$diet[10,30]-0.05
MAB.rpath.params$diet[5,30]<-MAB.rpath.params$diet[5,30]+0.05
## OtherPelagics: Butterfish -5%, Scup +5%
MAB.rpath.params$diet[10,30]<-MAB.rpath.params$diet[10,30]-0.05
MAB.rpath.params$diet[34,30]<-MAB.rpath.params$diet[34,30]+0.05
## OtherPelagics: Butterfish -5%, WinterSkate +5%
MAB.rpath.params$diet[10,30]<-MAB.rpath.params$diet[10,30]-0.05
MAB.rpath.params$diet[49,30]<-MAB.rpath.params$diet[49,30]+0.05
## OtherPelagics: Butterfish -5%, YTFlounder +5%
MAB.rpath.params$diet[10,30]<-MAB.rpath.params$diet[10,30]-0.05
MAB.rpath.params$diet[50,30]<-MAB.rpath.params$diet[50,30]+0.05
## SilverHake: Butterfish -2%, AtlMackerel +2%
MAB.rpath.params$diet[10,38]<-MAB.rpath.params$diet[10,38]-0.02
MAB.rpath.params$diet[4,38]<-MAB.rpath.params$diet[4,38]+0.02
## SilverHake: Butterfish -2%, Micronekton +2%
MAB.rpath.params$diet[10,38]<-MAB.rpath.params$diet[10,38]-0.02
MAB.rpath.params$diet[23,38]<-MAB.rpath.params$diet[23,38]+0.02
## SilverHake: Butterfish -2%, RedHake +2%
MAB.rpath.params$diet[10,38]<-MAB.rpath.params$diet[10,38]-0.02
MAB.rpath.params$diet[33,38]<-MAB.rpath.params$diet[33,38]+0.02
## Bluefish: Butterfish -2%, AtlMackerel +2%
MAB.rpath.params$diet[10,10]<-MAB.rpath.params$diet[10,10]-0.02
MAB.rpath.params$diet[4,10]<-MAB.rpath.params$diet[4,10]+0.02
## Bluefish: Butterfish -2%, Micronekton +2%
MAB.rpath.params$diet[10,10]<-MAB.rpath.params$diet[10,10]-0.02
MAB.rpath.params$diet[23,10]<-MAB.rpath.params$diet[23,10]+0.02
## Bluefish: Butterfish -2%, OceanPout +2%
MAB.rpath.params$diet[10,10]<-MAB.rpath.params$diet[10,10]-0.02
MAB.rpath.params$diet[25,10]<-MAB.rpath.params$diet[25,10]+0.02
## Bluefish: Butterfish -2%, RedHake +2%
MAB.rpath.params$diet[10,10]<-MAB.rpath.params$diet[10,10]-0.02
MAB.rpath.params$diet[33,10]<-MAB.rpath.params$diet[33,10]+0.02
## Bluefish: Butterfish -2%, WinterFlounder +2%
MAB.rpath.params$diet[10,10]<-MAB.rpath.params$diet[10,10]-0.02
MAB.rpath.params$diet[48,10]<-MAB.rpath.params$diet[48,10]+0.02
## Bluefish: Butterfish -2%, YTFlounder +2%
MAB.rpath.params$diet[10,10]<-MAB.rpath.params$diet[10,10]-0.02
MAB.rpath.params$diet[50,10]<-MAB.rpath.params$diet[50,10]+0.02
## Odontocetes: Butterfish -4%, AtlMackerel +4%
MAB.rpath.params$diet[10,27]<-MAB.rpath.params$diet[10,27]-0.04
MAB.rpath.params$diet[4,27]<-MAB.rpath.params$diet[4,27]+0.04
## Odontocetes: Butterfish -4%, Goosefish +4%
MAB.rpath.params$diet[10,27]<-MAB.rpath.params$diet[10,27]-0.04
MAB.rpath.params$diet[14,27]<-MAB.rpath.params$diet[14,27]+0.04
## Odontocetes: Butterfish -4%, Micronekton +4%
MAB.rpath.params$diet[10,27]<-MAB.rpath.params$diet[10,27]-0.04
MAB.rpath.params$diet[23,27]<-MAB.rpath.params$diet[23,27]+0.04
## Odontocetes: Butterfish -4%, WinterSkate +4%
MAB.rpath.params$diet[10,27]<-MAB.rpath.params$diet[10,27]-0.04
MAB.rpath.params$diet[49,27]<-MAB.rpath.params$diet[49,27]+0.04

## Shift predation on WhiteHake
## SilverHake: WhiteHake -0.2%, AtlMackerel +0.2%
MAB.rpath.params$diet[46,38]<-MAB.rpath.params$diet[46,38]-0.002
MAB.rpath.params$diet[4,38]<-MAB.rpath.params$diet[4,38]+0.002
## SilverHake: WhiteHake -0.2%, Mesopelagics +0.2%
MAB.rpath.params$diet[46,38]<-MAB.rpath.params$diet[46,38]-0.002
MAB.rpath.params$diet[22,38]<-MAB.rpath.params$diet[22,38]+0.002
## SilverHake: WhiteHake -0.2%, Micronekton +0.2%
MAB.rpath.params$diet[46,38]<-MAB.rpath.params$diet[46,38]-0.002
MAB.rpath.params$diet[23,38]<-MAB.rpath.params$diet[23,38]+0.002
## SilverHake: WhiteHake -0.2%, RedHake +0.2%
MAB.rpath.params$diet[46,38]<-MAB.rpath.params$diet[46,38]-0.002
MAB.rpath.params$diet[33,38]<-MAB.rpath.params$diet[33,38]+0.002
## SilverHake: WhiteHake -0.2%, YTFlounder +0.2%
MAB.rpath.params$diet[46,38]<-MAB.rpath.params$diet[46,38]-0.002
MAB.rpath.params$diet[50,38]<-MAB.rpath.params$diet[50,38]+0.002

## Shift predation on Weakfish
## OtherPelagics: Weakfish -0.2%, AmLobster +0.2%
MAB.rpath.params$diet[45,30]<-MAB.rpath.params$diet[45,30]-0.002
MAB.rpath.params$diet[1,30]<-MAB.rpath.params$diet[1,30]+0.002
## OtherPelagics: Weakfish -0.2%, Micronekton +0.2%
MAB.rpath.params$diet[45,30]<-MAB.rpath.params$diet[45,30]-0.002
MAB.rpath.params$diet[23,30]<-MAB.rpath.params$diet[23,30]+0.002
## OtherPelagics: Weakfish -0.2%, Scup +0.2%
MAB.rpath.params$diet[45,30]<-MAB.rpath.params$diet[45,30]-0.002
MAB.rpath.params$diet[34,30]<-MAB.rpath.params$diet[34,30]+0.002

## Shift predation on SpinyDogfish
## OtherPelagics: SpinyDogfish -0.5%, AmLobster +0.5%
MAB.rpath.params$diet[43,30]<-MAB.rpath.params$diet[43,30]-0.005
MAB.rpath.params$diet[1,30]<-MAB.rpath.params$diet[1,30]+0.005
## OtherPelagics: SpinyDogfish -0.5%, AtlMackerel +0.5%
MAB.rpath.params$diet[43,30]<-MAB.rpath.params$diet[43,30]-0.005
MAB.rpath.params$diet[4,30]<-MAB.rpath.params$diet[4,30]+0.005
## OtherPelagics: SpinyDogfish -0.5%, Micronekton +0.5%
MAB.rpath.params$diet[43,30]<-MAB.rpath.params$diet[43,30]-0.005
MAB.rpath.params$diet[23,30]<-MAB.rpath.params$diet[23,30]+0.005
## OtherPelagics: SpinyDogfish -0.5%, RedHake +0.5%
MAB.rpath.params$diet[43,30]<-MAB.rpath.params$diet[43,30]-0.005
MAB.rpath.params$diet[33,30]<-MAB.rpath.params$diet[33,30]+0.005
## OtherPelagics: SpinyDogfish -0.5%, Scup +0.5%
MAB.rpath.params$diet[43,30]<-MAB.rpath.params$diet[43,30]-0.005
MAB.rpath.params$diet[34,30]<-MAB.rpath.params$diet[34,30]+0.005
## OtherPelagics: SpinyDogfish -0.5%, Goosefish +0.5%
MAB.rpath.params$diet[43,30]<-MAB.rpath.params$diet[43,30]-0.005
MAB.rpath.params$diet[14,30]<-MAB.rpath.params$diet[14,30]+0.005

## Shift predation on SilverHake
## SilverHake: SilverHake -1%, Micronekton +1%
MAB.rpath.params$diet[37,38]<-MAB.rpath.params$diet[37,38]-0.01
MAB.rpath.params$diet[23,38]<-MAB.rpath.params$diet[23,38]+0.01
## SilverHake: SilverHake -1%, RedHake +1%
MAB.rpath.params$diet[37,38]<-MAB.rpath.params$diet[37,38]-0.01
MAB.rpath.params$diet[33,38]<-MAB.rpath.params$diet[33,38]+0.01
## SilverHake: SilverHake -1%, YTFlounder +1%
MAB.rpath.params$diet[37,38]<-MAB.rpath.params$diet[37,38]-0.01
MAB.rpath.params$diet[50,38]<-MAB.rpath.params$diet[50,38]+0.01

## Shift predation on OtherDemersals
## SilverHake: OtherDemersals -1%, Loligo +1%
MAB.rpath.params$diet[28,38]<-MAB.rpath.params$diet[28,38]-0.01
MAB.rpath.params$diet[19,38]<-MAB.rpath.params$diet[19,38]+0.01
## SilverHake: OtherDemersals -1%, Micronekton +1%
MAB.rpath.params$diet[28,38]<-MAB.rpath.params$diet[28,38]-0.01
MAB.rpath.params$diet[23,38]<-MAB.rpath.params$diet[23,38]+0.01
## SilverHake: OtherDemersals -1%, RedHake +1%
MAB.rpath.params$diet[28,38]<-MAB.rpath.params$diet[28,38]-0.01
MAB.rpath.params$diet[33,38]<-MAB.rpath.params$diet[33,38]+0.01
## SilverHake: OtherDemersals -1%, AtlMackerel +1%
MAB.rpath.params$diet[28,38]<-MAB.rpath.params$diet[28,38]-0.01
MAB.rpath.params$diet[4,38]<-MAB.rpath.params$diet[4,38]+0.01

## Shift predation on WinterFlounder
## Bluefish: WinterFlounder -0.5%, Loligo +0.5%
MAB.rpath.params$diet[48,10]<-MAB.rpath.params$diet[48,10]-0.005
MAB.rpath.params$diet[19,10]<-MAB.rpath.params$diet[19,10]+0.005
## Bluefish: WinterFlounder -1%, Micronekton +1%
MAB.rpath.params$diet[48,10]<-MAB.rpath.params$diet[48,10]-0.005
MAB.rpath.params$diet[23,10]<-MAB.rpath.params$diet[23,10]+0.005
## Bluefish: WinterFlounder -1%, RedHake +1%
MAB.rpath.params$diet[48,10]<-MAB.rpath.params$diet[48,10]-0.005
MAB.rpath.params$diet[33,10]<-MAB.rpath.params$diet[33,10]+0.005

## Rerun model, check prebal diagnostics and save EEs
MAB.rpath<-rpath(MAB.rpath.params,eco.name='Mid-Atlantic Bight')
MAB.rpath
EE<-MAB.rpath$EE
EE[order(EE)]

source("MAB_prebal.R")
write.csv(MAB.rpath$EE,file="MAB_EE_temp.csv")
write.Rpath(MAB.rpath,morts=T,file="MAB.rpath_morts.csv")
write.csv(MAB.rpath.params$diet,file="MAB.rpath.diet.csv")
write.csv(MAB.rpath.params$model,file="MAB.rpath.model.csv")
MAB.rpath$PB[4]

## Script name: MAB_RPath.R
##
## Purpose of script: Compile all data to create functional RPath model.
##                    
##
## Author: Brandon Beltz, updated by Sarah J. Weisberg
##
##
## Email: sarah.j.weisberg@stonybrook.edu
#

# Fri Dec  8 16:54:53 2023 ------------------------------

## Load libraries, packages and functions
library(Rpath); library(data.table); library(dplyr); library(here)

# Model Setup -------------------------------------------------------------

## Add functional groups to model and generate rpath params
source(here("MAB_fleets.R"))
groups<-as.vector(groups_fleets$RPATH)
types<-c(rep(0,31),1,rep(0,17),rep(2,2),rep(3,11))
MAB.rpath.params<-create.rpath.params(group = groups, type = types)

## Add biomass estimates
source(here("MAB_biomass_estimates.R"))
biomass<-left_join(groups_fleets,MAB.biomass.80s,by = "RPATH")
biomass<-as.vector(biomass$Biomass)
MAB.rpath.params$model[,Biomass:=biomass]

## Add PB parameters
source(here("MAB_params.R"))
pb<-cbind(MAB.groups,MAB.PB)
pb<-left_join(groups_fleets,pb,by = "RPATH")
pb<-as.vector(pb$MAB.PB)
MAB.rpath.params$model[,PB:=pb]

## Add QB parameters
qb<-cbind(MAB.groups,MAB.QB)
qb<-left_join(groups_fleets,qb,by = "RPATH")
qb<-as.vector(qb$MAB.QB)
MAB.rpath.params$model[,QB:=qb]

## Add biomass accumulation
ba<-cbind(MAB.groups,MAB.Params$BA)
colnames(ba)[2]<-"BA"
ba<-left_join(groups_fleets,ba,by = "RPATH")
ba<-as.vector(ba$BA)
MAB.rpath.params$model[,BioAcc:=ba]

## Add unassimilated consumption
MAB.rpath.params$model[, Unassim := c(rep(0.2,5),0.4,rep(0.2,6),0.4,rep(0.2,3),0.4,rep(0.2,6),0.4,rep(0.2,7),0,rep(0.2,5),0.4,rep(0.2,11),rep(0,2),rep(NA,11))]

## Add detrital fate and set discards to 0
MAB.rpath.params$model[,Detritus:=c(rep(1,49),rep(0,13))]
MAB.rpath.params$model[,Discards:=c(rep(0,51),rep(1,11))]

## Add landings by gear type
source("MAB_discards.R")

## Fixed Gear
fixed<-left_join(groups_fleets,fixed,by="RPATH")
fixed<-as.vector(fixed$landings)
fixed[50:51]<-0
MAB.rpath.params$model[,"Fixed Gear":=fixed]

## Large Mesh
lg_mesh<-left_join(groups_fleets,lg_mesh,by="RPATH")
lg_mesh<-as.vector(lg_mesh$landings)
lg_mesh[50:51]<-0
MAB.rpath.params$model[, "LG Mesh" := lg_mesh]

## Other
other<-left_join(groups_fleets,other,by="RPATH")
other<-as.vector(other$landings)
other[50:51]<-0
MAB.rpath.params$model[, "Other" := other]

## Small Mesh
sm_mesh<-left_join(groups_fleets,sm_mesh,by="RPATH")
sm_mesh<-as.vector(sm_mesh$landings)
sm_mesh[50:51]<-0
MAB.rpath.params$model[, "SM Mesh" := sm_mesh]

## Scallop Dredge
scallop<-left_join(groups_fleets,scallop,by="RPATH")
scallop<-as.vector(scallop$landings)
scallop[50:51]<-0
MAB.rpath.params$model[, "Scallop Dredge" := scallop]

## Trap
trap<-left_join(groups_fleets,trap,by="RPATH")
trap<-as.vector(trap$landings)
trap[50:51]<-0
MAB.rpath.params$model[, "Trap" := trap]

## HMS Fleet
hms<-left_join(groups_fleets,hms,by="RPATH")
hms<-as.vector(hms$landings)
hms[50:51]<-0
MAB.rpath.params$model[, "HMS Fleet" := hms]

## Pelagic
pelagic<-left_join(groups_fleets,pelagic,by="RPATH")
pelagic<-as.vector(pelagic$landings)
pelagic[50:51]<-0
MAB.rpath.params$model[, "Pelagic" := pelagic]

## Other Dredge
other_dredge<-left_join(groups_fleets,other_dredge,by="RPATH")
other_dredge<-as.vector(other_dredge$landings)
other_dredge[50:51]<-0
MAB.rpath.params$model[, "Other Dredge" := other_dredge]

## Clam
clam<-left_join(groups_fleets,clam,by="RPATH")
clam<-as.vector(clam$landings)
clam[50:51]<-0
MAB.rpath.params$model[, "Clam Dredge" := clam]

## Recreational
source("MAB_rec_catch.R")
rec_catch<-left_join(groups_fleets,MAB.mrip_summary,by="RPATH")
rec_catch<-as.vector(rec_catch$Per_Area)
rec_catch[is.na(rec_catch)]<-0
rec_catch[50:51]<-0
MAB.rpath.params$model[,"Recreational":=rec_catch]

## Add discards by gear type
## Fixed Gear
fixed.d<-left_join(groups_fleets,fixed.d,by="RPATH")
fixed.d<-as.vector(fixed.d$discards)
fixed.d[50:51]<-0
MAB.rpath.params$model[, "Fixed Gear.disc" := fixed.d]

## Lg Mesh
lg_mesh.d<-left_join(groups_fleets,lg_mesh.d,by="RPATH")
lg_mesh.d<-as.vector(lg_mesh.d$discards)
lg_mesh.d[50:51]<-0
MAB.rpath.params$model[, "LG Mesh.disc" := lg_mesh.d]

## Other
other.d<-left_join(groups_fleets,other.d,by="RPATH")
other.d<-as.vector(other.d$discards)
other.d[50:51]<-0
MAB.rpath.params$model[, "Other.disc" := other.d]

## SM Mesh
sm_mesh.d<-left_join(groups_fleets,sm_mesh.d,by="RPATH")
sm_mesh.d<-as.vector(sm_mesh.d$discards)
sm_mesh.d[50:51]<-0
MAB.rpath.params$model[, "SM Mesh.disc" := sm_mesh.d]

## Scallop Dredge
scallop.d<-left_join(groups_fleets,scallop.d,by="RPATH")
scallop.d<-as.vector(scallop.d$discards)
scallop.d[50:51]<-0
MAB.rpath.params$model[, "Scallop Dredge" := scallop.d]

## Trap
trap.d<-left_join(groups_fleets,trap.d,by="RPATH")
trap.d<-as.vector(trap.d$discards)
trap.d[50:51]<-0
MAB.rpath.params$model[, "Trap.disc" := trap.d]

## HMS
hms.d<-left_join(groups_fleets,hms.d,by="RPATH")
hms.d<-as.vector(hms.d$discards)
hms.d[50:51]<-0
MAB.rpath.params$model[, "HMS Fleet.disc" := hms.d]

## Pelagic
pelagic.d<-left_join(groups_fleets,pelagic.d,by="RPATH")
pelagic.d<-as.vector(pelagic.d$discards)
pelagic.d[50:51]<-0
MAB.rpath.params$model[, "Pelagic.disc" := pelagic.d]

## Other Dredge
other_dredge.d<-left_join(groups_fleets,other_dredge.d,by="RPATH")
other_dredge.d<-as.vector(other_dredge.d$discards)
other_dredge.d[50:51]<-0
MAB.rpath.params$model[, "Other Dredge.disc" := other_dredge.d]

## Clam Dredge
clam.d<-c(rep(0,49),rep(0,2),rep(NA,11))
MAB.rpath.params$model[, "Clam Dredge.disc" := clam.d]

## Run diet matrix
source(here("MAB_diet.R"))

## Fill Rpath parameter file with diet
source(here("MAB_diet_fill.R"))


# Sarah_changes -----------------------------------------------------------

source(here("Sarah_R/rebalance.R"))

## Run model
MAB.rpath<-rpath(MAB.rpath.params,eco.name='Mid-Atlantic Bight')
MAB.rpath

## Create webplot
webplot(MAB.rpath, labels = T)

## Save .csv file with model information
#write.Rpath(MAB.rpath, file="MAB_model.csv")
#write.csv(MAB.rpath.params$model, file="MAB_prebalance_model.csv")
#write.csv(MAB.rpath.params$diet, file = "MAB_prebalance_diet.csv")

# ## Balance adjustments
# MAB.rpath<-rpath(MAB.rpath.params,eco.name='Mid-Atlantic Bight')
# MAB.rpath
# source(here("MAB_prebal.R"))
# EE<-MAB.rpath$EE
# EE[order(EE)]
# write.Rpath(MAB.rpath,morts=T,file="MAB.rpath_morts.csv")
# #write.Rpath(MAB.rpath, file="MAB_model.csv")

# Balancing changes -------------------
## OtherPelagics
## Increase biomass by 200x (Lucey, Link, Buccheister et al.)
MAB.rpath.params$model$Biomass[29]<-MAB.rpath.params$model$Biomass[29]*200

## OtherCephalopods
## Increase biomass by 600x (Prebal diagnostics)
MAB.rpath.params$model$Biomass[27]<-MAB.rpath.params$model$Biomass[27]*600

## SpinyDogfish
## Decrease biomass by 0.8x (Lucey, Buccheister et al.)
MAB.rpath.params$model$Biomass[43]<-MAB.rpath.params$model$Biomass[43]*.8

## OtherDemersals
## Increase biomass by 25x
MAB.rpath.params$model$Biomass[28]<-MAB.rpath.params$model$Biomass[28]*200

## Sharks
## Increase biomass by 1.5x
MAB.rpath.params$model$Biomass[36]<-MAB.rpath.params$model$Biomass[36]*1.5

## BlackSeaBass
## Increase biomass by 11x (Lucey)
MAB.rpath.params$model$Biomass[8]<-MAB.rpath.params$model$Biomass[8]*11

## SouthernDemersals
## Increase biomass by 10x (Prebal diagnostics)
MAB.rpath.params$model$Biomass[42]<-MAB.rpath.params$model$Biomass[42]*10

## Cod
## Increase biomass by 5x (Lucey, Okey, Buccheister et al.)
MAB.rpath.params$model$Biomass[11]<-MAB.rpath.params$model$Biomass[11]*10

## OtherShrimps
## Increase biomass by 100x (Buccheister et al., EMAX; Prebal diagnostics)
MAB.rpath.params$model$Biomass[30]<-MAB.rpath.params$model$Biomass[30]*100

## Megabenthos
## Increase biomass by 50x (Prebal diagnostics)
MAB.rpath.params$model$Biomass[21]<-MAB.rpath.params$model$Biomass[21]*50

## Bluefish
## Increase biomass by 2x (Buccheister et al.)
MAB.rpath.params$model$Biomass[9]<-MAB.rpath.params$model$Biomass[9]*2

## SmFlatfishes
## Increase biomass by 14x (Lucey)
MAB.rpath.params$model$Biomass[39]<-MAB.rpath.params$model$Biomass[39]*14

## AtlMackerel
## Increase biomass by 16x (Lucey, Buccheister et al.)
MAB.rpath.params$model$Biomass[4]<-MAB.rpath.params$model$Biomass[4]*16

## GelZooplankton
## Increase biomass by 2x (Prebal diagnostics)
MAB.rpath.params$model$Biomass[13]<-MAB.rpath.params$model$Biomass[13]*2

## SummerFlounder
## Increase biomass by 10x (Lucey, Buccheister et al.)
MAB.rpath.params$model$Biomass[44]<-MAB.rpath.params$model$Biomass[44]*10

## HMS
## Decrease biomass by 0.75x
MAB.rpath.params$model$Biomass[15]<-MAB.rpath.params$model$Biomass[15]*.75

## OceanPout
## Increase biomass by 10x (Lucey)
MAB.rpath.params$model$Biomass[25]<-MAB.rpath.params$model$Biomass[25]*10

## WinterSkate
## Increase biomass by 5x (Lucey)
MAB.rpath.params$model$Biomass[48]<-MAB.rpath.params$model$Biomass[48]*5

## Windowpane
## Increase biomass by 8x (Lucey)
MAB.rpath.params$model$Biomass[46]<-MAB.rpath.params$model$Biomass[46]*8

## SmPelagics
## Increase biomass by 18x (Buchheister)
MAB.rpath.params$model$Biomass[41]<-MAB.rpath.params$model$Biomass[41]*18

## LittleSkate
## Increase biomass by 5x (Lucey)
MAB.rpath.params$model$Biomass[18]<-MAB.rpath.params$model$Biomass[18]*5

## Scup
## Increase biomass by 5x (Lucey)
MAB.rpath.params$model$Biomass[34]<-MAB.rpath.params$model$Biomass[34]*5

## WinterFlounder
## Increase biomass by 8x (Lucey)
MAB.rpath.params$model$Biomass[47]<-MAB.rpath.params$model$Biomass[47]*8

## Mesopelagics
## Increase biomass by 2.5x (Prebal diagnostics)
MAB.rpath.params$model$Biomass[22]<-MAB.rpath.params$model$Biomass[22]*2.5

## AtlScallop
## Increase biomass by 5x (Lucey)
MAB.rpath.params$model$Biomass[5]<-MAB.rpath.params$model$Biomass[5]*5

## OtherSkates
## Increase biomass by 3x (Lucey)
MAB.rpath.params$model$Biomass[31]<-MAB.rpath.params$model$Biomass[31]*3

## Odontocetes
## Increase biomass by 1.5x (Lucey)
MAB.rpath.params$model$Biomass[26]<-MAB.rpath.params$model$Biomass[26]*2

## SilverHake
## Increase biomass by 2x (Lucey)
MAB.rpath.params$model$Biomass[37]<-MAB.rpath.params$model$Biomass[37]*2

## Goosefish
## Increase biomass by 2.5x (Okey)
MAB.rpath.params$model$Biomass[14]<-MAB.rpath.params$model$Biomass[14]*2.5

## AmShad
## Increase biomass by 2x (Prebal diagnostics)
MAB.rpath.params$model$Biomass[2]<-MAB.rpath.params$model$Biomass[2]*2

## Fourspot
## Increase biomass by 1.5x (Lucey)
MAB.rpath.params$model$Biomass[12]<-MAB.rpath.params$model$Biomass[12]*1.5

##Weakfish
##Increase biomass by 1.5x (Assessment)
MAB.rpath.params$model$Biomass[45]<-MAB.rpath.params$model$Biomass[45]*1.5

## PB changes
## Bluefish
## Increase PB by 2x (Buccheister et al.)
MAB.rpath.params$model$PB[9]<-MAB.rpath.params$model$PB[9]*2

## HMS
## Decrease PB by 2x
MAB.rpath.params$model$PB[15]<-MAB.rpath.params$model$PB[15]/2

## AtlMackerel
## Decrease PB by 2x
MAB.rpath.params$model$PB[4]<-MAB.rpath.params$model$PB[4]/2

## GelZooplankton
## Increase PB by 10x (Lucey)
MAB.rpath.params$model$PB[13]<-MAB.rpath.params$model$PB[13]*10

## Megabenthos
## Decrease PB by 2x (Lucey, Link et al.)
MAB.rpath.params$model$PB[21]<-MAB.rpath.params$model$PB[21]/2

## OtherShrimps
## Decrease PB by 0.6x (Lucey)
MAB.rpath.params$model$PB[30]<-MAB.rpath.params$model$PB[30]*0.6

## RedHake
## Decrease PB by 2x
MAB.rpath.params$model$PB[33]<-MAB.rpath.params$model$PB[33]/2

## SilverHake
## Decrease PB by 2x
MAB.rpath.params$model$PB[37]<-MAB.rpath.params$model$PB[37]/2

## SmoothDogfish
## Decrease PB by 4x
MAB.rpath.params$model$PB[40]<-MAB.rpath.params$model$PB[40]/4

## SmPelagics
## Decrease PB by 2x
MAB.rpath.params$model$PB[41]<-MAB.rpath.params$model$PB[41]/2

## Windowpane
## Decrease PB by 0.6x
MAB.rpath.params$model$PB[46]<-MAB.rpath.params$model$PB[46]*0.6

## WinterFlounder
## Decrease PB by 0.6x
MAB.rpath.params$model$PB[47]<-MAB.rpath.params$model$PB[47]*0.6

## WinterSkate
## Decrease PB by 0.6x
MAB.rpath.params$model$PB[48]<-MAB.rpath.params$model$PB[48]*0.6


## QB changes
## AtlMackerel
## Increase QB to bring GE < 1
MAB.rpath.params$model$QB[4]<-MAB.rpath.params$model$QB[4]*1.5

## OtherShrimps
## Increase QB to bring GE < 1
MAB.rpath.params$model$QB[30]<-MAB.rpath.params$model$QB[30]*1.75

## HMS
## Decrease QB by 2x
MAB.rpath.params$model$QB[15]<-MAB.rpath.params$model$QB[15]/2

## Seabirds
## Decrease QB by 2x
MAB.rpath.params$model$QB[35]<-MAB.rpath.params$model$QB[35]/2

## GelZooplankton
## Decrease QB by 0.75x (Lucey)
MAB.rpath.params$model$QB[13]<-MAB.rpath.params$model$QB[13]*0.75

## SummerFlounder
## Decrease QB by 0.75x (Lucey)
MAB.rpath.params$model$QB[44]<-MAB.rpath.params$model$QB[44]*0.75

## Odontocetes
## Decrease QB by 2x (Lucey)
MAB.rpath.params$model$QB[26]<-MAB.rpath.params$model$QB[26]/2

## RedHake
## Decrease QB by 4x (Lucey)
MAB.rpath.params$model$QB[33]<-MAB.rpath.params$model$QB[33]/4

## Fishing changes
## Sharks
## Reduce recreational pressure
MAB.rpath.params$model$Recreational[36]<-MAB.rpath.params$model$Recreational[36]*0
## Reduce trap fishing
MAB.rpath.params$model$Trap[36]<-MAB.rpath.params$model$Trap[36]*0

## OtherPelagics
## Reduce trap fishing
MAB.rpath.params$model$Trap[29]<-MAB.rpath.params$model$Trap[29]*0
## Reduce recreational fishing by 0.01x
#MAB.rpath.params$model$Recreational[29]<-MAB.rpath.params$model$Recreational[29]*0.01
## Reduce pelagic fishing by 0.1x
MAB.rpath.params$model$Pelagic[29]<-MAB.rpath.params$model$Pelagic[29]*0.1

## Bluefish
## Reduce recreational fishing
MAB.rpath.params$model$Recreational[9]<-MAB.rpath.params$model$Recreational[9]*.01

## Scup
## Reduce recreational fishing
## SW addition
#MAB.rpath.params$model$Recreational[34]<-MAB.rpath.params$model$Recreational[34]*0.05

## SouthernDemersals
## Reduce recreational fishing
#MAB.rpath.params$model$Recreational[42]<-MAB.rpath.params$model$Recreational[42]*.01
## Reduce trap fishing
MAB.rpath.params$model$Trap[42]<-MAB.rpath.params$model$Trap[42]*.01

## BlackSeaBass
## Reduce recreational fishing
#MAB.rpath.params$model$Recreational[8]<-MAB.rpath.params$model$Recreational[8]*.01

## Cod
## Reduce recreational fishing
#MAB.rpath.params$model$Recreational[11]<-MAB.rpath.params$model$Recreational[11]*.1

## AtlMackerel
## Reduce recreational fishing
#MAB.rpath.params$model$Recreational[4]<-MAB.rpath.params$model$Recreational[4]*.01

## OtherDemersals
## Reduce trap fishing
MAB.rpath.params$model$Trap[28]<-MAB.rpath.params$model$Trap[28]*.01
## Reduce recreational fishing
#MAB.rpath.params$model$Recreational[28]<-MAB.rpath.params$model$Recreational[28]*.01

## SilverHake
## Reduce SM Mesh fishing
MAB.rpath.params$model$`SM Mesh`[37]<-MAB.rpath.params$model$`SM Mesh`[37]*.1

## Weakfish
## Reduce recreational fishing
## SW added
MAB.rpath.params$model$Recreational[45]<-MAB.rpath.params$model$Recreational[45]*.5

## RedHake
## Reduce recreational fishing
#MAB.rpath.params$model$Recreational[33]<-MAB.rpath.params$model$Recreational[33]*.01

## SummerFlounder
## Reduce recreational fishing
#MAB.rpath.params$model$Recreational[44]<-MAB.rpath.params$model$Recreational[44]*.1

## Mesopelagics
## Reduce trap fishing
MAB.rpath.params$model$Trap[22]<-MAB.rpath.params$model$Trap[22]*.1

## Diet changes

## Relieve predation pressure on OtherCephalopods
## SummerFlounder: OtherCephalopods -10%, Illex +10%
MAB.rpath.params$diet[27,45]<-MAB.rpath.params$diet[27,45]-0.10
MAB.rpath.params$diet[16,45]<-MAB.rpath.params$diet[16,45]+0.10

## SpinyDogfish: OtherCephalopods -3%, Illex +3%
MAB.rpath.params$diet[27,44]<-MAB.rpath.params$diet[27,44]-0.03
MAB.rpath.params$diet[16,44]<-MAB.rpath.params$diet[16,44]+0.03

## SilverHake: OtherCephalopods -4%, Illex +4%
MAB.rpath.params$diet[27,38]<-MAB.rpath.params$diet[27,38]-0.04
MAB.rpath.params$diet[16,38]<-MAB.rpath.params$diet[16,38]+0.04

## Bluefish: OtherCephalopods -5%, Illex +5%
MAB.rpath.params$diet[27,10]<-MAB.rpath.params$diet[27,10]-0.05
MAB.rpath.params$diet[16,10]<-MAB.rpath.params$diet[16,10]+0.05

## Butterfish: OtherCephalopods -.5%, Illex +.5%
MAB.rpath.params$diet[27,11]<-MAB.rpath.params$diet[27,11]-0.005
MAB.rpath.params$diet[16,11]<-MAB.rpath.params$diet[16,11]+0.005

## Fourspot: OtherCephalopods -10%, Illex +10%
MAB.rpath.params$diet[27,13]<-MAB.rpath.params$diet[27,13]-0.10
MAB.rpath.params$diet[16,13]<-MAB.rpath.params$diet[16,13]+0.10

## RedHake: OtherCephalopods -4%, Illex +4%
MAB.rpath.params$diet[27,34]<-MAB.rpath.params$diet[27,34]-0.04
MAB.rpath.params$diet[16,34]<-MAB.rpath.params$diet[16,34]+0.04

## Relieve predation pressure on OtherPelagics
## SpinyDogfish: OtherPelagics -5%, Illex +5%
MAB.rpath.params$diet[29,44]<-MAB.rpath.params$diet[29,44]-0.05
MAB.rpath.params$diet[16,44]<-MAB.rpath.params$diet[16,44]+0.05

## SpinyDogfish: OtherPelagics -5%, Macrobenthos +5%
MAB.rpath.params$diet[29,44]<-MAB.rpath.params$diet[29,44]-0.05
MAB.rpath.params$diet[20,44]<-MAB.rpath.params$diet[20,44]+0.05

## Relieve predation pressure on AtlMackerel
## SpinyDogfish: AtlMackerel -7%, Macrobenthos +7% 
MAB.rpath.params$diet[4,44]<-MAB.rpath.params$diet[4,44]-0.07
MAB.rpath.params$diet[20,44]<-MAB.rpath.params$diet[20,44]+0.07

## Goosefish: AtlMackerel -4%, Macrobenthos +4% 
MAB.rpath.params$diet[4,15]<-MAB.rpath.params$diet[4,15]-0.04
MAB.rpath.params$diet[20,15]<-MAB.rpath.params$diet[20,15]+0.04

## SilverHake: AtlMackerel -4%, Macrobenthos +4% 
MAB.rpath.params$diet[4,38]<-MAB.rpath.params$diet[4,38]-0.04
MAB.rpath.params$diet[20,38]<-MAB.rpath.params$diet[20,38]+0.04

## SummerFlounder: AtlMackerel -4%, Macrobenthos +4% 
MAB.rpath.params$diet[4,45]<-MAB.rpath.params$diet[4,45]-0.04
MAB.rpath.params$diet[20,45]<-MAB.rpath.params$diet[20,45]+0.04

## Relieve predation pressure on SmFlatfishes
## SpinyDogfish: SmFlatfishes -2.5%, Macrobenthos +2.5%
MAB.rpath.params$diet[39,44]<-MAB.rpath.params$diet[39,44]-0.025
MAB.rpath.params$diet[20,44]<-MAB.rpath.params$diet[20,44]+0.025

## Goosefish: SmFlatfishes -1.2%, Macrobenthos +1.2%
MAB.rpath.params$diet[39,15]<-MAB.rpath.params$diet[39,15]-0.012
MAB.rpath.params$diet[20,15]<-MAB.rpath.params$diet[20,15]+0.012

## OtherSkates: SmFlatfishes -2.5%, Macrobenthos +2.5%
MAB.rpath.params$diet[39,32]<-MAB.rpath.params$diet[39,32]-0.025
MAB.rpath.params$diet[20,32]<-MAB.rpath.params$diet[20,32]+0.025

## Cod: SmFlatfishes -.5%, Macrobenthos +.5%
MAB.rpath.params$diet[39,12]<-MAB.rpath.params$diet[39,12]-0.005
MAB.rpath.params$diet[20,12]<-MAB.rpath.params$diet[20,12]+0.005

## LittleSkate: SmFlatfishes -.5%, Macrobenthos +.5%
MAB.rpath.params$diet[39,19]<-MAB.rpath.params$diet[39,19]-0.005
MAB.rpath.params$diet[20,19]<-MAB.rpath.params$diet[20,19]+0.005

## RedHake: SmFlatfishes -2.5%, Macrobenthos +2.5%
MAB.rpath.params$diet[39,34]<-MAB.rpath.params$diet[39,34]-0.025
MAB.rpath.params$diet[20,34]<-MAB.rpath.params$diet[20,34]+0.025

## SilverHake: SmFlatfishes -.3%, Macrobenthos +.3%
MAB.rpath.params$diet[39,38]<-MAB.rpath.params$diet[39,38]-0.003
MAB.rpath.params$diet[20,38]<-MAB.rpath.params$diet[20,38]+0.003

## SummerFlounder: SmFlatfishes -.6%, Macrobenthos +.6%
MAB.rpath.params$diet[39,45]<-MAB.rpath.params$diet[39,45]-0.006
MAB.rpath.params$diet[20,45]<-MAB.rpath.params$diet[20,45]+0.006

## Windowpane: SmFlatfishes -2.4%, Macrobenthos +2.4%
MAB.rpath.params$diet[39,47]<-MAB.rpath.params$diet[39,47]-0.024
MAB.rpath.params$diet[20,47]<-MAB.rpath.params$diet[20,47]+0.024

## WinterSkate: SmFlatfishes -2.3%, Macrobenthos +2.3%
MAB.rpath.params$diet[39,49]<-MAB.rpath.params$diet[39,49]-0.023
MAB.rpath.params$diet[20,49]<-MAB.rpath.params$diet[20,49]+0.023

## Relieve predation pressure on OtherDemersals
## SummerFlounder: OtherDemersals -5%, Macrobenthos +5%
MAB.rpath.params$diet[28,45]<-MAB.rpath.params$diet[28,45]-0.05
MAB.rpath.params$diet[20,45]<-MAB.rpath.params$diet[20,45]+0.05

## Relieve predation pressure on Megabenthos
## Macrobenthos: Megabenthos -1%, Macrobenthos +1%
MAB.rpath.params$diet[21,21]<-MAB.rpath.params$diet[21,21]-0.01
MAB.rpath.params$diet[20,21]<-MAB.rpath.params$diet[20,21]+0.01

## Relieve predation pressure on SilverHake
## SilverHake: SilverHake -10%, Macrobenthos +10%
MAB.rpath.params$diet[37,38]<-MAB.rpath.params$diet[37,38]-0.1
MAB.rpath.params$diet[20,38]<-MAB.rpath.params$diet[20,38]+0.1

## Goosefish: SilverHake -7%, Macrobenthos +7%
MAB.rpath.params$diet[37,15]<-MAB.rpath.params$diet[37,15]-0.07
MAB.rpath.params$diet[20,15]<-MAB.rpath.params$diet[20,15]+0.07

## SpinyDogfish: SilverHake -1.5%, Macrobenthos +1.5%
MAB.rpath.params$diet[37,44]<-MAB.rpath.params$diet[37,44]-0.015
MAB.rpath.params$diet[20,44]<-MAB.rpath.params$diet[20,44]+0.015

## Relieve predation pressure on WinterSkate
## Macrobenthos: WinterSkate -0.003%, Macrobenthos +0.003%
MAB.rpath.params$diet[48,21]<-MAB.rpath.params$diet[48,21]-0.000037
MAB.rpath.params$diet[20,21]<-MAB.rpath.params$diet[20,21]+0.000037

## Relieve predation pressure on RedHake
## Macrobenthos: RedHake -0.004%, Macrobenthos +0.004%
MAB.rpath.params$diet[33,21]<-MAB.rpath.params$diet[33,21]-0.000042
MAB.rpath.params$diet[20,21]<-MAB.rpath.params$diet[20,21]+0.000042

## OtherDemersals: RedHake -1.5%, Macrobenthos +1.5%
MAB.rpath.params$diet[33,29]<-MAB.rpath.params$diet[33,29]-0.015
MAB.rpath.params$diet[20,29]<-MAB.rpath.params$diet[20,29]+0.015

## Relieve predation pressure on SmPelagics
## SpinyDogfish: SmPelagics -10%, Macrobenthos +10%
MAB.rpath.params$diet[41,44]<-MAB.rpath.params$diet[41,44]-0.1
MAB.rpath.params$diet[20,44]<-MAB.rpath.params$diet[20,44]+0.1

## Bluefish: SmPelagics -10%, Macrobenthos +10%
MAB.rpath.params$diet[41,10]<-MAB.rpath.params$diet[41,10]-0.1
MAB.rpath.params$diet[20,10]<-MAB.rpath.params$diet[20,10]+0.1

## SilverHake: SmPelagics -10%, Macrobenthos +10%
MAB.rpath.params$diet[41,38]<-MAB.rpath.params$diet[41,38]-0.1
MAB.rpath.params$diet[20,38]<-MAB.rpath.params$diet[20,38]+0.1

## Weakfish: SmPelagics -20%, Macrobenthos +20%
MAB.rpath.params$diet[41,46]<-MAB.rpath.params$diet[41,46]-0.2
MAB.rpath.params$diet[20,46]<-MAB.rpath.params$diet[20,46]+0.2

## SummerFlounder: SmPelagics -10%, Macrobenthos +10%
MAB.rpath.params$diet[41,45]<-MAB.rpath.params$diet[41,45]-0.1
MAB.rpath.params$diet[20,45]<-MAB.rpath.params$diet[20,45]+0.1

## AtlCroaker: SmPelagics -10%, Macrobenthos +10%
MAB.rpath.params$diet[41,4]<-MAB.rpath.params$diet[41,4]-0.1
MAB.rpath.params$diet[20,4]<-MAB.rpath.params$diet[20,4]+0.1

## Cod: SmPelagics -10%, Macrobenthos +10%
MAB.rpath.params$diet[41,12]<-MAB.rpath.params$diet[41,12]-0.1
MAB.rpath.params$diet[20,12]<-MAB.rpath.params$diet[20,12]+0.1

## Relieve predation pressure on OtherShrimps
## Loligo: OtherShrimps -2%, Macrobenthos +2%
MAB.rpath.params$diet[30,20]<-MAB.rpath.params$diet[30,20]-0.02
MAB.rpath.params$diet[20,20]<-MAB.rpath.params$diet[20,20]+0.02

## Relieve predation pressure on Windowpane
## Bluefish: Windowpane -1%, Macrobenthos +1%
MAB.rpath.params$diet[46,10]<-MAB.rpath.params$diet[46,10]-0.01
MAB.rpath.params$diet[20,10]<-MAB.rpath.params$diet[20,10]+0.01

## Relieve predation pressure on Butterfish
## OtherPelagics: Butterfish -10%, Macrobenthos +10%
MAB.rpath.params$diet[10,30]<-MAB.rpath.params$diet[10,30]-0.1
MAB.rpath.params$diet[20,30]<-MAB.rpath.params$diet[20,30]+0.1

## Loligo: Butterfish -1.5%, Macrobenthos +1.5%
MAB.rpath.params$diet[10,20]<-MAB.rpath.params$diet[10,20]-0.015
MAB.rpath.params$diet[20,20]<-MAB.rpath.params$diet[20,20]+0.015

## Relieve predation pressure on Weakfish
## SW added
## SouthernDemersals: Weakfish -4%, Macrobenthos +4%
MAB.rpath.params$diet[45,43]<-MAB.rpath.params$diet[45,43]-0.04
MAB.rpath.params$diet[20,43]<-MAB.rpath.params$diet[20,43]+0.04

MAB.rpath<-rpath(MAB.rpath.params,eco.name='Mid-Atlantic Bight')
MAB.rpath
source("MAB_prebal.R")
EE<-MAB.rpath$EE
EE[order(EE)]
write.Rpath(MAB.rpath,morts=T,file="MAB.rpath_morts.csv")
#write.Rpath(MAB.rpath, file="MAB_model.csv")
write.csv(MAB.rpath.params$diet, file = "MAB_diet.csv")

#look at landings
landings<-as.data.frame(MAB.rpath[["Landings"]])
landings<-landings %>% mutate(total=rowSums(landings))
landings <- landings %>% mutate(prop_rec = Recreational/total)

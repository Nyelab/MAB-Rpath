
## Script name: MAB_RPath_no_disc.R
##
## Purpose of script: Compile all data to create functional RPath model.
##                    Balance model. Eliminate discards for purposes of ENA analyses.
##
## Author: Brandon Beltz, updated by Sarah J. Weisberg
##
##
## Email: sarah.j.weisberg@stonybrook.edu
#

# Wed Apr 17 10:59:06 2024 ------------------------------

## Load libraries, packages and functions
remotes::install_github('NOAA-EDAB/Rpath')
library(Rpath); library(data.table); library(dplyr); library(here)

# Model Setup -------------------------------------------------------------

## Add functional groups to model and generate rpath params
source(here("MAB_fleets.R"))
#remove Discards from groups list
groups_fleets <- groups_fleets %>% filter(RPATH != "Discards")
groups<-as.vector(groups_fleets$RPATH)
types<-c(rep(0,31),1,rep(0,19),rep(2,1),rep(3,12))
MAB.rpath.params<-create.rpath.params(group = groups, type = types)

## Add biomass estimates
source(here("MAB_biomass_estimates.R"))
biomass<-left_join(groups_fleets,MAB.biomass.80s,by = "RPATH")

#Remove Discards 
biomass<-biomass %>% filter(RPATH !="Discards")
#and set Detritus biomass to NA
biomass[which(RPATH == "Detritus")]$Biomass<-NA
#Convert biomass to vector
biomass<-as.vector(biomass$Biomass)
MAB.rpath.params$model[,Biomass:=biomass]

## Add biological parameters
source(here("MAB_params.R"))
params<-left_join(groups_fleets,params,by = "RPATH")
## Add PB parameters
pb<-as.vector(params$PB)
MAB.rpath.params$model[,PB:=pb]

## Add QB parameters
qb<-as.vector(params$QB)
MAB.rpath.params$model[,QB:=qb]

## Add biomass accumulation
ba<-as.vector(params$BA)
ba[is.na(ba)]<-0
#manually add ba term for cod
ba[11]<-(-0.05)
ba[53:64]<-NA 
MAB.rpath.params$model[,BioAcc:=ba]

# Add unassimilated consumption
MAB.rpath.params$model[, Unassim := c(rep(0.2,52),rep(NA,12))]
#Increase unassim to 0.4 for zooplankton
MAB.rpath.params$model[Group %in% c('Microzooplankton', 'SmCopepods', 'LgCopepods'), Unassim := 0.4]

#Increase unassim to 0.3 for other detritavores
MAB.rpath.params$model[Group %in% c('AmLobster', 'Macrobenthos', 'Megabenthos', 
                                 'AtlScallop', 'OtherShrimps'), Unassim := 0.3]

## Add detrital fate and set discards to 0
MAB.rpath.params$model[,Detritus:=c(rep(1,51),rep(0,13))]
#MAB.rpath.params$model[,Discards:=c(rep(0,51),rep(1,11))]

## Add landings by gear type
source("MAB_discards.R")

## Fixed Gear
fixed<-left_join(groups_fleets,fixed,by="RPATH")
fixed<-as.vector(fixed$landings)
fixed[52]<-0
MAB.rpath.params$model[,"Fixed Gear":=fixed]

## Large Mesh
lg_mesh<-left_join(groups_fleets,lg_mesh,by="RPATH")
lg_mesh<-as.vector(lg_mesh$landings)
lg_mesh[52]<-0
MAB.rpath.params$model[, "LG Mesh" := lg_mesh]

## Other
other<-left_join(groups_fleets,other,by="RPATH")
other<-as.vector(other$landings)
other[52]<-0
MAB.rpath.params$model[, "Other" := other]

## Small Mesh
sm_mesh<-left_join(groups_fleets,sm_mesh,by="RPATH")
sm_mesh<-as.vector(sm_mesh$landings)
sm_mesh[52]<-0
MAB.rpath.params$model[, "SM Mesh" := sm_mesh]

## Scallop Dredge
scallop<-left_join(groups_fleets,scallop,by="RPATH")
scallop<-as.vector(scallop$landings)
scallop[52]<-0
MAB.rpath.params$model[, "Scallop Dredge" := scallop]

## Trap
trap<-left_join(groups_fleets,trap,by="RPATH")
trap<-as.vector(trap$landings)
trap[52]<-0
MAB.rpath.params$model[, "Trap" := trap]

## HMS Fleet
hms<-left_join(groups_fleets,hms,by="RPATH")
hms<-as.vector(hms$landings)
hms[52]<-0
MAB.rpath.params$model[, "HMS Fleet" := hms]

## Pelagic
pelagic<-left_join(groups_fleets,pelagic,by="RPATH")
pelagic<-as.vector(pelagic$landings)
pelagic[52]<-0
MAB.rpath.params$model[, "Pelagic" := pelagic]

## Other Dredge
other_dredge<-left_join(groups_fleets,other_dredge,by="RPATH")
other_dredge<-as.vector(other_dredge$landings)
other_dredge[52]<-0
MAB.rpath.params$model[, "Other Dredge" := other_dredge]

## Clam
clam<-left_join(groups_fleets,clam,by="RPATH")
clam<-as.vector(clam$landings)
clam[52]<-0
MAB.rpath.params$model[, "Clam Dredge" := clam]

## Recreational
source("MAB_rec_catch.R")
rec_catch<-left_join(groups_fleets,MAB.mrip_summary,by="RPATH")
rec_catch<-as.vector(rec_catch$Per_Area)
rec_catch[is.na(rec_catch)]<-0
rec_catch[52]<-0
rec_catch[53:64]<-NA
MAB.rpath.params$model[,"Recreational":=rec_catch]
#MAB.rpath.params[["model"]][["Recreational"]][51:61] <-NA

#Manually add menhaden catch (SW)
purse_catch<-c(rep(0,50),0.35,0,rep(NA,12))
MAB.rpath.params$model[,"PurseSeine":=purse_catch]

## Run diet matrix
source(here("MAB_diet.R"))

## Fill Rpath parameter file with diet
source(here("MAB_diet_fill.R"))

#adjust copepod groups
source(here("Sarah_R/redo_copes_start.R"))

## Run model
MAB.rpath<-rpath(MAB.rpath.params,eco.name='Mid-Atlantic Bight')
MAB.rpath

# Balancing changes -------------------
# Biomass changes ---------------------------------------------------------

## OtherPelagics
## Increase biomass by 200x (Lucey, Link, Buccheister et al.)
#MAB.rpath.params$model$Biomass[29]<-MAB.rpath.params$model$Biomass[29]*200
#OR set EE to 0.85 like Sean
MAB.rpath.params$model$Biomass[29]<-NA
MAB.rpath.params$model$EE[29]<-0.85

## OtherCephalopods
## Increase biomass by 1000x (Prebal diagnostics)
#MAB.rpath.params$model$Biomass[27]<-MAB.rpath.params$model$Biomass[27]*1000
#OR set EE to 0.85 like Sean
MAB.rpath.params$model$Biomass[27]<-NA
MAB.rpath.params$model$EE[27]<-0.85

## SpinyDogfish
## Decrease biomass by 0.7x (Lucey, Buccheister et al.)
MAB.rpath.params$model$Biomass[43]<-MAB.rpath.params$model$Biomass[43]*0.7

## OtherDemersals
## Increase biomass by 200x
#MAB.rpath.params$model$Biomass[28]<-MAB.rpath.params$model$Biomass[28]*200
#OR set EE to 0.9 like Sean
MAB.rpath.params$model$Biomass[28]<-NA
MAB.rpath.params$model$EE[28]<-0.9

## Sharks
## Increase biomass by 3.15x
MAB.rpath.params$model$Biomass[36]<-MAB.rpath.params$model$Biomass[36]*3.15

## BlackSeaBass
## Increase biomass by 7.5x 
MAB.rpath.params$model$Biomass[8]<-MAB.rpath.params$model$Biomass[8]*7.5

## OtherShrimps
## Increase biomass by 220x (Buccheister et al., EMAX; Prebal diagnostics)
#MAB.rpath.params$model$Biomass[30]<-MAB.rpath.params$model$Biomass[30]*220
#OR set EE to 0.85 like Sean
MAB.rpath.params$model$Biomass[30]<-NA
MAB.rpath.params$model$EE[30]<-0.85

## Bluefish
## Increase biomass by 4x
MAB.rpath.params$model$Biomass[9]<-MAB.rpath.params$model$Biomass[9]*4

## SmFlatfishes
## Increase biomass by 25
#MAB.rpath.params$model$Biomass[39]<-MAB.rpath.params$model$Biomass[39]*25
#OR set EE to 0.85 like Sean
MAB.rpath.params$model$Biomass[39]<-NA
MAB.rpath.params$model$EE[39]<-0.85

## AtlMackerel
## Increase biomass by 16x (Lucey, Buccheister et al.)
MAB.rpath.params$model$Biomass[4]<-MAB.rpath.params$model$Biomass[4]*16

## GelZooplankton
## Increase biomass by 2x (Prebal diagnostics)
MAB.rpath.params$model$Biomass[13]<-MAB.rpath.params$model$Biomass[13]*2

## SummerFlounder
## Increase biomass by 5x (Lucey, Buccheister et al.)
MAB.rpath.params$model$Biomass[44]<-MAB.rpath.params$model$Biomass[44]*5

## HMS
## Decrease biomass by 0.75x
MAB.rpath.params$model$Biomass[15]<-MAB.rpath.params$model$Biomass[15]*.75

## OceanPout
## Increase biomass by 5x 
MAB.rpath.params$model$Biomass[25]<-MAB.rpath.params$model$Biomass[25]*5

## WinterSkate
## Increase biomass by 3x 
MAB.rpath.params$model$Biomass[48]<-MAB.rpath.params$model$Biomass[48]*3

## Windowpane
## Increase biomass by 3x
MAB.rpath.params$model$Biomass[46]<-MAB.rpath.params$model$Biomass[46]*3

## SmPelagics
## Increase biomass by 18x (Buchheister)
#MAB.rpath.params$model$Biomass[41]<-MAB.rpath.params$model$Biomass[41]*18
#OR set EE to 0.85 like Sean
MAB.rpath.params$model$Biomass[41]<-NA
MAB.rpath.params$model$EE[41]<-0.85

## LittleSkate
## Increase biomass by 2x (Lucey)
MAB.rpath.params$model$Biomass[18]<-MAB.rpath.params$model$Biomass[18]*2

## Scup
## Increase biomass by 5x (Lucey)
MAB.rpath.params$model$Biomass[34]<-MAB.rpath.params$model$Biomass[34]*4

## WinterFlounder
## Increase biomass by 2x (Lucey)
MAB.rpath.params$model$Biomass[47]<-MAB.rpath.params$model$Biomass[47]*2

## Mesopelagics
## Increase biomass by 2.5x (Prebal diagnostics)
#MAB.rpath.params$model$Biomass[22]<-MAB.rpath.params$model$Biomass[22]*2.5
#OR set EE to 0.85 like Sean
MAB.rpath.params$model$Biomass[22]<-NA
MAB.rpath.params$model$EE[22]<-0.85

## AtlScallop
## Increase biomass by 3.5x (Lucey)
MAB.rpath.params$model$Biomass[5]<-MAB.rpath.params$model$Biomass[5]*3.5

## OtherSkates
## Increase biomass by 1.7x
#MAB.rpath.params$model$Biomass[31]<-MAB.rpath.params$model$Biomass[31]*1.7
#Or set EE to 0.85 like Sean
MAB.rpath.params$model$Biomass[31]<-NA
MAB.rpath.params$model$EE[31]<-0.85

## Odontocetes
## Increase biomass by 1.5x (Lucey)
MAB.rpath.params$model$Biomass[26]<-MAB.rpath.params$model$Biomass[26]*1.5

## SilverHake
## Increase biomass by 2x (Lucey)
MAB.rpath.params$model$Biomass[37]<-MAB.rpath.params$model$Biomass[37]*2

## Goosefish
## Increase biomass by 2.5x (Okey)
MAB.rpath.params$model$Biomass[14]<-MAB.rpath.params$model$Biomass[14]*2.5

## RiverHerring
## Increase biomass by 4x 
MAB.rpath.params$model$Biomass[2]<-MAB.rpath.params$model$Biomass[2]*4

## Fourspot
## Increase biomass by 1.5x (Lucey)
MAB.rpath.params$model$Biomass[12]<-MAB.rpath.params$model$Biomass[12]*1.5

##Weakfish
##Increase biomass by 1.5x (Assessment)
MAB.rpath.params$model$Biomass[45]<-MAB.rpath.params$model$Biomass[45]*1.5

#Decrease Macrobenthos biomass
MAB.rpath.params$model[Group=="Macrobenthos",Biomass := 45]

#Increase LgCopepods biomass
MAB.rpath.params$model[Group=="LgCopepods",Biomass := Biomass*1.3]

#Increase Micronekton biomass slightly
MAB.rpath.params$model[Group=="Micronekton",Biomass := Biomass*1.2]

#Increase YTFlounder biomass 
MAB.rpath.params$model[Group=="YTFlounder",Biomass := Biomass*2]

# PB changes --------------------------------------------------------------
## Bluefish
MAB.rpath.params$model[Group == "Bluefish",PB := 0.6]

## AtlMackerel
## Decrease PB by 4x
MAB.rpath.params$model[Group=="AtlMackerel",PB := PB/4]

## GelZooplankton
## Increase PB to 35 (Lucey)
MAB.rpath.params$model[Group == "GelZooplankton",PB := 35]

## Megabenthos
## Decrease PB by 2x (Lucey, Link et al.)
MAB.rpath.params$model[Group=="Megabenthos",PB := 2.5]


## SilverHake
## Decrease PB to 0.6
MAB.rpath.params$model[Group=="SilverHake",PB := 0.6]

## SmoothDogfish
## Decrease PB to 0.5
MAB.rpath.params$model[Group=="SmoothDogfish",PB:= 0.5]

## WinterFlounder
## Increase PB to 0.57
MAB.rpath.params$model[Group=="WinterFlounder",PB:= 0.57]

## WinterSkate
## Decrease PB by 0.6x
MAB.rpath.params$model[Group=="WinterSkate", PB := PB*0.6]

#SW changes from here
#decrease LgCopepods PB so Resp > 0
MAB.rpath.params$model[Group=="LgCopepods", PB := 61]

#Changes below based on longevity estimates
MAB.rpath.params$model[Group=="Cod", PB:= 0.4]
MAB.rpath.params$model[Group=="OtherSkates", PB := 0.55]
MAB.rpath.params$model[Group=="BlackSeaBass", PB := 0.5]
MAB.rpath.params$model[Group=="OceanPout", PB := 0.57]
MAB.rpath.params$model[Group=="YTFlounder", PB := 0.6]
MAB.rpath.params$model[Group=="SummerFlounder", PB := 0.7]
MAB.rpath.params$model[Group=="OtherDemersals", PB := 0.65]
MAB.rpath.params$model[Group=="SouthernDemersals", PB := 1.14]
MAB.rpath.params$model[Group=="SmFlatfishes",PB := 1.64]
MAB.rpath.params$model[Group=="Sharks",PB := 0.16]
MAB.rpath.params$model[Group=="LittleSkate",PB := 0.5]
MAB.rpath.params$model[Group=="RedHake",PB := 0.45]

#Changes below are to make invert groups more realistic
MAB.rpath.params$model[Group=="OtherShrimps", PB := 2]
MAB.rpath.params$model[Group=="Illex", PB := 3]
MAB.rpath.params$model[Group=="Loligo", PB := 3]
MAB.rpath.params$model[Group=="OtherCephalopods", PB := 3]
MAB.rpath.params$model[Group=="Macrobenthos", PB := 2.5]
# QB changes --------------------------------------------------------------
## HMS
## Decrease QB by 3x
MAB.rpath.params$model[Group=="HMS", QB :=QB/3]

## Seabirds
## Decrease QB by 2x
MAB.rpath.params$model[Group == "SeaBirds", QB:= QB/2]

## Odontocetes
## Decrease QB by 2x (Lucey)
MAB.rpath.params$model[Group=="Odontocetes",QB := QB/2]

## RedHake
## Decrease QB by 4x (Lucey)
MAB.rpath.params$model$QB[33]<-MAB.rpath.params$model$QB[33]/4

#Changes below made to keep GEs between 0.3 and 0.1 for fishes
#SW
MAB.rpath.params$model[Group=="SmPelagics",QB:=5.39]
MAB.rpath.params$model[Group=="WinterFlounder",QB := 2.024]
MAB.rpath.params$model[Group=="BlackSeaBass",QB := 1.66]
MAB.rpath.params$model[Group=="Scup",QB := 1.66]
MAB.rpath.params$model[Group=="Fourspot",QB := 1.61]
MAB.rpath.params$model[Group=="AtlMenhaden",QB:=5.39]
MAB.rpath.params$model[Group=="SpinyDogfish",QB:=1.16]
MAB.rpath.params$model[Group=="SmoothDogfish",QB:=2.44]
MAB.rpath.params$model[Group=="OceanPout", QB:=2]
MAB.rpath.params$model[Group=="LittleSkate",QB:=1.4]
MAB.rpath.params$model[Group=="Bluefish",QB:=3.5]
MAB.rpath.params$model[Group=="Goosefish",QB:=1.22]
MAB.rpath.params$model[Group=="RedHake",QB:=0.94]
MAB.rpath.params$model[Group=="OtherPelagics",QB:=2]
MAB.rpath.params$model[Group=="OtherSkates",QB:=1.1]
MAB.rpath.params$model[Group=="Cod",QB:=1.2]

#Adjusting Micronekton QB - not sure why it was so high in EMAX
MAB.rpath.params$model[Group=="Micronekton",QB:=QB/2]

#MAB.rpath<-rpath(MAB.rpath.params,eco.name='Mid-Atlantic Bight')
##GE

# Fishing Changes ---------------------------------------------------------
# 
# ## Fishing changes
# ## Sharks
# ## Reduce trap fishing
MAB.rpath.params$model$Trap[36]<-MAB.rpath.params$model$Trap[36]*0
# ## Reduce rec fishing
MAB.rpath.params$model$Recreational[36]<-MAB.rpath.params$model$Recreational[36]*0.85

# ## OtherPelagics
# ## Reduce trap fishing
MAB.rpath.params$model$Trap[29]<-MAB.rpath.params$model$Trap[29]*0
# ## SouthernDemersals
# ## Reduce trap fishing
MAB.rpath.params$model$Trap[42]<-MAB.rpath.params$model$Trap[42]*0

# ## OtherDemersals
# ## Reduce trap fishing
MAB.rpath.params$model$Trap[28]<-MAB.rpath.params$model$Trap[28]*.01

# ## Mesopelagics
# ## Reduce trap fishing
MAB.rpath.params$model$Trap[22]<-MAB.rpath.params$model$Trap[22]*.01

## OceanPout
## Reduce trap fishing
MAB.rpath.params$model$Trap[25]<-MAB.rpath.params$model$Trap[25]*.01
# Diet changes ------------------------------------------------------------
## Relieve predation pressure on OtherCephalopods
## SummerFlounder: OtherCephalopods -10%, Loligo +10%
MAB.rpath.params$diet[27,45]<-MAB.rpath.params$diet[27,45]-0.10
MAB.rpath.params$diet[19,45]<-MAB.rpath.params$diet[19,45]+0.10

## SpinyDogfish: OtherCephalopods -3%, Loligo +3%
MAB.rpath.params$diet[27,44]<-MAB.rpath.params$diet[27,44]-0.03
MAB.rpath.params$diet[19,44]<-MAB.rpath.params$diet[19,44]+0.03

## SilverHake: OtherCephalopods -4%, Loligo +4%
MAB.rpath.params$diet[27,38]<-MAB.rpath.params$diet[27,38]-0.04
MAB.rpath.params$diet[19,38]<-MAB.rpath.params$diet[19,38]+0.04

## Bluefish: OtherCephalopods -5%, Loligo +5%
MAB.rpath.params$diet[27,10]<-MAB.rpath.params$diet[27,10]-0.05
MAB.rpath.params$diet[19,10]<-MAB.rpath.params$diet[19,10]+0.05

## Butterfish: OtherCephalopods -.5%, Loligo +.5%
MAB.rpath.params$diet[27,11]<-MAB.rpath.params$diet[27,11]-0.005
MAB.rpath.params$diet[19,11]<-MAB.rpath.params$diet[19,11]+0.005

## Fourspot: OtherCephalopods -18%, Loligo +18%
MAB.rpath.params$diet[27,13]<-MAB.rpath.params$diet[27,13]-0.18
MAB.rpath.params$diet[19,13]<-MAB.rpath.params$diet[19,13]+0.18

## RedHake: OtherCephalopods -4%, Loligo +4%
MAB.rpath.params$diet[27,34]<-MAB.rpath.params$diet[27,34]-0.04
MAB.rpath.params$diet[19,34]<-MAB.rpath.params$diet[19,34]+0.04

## Goosefish: OtherCephalopods -4%, Loligo +4%
MAB.rpath.params$diet[Group=="OtherCephalopods",Goosefish := Goosefish - 0.04]
MAB.rpath.params$diet[Group=="Loligo",Goosefish := Goosefish + 0.04]

## Scip: OtherCephalopods -0.5%, Loligo +0.5%
MAB.rpath.params$diet[Group=="OtherCephalopods",Scup := Scup - 0.005]
MAB.rpath.params$diet[Group=="Loligo",Scup := Scup + 0.005]

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

## OtherPelagics: AtlMackerel -3%, SmPelagics +3% 
MAB.rpath.params$diet[Group=="AtlMackerel", OtherPelagics := OtherPelagics - 0.03]
MAB.rpath.params$diet[Group=="SmPelagics", OtherPelagics := OtherPelagics + 0.03]

## Bluefish: AtlMackerel -0.05%, SmPelagics +0.05% 
MAB.rpath.params$diet[Group=="AtlMackerel", Bluefish := Bluefish - 0.005]
MAB.rpath.params$diet[Group=="SmPelagics", Bluefish := Bluefish + 0.005]

## Relieve predation pressure on SmFlatfishes
## SpinyDogfish: SmFlatfishes -2.2%, Macrobenthos +2.2%
# SW changed, previously had been leading to negative diet 
MAB.rpath.params$diet[39,44]<-MAB.rpath.params$diet[39,44]-0.023
MAB.rpath.params$diet[20,44]<-MAB.rpath.params$diet[20,44]+0.023

## Goosefish: SmFlatfishes -1.2%, Macrobenthos +1.2%
MAB.rpath.params$diet[39,15]<-MAB.rpath.params$diet[39,15]-0.012-0.001
MAB.rpath.params$diet[20,15]<-MAB.rpath.params$diet[20,15]+0.012+0.001

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

## Goosefish: OtherDemersals -5%, Goosefish +5%
#SW added
MAB.rpath.params$diet[Group=="OtherDemersals",Goosefish := Goosefish - 0.05]
MAB.rpath.params$diet[Group=="Goosefish",Goosefish := Goosefish + 0.05]

## Goosefish: OtherDemersals -2.5%, SmPelagics +2.5%
#SW added
MAB.rpath.params$diet[Group=="OtherDemersals",Goosefish := Goosefish - 0.025]
MAB.rpath.params$diet[Group=="SmPelagics",Goosefish := Goosefish + 0.025]

## SpinyDogfish: OtherDemersals -3%, Macrobenthos +3%
#SW added
MAB.rpath.params$diet[Group=="OtherDemersals",SpinyDogfish := SpinyDogfish - 0.03]
MAB.rpath.params$diet[Group=="Macrobenthos",SpinyDogfish := SpinyDogfish + 0.03]

## Bluefish: OtherDemersals -1.5%, AtlMenhaden +1.5%
#SW added
MAB.rpath.params$diet[Group=="OtherDemersals",Bluefish := Bluefish - 0.015]
MAB.rpath.params$diet[Group=="AtlMenhaden",Bluefish := Bluefish + 0.015]

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
# MAB.rpath.params$diet[48,21]<-MAB.rpath.params$diet[48,21]+0.000037
# MAB.rpath.params$diet[20,21]<-MAB.rpath.params$diet[20,21]-0.000037

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

## Loligo: Butterfish -1.5%, Macrobenthos +1.5%
MAB.rpath.params$diet[10,20]<-MAB.rpath.params$diet[10,20]-0.015
MAB.rpath.params$diet[20,20]<-MAB.rpath.params$diet[20,20]+0.015

## Relieve predation pressure on Weakfish
## SW added
## SouthernDemersals: Weakfish -4%, Macrobenthos +4%
MAB.rpath.params$diet[45,43]<-MAB.rpath.params$diet[45,43]-0.04
MAB.rpath.params$diet[20,43]<-MAB.rpath.params$diet[20,43]+0.04

## Adjust predation on AtlMenhaden 
## Want to better reflect relative biomass of AtlMenhaden, SmPelagics
## SW added
#HMS: AtlMenhaden -50%, SmPelagics +50%
MAB.rpath.params$diet[Group=="AtlMenhaden",HMS := HMS - 0.5]
MAB.rpath.params$diet[Group=="SmPelagics",HMS := HMS + 0.5]

#SeaBirds: AtlMenhaden -14%, SmPelagics +14%
MAB.rpath.params$diet[Group=="AtlMenhaden",SeaBirds := SeaBirds - 0.14]
MAB.rpath.params$diet[Group=="SmPelagics",SeaBirds := SeaBirds + 0.14]

#BaleenWhales: AtlMenhaden -6%, SmPelagics +6%
MAB.rpath.params$diet[Group=="AtlMenhaden",BaleenWhales := BaleenWhales - 0.06]
MAB.rpath.params$diet[Group=="SmPelagics",BaleenWhales := BaleenWhales + 0.06]

#OtherPelagics: AtlMenhaden -40%, SmPelagics +40%
MAB.rpath.params$diet[Group=="AtlMenhaden",OtherPelagics := OtherPelagics - 0.4]
MAB.rpath.params$diet[Group=="SmPelagics",OtherPelagics := OtherPelagics + 0.4]

#Odontocetes: AtlMenhaden -10%, SmPelagics +10%
MAB.rpath.params$diet[Group=="AtlMenhaden",Odontocetes := Odontocetes - 0.1]
MAB.rpath.params$diet[Group=="SmPelagics",Odontocetes := Odontocetes + 0.1]

#Relieve predation on OtherSkates
#Goosefish: OtherSkates -3%, LittleSkate +3%
MAB.rpath.params$diet[Group=="OtherSkates",Goosefish := Goosefish - 0.03]
MAB.rpath.params$diet[Group=="LittleSkate",Goosefish := Goosefish + 0.03]
#Macrobenthos: OtherSkates -0.004%, Megabenthos +0.004%
MAB.rpath.params$diet[Group=="OtherSkates",Macrobenthos := Macrobenthos-0.00004]
MAB.rpath.params$diet[Group=="Megabenthos",Macrobenthos := Macrobenthos+0.00004]

#Relieve predation on OceanPout
#Bluefish: OceanPout - 2.5%, AtlMenhaden +2.5%
MAB.rpath.params$diet[Group=="OceanPout",Bluefish := Bluefish - 0.025]
MAB.rpath.params$diet[Group=="AtlMenhaden",Bluefish := Bluefish + 0.025]

#SpinyDogfish: OceanPout - 0.4%, SmPelagics +0.4%
MAB.rpath.params$diet[Group=="OceanPout",SpinyDogfish := SpinyDogfish - 0.004]
MAB.rpath.params$diet[Group=="SmPelagics",SpinyDogfish := SpinyDogfish + 0.004]
# Check for balance -------------------------------------------------------

#Load Sean's prebal functions
source(url("https://github.com/NOAA-EDAB/GBRpath/blob/master/R/PreBal.R?raw=true"))
#add data pedigree
#source("Sarah_R/data_pedigree.R")

check.rpath.params(MAB.rpath.params)

MAB.rpath<-rpath(MAB.rpath.params,eco.name='Mid-Atlantic Bight')
check.ee(MAB.rpath)

webplot(MAB.rpath,labels = T)
#Save files
save(MAB.rpath, file = "outputs/MAB_Rpath_no_disc.RData")
save(MAB.rpath.params,file = "outputs/MAB_params_Rpath_no_disc.RData")

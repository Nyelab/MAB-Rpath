## ---------------------------
## Script name: MAB_biomass_estimates.R
##
## Purpose of script: Calculate biomass estimates for single-species and 
##                    multi-species functional groups using survey data and 
##                    EMAX estimates, respectively.
##
## Author: Brandon Beltz
##
## Date Created: 2021-06-15
##
## Email: brandon.beltz@stonybrook.edu
## ---------------------------
##
## Notes: This version is specific to the MABRpath model.
##
## ---------------------------

## Load libraries, packages and functions

library(data.table)
library(here)
#remotes::install_github('NOAA-EDAB/survdat')
library(survdat)
'%notin%' <-Negate('%in%')

## Load Survdat, species list and strata
load(here("data/Survdat.RData"))
load(here("data/Species_codes.RData"))

#Calculate total MAB area
area<-sf::st_read(dsn=system.file("extdata","strata.shp",package="survdat"))
area<-get_area(areaPolygon = area, areaDescription="STRATA")
MAB.area<-subset(area, area$STRATUM %in% MAB.strata)
MAB.area<-sum(MAB.area$Area)
rm(area)

# strata<-readOGR('data/strata','strata')
# 
# ## Generate area table
# strat.area<-getarea(strata, 'STRATA')
# setnames(strat.area,'STRATA','STRATUM')

## Load basic inputs
source(here('MAB_basic_inputs.R'))

## Aggregate low biomass species
spp <- spp[!duplicated(spp$SVSPP),]
spp <- spp[RPATH == 'AtlHerring', RPATH := 'SmPelagics']
spp <- spp[RPATH == 'Clams', RPATH := 'Megabenthos']
spp <- spp[RPATH == 'Haddock', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'LargePelagics', RPATH := 'OtherPelagics']
spp <- spp[RPATH == 'OffHake', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'OtherFlatfish', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'Pollock', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'Rays', RPATH := 'OtherSkates']
spp <- spp[RPATH == 'RedCrab', RPATH := 'Megabenthos']
spp <- spp[RPATH == 'RiverHerring', RPATH := 'SmPelagics']
spp <- spp[RPATH == 'Tilefish', RPATH := 'SouthernDemersals']
spp <- spp[RPATH == 'WitchFlounder', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'WhiteHake', RPATH := 'OtherDemersals']
#spp <- spp[SCINAME == 'BREVOORTIA', RPATH := 'AtlMenhaden']

#Calculate swept area biomass
#Fall season only
swept<-calc_swept_area(surveyData=survdat, areaPolygon = 'NEFSC strata', areaDescription = 'STRATA', 
                       filterByArea = MAB.strata, filterBySeason= "FALL", 
                       groupDescription = "SVSPP", filterByGroup = "all", mergesexFlag = T,tidy = F, q = NULL, a = 0.0384)

# ## Subset by Fall and MAB strata
# MAB.fall<-survdat[SEASON == 'FALL' & STRATUM %in% MAB.strata,]
# 
# ## Run stratification prep
# MAB.prep<-stratprep(MAB.fall,strat.area,strat.col = 'STRATUM',area.col = 'Area')

## Merge with RPATH names
spp <- spp[!duplicated(spp$SVSPP),]

## Calculate stratified means
#mean.biomass<-stratmean(MAB.prep,group.col = 'SVSPP',strat.col = 'STRATUM')

## Merge with RPATH names
#mean.biomass<-merge(swept,spp[,list(SVSPP,RPATH,SCINAME,Fall.q)], by = 'SVSPP')

## Calculate total biomass from swept area
#total.biomass<-sweptarea(MAB.prep, mean.biomass, strat.col = 'STRATUM', area.col = 'Area')

## Merge with RPATH names
total.biomass<-merge(swept,spp[,list(SVSPP,RPATH,SCINAME,Fall.q)], by = 'SVSPP')

## Calculate total area
#MAB.strat.area<-strat.area[STRATUM %in% MAB.strata,sum(Area)]

## Convert to b/a in mt/km^2
total.biomass <- total.biomass[, biomass.t_area :=(tot.biomass*.001)/(Fall.q*MAB.area)]

#Average for 1980-85
MAB.biomass.80s <- total.biomass[YEAR %in% 1980:1985, mean(biomass.t_area), by = RPATH]
setnames(MAB.biomass.80s,'V1','Biomass')

##Save output
save(MAB.biomass.80s, file = 'data/MAB_biomass_fall_80s.RData')

## Add EMAX groups
load("data/EMAX_params.RData")
MAB.EMAX<-as.data.table(EMAX.params)

## Merge groups with biomass estimates
MAB.groups<-merge(MAB.biomass.80s, MAB.groups, by = 'RPATH', all.y=TRUE)
setnames(MAB.groups,'V1','Biomass', skip_absent = TRUE)

## Subset EMAX model for group and biomass
setnames(MAB.EMAX,'Group','RPATH')
setnames(MAB.EMAX,'Biomass','Biomass')
MAB.EMAX<-MAB.EMAX[, c('RPATH','Biomass')]

## Match EMAX names with RPATH groups
MAB.EMAX<-MAB.EMAX[RPATH == 'Small copepods', RPATH := 'SmCopepods',]
MAB.EMAX<-MAB.EMAX[RPATH == 'Large Copepods', RPATH := 'LgCopepods',]
MAB.EMAX<-MAB.EMAX[RPATH == 'Sea Birds', RPATH := 'SeaBirds',]
MAB.EMAX<-MAB.EMAX[RPATH %like% 'Macro', RPATH:='Macrobenthos',]
MAB.EMAX<-MAB.EMAX[RPATH %like% 'Baleen', RPATH := 'BaleenWhales',]
MAB.EMAX<-MAB.EMAX[RPATH %like% 'Gel',RPATH := 'GelZooplankton',]
MAB.EMAX<-MAB.EMAX[RPATH %like% 'Phyto', RPATH :='Phytoplankton',]
MAB.EMAX<-MAB.EMAX[RPATH %like% 'Megabenthos',RPATH := 'Mega',]
MAB.EMAX<-MAB.EMAX[RPATH %like% 'Pelagics',RPATH := 'Pelagics',]
MAB.EMAX<-MAB.EMAX[RPATH %like% 'Demersals',RPATH := 'Demersals',]
MAB.EMAX<-MAB.EMAX[RPATH %like% 'Sharks', RPATH :='Sharks']

## Aggregate EMAX sharks
SharksBiomass<-MAB.EMAX[RPATH == 'Sharks', sum(Biomass),]
MAB.EMAX<-MAB.EMAX[RPATH == 'Sharks', Biomass:=SharksBiomass,]

## Aggregate EMAX macrobenthos
BenthosBiomass<-MAB.EMAX[RPATH == 'Macrobenthos', sum(Biomass),]
MAB.EMAX<-MAB.EMAX[RPATH == 'Macrobenthos', Biomass:=BenthosBiomass,]

##Assign a portion of micronekton biomass to krill
#SW added
krill_prop<-0.1612
KrillBiomass<-MAB.EMAX[RPATH == 'Micronekton', Biomass]*krill_prop
MAB.EMAX<-MAB.EMAX[RPATH == 'Micronekton', Biomass := Biomass*(1-krill_prop)]
MAB.EMAX<-rbind(MAB.EMAX,list("Krill",KrillBiomass))

## Remove unused groups
MAB.EMAX<-MAB.EMAX[RPATH %notin% c('Larval-juv fish- all','Shrimp et al.','Mega','Pelagics','Demersals','Discard','Detritus-POC','Fishery'),]
MAB.EMAX<-unique(MAB.EMAX)

## Combine survey and EMAX biomass estimates
MAB.biomass.80s<-merge(MAB.groups,MAB.EMAX,by='RPATH', all=TRUE)
MAB.biomass.80s<-MAB.biomass.80s[is.na(Biomass.y) , Biomass.y := Biomass.x]
MAB.biomass.80s<-MAB.biomass.80s[,Biomass.x :=NULL]
setnames(MAB.biomass.80s,"Biomass.y","Biomass")

##Manually add menhaden biomass - based on Chagaris et al. (2020)
##Assuming 10% of menhaden biomass is in relevant MAB area
MAB.biomass.80s<-MAB.biomass.80s[RPATH == "AtlMenhaden", Biomass :=1.775190819]

## Output to .RData
save(MAB.biomass.80s, file = 'data/MAB_biomass_estimates.RData')


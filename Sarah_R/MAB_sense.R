#Title: GOM Ecosense

# Purpose: This script uses Ecosense simplified Bayesian synthesis to
#           generate plausible versions of the MAB food web (1980-85)
#           These plausible webs incorporate parameter uncertainty, as
#           determined by data pedigree.

# DataFiles:"GOM_params_Rpath.RData"; "GOM_Rpath.RData"

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Sat Jan 27 12:29:25 2024 ------------------------------


#Required packages--------------------------------------------------------------
library(here); library(data.table); library(Rpath)

#Source code from sense_beta branch of Rpath Repo
library(devtools)
source_url('https://raw.githubusercontent.com/NOAA-EDAB/Rpath/sense_beta/R/ecosense.R')

#Load balanced model
load(here("outputs/MAB_params_Rpath.RData"))
load(here("outputs/MAB_Rpath.RData"))


#Fix PB/QB pedigree values so that GE cannot >1
GE<-MAB.rpath$GE
limits<-(1-MAB.rpath.params$pedigree[,QB])/(1+MAB.rpath.params$pedigree[, PB])
#Identify which groups violate this rule
fixers<-which(limits<GE[1:length(limits)])
#exclude Phytoplankton (19)
fixers <- fixers[-19]

#adjust QB and PB values where needed
for (i in 1:length(fixers)){
  to_fix<-fixers[i]
  GE_group<-GE[to_fix]
  QB_ped<-MAB.rpath.params$pedigree[to_fix,QB]
  PB_ped<-MAB.rpath.params$pedigree[to_fix,PB]
  limit<-(1-QB_ped)/(1+PB_ped)
  while(GE_group>limit){
    if(QB_ped >= PB_ped){
      QB_ped <- QB_ped - 0.1
    }
    else{
      PB_ped <- PB_ped - 0.1
    }
    limit<-(1-QB_ped)/(1+PB_ped)
  }
  MAB.rpath.params$pedigree[to_fix,PB := PB_ped]
  MAB.rpath.params$pedigree[to_fix,QB := QB_ped]
}

#Set up sense runs
all_years <- 1:50
scene <- rsim.scenario(MAB.rpath, MAB.rpath.params, years = all_years)
orig.biomass<-scene$start_state$Biomass

# ----- Set up ecosense generator ----- #######################################
scene$params$BURN_YEARS <- 50
NUM_RUNS <- 50000
parlist <- as.list(rep(NA, NUM_RUNS))
kept <- rep(NA, NUM_RUNS)

set.seed(19)
for (irun in 1:NUM_RUNS){
  MABsense <- copy(scene)
  # INSERT SENSE ROUTINE BELOW
  parlist[[irun]] <- MABsense$params 		# Base ecosim params
  parlist[[irun]] <- rsim.sense(MABsense, MAB.rpath.params)	# Replace the base params with Ecosense params  
  #MABsense$start_state$Biomass <- parlist[[irun]]$B_BaseRef #took out this line on May 2, 2022
  parlist[[irun]]$BURN_YEARS <- 50			# Set Burn Years to 50
  MABsense$params <- parlist[[irun]]
  MABtest <- rsim.run(MABsense, method = "RK4", years = all_years)
  failList <- which((is.na(MABtest$end_state$Biomass) | MABtest$end_state$Biomass/orig.biomass > 1000 | MABtest$end_state$Biomass/orig.biomass < 1/1000))
  {if (length(failList)>0)
  {cat(irun,": fail in year ",MABtest$crash_year,": ",failList,"\n"); kept[irun] <- F; flush.console()}
    else 
    {cat(irun,": success!\n"); kept[irun]<-T;  flush.console()}}
  parlist[[irun]]$BURN_YEARS <- 1
}

# KEPT tells you which ecosystems were kept
KEPT <- which(kept==T)
nkept <- length(KEPT)
nkept
MAB_sense <- parlist[KEPT]

save(MAB_sense, file = "outputs/MAB_sense_50k_2024.RData")


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
scene <- rsim.scenario(GOM, GOM.params, years = all_years)
orig.biomass<-scene$start_state$Biomass

# ----- Set up ecosense generator ----- #######################################
scene$params$BURN_YEARS <- 50
NUM_RUNS <- 50000
parlist <- as.list(rep(NA, NUM_RUNS))
kept <- rep(NA, NUM_RUNS)

set.seed(19)
for (irun in 1:NUM_RUNS){
  GOMsense <- copy(scene)
  # INSERT SENSE ROUTINE BELOW
  parlist[[irun]] <- GOMsense$params 		# Base ecosim params
  parlist[[irun]] <- rsim.sense(GOMsense, GOM.params)	# Replace the base params with Ecosense params  
  #GOMsense$start_state$Biomass <- parlist[[irun]]$B_BaseRef #took out this line on May 2, 2022
  parlist[[irun]]$BURN_YEARS <- 50			# Set Burn Years to 50
  GOMsense$params <- parlist[[irun]]
  GOMtest <- rsim.run(GOMsense, method = "RK4", years = all_years)
  failList <- which((is.na(GOMtest$end_state$Biomass) | GOMtest$end_state$Biomass/orig.biomass > 1000 | GOMtest$end_state$Biomass/orig.biomass < 1/1000))
  {if (length(failList)>0)
  {cat(irun,": fail in year ",GOMtest$crash_year,": ",failList,"\n"); kept[irun] <- F; flush.console()}
    else 
    {cat(irun,": success!\n"); kept[irun]<-T;  flush.console()}}
  parlist[[irun]]$BURN_YEARS <- 1
}

# KEPT tells you which ecosystems were kept
KEPT <- which(kept==T)
nkept <- length(KEPT)
nkept
GOM_sense <- parlist[KEPT]
#GOM_sense<-parlist

save(GOM_sense, file = "outputs/GOM_sense_50k_2024.RData")


## ---------------------------
## Script name: MAB_biomass_accumulation.R
##
## Purpose of script: Estimate biomass accumulation for all function groups
##                    and export based on significance.
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
## Set working directory

setwd("C:/Users/beven/OneDrive - Stony Brook University/Research/Rpath-MAB")

## Load libraries, packages and functions

library(Survdat); library(data.table); library(rgdal)

## Run MAB_biomass_estimates.R
source('MAB_biomass_estimates.R')

## Get RPATH groups from biomass estimates
MAB.RPATH<-unique(total.biomass$RPATH,na.rm=true)

## Run linear model for all functional groups
ba<-c()
p<-c()
for (i in 2:50){
  lm<-lm(biomass.t_area~YEAR, data = subset(total.biomass, RPATH == MAB.RPATH[i]))
  spF <- as.numeric(summary(lm)$fstatistic)
  p[i] <- pf(spF[1], spF[2], spF[3], lower = F)
  ba[i] <-as.numeric(lm[[1]][2])
}

## Bind results and subset by significance
MAB.bioacc<-cbind(MAB.RPATH,ba,p)
MAB.bioacc_sig<-subset(MAB.bioacc, p<=0.05 & abs(ba) >= 0.005)

## Output to .csv
save(MAB.bioacc_sig, file = 'data/MAB_biomass_accumulation.RData')
## ---------------------------
## Script name: MAB_rec_catch.R
##
## Purpose of script: Pull recreational catch for species and groups in the MAB
##                    RPath model.
##
## Author: Brandon Beltz
##
## Last updated: 17 Aug 2021
##
## Email: brandon.beltz@stonybrook.edu
## ---------------------------
##
## Notes:
##
## ---------------------------
## Set working directory

setwd("C:/Users/beven/Desktop/MAB-Rpath")

## Load libraries, packages and functions
library(here);library(tidyr);library(data.table);library(rgdal);library(Survdat); library(dplyr)

## Load MRIP species list, RPath species list and MAB groups
load("data/Species_codes.RData")
load("data/mrip_species.RData")
load("data/mrip_data.RData")
source("MAB_basic_inputs.R")

## Merge MRIP and RPath species lists by SCINAME
colnames(MAB.mrip_species)[3]<-"SCINAME"
species_key<-merge(MAB.mrip_species,spp,by = "SCINAME")
species_key<-species_key[,c(2,20)]
species_key<-merge(MAB.groups,species_key,by = "RPATH")
colnames(species_key)[2]<-"SP_CODE"

## Subset MRIP data by state and area
MAB.mrip<-subset(MAB.mrip,MAB.mrip$ST == c(9,10,24,34,36,37,44,51) | MAB.mrip$AREA_X < 4, select=c(SP_CODE,WGT_AB1))

## Merge with RPath names
MAB.mrip<-merge(MAB.mrip,species_key,by = "SP_CODE")
MAB.mrip<-MAB.mrip[,-1]

## Sum recreational catch for each RPath group
MAB.mrip<-MAB.mrip %>%
  group_by(RPATH) %>%
  summarise(WGT_AB1 = sum(WGT_AB1))

## Divide by MAB area to get kg/km^2
strata<-readOGR('data/strata','strata')
strat.area<-getarea(strata, 'STRATA')
setnames(strat.area,'STRATA','STRATUM')
MAB.strat.area<-strat.area[STRATUM %in% MAB.strata,sum(Area)]
MAB.mrip$Per_Area<-(MAB.mrip$WGT_AB1/1000)/MAB.strat.area
## ---------------------------
## Script name: MAB_fleets.R
##
## Purpose of script: Create a data table of groups and fleets for use in the
##                    final MAB Rpath model.
##                    
## Author: Brandon Beltz, updated by Sarah J. Weisberg
##
##
## Email: brandon.beltz@stonybrook.edu

# Fri Dec  8 16:55:53 2023 ------------------------------


## Load libraries, packages and functions
library(data.table)

## Load commercial landings from Sean and MAB basic inputs
load("data/mean_landings_mab_80_85.RData")
source(here("MAB_basic_inputs.R"))

## Rename HMS to HMS Fleet
mean.land[FLEET == "HMS",FLEET := "HMS Fleet"]

## Add columns for discards and detritus
dis_det_cols<-as.data.table(rbind("Detritus","Discards"))
colnames(dis_det_cols)<-"RPATH"
groups_fleets<-rbind(MAB.groups,dis_det_cols)

## Pull unique fleets
MAB.fleets<-as.data.table(unique(mean.land$FLEET))
colnames(MAB.fleets)<-"RPATH"

## Bind fleets and model functional groups
rec<-data.frame("Recreational")
names(rec)<-c("RPATH")
groups_fleets<-rbind(groups_fleets,MAB.fleets,rec)

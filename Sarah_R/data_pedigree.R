#Data pedigree assignments
#Author: Sarah J. Weisberg

#source file: MAB_data_pedigree.csv

#See Whitehouse & Aydin 2020 for more details on pedigree assignments

# Sat Jan 27 14:46:38 2024 ------------------------------


#0.2 if estimate is directly from data without modification
#0.4 if estimate is from data but data are variable or sparse
#0.5 if estimate is from stock assessment/regional analysis
#0.6 if estimate is from FishBase or other source
#0.8 if estimate was dramatically adjusted in balancing 

#Load required packages
library(data.table); library(dplyr)

#Load pedigree table
pedigree<-as.data.table(read.csv(here('data/MAB_data_pedigree.csv')))
#Remove unneeded columns
pedigree<-pedigree %>% dplyr::select(RPATH, Biomass, PB, QB, Diet)

#Remove discards
pedigree <- pedigree %>% filter(!RPATH == "Discards")

#Remove pedigree inputs for fleets
ngroups<-nliving <- nrow(MAB.rpath.params$model[Type <  3, ])
MAB.rpath.params$pedigree <- MAB.rpath.params$pedigree[1:ngroups]

#Biomass
MAB.rpath.params$pedigree[, Biomass := pedigree$Biomass]

#PB
MAB.rpath.params$pedigree[, PB := pedigree$PB]

#QB
MAB.rpath.params$pedigree[, QB := pedigree$QB]

#Diet
MAB.rpath.params$pedigree[, Diet := pedigree$Diet]


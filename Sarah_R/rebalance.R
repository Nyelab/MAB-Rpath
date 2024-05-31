#Code to rebalance 1980-85 Mid-Atlantic Bight Rpath model
#Goal is to align starting model with Gulf of Maine, Georges Bank models

#Author: Sarah J. Weisberg

# Tue Dec  5 12:54:24 2023 ------------------------------

#load packages
library(here)
library(Rpath)
library(dplyr)


#correct detritus, discards BA, should be 0
#had been NA
MAB.rpath.params[["model"]][["BioAcc"]][50:51] <- 0

#correct fishing matrix
#fleets are catching other fleets
#issue is with rec fishery
MAB.rpath.params[["model"]][["Recreational"]][52:62] <-NA

#decrease LgCopepods so Resp > 0
MAB.rpath$PB[17]<- 61

#redo copepods
source(here("Sarah_R/redo_copes_start.R"))




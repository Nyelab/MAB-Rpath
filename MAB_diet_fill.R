## ---------------------------
## Script name: MAB_diet_fill.R
##
## Purpose of script: Reformat existing diet matrix for use in RPath.
##                    
##
## Author: Brandon Beltz, updated by Sarah J. Weisberg
##
## Email: brandon.beltz@stonybrook.edu
## ---------------------------
##
## Notes: Modified from Sarah Weisberg's GOM RPath model
##
## ---------------------------

# Fri Dec  8 17:06:25 2023 ------------------------------

## Load libraries, packages and functions
library(readr);library(here)

## Load original diet matrix
load("data/MAB_diet.RData")

for (i in 1:length(MAB.diet$Rpred)){
  temp_group<-MAB.diet$Rprey[i]
  MAB.rpath.params$diet[Group == temp_group,(MAB.diet$Rpred[i]):= MAB.diet$preyper[i]]
}

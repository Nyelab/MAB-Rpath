## ---------------------------
## Script name: MAB_diet_fill.R
##
## Purpose of script: Reformat existing diet matrix for use in RPath.
##                    
##
## Author: Brandon Beltz
##
## Last updated: 02 Sep 2021
##
## Email: brandon.beltz@stonybrook.edu
## ---------------------------
##
## Notes: Modified from Sarah Weisberg's GOM RPath model
##
## ---------------------------
## Set working directory

setwd("C:/Users/beven/Desktop/MAB-Rpath")

## Load libraries, packages and functions
library(readr);library(here)

## Load original diet matrix
load("data/MAB_diet.RData")

for (i in 1:length(MAB.diet$Rpred)){
  temp_group<-MAB.diet$Rprey[i]
  MAB.rpath.params$diet[Group == temp_group,(MAB.diet$Rpred[i]):= MAB.diet$preyper[i]]
}
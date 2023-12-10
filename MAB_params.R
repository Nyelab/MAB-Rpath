## ---------------------------
## Script name: MAB_params.R
##
## Purpose of script: Reformat parameters for compatibility with final RPath
##                    model.
##                    
##
## Author: Brandon Beltz, updated by Sarah J. Weisberg
##
## Email: brandon.beltz@stonybrook.edu

# Fri Dec  8 16:59:07 2023 ------------------------------

## Load libraries, packages and functions
library(here);library(data.table);library(dplyr);library(tidyverse)

## Load basic inputs and initial parameter set
load("data/MAB_params.RData")
source(here('MAB_basic_inputs.R'))

## Remove unnecessary annotations from MAB_params.RData
params<-MAB.Params[,c("RPATH","PB","QB")]

## Convert PB and QB values into vectors
MAB.PB<-params[,"PB"]
MAB.QB<-params[,"QB"]

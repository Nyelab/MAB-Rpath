## ---------------------------
## Script name: MAB_params.R
##
## Purpose of script: Reformat parameters for compatibility with final RPath
##                    model.
##                    
##
## Author: Brandon Beltz
##
## Date Created: 19 Aug 2021
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
library(here);library(data.table);library(dplyr);library(tidyverse)

## Load basic inputs and initial parameter set
load("data/MAB_params.RData")
source('MAB_basic_inputs.R')

## Remove unnecessary annotations from MAB_params.RData
params<-MAB.Params[,c("RPATH","PB","QB")]

## Convert PB and QB values into vectors
MAB.PB<-params[,"PB"]
MAB.QB<-params[,"QB"]
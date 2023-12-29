## ---------------------------
## Script name: MAB_rec_catch.R
##
## Purpose of script: Pull recreational catch for species and groups in the MAB
##                    RPath model.
##
## Author: Brandon Beltz, updated by Sarah J. Weisberg
##
## Email: brandon.beltz@stonybrook.edu
## ---------------------------

# Fri Dec 29 15:16:51 2023 ------------------------------


## Load libraries, packages and functions
library(here);library(tidyr);library(data.table);library(survdat); library(dplyr)

## Load MRIP species list, RPath species list and MAB groups
load(here("data/Species_codes.RData"))
load(here("data/mrip_species.RData"))
load(here("data/mrip_data.RData"))
source(here("MAB_basic_inputs.R"))

## Merge MRIP and RPath species lists by SCINAME
colnames(MAB.mrip_species)[3]<-"SCINAME"
species_key<-merge(MAB.mrip_species,spp,by = "SCINAME")
species_key<-species_key[,c(2,20)]
species_key<-merge(MAB.groups,species_key,by = "RPATH")
colnames(species_key)[2]<-"SP_CODE"

## Subset MRIP data by state and area
#MAB.mrip<-subset(MAB.mrip,MAB.mrip$ST == c(9,10,24,34,36,37,44,51) | MAB.mrip$AREA_X < 4, select=c(SP_CODE,WGT_AB1))

MAB.mrip_filter<- MAB.mrip %>% filter(ST %in% c(9,10,24,34,36,37,44,51) | MAB.mrip$AREA_X < 4) %>%
  select(SP_CODE,WGT_AB1,YEAR)

#filter for years 1980-1985
#SW addition
MAB.mrip_filter <-MAB.mrip_filter %>% filter(YEAR <= 1985)


## Merge with RPath names
MAB.mrip_filter<-merge(MAB.mrip_filter,species_key,by = "SP_CODE")
MAB.mrip_filter<-MAB.mrip_filter[,-1]

## Sum recreational catch for each RPath group by year
MAB.mrip_summary<-MAB.mrip_filter %>%
  group_by(RPATH,YEAR) %>%
  summarise(WGT_AB1 = sum(WGT_AB1))

#average
#SW addition
MAB.mrip_summary <- MAB.mrip_summary %>% group_by(RPATH) %>% summarise(catch = mean(WGT_AB1))

## Divide by MAB area to get kg/km^2
#Calculate total MAB area
area<-sf::st_read(dsn=system.file("extdata","strata.shp",package="survdat"))
area<-get_area(areaPolygon = area, areaDescription="STRATA")
MAB.area<-subset(area, area$STRATUM %in% MAB.strata)
MAB.area<-sum(MAB.area$Area)
rm(area)

MAB.mrip_summary$Per_Area<-(MAB.mrip_summary$catch/1000)/MAB.area

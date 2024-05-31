## ---------------------------
## Script name: MAB_biomass_accumulation.R
##
## Purpose of script: Estimate biomass accumulation for all function groups
##                    and export based on significance.
##
## Author: Brandon Beltz, updated by Sarah J. Weisberg
##
## Email: brandon.beltz@stonybrook.edu
## ---------------------------
##
## Notes: This version is specific to the MABRpath model.

# Fri Dec  8 17:05:05 2023 ------------------------------

## Load libraries, packages and functions

library(survdat); library(data.table) #library(rgdal)

## Run MAB_biomass_estimates.R
source(here('MAB_biomass_estimates.R'))

## Get RPATH groups from biomass estimates
MAB.RPATH<-unique(total.biomass$RPATH)
#Remove NA, Fauna
MAB.RPATH <- MAB.RPATH[!MAB.RPATH %in% c(NA,"Fauna")]

#Remove units
total.biomass$biomass.t_area<-as.numeric(total.biomass$biomass.t_area)

total.biomass %>% filter(RPATH == "Cod") %>%
  ggplot(aes(x=YEAR,y=biomass.t_area))+
  geom_point()

lm<-lm(biomass.t_area~YEAR, data = subset(total.biomass, RPATH == "Cod" & YEAR <1985))

## Run linear model for all functional groups
ba<-c()
p<-c()
for (i in 1: length(MAB.groups$RPATH)){
  lm<-lm(biomass.t_area~YEAR, data = subset(total.biomass, RPATH == MAB.RPATH[i]))
  spF <- as.numeric(summary(lm)$fstatistic)
  p[i] <- pf(spF[1], spF[2], spF[3], lower = F)
  ba[i] <-as.numeric(lm[[1]][2])
}

## Bind results and subset by significance
MAB.bioacc<-cbind(MAB.RPATH,ba,p)
MAB.bioacc_sig<-subset(MAB.bioacc, p<=0.05 & abs(ba) >= 0.005)

## Save output
save(MAB.bioacc_sig, file ='data/MAB_biomass_accumulation.RData')
MAB.ba<-MAB.bioacc_sig[,-3]
colnames(MAB.ba)[1]<-"RPATH"

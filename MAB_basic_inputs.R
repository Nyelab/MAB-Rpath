## ---------------------------
## Script name: MAB_basic_inputs.R
##
## Purpose of script: User enters basic model data for reference in future
##                    scripts.
##
## Author: Brandon Beltz
##
## Date Created: 2021-06-15
##
## Email: brandon.beltz@stonybrook.edu
## ---------------------------
##
## Notes: This version is specific to the MABRpath model.
##
## ---------------------------
## Set working directory

setwd("C:/Users/beven/OneDrive - Stony Brook University/Research/Rpath-MAB")

## Load libraries

library(data.table)

## ---------------------------

MAB.groups<-as.data.table(c('AmLobster','AmShad','AtlCroaker','AtlMackerel','AtlScallop','Bacteria','BaleenWhales','BlackSeaBass','Bluefish','Butterfish','Cod','Fourspot','GelZooplankton','Goosefish','HMS','Illex','LgCopepods','LittleSkate','Loligo','Macrobenthos','Megabenthos','Mesopelagics','Micronekton','Microzooplankton','OceanPout','Odontocetes','OtherCephalopods','OtherDemersals','OtherPelagics','OtherShrimps','OtherSkates','Phytoplankton','RedHake','Scup','SeaBirds','Sharks','SilverHake','SmCopepods','SmFlatfishes','SmoothDogfish','SmPelagics','SouthernDemersals','SpinyDogfish','SummerFlounder','Weakfish','WhiteHake','Windowpane','WinterFlounder','WinterSkate','YTFlounder'))
setnames(MAB.groups,'V1','RPATH')
MAB.strata<-c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510)
MAB.stareas<-c(537, 539, 600, 612:616, 621, 622, 625, 626, 631, 632)
## ---------------------------
## Script name: MAB_simulations.R
##
## Purpose of script: 
##                    
##
## Author: Brandon Beltz
##
## Last updated: 23 July 2022 
##
## Email: brandon.beltz@stonybrook.edu
## ---------------------------
##
## Notes: Experiencing unexpected biomass fluctuations in some lower trophic
##        functional groups. Explanation unknown.
##
## ---------------------------
## Set working directory

setwd("C:/Users/beven/Desktop/MAB-Rpath")

## Load libraries, packages and functions

library(Rpath); library(ggrepel); library(ggplot2); library(grid); library(gridExtra); library(data.table)

#Set up scenario object and run test simulation
MAB.scenario<-rsim.scenario(MAB.rpath,MAB.rpath.params,1:50)
MAB.initial<-rsim.run(MAB.scenario, method = 'AB',years=1)
MAB.testsim<-rsim.run(MAB.scenario, method = 'AB',years=1:5)

#####

#Spiny Dogfish
#Baseline scenario
Spiny.baseline<-copy(MAB.scenario)
Spiny.baseline<-adjust.forcing(Spiny.baseline,'ForcedMigrate','SpinyDogfish',sim.month = 3:5,bymonth = T,value=2.6)
Spiny.baseline<-adjust.forcing(Spiny.baseline,'ForcedMigrate','SpinyDogfish',sim.month = 9:11,bymonth = T,value=-2.6)
for (i in 0:49){
        Spiny.baseline<-adjust.forcing(Spiny.baseline,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=2.6)
        Spiny.baseline<-adjust.forcing(Spiny.baseline,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+9):((i*12)+11),bymonth = T,value=-2.6)  
}

#Increased migration scenario
Spiny.increased<-copy(MAB.scenario)
Spiny.increased<-adjust.forcing(Spiny.increased,'ForcedMigrate','SpinyDogfish',sim.month = 3:5,bymonth = T,value=3.3)
Spiny.increased<-adjust.forcing(Spiny.increased,'ForcedMigrate','SpinyDogfish',sim.month = 9:11,bymonth = T,value=-3.3)
for (i in 0:49){
        Spiny.increased<-adjust.forcing(Spiny.increased,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=3.3)
        Spiny.increased<-adjust.forcing(Spiny.increased,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+9):((i*12)+11),bymonth = T,value=-3.3)  
}

#Extended migration scenario
Spiny.extended<-copy(MAB.scenario)
Spiny.extended<-adjust.forcing(Spiny.extended,'ForcedMigrate','SpinyDogfish',sim.month = 3:5,bymonth = T,value=2.6)
Spiny.extended<-adjust.forcing(Spiny.extended,'ForcedMigrate','SpinyDogfish',sim.month = 10:12,bymonth = T,value=-2.6)
for (i in 0:49){
        Spiny.extended<-adjust.forcing(Spiny.extended,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=2.6)
        Spiny.extended<-adjust.forcing(Spiny.extended,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+10):((i*12)+12),bymonth = T,value=-2.6)  
}

#Combined migration scenario
Spiny.combined<-copy(MAB.scenario)
Spiny.combined<-adjust.forcing(Spiny.combined,'ForcedMigrate','SpinyDogfish',sim.month = 3:5,bymonth = T,value=3.3)
Spiny.combined<-adjust.forcing(Spiny.combined,'ForcedMigrate','SpinyDogfish',sim.month = 10:12,bymonth = T,value=-3.3)
for (i in 0:49){
        Spiny.combined<-adjust.forcing(Spiny.combined,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=3.3)
        Spiny.combined<-adjust.forcing(Spiny.combined,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+10):((i*12)+12),bymonth = T,value=-3.3)  
}

#write.csv(Spiny.increased$forcing$ForcedMigrate,file="Check_forcing_spiny.csv")

#Calculate 50th year biomass
Spiny.baseline.50<-rsim.run(Spiny.baseline,method='AB',years = 1:50)
Spiny.increased.50<-rsim.run(Spiny.increased,method='AB',years = 1:50)
Spiny.extended.50<-rsim.run(Spiny.extended,method='AB',years = 1:50)
Spiny.combined.50<-rsim.run(Spiny.combined,method='AB',years = 1:50)

#Create table relative to baseline
Spiny.results<-as.data.frame(Spiny.baseline.50$end_state$Biomass[2:50],row.names=MAB.groups$RPATH)
Spiny.results<-cbind(Spiny.results,Spiny.increased.50$end_state$Biomass[2:50])
Spiny.results<-cbind(Spiny.results,Spiny.extended.50$end_state$Biomass[2:50])
Spiny.results<-cbind(Spiny.results,Spiny.combined.50$end_state$Biomass[2:50])
colnames(Spiny.results)<-c("Baseline","Increased","Extended","Combined")

Spiny.results$Rel.bas<-(Spiny.results$Baseline-Spiny.results$Baseline)/Spiny.results$Baseline
Spiny.results$Rel.inc<-(Spiny.results$Increased-Spiny.results$Baseline)/Spiny.results$Baseline
Spiny.results$Rel.ext<-(Spiny.results$Extended-Spiny.results$Baseline)/Spiny.results$Baseline
Spiny.results$Rel.com<-(Spiny.results$Combined-Spiny.results$Baseline)/Spiny.results$Baseline

#Update table to show percent
Spiny.results$Per.bas<-Spiny.results$Rel.bas*100
Spiny.results$Per.inc<-Spiny.results$Rel.inc*100
Spiny.results$Per.ext<-Spiny.results$Rel.ext*100
Spiny.results$Per.com<-Spiny.results$Rel.com*100

#####

#Smooth Dogfish
#Baseline scenario
Smooth.baseline<-copy(MAB.scenario)
Smooth.baseline<-adjust.forcing(Smooth.baseline,'ForcedMigrate','SmoothDogfish',sim.month = 3:5,bymonth = T,value=-2.6)
Smooth.baseline<-adjust.forcing(Smooth.baseline,'ForcedMigrate','SmoothDogfish',sim.month = 9:11,bymonth = T,value=2.6)
for (i in 0:49){
        Smooth.baseline<-adjust.forcing(Smooth.baseline,'ForcedMigrate','SmoothDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=-2.6)
        Smooth.baseline<-adjust.forcing(Smooth.baseline,'ForcedMigrate','SmoothDogfish',sim.month =((i*12)+9):((i*12)+11),bymonth = T,value=2.6)  
}

#Increased migration scenario
Smooth.increased<-copy(MAB.scenario)
Smooth.increased<-adjust.forcing(Smooth.increased,'ForcedMigrate','SmoothDogfish',sim.month = 3:5,bymonth = T,value=-3.3)
Smooth.increased<-adjust.forcing(Smooth.increased,'ForcedMigrate','SmoothDogfish',sim.month = 9:11,bymonth = T,value=3.3)
for (i in 0:49){
        Smooth.increased<-adjust.forcing(Smooth.increased,'ForcedMigrate','SmoothDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=-3.3)
        Smooth.increased<-adjust.forcing(Smooth.increased,'ForcedMigrate','SmoothDogfish',sim.month =((i*12)+9):((i*12)+11),bymonth = T,value=3.3)  
}

#Extended migration scenario
Smooth.extended<-copy(MAB.scenario)
Smooth.extended<-adjust.forcing(Smooth.extended,'ForcedMigrate','SmoothDogfish',sim.month = 3:5,bymonth = T,value=-2.6)
Smooth.extended<-adjust.forcing(Smooth.extended,'ForcedMigrate','SmoothDogfish',sim.month = 10:12,bymonth = T,value=2.6)
for (i in 0:49){
        Smooth.extended<-adjust.forcing(Smooth.extended,'ForcedMigrate','SmoothDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=-2.6)
        Smooth.extended<-adjust.forcing(Smooth.extended,'ForcedMigrate','SmoothDogfish',sim.month =((i*12)+10):((i*12)+12),bymonth = T,value=2.6)  
}

#Combined migration scenario
Smooth.combined<-copy(MAB.scenario)
Smooth.combined<-adjust.forcing(Smooth.combined,'ForcedMigrate','SmoothDogfish',sim.month = 3:5,bymonth = T,value=-3.3)
Smooth.combined<-adjust.forcing(Smooth.combined,'ForcedMigrate','SmoothDogfish',sim.month = 10:12,bymonth = T,value=3.3)
for (i in 0:49){
        Smooth.combined<-adjust.forcing(Smooth.combined,'ForcedMigrate','SmoothDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=-3.3)
        Smooth.combined<-adjust.forcing(Smooth.combined,'ForcedMigrate','SmoothDogfish',sim.month =((i*12)+10):((i*12)+12),bymonth = T,value=3.3)  
}

#write.csv(Smooth.increased$forcing$ForcedMigrate,file="Check_forcing_Smooth.csv")

#Calculate 50th year biomass
Smooth.baseline.50<-rsim.run(Smooth.baseline,method='AB',years = 1:50)
Smooth.increased.50<-rsim.run(Smooth.increased,method='AB',years = 1:50)
Smooth.extended.50<-rsim.run(Smooth.extended,method='AB',years = 1:50)
Smooth.combined.50<-rsim.run(Smooth.combined,method='AB',years = 1:50)

#Create table relative to baseline
Smooth.results<-as.data.frame(Smooth.baseline.50$end_state$Biomass[2:50],row.names=MAB.groups$RPATH)
Smooth.results<-cbind(Smooth.results,Smooth.increased.50$end_state$Biomass[2:50])
Smooth.results<-cbind(Smooth.results,Smooth.extended.50$end_state$Biomass[2:50])
Smooth.results<-cbind(Smooth.results,Smooth.combined.50$end_state$Biomass[2:50])
colnames(Smooth.results)<-c("Baseline","Increased","Extended","Combined")

Smooth.results$Rel.bas<-(Smooth.results$Baseline-Smooth.results$Baseline)/Smooth.results$Baseline
Smooth.results$Rel.inc<-(Smooth.results$Increased-Smooth.results$Baseline)/Smooth.results$Baseline
Smooth.results$Rel.ext<-(Smooth.results$Extended-Smooth.results$Baseline)/Smooth.results$Baseline
Smooth.results$Rel.com<-(Smooth.results$Combined-Smooth.results$Baseline)/Smooth.results$Baseline

#Update table to show percent
Smooth.results$Per.bas<-Smooth.results$Rel.bas*100
Smooth.results$Per.inc<-Smooth.results$Rel.inc*100
Smooth.results$Per.ext<-Smooth.results$Rel.ext*100
Smooth.results$Per.com<-Smooth.results$Rel.com*100

#####

#Spiny Dogfish
#Baseline scenario
Spiny.baseline<-copy(MAB.scenario)
Spiny.baseline<-adjust.forcing(Spiny.baseline,'ForcedMigrate','SpinyDogfish',sim.month = 3:5,bymonth = T,value=2.6)
Spiny.baseline<-adjust.forcing(Spiny.baseline,'ForcedMigrate','SpinyDogfish',sim.month = 9:11,bymonth = T,value=-2.6)
for (i in 0:49){
        Spiny.baseline<-adjust.forcing(Spiny.baseline,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=2.6)
        Spiny.baseline<-adjust.forcing(Spiny.baseline,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+9):((i*12)+11),bymonth = T,value=-2.6)  
}

#Increased migration scenario
Spiny.increased<-copy(MAB.scenario)
Spiny.increased<-adjust.forcing(Spiny.increased,'ForcedMigrate','SpinyDogfish',sim.month = 3:5,bymonth = T,value=3.3)
Spiny.increased<-adjust.forcing(Spiny.increased,'ForcedMigrate','SpinyDogfish',sim.month = 9:11,bymonth = T,value=-3.3)
for (i in 0:49){
        Spiny.increased<-adjust.forcing(Spiny.increased,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=3.3)
        Spiny.increased<-adjust.forcing(Spiny.increased,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+9):((i*12)+11),bymonth = T,value=-3.3)  
}

#Extended migration scenario
Spiny.extended<-copy(MAB.scenario)
Spiny.extended<-adjust.forcing(Spiny.extended,'ForcedMigrate','SpinyDogfish',sim.month = 3:5,bymonth = T,value=2.6)
Spiny.extended<-adjust.forcing(Spiny.extended,'ForcedMigrate','SpinyDogfish',sim.month = 10:12,bymonth = T,value=-2.6)
for (i in 0:49){
        Spiny.extended<-adjust.forcing(Spiny.extended,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=2.6)
        Spiny.extended<-adjust.forcing(Spiny.extended,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+10):((i*12)+12),bymonth = T,value=-2.6)  
}

#Combined migration scenario
Spiny.combined<-copy(MAB.scenario)
Spiny.combined<-adjust.forcing(Spiny.combined,'ForcedMigrate','SpinyDogfish',sim.month = 3:5,bymonth = T,value=3.3)
Spiny.combined<-adjust.forcing(Spiny.combined,'ForcedMigrate','SpinyDogfish',sim.month = 10:12,bymonth = T,value=-3.3)
for (i in 0:49){
        Spiny.combined<-adjust.forcing(Spiny.combined,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=3.3)
        Spiny.combined<-adjust.forcing(Spiny.combined,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+10):((i*12)+12),bymonth = T,value=-3.3)  
}

#write.csv(Spiny.increased$forcing$ForcedMigrate,file="Check_forcing_spiny.csv")

#Calculate 50th year biomass
Spiny.baseline.50<-rsim.run(Spiny.baseline,method='AB',years = 1:50)
Spiny.increased.50<-rsim.run(Spiny.increased,method='AB',years = 1:50)
Spiny.extended.50<-rsim.run(Spiny.extended,method='AB',years = 1:50)
Spiny.combined.50<-rsim.run(Spiny.combined,method='AB',years = 1:50)

#Create table relative to baseline
Spiny.results<-as.data.frame(Spiny.baseline.50$end_state$Biomass[2:50],row.names=MAB.groups$RPATH)
Spiny.results<-cbind(Spiny.results,Spiny.increased.50$end_state$Biomass[2:50])
Spiny.results<-cbind(Spiny.results,Spiny.extended.50$end_state$Biomass[2:50])
Spiny.results<-cbind(Spiny.results,Spiny.combined.50$end_state$Biomass[2:50])
colnames(Spiny.results)<-c("Baseline","Increased","Extended","Combined")

Spiny.results$Rel.bas<-(Spiny.results$Baseline-Spiny.results$Baseline)/Spiny.results$Baseline
Spiny.results$Rel.inc<-(Spiny.results$Increased-Spiny.results$Baseline)/Spiny.results$Baseline
Spiny.results$Rel.ext<-(Spiny.results$Extended-Spiny.results$Baseline)/Spiny.results$Baseline
Spiny.results$Rel.com<-(Spiny.results$Combined-Spiny.results$Baseline)/Spiny.results$Baseline

#Update table to show percent
Spiny.results$Per.bas<-Spiny.results$Rel.bas*100
Spiny.results$Per.inc<-Spiny.results$Rel.inc*100
Spiny.results$Per.ext<-Spiny.results$Rel.ext*100
Spiny.results$Per.com<-Spiny.results$Rel.com*100


#### Plots for thesis

# Lower trophic levels [6,13,17,20,21,23,32,38]
# Inverts [1,5,16,19,27,30]
# Fish [2,3,4,8,9,10,11,12,14,22,25,28,29,33,34,37,39,40,41,42,44:50]
# Pred [7,15,18,26,31,35,36,43]

## Spiny dogfish
# Spiny increased
Plot_spiny_inc<-data.frame(Rpath = rownames(Spiny.results),Rel.biomass = Spiny.results$Per.inc)
pdf("spiny_increased_draft.pdf",width = 8,height = 7) ## Changes per plot
sp_inc_all_plot<-ggplot(Plot_spiny_inc[-43], aes(x = Rpath, y = Rel.biomass)) + ## Change per species
        geom_bar(stat="identity") +
        coord_flip() +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Spiny dogfish - Increased biomass - All species') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()

Plot_spiny_inc_lower<-Plot_spiny_inc[c(6,13,17,20,21,23,24,32,38),]
pdf("spiny_increased_lower_draft.pdf",width = 8,height = 7) ## Changes per plot
sp_inc_lower_plot<-ggplot(Plot_spiny_inc_lower, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'A) Range shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.0002,0.0004)
dev.off()

Plot_spiny_inc_inverts<-Plot_spiny_inc[c(1,5,16,19,27,30),]
pdf("spiny_increased_inverts_draft.pdf",width = 8,height = 7) ## Changes per plot
sp_inc_inverts_plot<-ggplot(Plot_spiny_inc_inverts, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'A) Range shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.0005,0.002)
dev.off()


Plot_spiny_inc_fish<-Plot_spiny_inc[c(2,3,4,8,9,10,11,12,14,22,25,28,29,33,34,37,39,40,42,44:50),]
pdf("spiny_increased_fish_draft.pdf",width = 8,height = 7) ## Changes per plot
sp_inc_fish_plot<-ggplot(Plot_spiny_inc_fish, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") + 
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'A) Range shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.001,0.0055)
dev.off()

Plot_spiny_inc_pred<-Plot_spiny_inc[c(7,15,18,26,31,35,36,41),]
pdf("spiny_increased_pred_draft.pdf",width = 8,height = 7) ## Changes per plot
sp_inc_pred_plot<-ggplot(Plot_spiny_inc_pred, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'A) Range shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.001,0.003)
dev.off()

# Spiny extended
Plot_spiny_ext<-data.frame(Rpath = rownames(Spiny.results),Rel.biomass = Spiny.results$Rel.ext)
pdf("spiny_extended_draft.pdf",width = 8,height = 7) ## Changes per plot
sp_ext_all_plot<-ggplot(Plot_spiny_ext[-43,], aes(x = Rpath, y = Rel.biomass)) + ## Change per species
        geom_bar(stat="identity") +
        coord_flip() +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Spiny dogfish - extended biomass - All species') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()

Plot_spiny_ext_lower<-Plot_spiny_ext[c(6,13,17,20,21,23,24,32,38),]
pdf("spiny_extended_lower_draft.pdf",width = 8,height = 7) ## Changes per plot
sp_ext_lower_plot<-ggplot(Plot_spiny_ext_lower, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'B) Phenological shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.0002,0.0004)
dev.off()

Plot_spiny_ext_inverts<-Plot_spiny_ext[c(1,5,16,19,27,30),]
pdf("spiny_extended_inverts_draft.pdf",width = 8,height = 7) ## Changes per plot
sp_ext_inverts_plot<-ggplot(Plot_spiny_ext_inverts, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'B) Phenological shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.0005,0.002)
dev.off()


Plot_spiny_ext_fish<-Plot_spiny_ext[c(2,3,4,8,9,10,11,12,14,22,25,28,29,33,34,37,39,40,42,44:50),]
pdf("spiny_extended_fish_draft.pdf",width = 8,height = 7) ## Changes per plot
sp_ext_fish_plot<-ggplot(Plot_spiny_ext_fish, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") + 
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'B) Phenological shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.001,0.0055)
dev.off()

Plot_spiny_ext_pred<-Plot_spiny_ext[c(7,15,18,26,31,35,36,41),]
pdf("spiny_extended_pred_draft.pdf",width = 8,height = 7) ## Changes per plot
sp_ext_pred_plot<-ggplot(Plot_spiny_ext_pred, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'B) Phenological shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.001,0.003)
dev.off()

# Spiny combined
Plot_spiny_com<-data.frame(Rpath = rownames(Spiny.results),Rel.biomass = Spiny.results$Rel.com)
pdf("spiny_combined_draft.pdf",width = 8,height = 7) ## Changes per plot
sp_com_all_plot<-ggplot(Plot_spiny_com[-43,], aes(x = Rpath, y = Rel.biomass)) + ## Change per species
        geom_bar(stat="identity") +
        coord_flip() +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Spiny dogfish - combined biomass - All species') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()

Plot_spiny_com_lower<-Plot_spiny_com[c(6,13,17,20,21,23,24,32,38),]
pdf("spiny_combined_lower_draft.pdf",width = 8,height = 7) ## Changes per plot
sp_com_lower_plot<-ggplot(Plot_spiny_com_lower, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'C) Range shift, Phenological shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank())
dev.off()

Plot_spiny_com_inverts<-Plot_spiny_com[c(1,5,16,19,27,30),]
pdf("spiny_combined_inverts_draft.pdf",width = 8,height = 7) ## Changes per plot
sp_com_inverts_plot<-ggplot(Plot_spiny_com_inverts, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'C) Range shift, Phenological shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.0005,0.002)
dev.off()


Plot_spiny_com_fish<-Plot_spiny_com[c(2,3,4,8,9,10,11,12,14,22,25,28,29,33,34,37,39,40,42,44:50),]
pdf("spiny_combined_fish_draft.pdf",width = 8,height = 7) ## Changes per plot
sp_com_fish_plot<-ggplot(Plot_spiny_com_fish, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") + 
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'C) Range shift, Phenological shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.001,0.0055)
dev.off()

Plot_spiny_com_pred<-Plot_spiny_com[c(7,15,18,26,31,35,36,41),]
pdf("spiny_combined_pred_draft.pdf",width = 8,height = 7) ## Changes per plot
sp_com_pred_plot<-ggplot(Plot_spiny_com_pred, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'C) Range shift, Phenological shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.001,0.003)
dev.off()

## Smooth dogfish
# Smooth increased
Plot_smooth_inc<-data.frame(Rpath = rownames(Smooth.results),Rel.biomass = Smooth.results$Rel.inc)
pdf("smooth_increased_draft.pdf",width = 8,height = 7) ## Changes per plot
sm_inc_all_plot<-ggplot(Plot_smooth_inc[-41,], aes(x = Rpath, y = Rel.biomass)) + ## Change per species
        geom_bar(stat="identity") +
        coord_flip() +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'smooth dogfish - Increased biomass - All species') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()

Plot_smooth_inc_lower<-Plot_smooth_inc[c(6,13,17,20,21,23,24,32,38),]
pdf("smooth_increased_lower_draft.pdf",width = 8,height = 7) ## Changes per plot
sm_inc_lower_plot<-ggplot(Plot_smooth_inc_lower, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'smooth dogfish - Increased biomass - Lower trophic levels') +
        theme(plot.title=element_text(size = 18,face = 'bold'), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10)) +
        theme(axis.text.x = element_text(angle = 90))
dev.off()

Plot_smooth_inc_inverts<-Plot_smooth_inc[c(1,5,16,19,27,30),]
pdf("smooth_increased_inverts_draft.pdf",width = 8,height = 7) ## Changes per plot
sm_inc_inverts_plot<-ggplot(Plot_smooth_inc_inverts, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'smooth dogfish - Increased biomass - Invertebrates') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()


Plot_smooth_inc_fish<-Plot_smooth_inc[c(2,3,4,8,9,10,11,12,14,22,25,28,29,33,34,37,39,40,42,44:50),]
pdf("smooth_increased_fish_draft.pdf",width = 8,height = 7) ## Changes per plot
sm_inc_fish_plot<-ggplot(Plot_smooth_inc_fish, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") + 
        coord_flip() +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'smooth dogfish - Increased biomass - Fish') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()


Plot_smooth_inc_pred<-Plot_smooth_inc[c(7,15,18,26,31,35,36,43),]
pdf("smooth_increased_pred_draft.pdf",width = 8,height = 7) ## Changes per plot
sm_inc_pred_plot<-ggplot(Plot_smooth_inc_pred, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'smooth dogfish - Increased biomass - Top predators') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()

# Smooth extended
Plot_smooth_ext<-data.frame(Rpath = rownames(Smooth.results),Rel.biomass = Smooth.results$Rel.ext)
pdf("smooth_extended_draft.pdf",width = 8,height = 7) ## Changes per plot
sm_ext_all_plot<-ggplot(Plot_smooth_ext[-41,], aes(x = Rpath, y = Rel.biomass)) + ## Change per species
        geom_bar(stat="identity") +
        coord_flip() +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'smooth dogfish - extended biomass - All species') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()

Plot_smooth_ext_lower<-Plot_smooth_ext[c(6,13,17,20,21,23,24,32,38),]
pdf("smooth_extended_lower_draft.pdf",width = 8,height = 7) ## Changes per plot
sm_ext_lower_plot<-ggplot(Plot_smooth_ext_lower, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'smooth dogfish - extended biomass - Lower trophic levels') +
        theme(plot.title=element_text(size = 18,face = 'bold'), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10)) +
        theme(axis.text.x = element_text(angle = 90))
dev.off()

Plot_smooth_ext_inverts<-Plot_smooth_ext[c(1,5,16,19,27,30),]
pdf("smooth_extended_inverts_draft.pdf",width = 8,height = 7) ## Changes per plot
sm_ext_inverts_plot<-ggplot(Plot_smooth_ext_inverts, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'smooth dogfish - extended biomass - Invertebrates') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()


Plot_smooth_ext_fish<-Plot_smooth_ext[c(2,3,4,8,9,10,11,12,14,22,25,28,29,33,34,37,39,40,42,44:50),]
pdf("smooth_extended_fish_draft.pdf",width = 8,height = 7) ## Changes per plot
sm_ext_fish_plot<-ggplot(Plot_smooth_ext_fish, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") + 
        coord_flip() +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'smooth dogfish - extended biomass - Fish') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()


sm_ext_pred_plot<-Plot_smooth_ext_pred<-Plot_smooth_ext[c(7,15,18,26,31,35,36,43),]
pdf("smooth_extended_pred_draft.pdf",width = 8,height = 7) ## Changes per plot
ggplot(Plot_smooth_ext_pred, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'smooth dogfish - extended biomass - Top predators') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()

# Smooth combined
Plot_smooth_com<-data.frame(Rpath = rownames(Smooth.results),Rel.biomass = Smooth.results$Rel.com)
pdf("smooth_combined_draft.pdf",width = 8,height = 7) ## Changes per plot
sm_com_all_plot<-ggplot(Plot_smooth_com[-41,], aes(x = Rpath, y = Rel.biomass)) + ## Change per species
        geom_bar(stat="identity") +
        coord_flip() +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'smooth dogfish - combined biomass - All species') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()

Plot_smooth_com_lower<-Plot_smooth_com[c(6,13,17,20,21,23,24,32,38),]
pdf("smooth_combined_lower_draft.pdf",width = 8,height = 7) ## Changes per plot
sm_com_lower_plot<-ggplot(Plot_smooth_com_lower, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'smooth dogfish - combined biomass - Lower trophic levels') +
        theme(plot.title=element_text(size = 18,face = 'bold'), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10)) +
        theme(axis.text.x = element_text(angle = 90))
dev.off()

Plot_smooth_com_inverts<-Plot_smooth_com[c(1,5,16,19,27,30),]
pdf("smooth_combined_inverts_draft.pdf",width = 8,height = 7) ## Changes per plot
sm_com_inverts_plot<-ggplot(Plot_smooth_com_inverts, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'smooth dogfish - combined biomass - Invertebrates') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()


Plot_smooth_com_fish<-Plot_smooth_com[c(2,3,4,8,9,10,11,12,14,22,25,28,29,33,34,37,39,40,42,44:50),]
pdf("smooth_combined_fish_draft.pdf",width = 8,height = 7) ## Changes per plot
sm_com_fish_plot<-ggplot(Plot_smooth_com_fish, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") + 
        coord_flip() +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'smooth dogfish - combined biomass - Fish') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()

Plot_smooth_com_pred<-Plot_smooth_com[c(7,15,18,26,31,35,36,43),]
pdf("smooth_combined_pred_draft.pdf",width = 8,height = 7) ## Changes per plot
sm_com_pred_plot<-ggplot(Plot_smooth_com_pred, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'smooth dogfish - combined biomass - Top predators') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()

## Sharks
# sharks increased
Plot_sharks_inc<-data.frame(Rpath = rownames(Sharks.results),Rel.biomass = Sharks.results$Rel.inc)
pdf("sharks_increased_draft.pdf",width = 8,height = 7) ## Changes per plot
sh_inc_all_plot<-ggplot(Plot_sharks_inc[-36,], aes(x = Rpath, y = Rel.biomass)) + ## Change per species
        geom_bar(stat="identity") +
        coord_flip() +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Sharks - Increased biomass - All species') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()

Plot_sharks_inc_lower<-Plot_sharks_inc[c(6,13,17,20,21,23,24,32,38),]
pdf("sharks_increased_lower_draft.pdf",width = 8,height = 7) ## Changes per plot
sh_inc_lower_plot<-ggplot(Plot_sharks_inc_lower, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Sharks - Increased biomass - Lower trophic levels') +
        theme(plot.title=element_text(size = 18,face = 'bold'), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10)) +
theme(axis.text.x = element_text(angle = 90))
        dev.off()

Plot_sharks_inc_inverts<-Plot_sharks_inc[c(1,5,16,19,27,30),]
pdf("sharks_increased_inverts_draft.pdf",width = 8,height = 7) ## Changes per plot
sh_inc_inverts_plot<-ggplot(Plot_sharks_inc_inverts, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Sharks - Increased biomass - Invertebrates') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()


Plot_sharks_inc_fish<-Plot_sharks_inc[c(2,3,4,8,9,10,11,12,14,22,25,28,29,33,34,37,39,40,42,44:50),]
pdf("sharks_increased_fish_draft.pdf",width = 8,height = 7) ## Changes per plot
sh_inc_fish_plot<-ggplot(Plot_sharks_inc_fish, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") + 
        coord_flip() +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Sharks - Increased biomass - Fish') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()


Plot_sharks_inc_pred<-Plot_sharks_inc[c(7,15,18,26,31,35,41,43),]
pdf("sharks_increased_pred_draft.pdf",width = 8,height = 7) ## Changes per plot
sh_inc_pred_plot<-ggplot(Plot_sharks_inc_pred, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Sharks - Increased biomass - Top predators') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()

# sharks extended
Plot_sharks_ext<-data.frame(Rpath = rownames(Sharks.results),Rel.biomass = Sharks.results$Rel.ext)
pdf("sharks_extended_draft.pdf",width = 8,height = 7) ## Changes per plot
sh_ext_all_plot<-ggplot(Plot_sharks_ext[-36,], aes(x = Rpath, y = Rel.biomass)) + ## Change per species
        geom_bar(stat="identity") +
        coord_flip() +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Sharks - extended biomass - All species') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()

Plot_sharks_ext_lower<-Plot_sharks_ext[c(6,13,17,20,21,23,24,32,38),]
pdf("sharks_extended_lower_draft.pdf",width = 8,height = 7) ## Changes per plot
sh_ext_lower_plot<-ggplot(Plot_sharks_ext_lower, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Sharks - extended biomass - Lower trophic levels') +
        theme(plot.title=element_text(size = 18,face = 'bold'), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10)) +
theme(axis.text.x = element_text(angle = 90))
        dev.off()

Plot_sharks_ext_inverts<-Plot_sharks_ext[c(1,5,16,19,27,30),]
pdf("sharks_extended_inverts_draft.pdf",width = 8,height = 7) ## Changes per plot
sh_ext_inverts_plot<-ggplot(Plot_sharks_ext_inverts, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Sharks - extended biomass - Invertebrates') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()


Plot_sharks_ext_fish<-Plot_sharks_ext[c(2,3,4,8,9,10,11,12,14,22,25,28,29,33,34,37,39,40,42,44:50),]
pdf("sharks_extended_fish_draft.pdf",width = 8,height = 7) ## Changes per plot
sh_ext_fish_plot<-ggplot(Plot_sharks_ext_fish, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") + 
        coord_flip() +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Sharks - extended biomass - Fish') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()


Plot_sharks_ext_pred<-Plot_sharks_ext[c(7,15,18,26,31,35,41,43),]
pdf("sharks_extended_pred_draft.pdf",width = 8,height = 7) ## Changes per plot
sh_ext_pred_plot<-ggplot(Plot_sharks_ext_pred, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Sharks - extended biomass - Top predators') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()

# sharks combined
Plot_sharks_com<-data.frame(Rpath = rownames(Sharks.results),Rel.biomass = Sharks.results$Rel.com)
pdf("sharks_combined_draft.pdf",width = 8,height = 7) ## Changes per plot
sh_com_all_plot<-ggplot(Plot_sharks_com[-36,], aes(x = Rpath, y = Rel.biomass)) + ## Change per species
        geom_bar(stat="identity") +
        coord_flip() +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Sharks - combined biomass - All species') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()

Plot_sharks_com_lower<-Plot_sharks_com[c(6,13,17,20,21,23,24,32,38),]
pdf("sharks_combined_lower_draft.pdf",width = 8,height = 7) ## Changes per plot
sh_com_lower_plot<-ggplot(Plot_sharks_com_lower, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Sharks - combined biomass - Lower trophic levels') +
        theme(plot.title=element_text(size = 18,face = 'bold'), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10)) +
theme(axis.text.x = element_text(angle = 90))
        dev.off()

Plot_sharks_com_inverts<-Plot_sharks_com[c(1,5,16,19,27,30),]
pdf("sharks_combined_inverts_draft.pdf",width = 8,height = 7) ## Changes per plot
sh_com_inverts_plot<-ggplot(Plot_sharks_com_inverts, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Sharks - combined biomass - Invertebrates') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()


Plot_sharks_com_fish<-Plot_sharks_com[c(2,3,4,8,9,10,11,12,14,22,25,28,29,33,34,37,39,40,42,44:50),]
pdf("sharks_combined_fish_draft.pdf",width = 8,height = 7) ## Changes per plot
sh_com_fish_plot<-ggplot(Plot_sharks_com_fish, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") + 
        coord_flip() +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Sharks - combined biomass - Fish') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()


Plot_sharks_com_pred<-Plot_sharks_com[c(7,15,18,26,31,35,41,43),]
pdf("sharks_combined_pred_draft.pdf",width = 8,height = 7) ## Changes per plot
sh_com_pred_plot<-ggplot(Plot_sharks_com_pred, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'Sharks - combined biomass - Top predators') +
        theme(plot.title=element_text(size = 18,face = 'bold',hjust = 0.5), axis.title=element_text(size = 16, face = 'bold'), axis.text= element_text(color = 'black', size = 10))
dev.off()

## All predators, simultaneous scenario
#Baseline scenario
All.preds.baseline<-MAB.scenario

All.preds.baseline<-adjust.forcing(All.preds.baseline,'ForcedMigrate','SpinyDogfish',sim.month = 3:5,bymonth = T,value=.26)
All.preds.baseline<-adjust.forcing(All.preds.baseline,'ForcedMigrate','SpinyDogfish',sim.month = 9:11,bymonth = T,value=-.26)
for (i in 0:49){
        All.preds.baseline<-adjust.forcing(All.preds.baseline,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=.26)
        All.preds.baseline<-adjust.forcing(All.preds.baseline,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+9):((i*12)+11),bymonth = T,value=-.26)  
}

All.preds.baseline<-adjust.forcing(All.preds.baseline,'ForcedMigrate','SmoothDogfish',sim.month = 3:5,bymonth = T,value=-.26)
All.preds.baseline<-adjust.forcing(All.preds.baseline,'ForcedMigrate','SmoothDogfish',sim.month = 9:11,bymonth = T,value=.26)
for (i in 0:49){
        All.preds.baseline<-adjust.forcing(All.preds.baseline,'ForcedMigrate','SmoothDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=-.26)
        All.preds.baseline<-adjust.forcing(All.preds.baseline,'ForcedMigrate','SmoothDogfish',sim.month =((i*12)+9):((i*12)+11),bymonth = T,value=.26)  
}

All.preds.baseline<-adjust.forcing(All.preds.baseline,'ForcedMigrate','Sharks',sim.month = 3:5,bymonth = T,value=.26)
All.preds.baseline<-adjust.forcing(All.preds.baseline,'ForcedMigrate','Sharks',sim.month = 9:11,bymonth = T,value=-.26)
for (i in 0:49){
        All.preds.baseline<-adjust.forcing(All.preds.baseline,'ForcedMigrate','Sharks',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=.26)
        All.preds.baseline<-adjust.forcing(All.preds.baseline,'ForcedMigrate','Sharks',sim.month =((i*12)+9):((i*12)+11),bymonth = T,value=-.26)  
}

write.csv(All.preds.baseline$forcing$ForcedMigrate,file="Check_forcing_allpreds.csv")

#Increased migration scenario
All.preds.increased<-MAB.scenario

All.preds.increased<-adjust.forcing(All.preds.increased,'ForcedMigrate','SpinyDogfish',sim.month = 3:5,bymonth = T,value=.33)
All.preds.increased<-adjust.forcing(All.preds.increased,'ForcedMigrate','SpinyDogfish',sim.month = 9:11,bymonth = T,value=-.33)
for (i in 0:49){
        All.preds.increased<-adjust.forcing(All.preds.increased,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=.33)
        All.preds.increased<-adjust.forcing(All.preds.increased,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+9):((i*12)+11),bymonth = T,value=-.33)  
}

All.preds.increased<-adjust.forcing(All.preds.increased,'ForcedMigrate','SmoothDogfish',sim.month = 3:5,bymonth = T,value=-.33)
All.preds.increased<-adjust.forcing(All.preds.increased,'ForcedMigrate','SmoothDogfish',sim.month = 9:11,bymonth = T,value=.33)
for (i in 0:49){
        All.preds.increased<-adjust.forcing(All.preds.increased,'ForcedMigrate','SmoothDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=-.33)
        All.preds.increased<-adjust.forcing(All.preds.increased,'ForcedMigrate','SmoothDogfish',sim.month =((i*12)+9):((i*12)+11),bymonth = T,value=.33)  
}

All.preds.increased<-adjust.forcing(All.preds.increased,'ForcedMigrate','Sharks',sim.month = 3:5,bymonth = T,value=.33)
All.preds.increased<-adjust.forcing(All.preds.increased,'ForcedMigrate','Sharks',sim.month = 9:11,bymonth = T,value=-.33)
for (i in 0:49){
        All.preds.increased<-adjust.forcing(All.preds.increased,'ForcedMigrate','Sharks',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=.33)
        All.preds.increased<-adjust.forcing(All.preds.increased,'ForcedMigrate','Sharks',sim.month =((i*12)+9):((i*12)+11),bymonth = T,value=-.33)  
}

write.csv(All.preds.increased$forcing$ForcedMigrate,file="Check_forcing_allpreds.csv")

#Extended migration scenario
All.preds.extended<-MAB.scenario

All.preds.extended<-adjust.forcing(All.preds.extended,'ForcedMigrate','SpinyDogfish',sim.month = 3:5,bymonth = T,value=.26)
All.preds.extended<-adjust.forcing(All.preds.extended,'ForcedMigrate','SpinyDogfish',sim.month = 10:12,bymonth = T,value=-.26)
for (i in 0:49){
        All.preds.extended<-adjust.forcing(All.preds.extended,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=.26)
        All.preds.extended<-adjust.forcing(All.preds.extended,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+10):((i*12)+12),bymonth = T,value=-.26)  
}

All.preds.extended<-adjust.forcing(All.preds.extended,'ForcedMigrate','SmoothDogfish',sim.month = 3:5,bymonth = T,value=-.26)
All.preds.extended<-adjust.forcing(All.preds.extended,'ForcedMigrate','SmoothDogfish',sim.month = 10:12,bymonth = T,value=.26)
for (i in 0:49){
        All.preds.extended<-adjust.forcing(All.preds.extended,'ForcedMigrate','SmoothDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=-.26)
        All.preds.extended<-adjust.forcing(All.preds.extended,'ForcedMigrate','SmoothDogfish',sim.month =((i*12)+10):((i*12)+12),bymonth = T,value=.26)  
}

All.preds.extended<-adjust.forcing(All.preds.extended,'ForcedMigrate','Sharks',sim.month = 3:5,bymonth = T,value=.26)
All.preds.extended<-adjust.forcing(All.preds.extended,'ForcedMigrate','Sharks',sim.month = 10:12,bymonth = T,value=-.26)
for (i in 0:49){
        All.preds.extended<-adjust.forcing(All.preds.extended,'ForcedMigrate','Sharks',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=.26)
        All.preds.extended<-adjust.forcing(All.preds.extended,'ForcedMigrate','Sharks',sim.month =((i*12)+10):((i*12)+12),bymonth = T,value=-.26)  
}

write.csv(All.preds.extended$forcing$ForcedMigrate,file="Check_forcing_allpreds.csv")

#Combined migration scenario
All.preds.combined<-MAB.scenario

All.preds.combined<-adjust.forcing(All.preds.combined,'ForcedMigrate','SpinyDogfish',sim.month = 3:5,bymonth = T,value=.33)
All.preds.combined<-adjust.forcing(All.preds.combined,'ForcedMigrate','SpinyDogfish',sim.month = 10:12,bymonth = T,value=-.33)
for (i in 0:49){
        All.preds.combined<-adjust.forcing(All.preds.combined,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=.33)
        All.preds.combined<-adjust.forcing(All.preds.combined,'ForcedMigrate','SpinyDogfish',sim.month =((i*12)+10):((i*12)+12),bymonth = T,value=-.33)  
}

All.preds.combined<-adjust.forcing(All.preds.combined,'ForcedMigrate','SmoothDogfish',sim.month = 3:5,bymonth = T,value=-.33)
All.preds.combined<-adjust.forcing(All.preds.combined,'ForcedMigrate','SmoothDogfish',sim.month = 10:12,bymonth = T,value=.33)
for (i in 0:49){
        All.preds.combined<-adjust.forcing(All.preds.combined,'ForcedMigrate','SmoothDogfish',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=-.33)
        All.preds.combined<-adjust.forcing(All.preds.combined,'ForcedMigrate','SmoothDogfish',sim.month =((i*12)+10):((i*12)+12),bymonth = T,value=.33)  
}

All.preds.combined<-adjust.forcing(All.preds.combined,'ForcedMigrate','Sharks',sim.month = 3:5,bymonth = T,value=.33)
All.preds.combined<-adjust.forcing(All.preds.combined,'ForcedMigrate','Sharks',sim.month = 10:12,bymonth = T,value=-.33)
for (i in 0:49){
        All.preds.combined<-adjust.forcing(All.preds.combined,'ForcedMigrate','Sharks',sim.month =((i*12)+3):((i*12)+5),bymonth = T,value=.33)
        All.preds.combined<-adjust.forcing(All.preds.combined,'ForcedMigrate','Sharks',sim.month =((i*12)+10):((i*12)+12),bymonth = T,value=-.33)  
}

write.csv(All.preds.combined$forcing$ForcedMigrate,file="Check_forcing_allpreds.csv")

#Calculate 50th year biomass
All.preds.baseline.50<-rsim.run(All.preds.baseline,method='AB',50)
All.preds.increased.50<-rsim.run(All.preds.increased,method='AB',50)
All.preds.extended.50<-rsim.run(All.preds.extended,method='AB',50)
All.preds.combined.50<-rsim.run(All.preds.combined,method='AB',50)

#Create table relative to baseline
All.preds.results<-as.data.frame(All.preds.baseline.50$end_state$Biomass[2:51],row.names=MAB.groups$RPATH)
All.preds.results<-cbind(All.preds.results,All.preds.increased.50$end_state$Biomass[2:51])
All.preds.results<-cbind(All.preds.results,All.preds.extended.50$end_state$Biomass[2:51])
All.preds.results<-cbind(All.preds.results,All.preds.combined.50$end_state$Biomass[2:51])
colnames(All.preds.results)<-c("Baseline","Increased","Extended","Combined")

All.preds.results$Rel.bas<-All.preds.results$Baseline/All.preds.results$Baseline
All.preds.results$Rel.inc<-All.preds.results$Increased/All.preds.results$Baseline
All.preds.results$Rel.ext<-All.preds.results$Extended/All.preds.results$Baseline
All.preds.results$Rel.com<-All.preds.results$Combined/All.preds.results$Baseline

#Update table to show percent change
All.preds.results$Rel.bas<-(1-All.preds.results$Rel.bas)*-1
All.preds.results$Rel.inc<-(1-All.preds.results$Rel.inc)*-1
All.preds.results$Rel.ext<-(1-All.preds.results$Rel.ext)*-1
All.preds.results$Rel.com<-(1-All.preds.results$Rel.com)*-1

## All predators
# Increased
Plot_all.preds_inc<-data.frame(Rpath = rownames(All.preds.results),Rel.biomass = All.preds.results$Rel.inc)
pdf("all.preds_increased_draft.pdf",width = 8,height = 7) ## Changes per plot
ap_inc_all_plot<-ggplot(Plot_all.preds_inc[c(-36,-41,-43),], aes(x = Rpath, y = Rel.biomass)) + ## Change per species
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'A) Range shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) #+
        #ylim(-0.0002,0.0004)
dev.off()

Plot_all.preds_inc_lower<-Plot_all.preds_inc[c(6,13,17,20,21,23,24,32,38),]
pdf("all.preds_increased_lower_draft.pdf",width = 8,height = 7) ## Changes per plot
ap_inc_lower_plot<-ggplot(Plot_all.preds_inc_lower, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'A) Range shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.00002,0.00001)
dev.off()

Plot_all.preds_inc_inverts<-Plot_all.preds_inc[c(1,5,16,19,27,30),]
pdf("all.preds_increased_inverts_draft.pdf",width = 8,height = 7) ## Changes per plot
ap_inc_inverts_plot<-ggplot(Plot_all.preds_inc_inverts, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'A) Range shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.001,0.001)
dev.off()


Plot_all.preds_inc_fish<-Plot_all.preds_inc[c(2,3,4,8,9,10,11,12,14,22,25,28,29,33,34,37,39,40,42,44:50),]
pdf("all.preds_increased_fish_draft.pdf",width = 8,height = 7) ## Changes per plot
ap_inc_fish_plot<-ggplot(Plot_all.preds_inc_fish, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'A) Range shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.02,0.01)
dev.off()


Plot_all.preds_inc_pred<-Plot_all.preds_inc[c(7,15,18,26,31,35),]
pdf("all.preds_increased_pred_draft.pdf",width = 8,height = 7) ## Changes per plot
ap_inc_pred_plot<-ggplot(Plot_all.preds_inc_pred, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'A) Range shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.0005,0.002)
dev.off()

# Extended
Plot_all.preds_ext<-data.frame(Rpath = rownames(All.preds.results),Rel.biomass = All.preds.results$Rel.ext)
pdf("all.preds_extended_draft.pdf",width = 8,height = 7) ## Changes per plot
ap_ext_all_plot<-ggplot(Plot_all.preds_ext[c(-36,-41,-43),], aes(x = Rpath, y = Rel.biomass)) + ## Change per species
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'A) Range shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) #+
        #ylim(-0.0002,0.0004)
dev.off()

Plot_all.preds_ext_lower<-Plot_all.preds_ext[c(6,13,17,20,21,23,24,32,38),]
pdf("all.preds_extended_lower_draft.pdf",width = 8,height = 7) ## Changes per plot
ap_ext_lower_plot<-ggplot(Plot_all.preds_ext_lower, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'B) Phenological shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.00002,0.00001)
dev.off()

Plot_all.preds_ext_inverts<-Plot_all.preds_ext[c(1,5,16,19,27,30),]
pdf("all.preds_extended_inverts_draft.pdf",width = 8,height = 7) ## Changes per plot
ap_ext_inverts_plot<-ggplot(Plot_all.preds_ext_inverts, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'B) Phenological shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.001,0.001)
dev.off()


Plot_all.preds_ext_fish<-Plot_all.preds_ext[c(2,3,4,8,9,10,11,12,14,22,25,28,29,33,34,37,39,40,42,44:50),]
pdf("all.preds_extended_fish_draft.pdf",width = 8,height = 7) ## Changes per plot
ap_ext_fish_plot<-ggplot(Plot_all.preds_ext_fish, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'B) Phenological shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.02,0.01)
dev.off()


Plot_all.preds_ext_pred<-Plot_all.preds_ext[c(7,15,18,26,31,35),]
pdf("all.preds_extended_pred_draft.pdf",width = 8,height = 7) ## Changes per plot
ap_ext_pred_plot<-ggplot(Plot_all.preds_ext_pred, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'B) Phenological shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.0005,0.002)
dev.off()

# Combined
Plot_all.preds_com<-data.frame(Rpath = rownames(All.preds.results),Rel.biomass = All.preds.results$Rel.com)
pdf("all.preds_combined_draft.pdf",width = 8,height = 7) ## Changes per plot
ap_com_all_plot<-ggplot(Plot_all.preds_com[c(-36,-41,-43),], aes(x = Rpath, y = Rel.biomass)) + ## Change per species
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'A) Range shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) #+
        #ylim(-0.0002,0.0004)
dev.off()

Plot_all.preds_com_lower<-Plot_all.preds_com[c(6,13,17,20,21,23,24,32,38),]
pdf("all.preds_combined_lower_draft.pdf",width = 8,height = 7) ## Changes per plot
ap_com_lower_plot<-ggplot(Plot_all.preds_com_lower, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'C) Range shift, Phenological shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.00002,0.00001)
dev.off()

Plot_all.preds_com_inverts<-Plot_all.preds_com[c(1,5,16,19,27,30),]
pdf("all.preds_combined_inverts_draft.pdf",width = 8,height = 7) ## Changes per plot
ap_com_inverts_plot<-ggplot(Plot_all.preds_com_inverts, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'C) Range shift, Phenological shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.001,0.001)
dev.off()


Plot_all.preds_com_fish<-Plot_all.preds_com[c(2,3,4,8,9,10,11,12,14,22,25,28,29,33,34,37,39,40,42,44:50),]
pdf("all.preds_combined_fish_draft.pdf",width = 8,height = 7) ## Changes per plot
ap_com_fish_plot<-ggplot(Plot_all.preds_com_fish, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'C) Range shift, Phenological shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.02,0.01)
dev.off()


Plot_all.preds_com_pred<-Plot_all.preds_com[c(7,15,18,26,31,35),]
pdf("all.preds_combined_pred_draft.pdf",width = 8,height = 7) ## Changes per plot
ap_com_pred_plot<-ggplot(Plot_all.preds_com_pred, aes(x = Rpath, y = Rel.biomass)) +
        geom_bar(stat="identity") +
        theme_bw() +
        labs (y = bquote("Relative biomass"), x = 'Functional group', title = 'C) Range shift, Phenological shift') +
        theme(plot.title=element_text(size = 24,face = 'bold'), axis.title=element_text(size = 20, face = 'bold'), axis.text= element_text(color = 'black', size = 18)) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.x = element_blank()) +
        ylim(-0.0005,0.002)
dev.off()

## Create paneled summary plots ##
sp_lower_plot<-grid.arrange(sp_inc_lower_plot,sp_ext_lower_plot,sp_com_lower_plot, bottom = textGrob("Functional group", gp = gpar(fontface = "bold", cex = 1.5)))
sp_inverts_plot<-grid.arrange(sp_inc_inverts_plot,sp_ext_inverts_plot,sp_com_inverts_plot, bottom = textGrob("Functional group", gp = gpar(fontface = "bold", cex = 1.5)))
sp_fish_plot<-grid.arrange(sp_inc_fish_plot,sp_ext_fish_plot,sp_com_fish_plot, bottom = textGrob("Functional group", gp = gpar(fontface = "bold", cex = 1.5)))
sp_pred_plot<-grid.arrange(sp_inc_pred_plot,sp_ext_pred_plot,sp_com_pred_plot, bottom = textGrob("Functional group", gp = gpar(fontface = "bold", cex = 1.5)))

ap_lower_plot<-grid.arrange(ap_inc_lower_plot,ap_ext_lower_plot,ap_com_lower_plot, bottom = textGrob("Functional group", gp = gpar(fontface = "bold", cex = 1.5)))
ap_inverts_plot<-grid.arrange(ap_inc_inverts_plot,ap_ext_inverts_plot,ap_com_inverts_plot, bottom = textGrob("Functional group", gp = gpar(fontface = "bold", cex = 1.5)))
ap_fish_plot<-grid.arrange(ap_inc_fish_plot,ap_ext_fish_plot,ap_com_fish_plot, bottom = textGrob("Functional group", gp = gpar(fontface = "bold", cex = 1.5)))
ap_pred_plot<-grid.arrange(ap_inc_pred_plot,ap_ext_pred_plot,ap_com_pred_plot, bottom = textGrob("Functional group", gp = gpar(fontface = "bold", cex = 1.5)))

## Create table for additive v non-additive for Spiny dogfish scenarios
spiny_interaction_table<-Spiny.results
spiny_interaction_table$IE<-spiny_interaction_table$Rel.inc+spiny_interaction_table$Rel.ext
spiny_interaction_table$CIE<-spiny_interaction_table$Rel.com/spiny_interaction_table$IE
spiny_interaction_table$change<-spiny_interaction_table$CIE-1
write.csv(spiny_interaction_table,file="spiny_interaction_table.csv")

## Export Spiny results
write.csv(Spiny.results, file = "Spiny_results_fixed.csv")

## Import data file for Spiny dogfish synthesis figures
Spiny_figureinfo<-read.csv(file = "Spiny_figureinfo.csv")

## Plot absolute relative change v TL
plot(Spiny_figureinfo$TL, Spiny_figureinfo$Rel.com.abs,xlab = "Trophic Level", ylab = "Change relative to baseline", pch = 16, cex = 2, col = "black", cex.lab = 1.5, cex.axis = 1.5)
abline(v=4.21, col = "red", lty = 2, lwd = 3)

## Plot absolute relative change v Proportion in diet
attach(Spiny_figureinfo)

plot(Spiny_figureinfo$Diet.prop, Spiny_figureinfo$Rel.com.abs, xlab = "Proportion of spiny dogfish diet", ylab = "Change relative to baseline", pch = 16, cex = 2, col = "black", cex.lab = 1.5, cex.axis = 1.5)

#selected<-c(4,20,29,39,40)
#selected2<-c(41)

#text(Spiny_figureinfo$Diet.prop[selected], Spiny_figureinfo$Rel.com.abs[selected],labels = Spiny_figureinfo$Group[selected], cex = 1.25, pos = 2, col = "Black")
#text(Spiny_figureinfo$Diet.prop[selected2], Spiny_figureinfo$Rel.com.abs[selected2],labels = Spiny_figureinfo$Group[selected2], cex = 1.25, pos = 4, col = "Black")

detach(Spiny_figureinfo)

## 3D Plot: Change v Diet Prop v Mortality
plot3d(Spiny_figureinfo$Rel.com.abs, Spiny_figureinfo$Diet.prop, Spiny_figureinfo$Mortality, xlab = "Proportion of spiny dogfish diet",
       ylab = "Change relative to baseline", zlab = "Mortality from spiny dogfish", size = 10)

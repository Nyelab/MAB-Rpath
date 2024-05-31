
# Set-up ------------------------------------------------------------------


#Install package
#Note that I had to pull from github; CRAN version is depreciated
library(devtools)
install_github('SEELab/enaR',ref='borretts', force = T)
library(enaR); library(sna); library(here); library(dplyr);library(Rpath);library(data.table)

#Load initial model
load(here("outputs/MAB_params_Rpath_no_disc.RData"))
load(here("outputs/MAB_Rpath_no_disc.RData"))

#First Ecosense, convert Rsim outputs to Rpath models
#load(here("outputs/MAB_sense_Rpath_50k_newcopes.RData"))

#Need to calculate respiration, exports, detritus input
#To do so, need to pull unassim, M0, and F (fishing mortality)

#Set up model with group names and types
groups<-as.vector(MAB.rpath$Group)

#Count number of each group type
#ngroups <- nrow(MAB.rpath.params)
nliving <- nrow(MAB.rpath.params$model[Type <  2, ])
ndead   <- nrow(MAB.rpath.params$model[Type == 2, ])

#find index of pp
pp <- which(groups == "Phytoplankton")


# Calculations on original model ------------------------------------------


#Calculate network analysis outputs for original model (balanced)
#Pull diet matrix
diet<-MAB.rpath$DC
#Get consumption values by DC*QB*Biomass
QQ<-matrix(nrow = (nliving + ndead + 1),ncol=nliving)
for (j in 1:nliving){
  QQ[,j]<-diet[,j]*MAB.rpath$QB[j]*MAB.rpath$Biomass[j]
}
#Ignore Imports
QQ<-QQ[1:(nliving+ndead),]
colnames(QQ)<-groups[1:nliving]
rownames(QQ)<-groups[1:(nliving+ndead)]
#Sum discards
#Discards<-rowSums(MAB.rpath$Discards)
#Discards<-Discards[1:(nliving+ndead)]
#Calculate flow to detritus
M0<-MAB.rpath$PB*(1-MAB.rpath$EE)
Detritus<-(M0*MAB.rpath$Biomass+MAB.rpath$QB*MAB.rpath$Biomass*MAB.rpath$Unassim)*MAB.rpath$DetFate[,1]
#Detritus<-(MAB.rpath$QB*MAB.rpath$Biomass*MAB.rpath$Unassim)*MAB.rpath$DetFate[,1]
Detritus<-Detritus[1:(nliving+ndead)]
#Deal with flow to detritus from discards
#Should be equal to all flow to discards minus consumption by SeaBirds(45)
#DetInDisc<-sum(Discards)
#Detritus[(nliving+ndead)]<-DetInDisc
#Flow to detritus from detritus = 0
Detritus[(nliving+1)]<-0
#Bind diet matrix (QQ) with flow to detritus, discards
QQ<-cbind(QQ,Detritus)
#Calculate exports
#First sum catch
Catch<-rowSums(MAB.rpath$Landings)
#Add positive biomass accumulation terms
Export<-Catch+(ifelse(MAB.rpath$BA>0,MAB.rpath$BA*MAB.rpath$Biomass,0))
Export<-Export[1:(nliving+ndead)]
for (i in 1:ndead){
  Export[nliving+i]<-M0[nliving+i]*MAB.rpath$Biomass[nliving+i]
}
#Calculate respiration
#Assume detritus, discards have 0 respiration
Resp<-((1-MAB.rpath$Unassim)*MAB.rpath$QB-MAB.rpath$PB)*MAB.rpath$Biomass
Resp<-ifelse(Resp>0,Resp,0)
Resp<-Resp[1:(nliving+ndead)]
Resp[(nliving+1):(nliving+ndead)]<-0
#Deal with Primary Production
#First, estimate GROSS production = Imports
#P/B in Ecopath model gives NET production
#Ratio of gross:net is going to be fixed based on EMAX
gross_net<-(4251.874+1062.968)/4251.874
gross<-gross_net*MAB.rpath$PB[pp]*MAB.rpath$Biomass[pp]
Resp[pp]<-gross-(MAB.rpath$PB[pp]*MAB.rpath$Biomass[pp])
#Calculate imports
#Negative biomass accumulation terms
#Gross primary production
Import<-abs(ifelse(MAB.rpath$BA<0,MAB.rpath$BA*MAB.rpath$Biomass,0))
Import[pp]<-gross
Import<-Import[1:(nliving+ndead)]
#Trim biomass
Biomass<-MAB.rpath$Biomass[1:(nliving+ndead)]
#Pack the model directly and store
orig.network_MAB<-enaR::pack(flow = QQ,
                         input = Import,
                         export = Export,
                         living = c(rep(TRUE,nliving),rep(FALSE,ndead)),
                         respiration = Resp,
                         storage = Biomass)




#Information analysis of original model
info.orig<-enaAscendency(orig.network_MAB)
info.orig
info.orig<-as.data.frame(info.orig)
info.orig <- info.orig %>% mutate(A.internal.CAP = A.internal/CAP.internal)

#connectance
struc.orig<-enaStructure(orig.network)
#C value is directed, can also use this to calculate undirected connectance
ns<-as.data.frame(struc.orig$ns)
ns$L/(0.5*ns$n*(ns$n-1))

#FCI
flow<-enaFlow(orig.network,balance.override = T)
ns.orig<-as.data.frame(flow$ns)

#flows in original model
#FCI
flow.orig<-enaFlow(orig.network,balance.override = T)
flow.orig<-as.data.frame(cbind(groups[1:(nliving+ndead)],as.numeric(flow.orig$T))) %>% 
  rename(group=V1,orig=V2)

#MTI
mti<- enaMTI(orig.network_MAB,balance.override = T)
M<-mti$M

#control
control<-enaControl(orig.network_MAB)
sc<-as.data.frame(cbind(control$sc,groups[1:(nliving+ndead)]))
colnames(sc)<-c("sc","Group")

#save orig MAB network for comparisons
save(orig.network_MAB,file = "Sarah_R/orig.network_MAB.Rdata")
# Calculations on Ecosense outputs ----------------------------------------


#Load results of Ecosense, conversion of Rsim outputs to Rpath models
load(here("outputs/MAB_sense_50k_2024.RData"))

#Copy initial Rpath parameters
#Alternative scenarios will be the same except for Biomass, PB, QB, Diet, M0
alt<-copy(MAB.rpath.params)
alt.diet<-copy(MAB.rpath.params$diet)

#Count number of each group type
ngroups <- nrow(alt$model)
nliving <- nrow(alt$model[Type <  2, ])
ndead   <- nrow(alt$model[Type == 2, ])
ngear   <- nrow(alt$model[Type == 3, ])


#calculate ena metrics for all ecosense outputs
alt.networks<-as.list(rep(NA,length(MAB_sense)))

for (i in 1:length(alt.networks)) {
  #Copy initial Rpath parameters
  Rpath.alt<-copy(MAB.rpath.params)
  #Copy scenario
  MAB.alt<-MAB_sense[[i]]
  #Assign biomass
  #Ignore Outside
  Biomass<-MAB.alt$B_BaseRef[2:(nliving+ndead+1)]
  #Assign PB
  PB<-MAB.alt$PBopt[2:(nliving+ndead+1)]
  #Assign QB
  QB<-MAB.alt$FtimeQBOpt[2:(nliving+ndead+1)]
  QB[pp]<-0
  #Assign M0
  M0<-MAB.alt$MzeroMort[2:(nliving+ndead+1)]
  #Assign diet
  #Remove first two entries which represent 'outside' flow to 'outside' and 'PP'
  PreyFrom<-MAB.alt$PreyFrom[-c(1,2)]
  PreyTo<-MAB.alt$PreyTo[-c(1,2)]
  predpreyQ<-MAB.alt$QQ[-c(1,2)]
  #Fill consumption matrix
  QQ<-matrix(nrow = (nliving + ndead + 1),ncol=nliving)
  for (j in 1:length(PreyFrom)){
    prey<-PreyFrom[j]
    pred<-PreyTo[j]
    QQ[prey,pred]<-predpreyQ[j]
  }
  #convert NAs to 0s
  QQ[which(is.na(QQ)==T)]<-0
  #Ignore Imports
  QQ<-QQ[1:(nliving+ndead),]
  colnames(QQ)<-groups[1:nliving]
  rownames(QQ)<-groups[1:(nliving+ndead)]
  #Calculate flows to detritus
  Unassim<-MAB.alt$UnassimRespFrac[2:(nliving+ndead+1)]
  DetFate<-MAB.alt$DetFrac[2:(nliving+ndead+1)]
  Detritus<-(M0*Biomass+QB*Biomass*Unassim)*DetFate
  #Detritus<-(model$QB*model$Biomass*model$Unassim)*model$DetFate[,1]
  #Detritus<-Detritus[1:(nliving+ndead)]
  #Deal with flow to detritus from discards
  #Should be equal to all flow to discards minus consumption by SeaBirds(45)
  #DetInDisc<-sum(Discards)
  #Detritus[58]<-DetInDisc-QQ[58,45]
  #Flow to detritus from detritus = 0
  Detritus[(nliving+ndead)]<-0
  #Bind diet matrix (QQ) with flow to detritus, discards
  QQ<-cbind(QQ,Detritus)
  #Calculate exports
  #First sum catch
  Catch<-rowSums(MAB.rpath$Landings)[1:(nliving+ndead)]
  #Add positive biomass accumulation terms
  BA<-MAB.rpath$BA[1:(nliving+ndead)]
  Export<-Catch+(ifelse(BA>0,BA*Biomass,0))
  for (k in 1:ndead){
    Export[nliving+k]<-M0[nliving+k]*Biomass[nliving+k]
  }
  Export<-Export[1:(nliving+ndead)]
  #Calculate respiration
  #Assume detritus, discards have 0 respiration
  Resp<-((1-Unassim)*QB-PB)*Biomass
  Resp<-ifelse(Resp>0,Resp,0)
  Resp<-Resp[1:(nliving+ndead)]
  Resp[(nliving+1):(nliving+ndead)]<-0
  #Deal with Primary Production
  #First, estimate GROSS production = Imports
  #P/B in Ecopath model gives NET production
  #Ratio of gross:net is going to be fixed based on EMAX
  gross_net<-(4251.874+1062.968)/4251.874
  gross<-gross_net*PB[pp]*Biomass[pp]
  Resp[pp]<-gross-(PB[pp]*Biomass[pp])
  #Calculate imports
  #Negative biomass accumulation terms
  #Gross primary production
  Import<-abs(ifelse(BA<0,BA*Biomass,0))
  #EE_Biomass<-ifelse(model$EE>1,(model$EE-1)*model$Biomass,0)
  #Import<-BA_Biomass+EE_Biomass
  Import[pp]<-gross
  Import<-Import[1:(nliving+ndead)]
  #Pack the model directly and store
  alt.networks[[i]]<-enaR::pack(flow = QQ,
                                input = Import,
                                export = Export,
                                living = c(rep(TRUE,nliving),rep(FALSE,ndead)),
                                respiration = Resp,
                                storage = Biomass)
}


#Run ascendancy analysis on all networks
#alt.info<-enaAscendency(alt.networks[[i]])
info_bound<-c()
for (i in 1:length(alt.networks)){
  asc<-enaAscendency(alt.networks[[i]])
  info_bound<-rbind(info_bound,asc)
}
info_bound<-as.data.frame(info_bound)

#classify models as low or high based on lit values (Ulanowicz 2014)
info_bound<-info_bound %>% 
  #mutate(A.CAP.internal=A.internal/CAP.internal) %>% 
  mutate(range = ifelse(ASC.CAP<0.36,"low",ifelse(ASC.CAP>0.44,"high","norm"))) 

ggplot(data=info_bound,aes(x=ASC.CAP,fill=range))+
  geom_histogram()

#pull nodal flows out of each alternate model
flow_T<-c()
for (i in 1:length(alt.networks)){
  alt.flow<-enaFlow(alt.networks[[i]],balance.override=T)
  flow_group<-cbind(groups[1:(nliving+ndead)],alt.flow$T)
  flow_T<-rbind(flow_T,flow_group)
}
#bind with original values
flow_T <- as.data.frame(flow_T)
rownames(flow_T)<-c()
colnames(flow_T)<-c("group","flow")
flow_T$flow<-as.numeric(flow_T$flow)
flow_T$model<-rep(1:length(MAB_sense),each = (nliving+ndead))

#pull nodal flows out of each unbounded model
flow_unbound<-c()
for (i in 1:length(alt.networks)){
  alt.flow<-enaFlow(alt.networks[[i]],balance.override=T)
  flow_group<-cbind(groups[1:(nliving+ndead)],alt.flow$T)
  flow_unbound<-rbind(flow_unbound,flow_group)
}
#bind with original values
flow_unbound <- as.data.frame(flow_unbound)
rownames(flow_unbound)<-c()
colnames(flow_unbound)<-c("group","flow")
flow_unbound$flow<-as.numeric(flow_unbound$flow)

#get means and 90th CI for each group's flow
flow_avg<-flow_T %>% group_by(group) %>% 
  mutate(avg = mean(flow),lower = quantile(flow,0.05),upper = quantile(flow,0.95)) %>%
  select(group,avg,upper,lower) %>% distinct 

#compare alternate flows with starting model
flow_comp <- left_join(flow_avg,flow.orig,by="group") %>% mutate(orig = as.numeric(orig)) %>%
  mutate(diff_orig = (avg-orig)/orig) %>% mutate(test = ifelse(orig<lower | orig > upper,T,F)) %>%
  mutate(sign = ifelse(diff_orig>0,"pos","neg")) 
ns<-c()
for (i in 1:length(alt.networks)){
  asc<-enaFlow(alt.networks[[i]],balance.override = T)
  ns<-rbind(ns,asc$ns)
}
ns<-as.data.frame(ns)
write.csv(ns,"Sarah_R/ns_MAB.csv")

#classify models by ASC cutoff
high_ASC<-flow_T %>% filter (model %in% which(info_bound$range == "high"))
low_ASC<-flow_T %>% filter (model %in% which(info_bound$range == "low"))
norm_ASC<-flow_T %>% filter (model %in% which(info_bound$range == "norm"))
#t-tests for flow differences
#probably a more elegant way to do w
p_values<-c()
for(i in 1:(nliving+ndead)){
  g<-groups[i]
  high<-high_ASC %>% filter(group == g)
  low<-low_ASC %>% filter(group == g)
  norm<-norm_ASC %>% filter(group == g)
  test<-t.test(high$flow,low$flow)
  out<-cbind(test$p.value,g,"high_low")
  p_values<-rbind(out,p_values)
  test<-t.test(high$flow,norm$flow)
  out<-cbind(test$p.value,g,"high_norm")
  p_values<-rbind(out,p_values)
  test<-t.test(norm$flow,low$flow)
  out<-cbind(test$p.value,g,"norm_low")
  p_values<-rbind(out,p_values)
}
p_values<-as.data.frame(p_values) %>% rename(p = V1, comp=V3)
p_values$adjusted<-p.adjust(p_values$p, method = "bonferroni")

p_values<-p_values %>% filter(adjusted<0.05)
flow_T<- flow_T %>% 
  mutate(range = ifelse(model %in% which(info_bound$range == "high"),"high",ifelse(model %in% which(info_bound$range == "low"),"low","norm")))

# plotting ----------------------------------------------------------------
library(showtext);library(RColorBrewer);library(ggplot2); library(forcats)
font_add_google("Roboto","Roboto")
roboto<-"Roboto"
showtext_auto()
my.pal<-brewer.pal(8,"Set2")

ggplot(info,aes(x=ASC.CAP*100))+
  geom_density(fill= "brown",alpha=0.75)+
  geom_vline(xintercept=info.orig$ASC.CAP*100,color=my.pal[4],linetype="dashed",linewidth=1)+
  labs(x="Relative Efficiency (%)",y="density")+
  theme(axis.title = element_text(size=14,family = "Roboto"),
        axis.text = element_text(size=10,family="Roboto"))+
  annotate("text",x=46.25,y=0.06,label="n = 1536",size=6,family="Roboto")

ggplot(info,aes(A.CAP.internal*100))+
  geom_density(fill= "green",alpha=0.75)+
  geom_vline(xintercept=info.orig$A.internal.CAP*100,color=my.pal[4],linetype="dashed",linewidth=1)+  
  labs(x="Relative Efficiency - Internal (%)",y="density")+
  theme(axis.title = element_text(size=14,family = "Roboto"),
        axis.text = element_text(size=10,family="Roboto"))+
  annotate("text",x=38.5,y=0.06,label="n = 1787",size=6,family="Roboto")

ggplot(ns,aes(x=FCI))+
  geom_density(fill= "magenta",alpha=0.75)+
  geom_vline(xintercept=ns.orig$FCI,color="black",linetype="dashed",linewidth=1)+  
  labs(x="FCI",y="density")+
  theme(axis.title = element_text(size=14,family = "Roboto"),
        axis.text = element_text(size=10,family="Roboto"))


flow_comp %>% 
  filter(abs(diff_orig) >=0.05) %>%
  mutate(group=fct_reorder(group,diff_orig))%>%
  ggplot()+
  (aes(x=reorder(group,diff_orig),y=diff_orig*100,fill=sign)) +
  geom_bar(stat = "identity",show.legend = F,alpha=0.75) +
  #scale_y_continuous(breaks = seq(-25, 10, 2)) +
  scale_fill_manual(values = c("#034e7b","#99000d")) +
  labs(y = "Flow difference (%)", x = "") +   
  theme(axis.title = element_text(size=12,family = "Roboto"),
        axis.text = element_text(size=8,family="Roboto"))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#flow distribution comparisons
flow_unbound$test<-"unbounded"
flow_T$test<-"bounded"
flow_comp<-rbind(flow_T,flow_unbound)
ggplot(data=flow_comp,aes(x=flow,fill=test))+
  geom_density(alpha = 0.4)+
  scale_fill_manual(values = c("green","red"))+
  facet_wrap(vars(group),nrow = 6,scales = "free")

#portioned out by ASC values
high_low<-flow_T %>% filter(group %in% p_values$g[which(p_values$comp == "high_low")],range %in% c("high","low")) 
high_norm<-flow_T %>% filter(group %in% p_values$g[which(p_values$comp == "high_norm")],range %in% c("high","norm")) 
norm_low<-flow_T %>% filter(group %in% p_values$g[which(p_values$comp == "norm_low")],range %in% c("norm","low")) 


norm_low %>% mutate(range=fct_relevel(range,"low","norm")) %>% filter(group %in% p_values$g) %>%
  ggplot(aes(x=range,y=flow,color=range))+  
  scale_color_manual(values=c(my.pal[3],my.pal[1]))+
  ggbeeswarm::geom_quasirandom(
    size = 1, width = .2, alpha = .25
  ) +
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 35
  ) + 
  # ggbeeswarm::geom_quasirandom(
  #   size = 2, width = .33, shape = 1, color = "black", stroke = .25
  # )+
  facet_wrap(~group,scales = "free")+
  theme(axis.title = element_text(size=14,family = "Roboto"),
        axis.text = element_text(size=10,family="Roboto"))+
  theme(legend.position = "none")

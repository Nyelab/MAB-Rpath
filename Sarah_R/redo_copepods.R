#Code to reorganize copepod groups so that large copepod group
# contains only Calanus finmarchicus

#Author: Sarah J. Weisberg

# Tue Dec  5 17:28:11 2023 ------------------------------


#redo copepods
world <- ne_countries(scale = "medium", returnclass = "sf")

#load data
ecomon <- read.csv(here("data/EcoMon_Plankton_Data_v3_7-Data.csv"))
#convert data and create year, month, day columns: 
ecomon$date <- as.Date(ecomon$date, format =  "%d-%b-%y")
ecomon$month <- month(ecomon$date)
ecomon$year <- year(ecomon$date)
ecomon$day <- day(ecomon$date)

#combine with the strata dataset 
#load strata
strata <- read_sf(here("data/EcomonStrata_v4b.shp"))
#set coordinate system
st_crs(strata) = 4326 #this is WGS 1984

#make ecomon spatial with the same defined coordinate system
ecomon_sp <- st_as_sf(x = ecomon, 
                      coords = c("lon", "lat"),
                      crs = 4326)

#extract stratum values to points
#get a weird error - need to follow these steps: https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
sf::sf_use_s2(FALSE)
ecomon_extract <- st_intersection(ecomon_sp, strata)
#checkit with a plot 

ecomon_extract <- as.data.frame(ecomon_extract)#note you need to convert it to a data frame to do any sort of aesthetics in ggplot because ggplot doesn't like spatial objects. Could use base R if you want
ecomon <- ecomon_extract

#filter for MAB and SNE only
ecomon_extract <- ecomon_extract %>% filter(Region %in% c("MAB","SNE"))
ecomon <- ecomon_extract

#create season column based on 2 month period
ecomon$season <- ifelse(ecomon$month == 1 | ecomon$month == 2, "JanFeb", ifelse(ecomon$month == 3 | ecomon$month == 4, "MarApril",ifelse(ecomon$month == 5 | ecomon$month == 6, "MayJun",ifelse(ecomon$month == 7 | ecomon$month == 8, "JulAug",ifelse(ecomon$month == 9 | ecomon$month == 10, "SepOct",ifelse(ecomon$month == 11 | ecomon$month == 12, "NovDec", NA))))))

#filter for copepods of interest
#tidy
ecomon_y<-ecomon %>% select(ctyp_10m2,calfin_10m2,pseudo_10m2,tlong_10m2,cham_10m2,para_10m2,mlucens_10m2,oithspp_10m2,clauso_10m2)
ecomon_x<-as.data.frame(cbind(ecomon$year, ecomon$season, ecomon$Name, ecomon$Area))
colnames(ecomon_x)<-c("Year","Season","Stratum","Area")
ecomon_y[is.na(ecomon_y)] <- 0 #NAs should be true 0s
ecomon <- cbind(ecomon_x, ecomon_y)

#pull out species names
ecomon_ynames <- colnames(ecomon_y)

#make long dataframe
ecomon_long <- ecomon %>% pivot_longer(all_of(ecomon_ynames), names_to = "spp")

#make Area numeric
ecomon_long$Area<-as.numeric(ecomon_long$Area)

#do this on a grouped dataset
ecomon_grouped_long <- ecomon_long %>% group_by(Year,Season, Stratum, spp, Area) %>% 
  dplyr::summarise(value = mean(value)) %>% 
  mutate(value = value*Area) #get abundance per stratum
# Jennifer Selgrath
# NOAA CINMS
#
# Goal - sorting out gear and species codes = COMMCD from valuation calculations
# -----------------------------------------------


#----------------------------------------------------------
library(tidyverse); library(ggplot2);library(RColorBrewer); library(colorspace)
#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/Jennifer.Selgrath/Documents/research/R_projects/dsc_valuation/")

# load files, select relevant columns and filter out unneeded data
d0<-read_csv("./data/commcd_dat.csv")%>%  
  mutate(DESCRIPTION=ifelse(COMMCD=="CRBNA","Crab, Unknown Gear",DESCRIPTION),   
         DESCRIPTION=ifelse(COMMCD=="CPSNA","CPS, Unknown Gear",DESCRIPTION),
         DESCRIPTION=ifelse(COMMCD=="DTNA","Dover_Thornyhead, Unknown Gear",DESCRIPTION),
         DESCRIPTION=ifelse(COMMCD=="GRDNA","Other Groundfish, Unknown Gear",DESCRIPTION),
         DESCRIPTION=ifelse(COMMCD=="HALNA","Halibut, Unknown Gear",DESCRIPTION),
         DESCRIPTION=ifelse(COMMCD=="NANA","Unknown species, Unknown Gear",DESCRIPTION),
         DESCRIPTION=ifelse(COMMCD=="OTRNA","Other Species, Unknown Gear",DESCRIPTION),
         DESCRIPTION=ifelse(COMMCD=="SAMNA","Salmon, Unknown Gear",DESCRIPTION),
         DESCRIPTION=ifelse(COMMCD=="SBLNA","Sablefish, Unknown Gear",DESCRIPTION),
         DESCRIPTION=ifelse(COMMCD=="SHPNA","Shrimp, Unknown Gear",DESCRIPTION),
         
         DESCRIPTION=ifelse(COMMCD=="NAFX","Unknown species, Fixed Gear",DESCRIPTION),   
         DESCRIPTION=ifelse(COMMCD=="NANA","Unknown species, Unknown Gear",DESCRIPTION),
         DESCRIPTION=ifelse(COMMCD=="NANT","Unknown species, Net",DESCRIPTION),
         DESCRIPTION=ifelse(COMMCD=="NAOTRG","Unknown species, Other Gear",DESCRIPTION),
         DESCRIPTION=ifelse(COMMCD=="NATW","Unknown species, Trawl",DESCRIPTION))%>%
  glimpse()



d1<-read_csv("./results/ec_port_all_long.csv")%>%  glimpse()
d2<-read_csv("./results/ec_state_all_long.csv")%>%  glimpse()
# d3<-read_csv("./results/ec_annual_all_long.csv") %>%  glimpse() # no commodity codes so can't match
d4<-read_csv("./results/ec_noMult_all_long.csv")%>%  glimpse()

d2%>%filter(COMMCD=="CRBFX")

# merge files ------------------------
d1a<-d1%>%
  left_join(d0)%>%
  arrange(year)%>%
  glimpse()

d2a<-d2%>%
  left_join(d0)%>%
  arrange(year)%>%
  glimpse()

# d3a<-d3%>%
#   left_join(d0)%>%
#   arrange(year)%>%
#   glimpse()

d4a<-d4%>%
  left_join(d0)%>%
  arrange(year)%>%
  glimpse()


# filter out NA categories ------------------------------------------
# filter(COMMCD!="CPSFX" & COMMCD!="CPSNA" & COMMCD!= "CPSNT" & COMMCD!="CPSOTRG" & COMMCD!="CPSTW" & COMMCD!="HMSFX" & COMMCD!="HMSNA" & COMMCD!="HMSNT" & COMMCD!="HMSOTRG" & COMMCD!="HMSTW" & COMMCD!="SAMFX" & COMMCD!="SAMNA" & COMMCD!= "SAMNT" & COMMCD!="SAMOTRG" & COMMCD!="SAMTW" &   & COMMCD!="NANA")%>% 
# NOt these:   "CRBNA" "DTNA" "GRDNA" "HALNA" "NANA" "OTRNA" "SBLNA" "SHPNA"




# looks for NAs in data ---------------------------------------------
d1b<-d1a%>%
  group_by(COMMCD, COMMCD2, DESCRIPTION)%>%
  dplyr::summarize(
    n=n(),
    v_min_bl=min(value_state_allSp,na.rm=T),
    v_max_bl=max(value_state_allSp,na.rm=T)
    # prp_min_bl=min(value_state_bl_prp,na.rm=T),
    # prp_max_bl=max(value_state_bl_prp,na.rm=T)
  )%>%
  arrange(COMMCD)%>%
  glimpse()

view(d1b)

d2b<-d2a%>%
  group_by(COMMCD, COMMCD2, DESCRIPTION)%>%
  dplyr::summarize(
    n=n(),
    v_min_bl=min(value_state_allSp,na.rm=T),
    v_max_bl=max(value_state_allSp,na.rm=T)
    # prp_min_bl=min(value_state_bl_prp,na.rm=T),
    # prp_max_bl=max(value_state_bl_prp,na.rm=T)
  )%>%
  arrange(COMMCD)%>%
  glimpse()

view(d2b)




# check for NAs by year
d7<-d5%>%
  group_by(COMMCD, DESCRIPTION,assoc,year)%>%
  summarize(
    n=n(),
    v_min=min(value_state_1,na.rm=T),
    v_max=max(value_state_1,na.rm=T),
    prp_min=min(value_state_1_prp,na.rm=T),
    prp_max=max(value_state_1_prp,na.rm=T))%>%
  glimpse()
d7
# view(d7) # There are zeros in some years for NA gear. Leaving for now.





# save file
write_csv(d3,"./results/ec_comm_all_long2.csv")
write_csv(d5,"./results/ec_comm_all_long3.csv")
write_csv(d6,"./doc/comm_all_summary.csv")
write_csv(d7,"./doc/comm_all_summary_year.csv")

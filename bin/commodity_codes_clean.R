# Jennifer Selgrath
# NOAA CINMS
#
# Goal - clean commondity code descriptions
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

unique(d0$DESCRIPTION)

write_csv(d0,"./results/commcd_dat2.csv")

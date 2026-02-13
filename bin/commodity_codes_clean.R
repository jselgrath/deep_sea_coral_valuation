# Jennifer Selgrath
# NOAA CINMS
#
# Goal - clean commondity code descriptions
# -----------------------------------------------


#----------------------------------------------------------
library(tidyverse); library(ggplot2);library(RColorBrewer); library(colorspace)
#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/research/r_results/dsc_val_fishticket")

# load files, select relevant columns and filter out unneeded data
d0<-read_csv("./results/fishtix_2010_2024_no_pii5_ca.csv")%>%  
  mutate(iopac_commcd_long=ifelse(iopac_commcd=="CRBNA","Crab, Unknown Gear",iopac_commcd_long),
         iopac_commcd_long=ifelse(iopac_commcd=="CPSNA","CPS, Unknown Gear",iopac_commcd_long),
         iopac_commcd_long=ifelse(iopac_commcd=="DTNA","Dover_Thornyhead, Unknown Gear",iopac_commcd_long),
         iopac_commcd_long=ifelse(iopac_commcd=="GRDNA","Other Groundfish, Unknown Gear",iopac_commcd_long),
         iopac_commcd_long=ifelse(iopac_commcd=="HALNA","Halibut, Unknown Gear",iopac_commcd_long),
         iopac_commcd_long=ifelse(iopac_commcd=="NANA","Unknown species, Unknown Gear",iopac_commcd_long),
         iopac_commcd_long=ifelse(iopac_commcd=="OTRNA","Other Species, Unknown Gear",iopac_commcd_long),
         iopac_commcd_long=ifelse(iopac_commcd=="SAMNA","Salmon, Unknown Gear",iopac_commcd_long),
         iopac_commcd_long=ifelse(iopac_commcd=="SBLNA","Sablefish, Unknown Gear",iopac_commcd_long),
         iopac_commcd_long=ifelse(iopac_commcd=="SHPNA","Shrimp, Unknown Gear",iopac_commcd_long),

         iopac_commcd_long=ifelse(iopac_commcd=="NAFX","Unknown species, Fixed Gear",iopac_commcd_long),
         iopac_commcd_long=ifelse(iopac_commcd=="NANA","Unknown species, Unknown Gear",iopac_commcd_long),
         iopac_commcd_long=ifelse(iopac_commcd=="NANT","Unknown species, Net",iopac_commcd_long),
         iopac_commcd_long=ifelse(iopac_commcd=="NAOTRG","Unknown species, Other Gear",iopac_commcd_long),
         iopac_commcd_long=ifelse(iopac_commcd=="NATW","Unknown species, Trawl",iopac_commcd_long))%>%
  glimpse()

unique(d0$iopac_commcd_long)

write_csv(d0,"./results/fishtix_2010_2024_no_pii5a_ca.csv")

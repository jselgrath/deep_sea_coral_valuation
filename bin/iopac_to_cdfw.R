# Jennifer Selgrath 
# NOAA CINMS
# Deep sea coral valuation
#
# GOAL: join tables with cdfw codes and iopac codes - fill in missing values

#----------------------------------------------------------
library(tidyverse)

# ------------------------
remove(list=ls())
setwd("G:/My Drive/research/r_projects/dsc_valuation")

# cdfw and codes - missing some values
d1<-read_csv("./data/iopac_sp_key.csv")%>%
  mutate(SPGNM_orig=SPGNM,
         cdfw_species_id=SpeciesID)%>%
  filter(SpeciesID!=306&SpeciesID!=340)%>% # remove roe and tilapia (freshwater)
  arrange(cdfw_species_id)%>%
  glimpse()

# SpeciesID - cdfw ID
# SpeciesName - CDFW name
# SPECIES_NAME = IOPAC name
# SPID = IOPAC code
# SPGNM = iopac species group number
# sp_sum = iopac species group code


# fill in missing values
d1%>%
  filter(is.na(SPECIES_NAME))

d2<-d1%>%
  # escolar
  mutate(SPECIES_NAME=if_else(SpeciesName=="Escolar","Unsp. Mackerel",SPECIES_NAME))%>%
  mutate(SPID=if_else(SpeciesName=="Escolar","UMCK",SPID))%>%
  mutate(SPGNM=if_else(SpeciesName=="Escolar",45,SPGNM))%>%
  mutate(SP_SUM=if_else(SpeciesName=="Escolar","CPS",SP_SUM))%>%
  
  # Senorita, wrasse, opah > misc fish
  mutate(SPECIES_NAME=if_else((SpeciesName=="Senorita"|SpeciesName=="Wrasse  rock" | SpeciesName=="Opah"| SpeciesName=="Lizardfish  California" | SpeciesName=="Fish  unspecified"), "Miscellaneous Fish", SPECIES_NAME))%>%
  mutate(SPID=if_else((SpeciesName=="Senorita"|SpeciesName=="Wrasse  rock" | SpeciesName=="Opah"| SpeciesName=="Lizardfish  California" | SpeciesName=="Fish  unspecified"),"MSC2",SPID))%>%
  mutate(SPGNM=if_else((SpeciesName=="Senorita"|SpeciesName=="Wrasse  rock" | SpeciesName=="Opah"| SpeciesName=="Lizardfish  California" | SpeciesName=="Fish  unspecified"),159,SPGNM))%>%
  mutate(SP_SUM=if_else((SpeciesName=="Senorita"|SpeciesName=="Wrasse  rock" | SpeciesName=="Opah"| SpeciesName=="Lizardfish  California" | SpeciesName=="Fish  unspecified"),"OTR",SP_SUM))%>%
  # https://marinespecies.wildlife.ca.gov/opah/false/
  # https://www.pierfishing.com/california-lizardfish/
    
  # sargo, blacksmith, halfmoon > surf perch misc
  mutate(SPECIES_NAME=if_else((SpeciesName=="Opaleye"|SpeciesName=="Halfmoon"|SpeciesName=="Blacksmith"|SpeciesName=="Sargo"),"Surfperch Spp.",SPECIES_NAME))%>%
  mutate(SPID=if_else((SpeciesName=="Opaleye"|SpeciesName=="Halfmoon"|SpeciesName=="Blacksmith"|SpeciesName=="Sargo"),"SRFP",SPID))%>%
  mutate(SPGNM=if_else((SpeciesName=="Opaleye"|SpeciesName=="Halfmoon"|SpeciesName=="Blacksmith"|SpeciesName=="Sargo"),303,SPGNM))%>%
  mutate(SP_SUM=if_else((SpeciesName=="Opaleye"|SpeciesName=="Halfmoon"|SpeciesName=="Blacksmith"|SpeciesName=="Sargo"),"OTR",SP_SUM))%>%
  # https://marinespecies.wildlife.ca.gov/halfmoon/
  # https://marinespecies.wildlife.ca.gov/blacksmith/false/  https://www.pierfishing.com/blacksmith/
  # https://www.pierfishing.com/sargo/https://www.pierfishing.com/sargo/
             
 # flatworm, themiste (peanut worm), sponges > Misc. Fish/Animal
  mutate(SPECIES_NAME=if_else((SpeciesName=="Flatworm  marine"|SpeciesName=="Sponges"|SpeciesName=="Themiste"),"Misc. Fish/Animals",SPECIES_NAME))%>%
  mutate(SPID=if_else((SpeciesName=="Flatworm  marine"|SpeciesName=="Sponges"|SpeciesName=="Themiste"),"MISC",SPID))%>%
  mutate(SPGNM=if_else((SpeciesName=="Flatworm  marine"|SpeciesName=="Sponges"|SpeciesName=="Themiste"),156,SPGNM))%>%
  mutate(SP_SUM=if_else((SpeciesName=="Flatworm  marine"|SpeciesName=="Sponges"|SpeciesName=="Themiste"),"OTR",SP_SUM))%>%
  
  select(-SpeciesID)%>%
  glimpse()
  
d2%>%
  arrange(cdfw_species_id)%>%
  filter(is.na(SPECIES_NAME))

d2%>%
  arrange(cdfw_species_id)%>%
  filter(is.na(SPGNM_orig))%>%
  glimpse()
 
# sAVE
write_csv(d2,"./results/iopac_cdfw_sp_key2.csv")

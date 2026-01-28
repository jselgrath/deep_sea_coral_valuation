# Jennifer Selgrath 
# NOAA CINMS
# Deep sea coral valuation
#
# GOAL: Pull drivers for econ data
# see: https://github.com/allen-chen-noaa-gov/IOPAC_pub
#----------------------------------------------------------
library(tidyverse)

# ------------------------
remove(list=ls())
setwd("G:/My Drive/research/r_projects/dsc_valuation")

# codes
c1<-read_csv("./data/commod_dat.csv")%>%
  select(id_commcd=ID,COMMCD:IMPLAN1,Name=DESCRIPTION)%>%
  select(id_commcd, commcd=COMMCD,Name)%>%
  glimpse()

c2<-read_csv("./data/species_dat.csv")%>%
  select(id_species=ID,SPID:SP_SUM)%>%
  glimpse()

c3<-read_csv("./data/gear_dat.csv")%>%
  select(id_gear=ID,GEAR,gear_name=DESCRIPTION,GEAR_SUM)%>%
  glimpse()



# all multipliers
d1<-read_csv("./results/multipliers_2023.csv")%>%
  separate(Name, 
           into = c("SPECIES_NAME", "gear_name"), 
           sep = ", ", 
           remove = FALSE)%>%
  left_join(c1)%>% # drops a few codes
  # left_join(c2)%>%
  # left_join(c3)%>%
  glimpse()

head(d1)

# ca ports and port complexes
d2<-read_csv("./data/portlist_allCA3.csv")%>%
  mutate(state="CA", Region=PortGroup_IOPAC)%>%
  select(state,Region)%>%
  unique()%>%
  glimpse()

# CA state multipliers
d3<-d1%>%
  filter(Region=="California")%>%
  glimpse()
head(d3)

# port complex multipliers
d4<-d1%>%
  filter(Region!="California"&Region!="Oregon"& Region!="Washington")%>% # remove state multipliers
  inner_join(d2)%>%
  glimpse()

head(d4)

#save
write_csv(d3,"./results/multipliers_2023_ca.csv")
write_csv(d4,"./results/multipliers_2023_port.csv")

# Jenny Selgrath
# NOAA CINMS
# Deep Sea Coral Valuation

# goal: remove freshwater species, roe, and algae
# note: have not checked if this captures all species codes for all species from older data - should check if this becomes relevant
# 2010 included: "Anchovy  deepbody", "Eulachon", "Salmon  coho", "Seabass  totuava", "Limpet  unspecified", "Scallop  rock", "Oyster  giant Pacific", "Oyster  european flat" - but totuava not landed in CA

# ---------------------------------------------------
library(tidyverse)

# ---------------------------------------------------
remove(list=ls())
setwd("G:/My Drive/research/r_projects/dsc_valuation")


# ---------------------------------------
#freshwater assoc data 
d0<-read_csv("./data/dsc_val_associations_freshwater3.csv")%>% # has codes 1973-2024 (note older ones added manually, not included in published version from selgrath 2025.) 
  glimpse()

# fisheries data - -------------------
setwd("G:/My Drive/research/r_projects/dsc_valuation")

d1b<-read_csv("./doc/fishtix_spp_2010_2024.csv")%>%
  mutate(species_id=SpeciesID,species_name=SpeciesName)%>%
  unique()%>%
  glimpse()

# set new wd 
setwd("C:/Users/jennifer.selgrath/Documents/research/r_results/dsc_val_fishticket")

# all years fisheries data
d1<-read_csv("./results/fishtix_1973_2024_no_pii.csv")%>% 
  mutate(species_id=SpeciesID,species_name=SpeciesName)%>%
  glimpse()

# dsc years fisheries data
d1a<-read_csv("./results/fishtix_2010_2024_no_pii.csv")%>% 
  mutate(species_id=SpeciesID,species_name=SpeciesName)%>%
  unique()%>%
  glimpse()


#remove freshwater spp, algae, roe-------------------

#  list of species -------------------
d2b<-d0%>%
  inner_join(d1b,relationship = "many-to-many")%>%
  filter(freshwater==0)%>%
  select(-freshwater)%>%
  # remove algae
  filter(species_name!="Agar"& species_name!="Algae marine" & species_name!="Kelp giant")%>%
  #remove roe
  filter(species_name!="Herring Pacific - roe"& species_name!="Herring Pacific - roe on kelp"&  species_name!="Salmon Roe (Chinook Coho)" )%>%
  filter(species_id!=306 & species_id!= 953 & species_id!= 951& species_id!= 950& species_id!= 122 & species_id!= 995 & species_id!= 410)%>% #410 is totauba - not landed in CA
  unique()%>%
  # for figuring out what is joining
  glimpse()
  
unique(d2b$species_id)

# all years -------------
#remove freshwater spp, algae, roe
d2<-d0%>%
  inner_join(d1,relationship = "many-to-many")%>%
    filter(freshwater==0)%>%
    select(-freshwater)%>%
  # remove algae
  filter(species_name!="Agar"& species_name!="Algae marine" & species_name!="Kelp giant")%>%
  #remove roe
  filter(species_name!="Herring Pacific - roe"& species_name!="Herring Pacific - roe on kelp"&  species_name!="Salmon Roe (Chinook Coho)" )%>%
  filter(species_id!=306 & species_id!= 953 & species_id!= 951& species_id!= 950& species_id!= 122 & species_id!= 995& species_id!= 410)%>%
  # for figuring out what is joining
  glimpse()

# check 
d2%>%filter(species_name=="Grenadier")%>%
  glimpse() # species id = 198 (3 species with same code FYI)

# dsc years -------------
#remove freshwater spp, algae, roe
d2a<-d0%>%
  inner_join(d1a,relationship = "many-to-many")%>%
  filter(freshwater==0)%>%
  select(-freshwater)%>%
  # remove algae
  filter(species_name!="Agar"& species_name!="Algae marine" & species_name!="Kelp giant")%>%
  #remove roe
  filter(species_name!="Herring Pacific - roe"& species_name!="Herring Pacific - roe on kelp"&  species_name!="Salmon Roe (Chinook Coho)" )%>%
  filter(species_id!=306 & species_id!= 953 & species_id!= 951& species_id!= 950& species_id!= 122 & species_id!= 995& species_id!= 410)%>%
  # for figuring out what is joining
  glimpse()

# check 
d2a%>%filter(species_name=="Grenadier")%>%
  glimpse() # species id = 198 (3 species with same code FYI)





# -----------------
write_csv(d2,"./results/fishtix_1973_2024_no_pii2.csv")
write_csv(d2a,"./results/fishtix_2010_2024_no_pii2.csv")

setwd("G:/My Drive/research/r_projects/dsc_valuation")
write_csv(d2b,"./results/fishtix_spp_2010_2024_no_fresh.csv")


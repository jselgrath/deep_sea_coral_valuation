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
# has codes 1973-2024 (note older ones added manually, not included in published version from selgrath 2025.) 
d0<-read_csv("./data/dsc_val_associations_freshwater3.csv")%>% 
  mutate(cdfw_species_id=species_id)%>%
  glimpse()

# fisheries data - -------------------
setwd("G:/My Drive/research/r_projects/dsc_valuation")

d1b<-read_csv("./doc/fishtix_spp_2010_2024.csv")%>%
  mutate(cdfw_species_id=SpeciesID,cdfw_species_name=SpeciesName)%>%
  unique()%>%
  glimpse()

# set new wd 
setwd("C:/Users/jennifer.selgrath/Documents/research/r_results/dsc_val_fishticket")

# all years fisheries data
d1<-read_csv("./results/fishtix_1973_2024_no_pii1.csv")%>% 
  glimpse()

# dsc years fisheries data
d1a<-read_csv("./results/fishtix_2010_2024_no_pii1.csv")%>% 
  unique()%>%
  glimpse()


#remove freshwater spp, algae, roe-------------------

#  list of species -------------------
d2b<-d0%>%
  inner_join(d1b,relationship = "many-to-many")%>%
  filter(freshwater==0)%>%
  select(-freshwater)%>%
  # remove algae
  filter(cdfw_species_name!="Agar"& cdfw_species_name!="Algae marine" & cdfw_species_name!="Kelp giant")%>%
  #remove roe
  filter(cdfw_species_name!="Herring Pacific - roe"& cdfw_species_name!="Herring Pacific - roe on kelp"&  cdfw_species_name!="Salmon Roe (Chinook Coho)" )%>%
  filter(cdfw_species_id!=306 & cdfw_species_id!= 953 & cdfw_species_id!= 951& cdfw_species_id!= 950& cdfw_species_id!= 122 & cdfw_species_id!= 995 & cdfw_species_id!= 410)%>% #410 is totauba - not landed in CA
  unique()%>%
  select(-SpeciesID,-SpeciesName)%>%
  glimpse()
  
unique(d2b$cdfw_species_id)

# all years -------------
#remove freshwater spp, algae, roe
d2<-d0%>%
  inner_join(d1,relationship = "many-to-many")%>%
    filter(freshwater==0)%>%
    select(-freshwater)%>%
  # remove algae
  filter(cdfw_species_name!="Agar"& cdfw_species_name!="Algae marine" & cdfw_species_name!="Kelp giant")%>%
  #remove roe
  filter(cdfw_species_name!="Herring Pacific - roe"& cdfw_species_name!="Herring Pacific - roe on kelp"&  cdfw_species_name!="Salmon Roe (Chinook Coho)" )%>%
  filter(cdfw_species_id!=306 & cdfw_species_id!= 953 & cdfw_species_id!= 951& cdfw_species_id!= 950& cdfw_species_id!= 122 & cdfw_species_id!= 995& cdfw_species_id!= 410)%>%
  glimpse()

# check 
d2%>%filter(cdfw_species_name=="Grenadier")%>%
  glimpse() # species id = 198 (3 species with same code FYI)

# dsc years -------------
#remove freshwater spp, algae, roe
d2a<-d0%>%
  inner_join(d1a,relationship = "many-to-many")%>%
  filter(freshwater==0)%>%
  select(-freshwater)%>%
  # remove algae
  filter(cdfw_species_name!="Agar"& cdfw_species_name!="Algae marine" & cdfw_species_name!="Kelp giant")%>%
  #remove roe
  filter(cdfw_species_name!="Herring Pacific - roe"& cdfw_species_name!="Herring Pacific - roe on kelp"&  cdfw_species_name!="Salmon Roe (Chinook Coho)" )%>%
  filter(cdfw_species_id!=306 & cdfw_species_id!= 953 & cdfw_species_id!= 951& cdfw_species_id!= 950& cdfw_species_id!= 122 & cdfw_species_id!= 995& cdfw_species_id!= 410)%>%
  glimpse()

# check 
d2a%>%filter(cdfw_species_name=="Grenadier")%>%
  glimpse() # species id = 198 (3 species with same code FYI)





# -----------------
write_csv(d2,"./results/fishtix_1973_2024_no_pii2.csv") 
write_csv(d2a,"./results/fishtix_2010_2024_no_pii2.csv")

setwd("G:/My Drive/research/r_projects/dsc_valuation")
write_csv(d2b,"./results/fishtix_spp_2010_2024_no_fresh.csv")


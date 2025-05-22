# Jenny Selgrath
# NOAA CINMS
# Deep Sea Coral Valuation

# goal: remove freshwater species, roe, and algae
# note: have not checked if this captures all species codes for all species from older data - should check if this becomes relevant

# ---------------------------------------------------
library(tidyverse)

# ---------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/dsc_valuation/")

# ---------------------------------------
#freshwater assoc
d0<-read_csv("./data/dsc_val_associations_freshwater2.csv")%>%
  glimpse()

# all years fisheries data
d1<-read_csv("./results/fishtix_1973_2024_no_pii.csv")%>% 
  mutate(species_id=SpeciesID,species_name=SpeciesName)%>%
  glimpse()

#remove freshwater spp, algae, roe
d2<-d0%>%
  inner_join(d1,relationship = "many-to-many")%>%
    filter(freshwater==0)%>%
    select(-freshwater)%>%
  #   # # remove algae
  filter(species_name!="Agar"& species_name!="Algae marine" & species_name!="Kelp giant")%>%
  #   # #remove roe
  filter(species_name!="Herring Pacific - roe"& species_name!="Herring Pacific - roe on kelp"&  species_name!="Salmon Roe (Chinook Coho)" )%>%
  # for figuring out what is joining
  glimpse()

# check 
d2%>%filter(species_name=="Grenadier")%>%
  glimpse() # species id = 198 (3 species with same code FYI)



# -----------------
write_csv(d2,"./results/fishtix_1973_2024_no_pii2.csv")

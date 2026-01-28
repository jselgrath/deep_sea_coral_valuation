# Jenny Selgrath
# NOAA CINMS
# Deep Sea Coral Valuation

# goal: update counts of species with updated dataset

# association variable details ----------------------
# ------------------------------------------------------------
# body length
# proximity
# habitat & depth
# --------------------------------------------------------------


# ---------------------------------------------------
library(tidyverse)

# ---------------------------------------------------
remove(list=ls())
setwd("G:/My Drive/research/r_projects/dsc_valuation")

# list of species caught 2010-2024
d1<-read_csv("./results/fishtix_spp_2010_2024_no_fresh.csv")%>% # if update names I can use this other one: ("./results/fishtix_spp_2010_2020_new_mult.csv")%>%  # these are species with updated codes to match more implan codes
  mutate(species_id=SpeciesID)%>%   # change to SpeciesID_orig if update names for IMPLAN merge
  select(-species_name)%>%
  # unique()%>%
  glimpse()

#load dsc-marine life association dataset (2024)
d2 <- read.csv("./data/association_long2.csv")%>% 
  select(-new,-ref,-Group)%>%
  glimpse()

d1%>%
  filter(species_id==198) # 3 grenadier spp have 1 species_id (198)


# what species are not in list?
d4b<-d1%>%
  anti_join(d2)%>% # removes sp in dsc list (not caught 2010-2020) - does not keep algae, agar, freshwater spp, and other ones removed in analysis
  arrange(species_id)%>%
  glimpse()

unique(d4b$species_id) # note: # 2010 included: "Anchovy  deepbody", "Eulachon", "Salmon  coho", "Seabass  totuava", "Limpet  unspecified", "Scallop  rock", "Oyster  giant Pacific", "Oyster  european flat" - but totuava not landed in CA. these are 7 extra spp.

# check matching ----------------------------
d4b%>%
  filter(species_id!=SpeciesID) # all match  (if change above, then these are the updated codes for IMPLAN)

# what species are in list?
d4<-d1%>%
  inner_join(d2)%>% # removes sp in dsc list (not caught 2010-2020) - does not keep algae, agar, freshwater spp, and other ones removed in analysis
  arrange(species_id)%>%
  glimpse()

# check for NAs ---------------------------------
d4%>%filter(is.na(adjacent))%>%
  print(n=80) #0

d4%>%filter(is.na(general_prox))%>%
  print(n=80) #0

d4%>%filter(is.na(habitat_depth))%>%
  print(n=50) #0


# save
write_csv(d4,"./results/association_long_2010_2024.csv")

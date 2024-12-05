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
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/dsc_valuation/")

# list of species caught 2010-2020
d1<-read_csv("./doc/fishtix_spp_2010_2020.csv")%>%# if update names I can use this other one: ("./results/fishtix_spp_2010_2020_new_mult.csv")%>%  # these are species with updated codes to match more implan codes
  glimpse()


#load dsc-marine life association dataset (2024)
d2 <- read.csv("C:/Users/jennifer.selgrath/Documents/research/R_projects/dsc_associations_fishery/results/association_long.csv")%>% 
  glimpse()

d2%>%
  filter(species_id==198) # 3 duplicates because 3 grenadier spp have 1 species_id (198)

# merge - will include NAs where species are not 
d4<-d1%>%
  mutate(species_id=SpeciesID)%>%   # change to SpeciesID_orig if update names for IMPLAN merge
  inner_join(d2)%>% # removes sp in dsc list (not caught 2010-2020) - does not keep algae, agar, freshwater spp, and other ones removed in analysis
  arrange(species_id)%>%
  # filter(species_id!= 953& species_id!= 951)%>% # remove algae and agar
  select(-Group)%>% # duplicate, was for graphing
  glimpse()

# check matching ----------------------------
d4%>%
  filter(species_id!=SpeciesID) # all match  (if change above, then these are the updated codes for IMPLAN)
# d4%>%
#   filter(SpeciesID_orig!=species_id) 


# check for NAs ---------------------------------
d4%>%filter(is.na(assoc_body_length))%>%
  print(n=80) #72

d4%>%filter(is.na(assoc_proximity))%>%
  print(n=80) #65

d4%>%filter(is.na(assoc_habitat))%>%
  print(n=50) #33


# save
write_csv(d4,"./results/association_long_2010_2020.csv")

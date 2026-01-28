# Jenny Selgrath 
# NOAA ONMS
# jan 2024
# project: value for deep sea coral associated species caught by commercial fisheries

# goal: identify species that do not have multipliers and assign similar species codes

# -------------------------------------------------------------------------------
## References - i-o multipliers ----------------------
## Technical Appendix for 2013 Commercial Fisheries report
# https://nmssanctuaries.blob.core.windows.net/sanctuaries-prod/media/archive/science/socioeconomic/farallones/pdfs/techapp13.pdf

# ------------------------------------------------------------
library(tidyverse); library(lubridate); 
# library(scales); library(cowplot); 
# library(Kendall); library(zoo); library(imputeTS); library(stringr)
# ----------------------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/r_results/dsc_val_fishticket") 
setwd("G:/My Drive/research/r_projects/dsc_valuation")


d0<-read_csv("./doc/fishtix_spp_2010_2024.csv")%>%
  mutate(species_id_orig=SpeciesID)%>% # save original number
  mutate(species_name_orig=SpeciesName)%>% # save original number
  glimpse

# swithed number to get list of species names below
d0%>%
  filter(species_id_orig==146)%>%
  select(species_id_orig,species_name_orig)

# these were identified when further through code via what did not match
# unmatched species - dsc assoc - info

#  -1 ??? - not in list
#  15 Escolar NA - mackrel
# 144 Senorita 0
# 146 Wrasse  rock 0
# 306 Salmon  Roe (Chinook  Coho) NA
# 340 ???
# 467 Opah NA
# 473 Lizardfish  California 1
# 475 Opaleye 0
# 478 Halfmoon - 0 - called blue perch - https://marinespecies.wildlife.ca.gov/halfmoon/
# 479 Blacksmith 0 - - perch/damselfish - https://marinespecies.wildlife.ca.gov/blacksmith/false/  https://www.pierfishing.com/blacksmith/
# 480 Sargo - 0 - perch - https://www.pierfishing.com/sargo/
# 689 Flatworm  marine - NA - only 3 records
# 760 Sponges - NA
# 851 Themiste - peanut worm
# 999 Fish  unspecified - NA

# assigning unmatched species to close species, with similar depth and catch patterns----------------------
d1<-d0%>%
  mutate(species_id_orig=SpeciesID)%>% # save original number
  mutate(species_name_orig=SpeciesName)%>% # save original number

  mutate(SpeciesName=if_else(SpeciesName=="Escolar","Mackerel jack",SpeciesName))%>%
  mutate(SpeciesName=if_else(SpeciesName=="Senorita","Surfperch pile",SpeciesName))%>%
  mutate(SpeciesName=if_else(SpeciesName=="Wrasse rock","Surfperch pile",SpeciesName))%>%
  
  mutate(SpeciesID=if_else(SpeciesID==15,55, SpeciesID))%>%#Escolar > species_name = Mackerel jack
  mutate(SpeciesID=if_else(SpeciesID==144,559, #Senorita > species_name =  Surfperch pile
           if_else(SpeciesID==146,559, # rock wrasse > species_name =  Surfperch  pile
             # if_else(SpeciesID==306,,  # not in data? - roe removed
             #  if_else(SpeciesID==340,, # not in data? - Tilapia - should be removed
                if_else(SpeciesID==467,481, # opah > Dolphinfish
                  if_else(SpeciesID==473,483, #lizardfish > longjaw mudsucker
                    if_else(SpeciesID==475,559, # > Surfperch pile
                      if_else(SpeciesID==478,559, # > Surfperch pile
                       if_else(SpeciesID==479,559, # > Surfperch pile
                        if_else(SpeciesID==480,559,SpeciesID)))))))))%>% # > Surfperch pile
                         # if_else(SpeciesID==689,, # not in data?
                          # if_else(SpeciesID==760,, # not in data?
                          #  if_else(SpeciesID==851,, # not in data?
                            # if_else(SpeciesID==999,655,SpeciesID))))))))))))))%>% #> Rockfish copper
  glimpse()
  
  filter (d1%>%
            filter(SpeciesID==306))


# save --------------
write_csv(d1,"./results/fishtix_spp_2010_2020_new_mult.csv")



# 
# d1<-read_csv("./results/triptix_dsc2.csv")%>%
#   mutate(species_id_orig=species_id)%>% # save original number
#   glimpse()
# 
# # species key for CDFW SpeciesName variable to IO-PAC species groupings ----------------------
# # for commodity multipliers
# sp_key_cdfw_iopac <- read.csv("./data/IMPLAN/sp_key_iopac.csv")%>% 
#   mutate(species_id=SpeciesID)%>% 
#   mutate(species_name_io=SPECIES_NAME)%>%
#   select(species_id, SPID, SP_SUM, SPGNM)%>% #species_id==CDFW, others = IOPAC # oringal code only: "SpeciesName", "SP_SUM"
#   arrange(species_id)%>%
#   glimpse()
# 
# # looked visually for unmatched spp
# sp_key_cdfw_iopac %>%
#   filter(SPID=="")
# sp_key_cdfw_iopac 
# 
# # swithed number to get list below
# d1%>%
#   filter(species_id==999)%>%
#   select(species_id,SpeciesName,dsc_b_v3,dsc_b_v2)
# 
# # unmatched species - dsc assoc - info
# #  -1 ???
# #  15 Escolar NA - mackrel
# # 144 Senorita 0
# # 146 Wrasse  rock 0
# # 306 Salmon  Roe (Chinook  Coho) NA
# # 340 ???
# # 467 Opah NA
# # 473 Lizardfish  California 1
# # 475 Opaleye 0
# # 478 Halfmoon - 0 - called blue perch - https://marinespecies.wildlife.ca.gov/halfmoon/
# # 479 Blacksmith 0 - - perch/damselfish - https://marinespecies.wildlife.ca.gov/blacksmith/false/  https://www.pierfishing.com/blacksmith/
# # 480 Sargo - 0 - perch - https://www.pierfishing.com/sargo/
# # 689 Flatworm  marine - NA - only 3 records
# # 760 Sponges - NA
# # 851 ???
# # 999 Fish  unspecified - NA
# 
# # assigning unmatched species to close species
# sp<-d1%>% arrange(SpeciesName)  
# unique (sp$SpeciesName)
# 
# 
# # gear key for CDFW GearName variable to IO-PAC gear groups --------------------
# # for commodity multipliers
# gear_key_cdfw_iopac <- read.csv(file = "./data/IMPLAN/gear_key_iopac.csv")%>%
#   glimpse
# 
# # port key -------------------------
# portkey_IOPAC <- read.csv("./data/portlist_allCA2.csv")%>% 
#   select(c("PortName", "PortGroup_IOPAC"))%>%
#   glimpse()

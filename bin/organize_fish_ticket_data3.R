# Jennifer Selgrath
# NOAA CINMS
# Deep sea coral valuation
#
# GOAL: build commodity codes
#----------------------------------------------------------
library(tidyverse); library(ggplot2)


# github: https://github.com/jselgrath/deep_sea_coral_valuation
# ## See Table 1.3 in 2013 Technical Appendix

#----------------------------------------------------------
remove(list=ls())

# set new wd 
setwd("C:/Users/jennifer.selgrath/Documents/research/r_results/dsc_val_fishticket")


# fishing data -----------------
d1a <- read_csv("./results/fishtix_2010_2024_no_pii2.csv")%>% # of v3? check
  glimpse()

# add shrimp ID for commodity codes below
shrimp_ids <- c(813, 815, 816, 810, 811, 812, 814, 817, 818, 819, 821) # cdfw_species_id of shrimps and prawns

d1<-d1a%>%
  mutate(shrimp_prawn = as.numeric(cdfw_species_id %in% shrimp_ids))%>%
  glimpse()

# main wd ----------------------
setwd("G:/My Drive/research/r_projects/dsc_valuation")

d2<-read_csv("./results/species_key_final.csv")%>%glimpse()

d3<-read_csv("./results/gear_key_final.csv")%>% glimpse()


# Joining dsc species codes and associations  with triptix -----------------------
# LEFT JOIN TO EXCLUDE SPECIES NOT CONSIDERED, INCLUDING INLAND SPECIES AND ALGAE AND ROE
d4 <- d1%>%
  left_join(d2, relationship = "many-to-many")%>%
  select(-species_id)%>%
  glimpse()

# join gear codes with triptix --------------------
d5<-d4%>%
  select(-cdfw_gear_name)%>% # do don not have duplicate values
  left_join(d3,by="cdfw_gear_id",relationship = "many-to-many")%>%
  glimpse()
glimpse(d5)

unique(d5$pacfin_gear_group_code)

# check 
# not all shrimp are captured in TWS but does not matter for this project because iopac = TW for all
d5%>%
filter(shrimp_prawn==1)%>%
  select(cdfw_species_name,cdfw_port_name,cdfw_gear_name,shrimp_prawn,pacfin_gear_group_code,iopac_gear_group)


# build commodity codes --------------------------------------
d6<-d5%>%
  mutate(iopac_commcd =paste0(iopac_species_group_code, iopac_gear_group))%>% # Concatenating SP_SUM (pacfin_group_id) and GEAR_SUM to create variable with commodity multiplier codes
  mutate(COMMCD3 =paste0(iopac_species_group_code, pacfin_gear_group_code))%>%
  glimpse()

# what species are not assigned a pacfin species group
d6%>%
  filter(is.na(iopac_species_group_code))%>%
  select(cdfw_species_id,cdfw_species_name,pacfin_gear_group_code)%>%
  unique()

# save
setwd("C:/Users/jennifer.selgrath/Documents/research/r_results/dsc_val_fishticket")
write_csv(d6,"./results/fishtix_2010_2024_no_pii3.csv")



# original  gear groups from Jack's code:
# ## Original from Table 1.5
# d7 <- d2 %>%
#   mutate(GearGroup = ifelse(GearID %in% c(7,8,9,32), "Troll",
#                             ifelse(GearID %in% c(20,21,22,25,27,38), "Pots/Traps",
#                                    ifelse(GearID %in% c(3,4,5,30,31), "Longlines",
#                                           ifelse(GearID %in% c(1,2,6), "Hook/Line",
#                                                  ifelse(GearID %in% c(13,14,15,18), "Hooka/Diving",
#                                                         ifelse(GearID %in% c(66), "SetGillNet",
#                                                                ifelse(GearID %in% c(33,34,47:59), "Trawl",
#                                                                       ifelse(GearID %in% c(71), "PurseSeine",
#                                                                              ifelse(GearID %in% c(35,40,73,74,78), "OtherSeine/DipNets",
#                                                                                     ifelse(GearID %in% c(65), "DriftNet",
#                                                                                            ifelse(GearID %in% c(12), "Harpoon/Spear",
#                                                                                                   ifelse(GearID %in% c(83,84), "BuoyGear",
#                                                                                                          ifelse(GearID %in% c(0), "Unspecified",
#                                                                                                                 ifelse(GearID %in% c(10,16,17,19,20,23:26,28,
#                                                                                                                                      36,41,42,72,80,90,91,95), "AllOther", "")))))))))))))))%>% glimpse()
  # glimpse()

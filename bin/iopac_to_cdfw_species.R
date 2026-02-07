# Jennifer Selgrath 
# NOAA CINMS
# Deep sea coral valuation
#
# GOAL: join tables with cdfw codes and iopac codes - fill in missing values

# notes:  pacfin_species_group_id=SP_SUM
#         pacfin_species_group_code=SPGNM
#         pacfin_species_id=ID
#         pacfin_species_code=SPID

#----------------------------------------------------------
library(tidyverse)

# ------------------------
remove(list=ls())
setwd("G:/My Drive/research/r_projects/dsc_valuation")


# pacfin translation codes from Brad Stenberg <bstenberg@psmfc.org> -------------------
# species_translations_pacfin2.csv - agency gear code tab
# species_translations_pacfin.csv - pacfin species code

d00<-read_csv("./data/species_translations_pacfin.csv")%>%
  select(pacfin_species_code=PACFIN_SPECIES_CODE,pacfin_management_group_code=MANAGEMENT_GROUP_CODE,pacfin_species_type_code=SPECIES_TYPE_CODE, pacfin_species_common_name=PACFIN_SPECIES_COMMON_NAME,pacfin_species_scientific_name=PACFIN_SPECIES_SCIENTIFIC_NAME,pacfin_species_sort_order=PACFIN_SPECIES_SORT_ORDER,active_date=ACTIVE_DATE,pacfin_comments=COMMENTS)%>% #pacfin_nominal_group_code=NOMINAL_GROUP_CODE,
  unique()%>%
  glimpse()


d0<-read_csv("./data/species_translations_pacfin2.csv")%>%
  filter(AGENCY_CODE=="C")%>% #California
  select(cdfw_species_id=SPECIES_CODE,pacfin_species_code=PACFIN_SPECIES_CODE,d0_species_code_name=SPECIES_CODE_NAME,active_date=ACTIVE_DATE)%>%
  mutate(cdfw_species_id=as.numeric(cdfw_species_id))%>%
  select(-active_date)%>%
  unique()%>%
  glimpse()


# cdfw and pacfin codes from chris free 
# https://chrismfree.com/california-fisheries-data/ca-species-codes/
d1<-read_csv("./data/species_key_cdfw.csv")%>%
  select(cdfw_species_id=spp_code_num,spp_code_chr:discontinue_date)%>%
  glimpse()

# iopac species list from allen chen - see email
d2<-read_csv("./data/species_dat.csv")%>%
  select(pacfin_species_id=ID, pacfin_species_code=SPID, pacfin_species_name=SPECIES_NAME,pacfin_species_group_code=SPGNM,pacfin_species_group_id=SP_SUM)%>% # id is not cdfw ID # pf = pacfin
  glimpse()

# association data modified from selgrath et al 2025
d3<-read_csv("./results/association_long_2010_2024.csv")%>%
  mutate(group_jcs=group)%>%
  select(cdfw_species_id=species_id,SpeciesID:assoc_habitat2,-group,group_jcs)%>%
  glimpse()


# join pacfin datasets from pfmc
d4<-d0%>%
  left_join(d00)%>%
  glimpse()
d4
# view(d4)


# join iopac data from nwfsc
d5<-d4%>%
  left_join(d2)%>%
  glimpse()
# view(d5)


# join assoc data - assuming non-matches are species that were caught pre 2010. right join to drop those species.
d6<-d5%>%
  right_join(d3)%>%
  glimpse()
# view(d6)



# join with free data for clean names
d7<-d6%>%
  left_join(d1)%>%
  select(cdfw_species_id, pacfin_species_id,pacfin_species_code,common_name,species_name, #SpeciesName, pacfin_species_name,pacfin_species_common_name,
         pacfin_species_scientific_name, genus,species,
         SpeciesGroup,species_group2, group_jcs,level,
         depth_min_m:group_jcs,
         spp_code_chr:
         pacfin_species_group_id, pacfin_species_group_code, pacfin_management_group_code,
         pacfin_species_type_code, pacfin_code,
         pacfin_species_sort_order,active_date,discontinue_date)%>%
  select(-SpeciesID,-spp_code_chr,-SpeciesName)%>%
  glimpse()
d7
names(d7)


# save
write_csv(d5,"./results/species_key_final.csv")

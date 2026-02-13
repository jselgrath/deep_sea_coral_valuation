# Jennifer Selgrath 
# NOAA CINMS
# Deep sea coral valuation
#
# GOAL: join tables with cdfw codes and iopac codes - fill in missing values

# notes:  iopac_species_group_id=SP_SUM
#         iopac_species_group_code=SPGNM
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
  select(pacfin_species_code=PACFIN_SPECIES_CODE,pacfin_management_group_code=MANAGEMENT_GROUP_CODE,
         # pacfin_species_type_code=SPECIES_TYPE_CODE, 
         # pacfin_species_common_name=PACFIN_SPECIES_COMMON_NAME,
         # pacfin_species_scientific_name=PACFIN_SPECIES_SCIENTIFIC_NAME,
         pacfin_species_sort_order=PACFIN_SPECIES_SORT_ORDER,
         pacfin_spp_active_date=ACTIVE_DATE)%>% #pacfin_nominal_group_code=NOMINAL_GROUP_CODE,pacfin_comments=COMMENTS
  unique()%>%
  glimpse()

d00%>%
  filter(pacfin_species_code=="BMO1")%>%
  glimpse()

d00%>%
  filter(pacfin_species_code=="PHAG")%>%
  glimpse()



d0<-read_csv("./data/species_translations_pacfin2.csv")%>%
  filter(AGENCY_CODE=="C")%>% #California
  select(cdfw_species_id=SPECIES_CODE,pacfin_species_code=PACFIN_SPECIES_CODE,cdfw_species_name=SPECIES_CODE_NAME,cdfw_spp_active_date=ACTIVE_DATE)%>%
  mutate(cdfw_species_id=as.numeric(cdfw_species_id))%>%
  # select(-active_date)%>%
  unique()%>%
  glimpse()

d0%>%
  filter(cdfw_species_id==202)%>%
  glimpse()

d0%>%
  filter(cdfw_species_id==458)%>%
  glimpse()


# cdfw and pacfin codes from chris free 
# https://chrismfree.com/california-fisheries-data/ca-species-codes/
d1<-read_csv("./data/species_key_cdfw.csv")%>%
  select(cdfw_species_id=spp_code_num,comm_name:discontinue_date,pacfin_species_code=pacfin_code)%>%
  glimpse()

# iopac species list from allen chen - see email Jan 2026 - does not have codes for all species
d2<-read_csv("./data/species_dat.csv")%>%
  select(pacfin_species_code=SPID, iopac_species_name=SPECIES_NAME,iopac_species_group_id=SPGNM,iopac_species_group_code=SP_SUM)%>% # id is not cdfw ID # pf = pacfin #pacfin_species_id=ID, 
  glimpse()



# association data modified from selgrath et al 2025
d3<-read_csv("./results/association_long_2010_2024.csv")%>%
  mutate(species_group_jcs=group)%>%
  select(-group)%>%
  glimpse()


# join pacfin datasets from pfmc
d4<-d0%>%
  left_join(d00)%>%
  glimpse()
glimpse(d4)
# view(d4)



# join iopac data from nwfsc to get codes. 
# I was getting joining errors so using different method (codes below) to bring iopac codes forward
d5<-d4%>%
  # select(-cdfw_species_name)%>%
  left_join(d2)%>%
  mutate(iopac_species_group_code=if_else(is.na(iopac_species_group_code)&pacfin_management_group_code=="OTHR","OTR",iopac_species_group_code))%>%
  mutate(iopac_species_group_code=if_else(is.na(iopac_species_group_code)&pacfin_management_group_code=="HMSP","HMS",iopac_species_group_code))%>%
  mutate(iopac_species_group_code=if_else(is.na(iopac_species_group_code)&pacfin_management_group_code=="CPEL","CPS",iopac_species_group_code))%>%
  
  mutate(iopac_species_group_code=if_else(is.na(iopac_species_group_code)&pacfin_management_group_code=="SAMN","SAM",iopac_species_group_code))%>%
  mutate(iopac_species_group_code=if_else(is.na(iopac_species_group_code)&pacfin_management_group_code=="SHLL","OTR",iopac_species_group_code))%>%
  mutate(iopac_species_group_code=if_else(is.na(iopac_species_group_code)&pacfin_management_group_code=="CRAB","CRB",iopac_species_group_code))%>%
  mutate(iopac_species_group_code=if_else(is.na(iopac_species_group_code)&pacfin_management_group_code=="GRND","GRD",iopac_species_group_code))%>%
  glimpse()



# view(d5)

unique(d5$iopac_species_group_code)
unique(d5$pacfin_management_group_code)


  
# join assoc data - assuming non-matches are species that were caught pre 2010. right join to drop those species.
d6<-d5%>%
  select(-cdfw_species_name)%>%
  full_join(d3)%>%
  glimpse()
# view(d6)




# join with free data for clean names
d7<-d6%>%
  select(-cdfw_species_name)%>%
  left_join(d1)%>%
  # select(-cdfw_species_name,-spp_code_chr)%>%
  glimpse()
d7
names(d7)

d2%>%
  filter(pacfin_species_code=="BMO1")%>%
  glimpse()

d6%>%
  filter(cdfw_species_id==202)%>%
  glimpse()



# save
write_csv(d7,"./results/species_key_final.csv")

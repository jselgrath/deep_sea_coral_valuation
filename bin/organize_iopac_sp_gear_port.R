## Jack Eynon & Jenny Selgrath
## NOAA ONMS
## Script to analyze commercial logbook records for ca fisheries and deep sea corals and extract indicators

## References:
## Technical Appendix for 2013 Commercial Fisheries report
# https://nmssanctuaries.blob.core.windows.net/sanctuaries-prod/media/archive/science/socioeconomic/farallones/pdfs/techapp13.pdf
# ---------------------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(scales)
library(cowplot)
library(Kendall)
library(zoo)
library(imputeTS)
# ---------------------------------------------------------------------------------------------------

## Clearing environment and setting WD
remove(list = ls())
setwd("C:/Users/jennifer.selgrath/Documents/research/r_results/dsc_val_fishticket")


# setwd("//aamb-s-clust01/Shared_Data/ONMS/Socioeconomic/California Work/CA Fish & Wildlife Data Agreement 2021/2021 Data Request/2021 CDFW Data Analysis/CDFW R Project 2021/CommercialTripTix/Data/")
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/dsc_valuation/jacks_files_from_danielle/CommercialTripTix")


# read fishing econ data -----------------
d1 <- read_csv("./results/fishtix_2010_2024_no_pii3.csv")%>%
  glimpse()

# ## Grouping species and species/gear combinations in to species groups
# ## See Table 1.3 in 2013 Technical Appendix
# ## **IMPORTANT** Using different groupings for different sanctuaries - (CINMS and CHNMS?) and (MBNMS/GFNMS/CBNMS?)
# ## All other category based on $<1,000 threshold (if greater than separate out)

## ==============================================================================
#### Analysis for economic contribution of DSC-associated species

# loading  DSC associations (see selgrath et al 2025 - fish and fisheries) -----------------------------
setwd("G:/My Drive/research/r_projects/dsc_valuation")
d2 <- read_csv("./results/association_long_2010_2024.csv")%>%
  glimpse()

d2%>%filter(species_id==198) # because 3 grenadier spp under one code it creates many to many join issues. select only one. they all have same associations

# Selecting only "speciesID" and "assoc" vars
d3 <-d2 %>% 
  select(SpeciesID, adjacent, general_prox, habitat_depth, SpeciesGroup, species_group2)%>%
  unique()%>% # filter out two grenadiers
  glimpse()

# Joining dsc associations  with triptix
# NOTE HERE NEEDS TO BE LEFT JOIN WITH DSC LIST TO EXCLUDE SPECIES NOT CONSIDERED, INCLUDING INLAND SPECIES AND ALGAE AND ROE
d4 <- d3%>%
  left_join(d1, by = "SpeciesID",relationship = "many-to-many")%>%
  glimpse()

# --------------------------------------------------
## Adding IO-PAC species and gear categories to bin catch by commodity multipliers
# --------------------------------------------------

# Reading in species key for CDFW SpeciesName variable to IO-PAC species groupings
# for commodity multipliers
sp_key_cdfw_iopac <- read_csv("./data/iopac_sp_key.csv") %>% 
  select(c("SpeciesName", "SP_SUM"))%>%
  glimpse()

# Joining column with IO-PAC species groupings to triptix to build multiplier groups
d5 <- d4%>%
  left_join(sp_key_cdfw_iopac, by = "SpeciesName")%>% 
  glimpse()


# Reading in key for CDFW GearName variable to IO-PAC gear groups
# for commodity multipliers
gear_key_cdfw_iopac <- read_csv("./data/iopac_gear_key.csv")%>%
  glimpse()

# Joining column with IO-PAC gear groupings to triptix to build multiplier groups
d6 <- d5%>%
  left_join(gear_key_cdfw_iopac, by = "GearName")%>%
  mutate(COMMCD =paste0(SP_SUM, GEAR_SUM))%>% # Concatenating SP_SUM and GEAR_SUM to create variable with commodity multiplier codes
  mutate(COMMCD2 =paste(SP_SUM, GEAR_SUM,sep="_"))%>%
  glimpse()

# --------------------------------------------------
### Determining port complex groupings (i.e. IO-PAC port groups)
# --------------------------------------------------

# Exporting list of ports to excel to map to IO-PAC port groups (provided at link - table 9)
# https://www.webapps.nwfsc.noaa.gov/assets/25/1620_08012011_142237_InputOutputModelTM111WebFinal.pdf
portlist <- unique(d6[c("PortName", "PortID")])%>%
  arrange(PortName)%>%
  glimpse()


# Reading in port key # --------------------------------------------------
# *** NOTE: manually grouped ports into port complexes according to above NOAA Tech Memo
# *** For ports in CDFW triptix without corresponding IOPAC port complex, grouped as "California" (and CA mults applied)
# note portlist3 fixes mill creek and big creek and assigns them to monterey - check with Robert about ONMS practices
portkey_IOPAC <- read_csv("./data/portlist_allCA3.csv")%>% 
  select(c("PortName", "PortGroup_IOPAC"))%>%
  unique()%>%
  glimpse()



# Joining IO-PAC port groups to triptix
d7 <- d6%>%
  left_join(portkey_IOPAC, by = "PortName", relationship = "many-to-many")%>%
  arrange(PortGroup_IOPAC)%>%
  glimpse()


# error checking -----------------------------
unique(d7$PortGroup_IOPAC)

d7%>%
  filter(is.na(PortGroup_IOPAC))%>%
  arrange(COMMCD)

d7%>%
  filter(PortGroup_IOPAC=="California")%>%
  select(PortName)%>%
  unique()

# these are all freshwater - omit about 60 records
d8<-d7%>%
  filter(PortGroup_IOPAC!="California")%>%
  glimpse()

# save --------------------------------
write_csv(d8,"./results/fishtix_2010_2024_no_pii4.csv")


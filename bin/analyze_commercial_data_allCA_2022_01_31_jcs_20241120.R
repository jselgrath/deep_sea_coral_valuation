## Jack Eynon & Jenny Selgrath
## NOAA ONMS
## 1/31/2022
## Script to analyze commercial logbook records for each CA sanctuary and extract indicators

## References:
## Technical Appendix for 2013 Commercial Fisheries report
# https://nmssanctuaries.blob.core.windows.net/sanctuaries-prod/media/archive/science/socioeconomic/farallones/pdfs/techapp13.pdf


library(tidyverse)
library(lubridate)
library(scales)
library(cowplot)
library(Kendall)
library(zoo)
library(imputeTS)

## Clearing environment and setting WD
remove(list = ls())
# setwd("//aamb-s-clust01/Shared_Data/ONMS/Socioeconomic/California Work/CA Fish & Wildlife Data Agreement 2021/2021 Data Request/2021 CDFW Data Analysis/CDFW R Project 2021/CommercialTripTix/Data/")
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/dsc_valuation/jacks_files_from_danielle/CommercialTripTix")


# **IMPORTANT** Select 5 year study period 
fiveyears <- c(2015:2019)
tenyears <- c(2010:2020)


triptix <- read_csv("./Results/triptix_allCA.csv")%>% 
  filter(year %in% tenyears)%>% # Filtering triptix for all CA by years of interest (2010-2020)
  glimpse()

## Defining "out" path to write files
# path_out = paste("../Results/", paste(sanctuary),"/", sep = '')

# Checking structure of data
str(triptix)

# Setting Landing Date variable as class "date"
triptix$LandingDate <- as.Date(triptix$LandingDate, format = "%Y-%m-%d")
# Adding "month" variable
triptix1 <- triptix1 %>% mutate(month = month(LandingDate, label = TRUE))

# Cleaning variables
triptix2 <- triptix1 %>% select(-c("X"))

# ===================== Analysis ==================================================

## List of indicators to extract
# - Top species by harvest revenue*
# - Time series: landings by pounds and revenue across all species*
# - Catch trends for top species*
# - Seasonal catch trends (5 year average, stacked bar chart)* e.g. https://media.fisheries.noaa.gov/2021-02/Southeast-COVID-19-Impact-Snapshot-webready.pdf
# - Top gear types by value and pounds*
# - Trends in number of vessels (total reporting catch in Sanctuary and by gear type)
# - Distribution of harvest revenue among vessels (e.g. Table 2.3 in GFNMS commercial report)
# - Vessel dependence on Sanctuary for catch (requires full CA data set)
# - Top ports for landing catch from sanctuary (pounds and value)
# - For top ports, dependency on sanctuary for catch value
# - Revenue distribution by businesses
# - Top CDFW blocks for catch (value)?
# - CPUE trends, using one trip as unit of effort

## =========================================================================================
## Grouping records by "Gear Type" based on methods used in 2013 report
## See Table 1.5 in 2013 Technical Appendix

# Assigning GearName == "Unspecified" and GearID == 0 to records w/ GearName as NA
triptix3 <- triptix2 %>% 
  mutate(GearName = ifelse(is.na(GearName) & is.na(GearID), "Unspecified", GearName))%>%
  mutate(GearID = ifelse(GearName == "Unspecified",0, GearID))

# ## Original from Table 1.5
triptix4 <- triptix3 %>%
  mutate(GearGroup = ifelse(GearID %in% c(7,8,9,32), "Troll",
ifelse(GearID %in% c(20,21,22,25,27,38), "Pots/Traps",
ifelse(GearID %in% c(3,4,5,30,31), "Longlines",
ifelse(GearID %in% c(1,2,6), "Hook/Line",
ifelse(GearID %in% c(13,14,15,18), "Hooka/Diving",
ifelse(GearID %in% c(66), "SetGillNet",
ifelse(GearID %in% c(33,34,47:59), "Trawl",
ifelse(GearID %in% c(71), "PurseSeine",
ifelse(GearID %in% c(35,40,73,74,78), "OtherSeine/DipNets",
ifelse(GearID %in% c(65), "DriftNet",
ifelse(GearID %in% c(12), "Harpoon/Spear",
ifelse(GearID %in% c(0), "Unspecified",
ifelse(GearID %in% c(10,16,17,19,20,23:26,28,
                     36,41,42,72,80,90,91,95), "AllOther", ""))))))))))))))


## Revised groupings based on CHNMS catch - reassigned several GearIDs from "All Other" category
# triptix <- triptix %>%
#   mutate(GearGroup = ifelse(GearID %in% c(7,8,9,32), "Troll",
# ifelse(GearID %in% c(20,21,22,25,27,37,38), "Pots/Traps",
# ifelse(GearID %in% c(3,4,5,30,31), "Longlines",
# ifelse(GearID %in% c(1,2,6), "Hook/Line",
# ifelse(GearID %in% c(13,14,15,18), "Hooka/Diving",
# ifelse(GearID %in% c(66:68), "SetGillNet",
# ifelse(GearID %in% c(33,34,47:59), "Trawl",
# ifelse(GearID %in% c(71), "PurseSeine",
# ifelse(GearID %in% c(35,40,73,74,78), "OtherSeine/DipNets",
# ifelse(GearID %in% c(63,65), "DriftNet",
# ifelse(GearID %in% c(11,12), "Harpoon/Spear",
# ifelse(GearID %in% c(0), "Unspecified",
# ifelse(GearID %in% c(83,84), "BuoyGear", "AllOther"))))))))))))))
# All Other includes 10,16,17,19,20,23:26,28,36,41,42,72,80,90,91,95 (from 2012 grouping)
# and 69,70,99, 60, and 61 (from All Other group for CHNMMS)
# #=============================================================================================
# 
# ## Grouping species and species/gear combinations in to species groups
# ## See Table 1.3 in 2013 Technical Appendix
# ## **IMPORTANT** Using different groupings for different sanctuaries - (CINMS and CHNMS?) and (MBNMS/GFNMS/CBNMS?)
# ## All other category based on $<1,000 threshold (if greater than separate out)
# # COMMENT/UNCOMMENT OUT BELOW SPECIES GROUPINGS BASED ON SANCTUARY
# 

## ==============================================================================\
#### Analysis for economic contribution of DSC-associated species

# loading spreadsheet of DSC associations from Jenny S.
dsc_sp <- read.csv("C:/Users/jennifer.selgrath/Documents/research/R_projects/dsc_associations_fishery/results/association_long.csv")%>%
  glimpse()

dsc_sp%>%filter(species_id==198) # because 3 grenadier spp under one code it creates many to many join issues. select only one. they all have same associations

# Selecting only "speciesID" and "assoc" vars
dsc_sp_join <- dsc_sp %>% 
  select(SpeciesID=species_id, assoc_body_length,assoc_proximity,assoc_habitat, assoc_body_length2,assoc_proximity2,assoc_habitat2,Group)%>%
  unique()%>% # filter out two grenadiers
  glimpse()



# Renaming speciesID to SpeciesID
# colnames(dsc_sp_join)[1] = "SpeciesID"

# Joining associations  with triptix
triptix5 <- left_join(triptix4, dsc_sp_join, by = "SpeciesID",relationship = "many-to-many")%>%
  glimpse()

# ---------------------------------------------------------------------------------------
# SET ASSOCIATION TYPE 
# ------------------------------------------------------------------------------------
# here basing associations on habitat associations (possible, and definite)
triptix_dsc <- triptix5 %>% filter(assoc_habitat !=0)%>% # 
  glimpse()

# --------------------------------------------------
## Adding IO-PAC species and gear categories to bin catch by commodity multipliers
# --------------------------------------------------

# Reading in species key for CDFW SpeciesName variable to IO-PAC species groupings
# for commodity multipliers
sp_key_cdfw_iopac <- read_csv("./data/IMPLAN/sp_key_iopac.csv") %>% 
  select(c("SpeciesName", "SP_SUM"))%>%
  glimpse()

# Joining column with IO-PAC species groupings to triptix to build multiplier groups
triptix_dsc2 <- triptix_dsc%>%
  left_join(sp_key_cdfw_iopac, by = "SpeciesName")%>% 
  glimpse()


# Reading in key for CDFW GearName variable to IO-PAC gear groups
# for commodity multipliers
gear_key_cdfw_iopac <- read_csv("./data/IMPLAN/gear_key_iopac.csv")%>%
  glimpse()

# Joining column with IO-PAC gear groupings to triptix to build multiplier groups
triptix_dsc3 <- left_join(triptix_dsc2, gear_key_cdfw_iopac, by = "GearName")%>%
  glimpse()


# Concatenating SP_SUM and GEAR_SUM to create variable with commodity multiplier codes
triptix_dsc3$COMMCD <- paste(triptix_dsc3$SP_SUM, triptix_dsc3$GEAR_SUM, sep = "")

# --------------------------------------------------
### Determining port complex groupings (i.e. IO-PAC port groups)
# --------------------------------------------------

# Exporting list of ports to excel to map to IO-PAC port groups (provided at link - table 9)
# https://www.webapps.nwfsc.noaa.gov/assets/25/1620_08012011_142237_InputOutputModelTM111WebFinal.pdf
portlist <- unique(triptix_dsc[c("PortName", "PortID")])

# Reading in port key
# *** NOTE: manually grouped ports into port complexes according to above NOAA Tech Memo
# *** For ports in CDFW triptix without corresponding IOPAC port complex, grouped as "California" (and CA mults applied)
portkey_IOPAC <- read_csv("./Results/DSC/portlist_allCA2.csv")%>% 
  select(c("PortName", "PortGroup_IOPAC"))%>%
  glimpse()

# Joining IO-Pac port groups to triptix
triptix_dsc4 <- triptix_dsc3%>%
  left_join(portkey_IOPAC, by = "PortName")%>%
  glimpse()

### Summarizing catch by commodity multiplier groups (i.e. COMMCD)

commsector_catch <- triptix_dsc4 %>% 
  filter(year >= 2010) %>%
  drop_na(Value) %>%
  group_by(LandingReceiptNum, LandingDate, FisherID, VesselID, PortGroup_IOPAC, COMMCD, year) %>% 
  dplyr::summarise(revenue = sum(Value))%>%
  glimpse()

# commsector_catch <- commsector_catch %>% group_by(LandingReceiptNum, LandingDate, FisherID, VesselID, PortGroup_IOPAC, COMMCD) %>%
#   complete(year = 2010:2020, fill = list(revenue = 0))

# --------------------------------------------------
### Linking 2020 commodity multiplier table to catch summary table
# --------------------------------------------------

# Reading in table
comm_mults_2020 <- read.csv(file = "./data/IMPLAN/comm_mults_2020.csv")%>%
  glimpse()
# will need to create separate tables for two spatial resolutions, state and by port

# Renaming PortGroup_IOPAC for consistency in join
colnames(comm_mults_2020)[1] <- "PortGroup_IOPAC"

# Selecting relevant columns
comm_mults_2020b <- comm_mults_2020 %>% select(c("PortGroup_IOPAC", "COMMCD",
                                                "Sector", "Vessel_output",
                                                "Vessel_income", "Vessel_employment",
                                                "Processor_output", "Processor_income",
                                                "Processor_employment", "TotOut",
                                                "TotInc", "TotEmp"))%>%
  glimpse()


# Joining tables
commsector_catch1 <- left_join(commsector_catch, comm_mults_2020b,
                              by = c("PortGroup_IOPAC", "COMMCD"))%>%
  glimpse()

# Creating table for catch groups with no matching multipliers
nomults <- commsector_catch1 %>% 
  filter(is.na(Vessel_output))%>%
  glimpse()

revenue_nomult <- nomults %>% 
  group_by(COMMCD, year) %>% 
  drop_na(revenue) %>%
  dplyr::summarise(revenue = sum(revenue))%>%
  glimpse()



# Summary of annual landings revenue without a corresponding multiplier
nomults_annrev <- revenue_nomult %>% 
  group_by(year) %>%
  dplyr::summarise(Revenue = round(sum(revenue),0))%>%
  glimpse()

# example of unmatched (no corresponding multiplier) category with high catch
# gears used were mainly UNKNOWN, diving, spear
GRDOTRG <- triptix_dsc4 %>% filter(COMMCD == "GRDOTRG")%>%
  glimpse()


econcontr_results <- commsector_catch1 %>%
  mutate(Vessel_output = revenue*Vessel_output,
         Vessel_income = revenue*Vessel_income,
         Vessel_employment = revenue*Vessel_employment,
         Processor_output = revenue*Processor_output,
         Processor_income = revenue*Processor_income,
         Processor_employment = revenue*Processor_employment,
         TotOut = revenue*TotOut,
         TotInc = revenue*TotInc,
         TotEmp = revenue*TotEmp)

# econcontr_SLO_SB <- econcontr_results %>%
#   filter(PortGroup_IOPAC %in% c("MorroBay", "SantaBarbara")) %>%
#   group_by(year) %>%
#   summarise(Revenue = sum(revenue, na.rm = T),
#             Vessel_output = sum(Vessel_output, na.rm = T),
#             Vessel_income = sum(Vessel_income, na.rm = T),
#             Vessel_employment = sum(Vessel_employment, na.rm = T),
#             Processor_output = sum(Processor_output, na.rm = T),
#             Processor_income = sum(Processor_income, na.rm = T),
#             Processor_employment = sum(Processor_employment, na.rm = T),
#             TotOut = sum(TotOut, na.rm = T),
#             TotInc = sum(TotInc, na.rm = T),
#             TotEmp = sum(TotEmp, na.rm = T))
# econcontr_SLO_SB <- round(econcontr_SLO_SB, digits = 0)
# 
# econcontr_SLO_SB_report <- econcontr_SLO_SB %>% select(c(1:2,9:11))



# --------------------------------------------------
## Creating summary of economic contribution of landings for state of CA
# --------------------------------------------------

# Aggregating revenue by year and commodity sector across all ports
# Note: for CHNMS, all ports happen to be in CA
commsector_catch_CA <- commsector_catch %>%
  group_by(LandingReceiptNum, LandingDate, FisherID, VesselID, COMMCD, year) %>%
  dplyr::summarise(revenue = sum(revenue, na.rm = T)) %>%
  mutate(PortGroup_IOPAC = "California")%>%
  glimpse()

# Adding commodity multipliers to table
commsector_catch_CA <- left_join(commsector_catch_CA, comm_mults_2020,
                                 by = c("PortGroup_IOPAC", "COMMCD"))%>%
  glimpse()

# Computing economic contributions for all CA
econcontr_results_CA <- commsector_catch_CA %>%
  mutate(Vessel_output = revenue*Vessel_output,
         Vessel_income = revenue*Vessel_income,
         Vessel_employment = revenue*Vessel_employment,
         Processor_output = revenue*Processor_output,
         Processor_income = revenue*Processor_income,
         Processor_employment = revenue*Processor_employment,
         TotOut = revenue*TotOut,
         TotInc = revenue*TotInc,
         TotEmp = revenue*TotEmp)%>%
  glimpse()

econcontr_summary_CA <- econcontr_results_CA %>% group_by(year) %>%
  dplyr::summarise(Revenue = sum(revenue, na.rm = T),
            Vessel_output = sum(Vessel_output, na.rm = T),
            Vessel_income = sum(Vessel_income, na.rm = T),
            Vessel_employment = sum(Vessel_employment, na.rm = T),
            Processor_output = sum(Processor_output, na.rm = T),
            Processor_income = sum(Processor_income, na.rm = T),
            Processor_employment = sum(Processor_employment, na.rm = T),
            TotOut = sum(TotOut, na.rm = T),
            TotInc = sum(TotInc, na.rm = T),
            TotEmp = sum(TotEmp, na.rm = T))%>%
  glimpse()

econcontr_CA_report <- round(econcontr_summary_CA %>% select(c(1:2,9:11)),0)%>%
  glimpse()


# Creating summary table of unmatched (no corresponding multiplier) annual revenue
nomatch_CA <- commsector_catch_CA %>% filter(is.na(Sector))
nomatch_annrev_CA <- nomatch_CA %>% group_by(year) %>%
  summarise(Revenue = sum(revenue, na.rm = T))
nomatch_annrev_CA <- round(nomatch_annrev_CA, digits = 0)

# multipliers for SB larger than multipliers for CA except for "halibut, trawl", "HMS, fixed gear", "HMS, net",
# "other species, trawl", "salmon, fixed gear", 
# No SB mults for Sablefish trawl, whiting trawl


# --------------------------------------------------
## Writing results to csv
# --------------------------------------------------

# Results for species with known DSC associations# --------------------------------------------------

# write.csv(nomults, file = "../Results/DSC/DSCb_1s/catch_nomults.csv", row.names = F)
# write.csv(commsector_catch, "../Results/DSC/DSCb_1s/block/revenue_bycommsector_wportmults.csv", row.names = F)
# write.csv(commsector_catch_CA, "../Results/DSC/DSCb_1s/block/revenue_bycommsector_wCAmults.csv", row.names = F)
# write.csv(econcontr_results, "../Results/DSC/DSCb_1s/block/econcontributions_portlevel.csv", row.names = F)
# write.csv(econcontr_results_CA, "../Results/DSC/DSCb_1s/block/econcontributions_statelevel.csv", row.names = F)
# write.csv(econcontr_summary_CA, "../Results/DSC/DSCb_1s/econcontributions_statelevel_ann.csv", row.names = F)

# Results for species with known or possible DSC associations# --------------------------------------------------
# write.csv(nomults, file = "../Results/DSC/DSCb_NAs/catch_nomults.csv", row.names = F)
# write.csv(commsector_catch, "../Results/DSC/DSCb_NAs/revenue_bycommsector_wportmults.csv", row.names = F)
# write.csv(commsector_catch_CA, "../Results/DSC/DSCb_NAs/revenue_bycommsector_wCAmults.csv", row.names = F)
# write.csv(econcontr_results, "../Results/DSC/DSCb_NAs/econcontributions_portlevel.csv", row.names = F)
# write.csv(econcontr_results_CA, "../Results/DSC/DSCb_NAs/econcontributions_statelevel.csv", row.names = F)
# write.csv(econcontr_summary_CA, "../Results/DSC/DSCb_NAs/econcontributions_statelevel_ann.csv", row.names = F)

# Results for all species (all CA)# --------------------------------------------------
# write.csv(nomults, file = "../Results/DSC/all_sp/catch_nomults.csv", row.names = F)
# write.csv(commsector_catch, "../Results/DSC/all_sp/revenue_bycommsector_wportmults.csv", row.names = F)
# write.csv(commsector_catch_CA, "../Results/DSC/all_sp/revenue_bycommsector_wCAmults.csv", row.names = F)
# write.csv(econcontr_results, "../Results/DSC/all_sp/econcontributions_portlevel.csv", row.names = F)
# write.csv(econcontr_results_CA, "../Results/DSC/all_sp/econcontributions_statelevel.csv", row.names = F)
# write.csv(econcontr_summary_CA, "../Results/DSC/all_sp/econcontributions_statelevel_ann.csv", row.names = F)
## Creating summary of 5-year average revenue, total output, income, and employment for CA and regional study areas

## Results for summary by CDFW block# --------------------------------------------------
# write.csv(commsector_catch, "../Results/DSC/all_sp/trip/revenue_bycommsector_wportmults.csv", row.names = F)
write.csv(commsector_catch_CA, "../Results/DSC/DSCb_1s/trip/revenue_bycommsector_wCAmults.csv", row.names = F)
# write.csv(econcontr_results, "../Results/DSC/all_sp/trip/econcontributions_portlevel.csv", row.names = F)
# write.csv(econcontr_results_CA, "../Results/DSC/all_sp/trip/econcontributions_statelevel.csv", row.names = F)

test <- econcontr_results %>% filter(year %in% c(2015:2019) & PortGroup_IOPAC %in% c("SantaBarbara","MorroBay")) %>%
  group_by(PortGroup_IOPAC, COMMCD) %>%
  dplyr::summarise(avg.rev = mean(revenue), avg.totOut = mean(TotOut), avg.totInc = mean(TotInc), avg.totEmp = mean(TotEmp))%>%
  glimpse()
test[,3:5] <- round(test[,3:5], digits = 0)
test[,6] <- round(test[,6], digits = 2)
test <- test %>% arrange(desc(avg.rev))%>%
  glimpse()

test2 <- econcontr_results_CA %>% filter(year %in% c(2015:2019)) %>% group_by(PortGroup_IOPAC, COMMCD, year) %>%
  dplyr::summarise(revenue = sum(revenue), TotOut = sum(TotOut), TotInc = sum(TotInc), TotEmp = sum(TotEmp)) %>% ungroup() %>%
  group_by(PortGroup_IOPAC, COMMCD) %>%
  dplyr::summarise(avg.rev = mean(revenue), avg.totOut = mean(TotOut), avg.totInc = mean(TotInc), avg.totEmp = mean(TotEmp))%>%
  glimpse()
test2[,3:5] <- round(test2[,3:5], digits = 0)
test2[,6] <- round(test2[,6], digits = 2)
test2 <- test2 %>% arrange(desc(avg.rev))%>%
  glimpse()

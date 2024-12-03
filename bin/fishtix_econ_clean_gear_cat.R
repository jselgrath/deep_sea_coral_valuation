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
setwd("C:/Users/Jennifer.Selgrath/Documents/research/R_projects/dsc_valuation/")
# setwd("//aamb-s-clust01/Shared_Data/ONMS/Socioeconomic/California Work/CA Fish & Wildlife Data Agreement 2021/2021 Data Request/2021 CDFW Data Analysis/CDFW R Project 2021/CommercialTripTix/Data/")
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/dsc_valuation/jacks_files_from_danielle/CommercialTripTix")


# **IMPORTANT** Select 5 year study period 
tenyears <- c(2010:2020)

# triptix_orig<- read_csv("./results/fishtix_2010_2020.csv")%>%
#   mutate(LandingDate = as.Date(LandingDate, format = "%Y-%m-%d"))%>% # Setting Landing Date variable as class "date"
#   mutate(month = month(LandingDate, label = TRUE))%>%  # Adding "month" variable
#   glimpse()

# value added to orig data - this dataset from Jack
triptix <- read_csv("./data/triptix_allCA.csv")%>%
  filter(year %in% tenyears)%>% # Filtering triptix for all CA by years of interest (2010-2020)
  mutate(LandingDate = as.Date(LandingDate, format = "%Y-%m-%d"))%>% # Setting Landing Date variable as class "date"
  mutate(month = month(LandingDate, label = TRUE))%>%  # Adding "month" variable
  select(-FisherName,-FGVesselName,-BusinessID,-FishBusinessName,-...1)%>% #remove identifiers
  glimpse()


# triptix%>%
#   dplyr::select(SpeciesName)%>%
#   arrange(SpeciesName)%>%
#   unique()%>%
#   print(n=300)

triptix%>%
  filter(SpeciesName=="Halibut  California")%>%
  glimpse()


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
triptix2 <- triptix %>% 
  mutate(GearName = ifelse(is.na(GearName) & is.na(GearID), "Unspecified", GearName))%>%
  mutate(GearID = ifelse(GearName == "Unspecified",0, GearID))

# ## Original from Table 1.5
triptix3 <- triptix2 %>%
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
ifelse(GearID %in% c(83,84), "BuoyGear",
ifelse(GearID %in% c(0), "Unspecified",
ifelse(GearID %in% c(10,16,17,19,20,23:26,28,
                     36,41,42,72,80,90,91,95), "AllOther", "")))))))))))))))%>%
  glimpse()


write_csv(triptix3,"./results/triptix_allCA2.csv")

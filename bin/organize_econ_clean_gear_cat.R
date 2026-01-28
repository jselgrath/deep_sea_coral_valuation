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
# setwd("G:/My Drive/research/r_projects/dsc_valuation")
setwd("C:/Users/jennifer.selgrath/Documents/research/r_results/dsc_val_fishticket")
# setwd("//aamb-s-clust01/Shared_Data/ONMS/Socioeconomic/California Work/CA Fish & Wildlife Data Agreement 2021/2021 Data Request/2021 CDFW Data Analysis/CDFW R Project 2021/CommercialTripTix/Data/")
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/dsc_valuation/jacks_files_from_danielle/CommercialTripTix")

# value added to orig data - this dataset from Jack
d1 <- read_csv("./results/fishtix_2010_2024_no_pii2.csv")%>%
  mutate(LandingDate = as.Date(LandingDate, format = "%Y-%m-%d"))%>% # Setting Landing Date variable as class "date"
  mutate(month = month(LandingDate, label = TRUE))%>%  # Adding "month" variable
  glimpse()

range(d1$year)

# check data with one example
d1%>%
  filter(SpeciesName=="Halibut  California")%>%
  glimpse()


## =========================================================================================
## Grouping records by "Gear Type" based on methods used in 2013 report
## See Table 1.5 in 2013 Technical Appendix

# Assigning GearName == "Unspecified" and GearID == 999 to records w/ GearName as NA
d2 <- d1 %>% 
  mutate(GearName = ifelse(is.na(GearName) & is.na(GearID), "Unspecified", GearName))%>%
  mutate(GearID = ifelse(GearName == "Unspecified",999, GearID))

# ## Original from Table 1.5
d3 <- d2 %>%
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


write_csv(d3,"./results/fishtix_2010_2024_no_pii3.csv")

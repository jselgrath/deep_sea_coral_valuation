# Jennifer Selgrath
# NOAA CINMS
# Deep sea coral valuation
#
# GOAL: calculate revenue  x commodity multipliers

## References:
## Technical Appendix for 2013 Commercial Fisheries report
# https://nmssanctuaries.blob.core.windows.net/sanctuaries-prod/media/archive/science/socioeconomic/farallones/pdfs/techapp13.pdf

# github: https://github.com/jselgrath/deep_sea_coral_valuation
# ## See Table 1.3 in 2013 Technical Appendix

# NOTES:
# multipliers for SB larger than multipliers for CA except for "halibut, trawl", "HMS, fixed gear", "HMS, net",
# "other species, trawl", "salmon, fixed gear", 
# No SB mults for Sablefish trawl, whiting trawl

# will need to create separate tables for two spatial resolutions, state and by port - two sets of multipliers
# ---------------------------------------------------------------------------------------------------
library(tidyverse); library(ggplot2)
library(lubridate)


#----------------------------------------------------------
remove(list=ls())

# fishtix wd
setwd("C:/Users/jennifer.selgrath/Documents/research/r_results/dsc_val_fishticket")

# fishing data -----------------
d1 <- read_csv("./results/fishtix_2010_2024_no_pii4.csv")%>%
  mutate(LandingDate = as.Date(LandingDate, format = "%Y-%m-%d"))%>% # Setting Landing Date variable as class "date"
  select(-LandingReceiptNum,-LandingDate,-cdfw_block_id,-cdfw_black_name,-shrimp_prawn)%>%  # ADJUST THIS INFO FOR BLOCK AND VMS ANALYSES
  glimpse()

# -------------------------
setwd("G:/My Drive/research/r_projects/dsc_valuation")

# note: columns ending in 2 use mean values to fill in empty multipliers 
# updating column names with _m for multiplier

# state/commodity level multipliers (sm)
d2<-read_csv("./results/multipliers_2023_ca2.csv")%>% 
  # mutate(clean_ports = str_replace_all(Region, "[^[:alnum:]]", "") %>% tolower())%>%
  select(iopac_species=SPECIES_NAME, iopac_commcd=commcd, iopac_commcd_long=Name, iopac_sector=Sector,# Selecting relevant columns
         Vessel_output_sm=Vessel_output2,
         Vessel_income_sm=Vessel_income2,
         Vessel_employment_sm=Vessel_employment2,
         Processor_output_sm=Processor_output2,
         Processor_income_sm=Processor_income2,
         Processor_employment_sm=Processor_employment2,
         TotOut_sm=TotOut2,
         TotInc_sm=TotInc2,
         TotEmp_sm=TotEmp2)%>%
  glimpse()
glimpse(d2)

# port multipliers (pm). Region = port complex
d3<-read_csv("./results/multipliers_2023_port2.csv")%>% 
  mutate(clean_ports = str_replace_all(Region, "[^[:alnum:]]", "") %>% tolower())%>%
  select(iopac_port=Region, clean_ports, iopac_species=SPECIES_NAME, iopac_commcd=commcd, iopac_commcd_long=Name, iopac_sector=Sector,# Selecting relevant columns
         Vessel_output_pm=Vessel_output2,
         Vessel_income_pm=Vessel_income2, 
         Vessel_employment_pm=Vessel_employment2,
         Processor_output_pm=Processor_output2, 
         Processor_income_pm=Processor_income2,
         Processor_employment_pm=Processor_employment2,
         TotOut_pm=TotOut2, 
         TotInc_pm=TotInc2, 
         TotEmp_pm=TotEmp2)%>%
  glimpse()
glimpse(d3)  


## START at LINE 59 in "fishtix_econ_dsc_summarize.R"

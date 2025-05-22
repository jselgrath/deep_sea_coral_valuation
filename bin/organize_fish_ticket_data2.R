# Jenny Selgrath
# NOAA CINMS
# Deep Sea Coral Valuation

# goal: subset data for valuation analysis & for recent analysis (post 1994 as advised by CDFW due to change in fishing blocks - see emails notes in google drive):

# text from email: "older data before 2000 does include use of 4-digit block codes.  Outreach occurred between 1992 and 1993, communicating the importance for fishermen to provide the most accurate location using 3-digit block codes for the catch as the 4-digit block codes represented larger catch areas (see attached Southern CA block chart). "

# note: have not checked if freshwater species, roe, and algae list captures all species codes for all species from older data - should check if this becomes relevant

# ---------------------------------------------------
library(tidyverse)

# ---------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/dsc_valuation/")

# ---------------------------------------
# all years fisheries data, no freshwater, algae, roe or PII
d1<-read_csv("./results/fishtix_1973_2024_no_pii2.csv")%>% 
  mutate(species_id=SpeciesID,species_name=SpeciesName)%>%
  glimpse()

# filter for 2010 and after
d2<-d1%>%
  filter(year>=2010)%>%
  glimpse()

# filter for 1994 and after
d3<-d1%>%
  filter(year>=1994)%>%
  glimpse()


# -----------------
write_csv(d2,"./results/fishtix_no_pii_2010_2024.csv")
write_csv(d3,"./results/fishtix_no_pii_1995_2024.csv")

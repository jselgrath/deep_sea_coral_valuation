# Jenny Selgrath
# NOAA CINMS
# Deep Sea Coral Valuation

# goal: identify species that were landed  by commercial fisheries in ca 2010-2020

# association variable details ----------------------
# ------------------------------------------------------------
# dsc_b: does this species use DSC during some or all of its lifehistory? binary response (0,1,NA) - NA if uncertain or not enough data - from Craig
# dsc_b_v2: updated from original values - uses body length for "association"
# dsc_b_v3: updated from original values - uses proximity of any type, broader than body length
# --------------------------------------------------------------


# ---------------------------------------------------
library(tidyverse)

# ---------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/dsc_valuation/")

d0<-read_csv("./data/cdfw_fishtix/2010.csv")%>%
  mutate(year=2010)%>%
  glimpse()
d1<-read_csv("./data/cdfw_fishtix/2011.csv")%>%
  mutate(year=2011)%>%
  glimpse()
d2<-read_csv("./data/cdfw_fishtix/2012.csv")%>%
  mutate(year=2012)%>%
  glimpse()
d3<-read_csv("./data/cdfw_fishtix/2013.csv")%>%
  mutate(year=2013)%>%
  glimpse()
d4<-read_csv("./data/cdfw_fishtix/2014.csv")%>%
  mutate(year=2014)%>%
  glimpse()
d5<-read_csv("./data/cdfw_fishtix/2015.csv")%>%
  mutate(year=2015)%>%
  glimpse()
d6<-read_csv("./data/cdfw_fishtix/2016.csv")%>%
  mutate(year=2016)%>%
  glimpse()
d7<-read_csv("./data/cdfw_fishtix/2017.csv")%>%
  mutate(year=2017)%>%
  glimpse()
d8<-read_csv("./data/cdfw_fishtix/2018.csv")%>%
  mutate(year=2018)%>%
  glimpse()
d9<-read_csv("./data/cdfw_fishtix/2019.csv")%>%
  mutate(year=2019)%>%
  glimpse()
d10<-read_csv("./data/cdfw_fishtix/2020.csv")%>%
  mutate(year=2020)%>%
  glimpse()

d20<-rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)%>%
  glimpse()


# estimate of unique spp from all years (2010-2020)
length(unique(d20$SpeciesID))%>% 
  glimpse() # 327

# unique names and IDs for all species
d21<-d20%>%
  select(SpeciesID,SpeciesName)%>%
  unique()%>%
  arrange(SpeciesID)%>%
  glimpse()

# save
write_csv(d20,"./results/fishtix_2010_2020.csv")
write_csv(d21,"./doc/fishtix_spp_2010_2020.csv")

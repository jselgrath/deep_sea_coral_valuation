# Jenny Selgrath
# NOAA CINMS
# Deep Sea Coral Valuation

# goal: combine landings for cdfw commercial landings data and create option with no PII

# ---------------------------------------------------
library(tidyverse)

# ---------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/research/data/cdfw_2025/MLDS/")
list.files("C:/Users/jennifer.selgrath/Documents/research/data/cdfw_2025/MLDS/")

# original project - 2010 - 2020
d0<-read_csv("./MLR Data Extract_2010.csv")%>%
  mutate(year=2010)%>%
  glimpse()
d1<-read_csv("./MLR Data Extract_2011.csv")%>%
  mutate(year=2011)%>%
  glimpse()
d2<-read_csv("./MLR Data Extract_2012.csv")%>%
  mutate(year=2012)%>%
  glimpse()
d3<-read_csv("./MLR Data Extract_2013.csv")%>%
  mutate(year=2013)%>%
  glimpse()
d4<-read_csv("./MLR Data Extract_2014.csv")%>%
  mutate(year=2014)%>%
  glimpse()
d5<-read_csv("./MLR Data Extract_2015.csv")%>%
  mutate(year=2015)%>%
  glimpse()
d6<-read_csv("./MLR Data Extract_2016.csv")%>%
  mutate(year=2016)%>%
  glimpse()
d7<-read_csv("./MLR Data Extract_2017.csv")%>%
  mutate(year=2017)%>%
  glimpse()
d8<-read_csv("./MLR Data Extract_2018.csv")%>%
  mutate(year=2018)%>%
  glimpse()
d9<-read_csv("./MLR Data Extract_2019.csv")%>%
  mutate(year=2019)%>%
  glimpse()
d10<-read_csv("./MLR Data Extract_2020.csv")%>%
  mutate(year=2020)%>%
  glimpse()

# new years - 00s - # some parsing problem for all years except 2001
d00<-read_csv("./MLR Data Extract_2000.csv")%>% 
  mutate(year=2000)%>%
  glimpse()
d11<-read_csv("./MLR Data Extract_2001.csv")%>%
  mutate(year=2001)%>%
  glimpse()
d12<-read_csv("./MLR Data Extract_2002.csv")%>% 
  mutate(year=2002)%>%
  glimpse()
d13<-read_csv("./MLR Data Extract_2003.csv")%>% 
  mutate(year=2003)%>%
  glimpse()
d14<-read_csv("./MLR Data Extract_2004.csv")%>% 
  mutate(year=2004)%>%
  glimpse()
d15<-read_csv("./MLR Data Extract_2005.csv")%>% 
  mutate(year=2005)%>%
  glimpse()
d16<-read_csv("./MLR Data Extract_2006.csv")%>% 
  mutate(year=2006)%>%
  glimpse()
d17<-read_csv("./MLR Data Extract_2007.csv")%>%
  mutate(year=2007)%>%
  glimpse()
d18<-read_csv("./MLR Data Extract_2008.csv")%>%
  mutate(year=2008)%>%
  glimpse()
d19<-read_csv("./MLR Data Extract_2009.csv")%>%
  mutate(year=2009)%>%
  glimpse()

# new years - 90s
d90<-read_csv("./MLR Data Extract_1990.csv")%>%
  mutate(year=1990)%>%
  glimpse()
d91<-read_csv("./MLR Data Extract_1991.csv")%>%
  mutate(year=1991)%>%
  glimpse()
d92<-read_csv("./MLR Data Extract_1992.csv")%>%
  mutate(year=1992)%>%
  glimpse()
d93<-read_csv("./MLR Data Extract_1993.csv")%>%
  mutate(year=1993)%>%
  glimpse()
d94<-read_csv("./MLR Data Extract_1994.csv")%>%
  mutate(year=1994)%>%
  glimpse()
d95<-read_csv("./MLR Data Extract_1995.csv")%>%
  mutate(year=1995)%>%
  glimpse()
d96<-read_csv("./MLR Data Extract_1996.csv")%>%
  mutate(year=1996)%>%
  glimpse()
d97<-read_csv("./MLR Data Extract_1997.csv")%>%
  mutate(year=1997)%>%
  glimpse()
d98<-read_csv("./MLR Data Extract_1998.csv")%>%
  mutate(year=1998)%>%
  glimpse()
d99<-read_csv("./MLR Data Extract_1999.csv")%>%
  mutate(year=1999)%>%
  glimpse()

# new years - 80s
d80<-read_csv("./MLR Data Extract_1980.csv")%>%
  mutate(year=1980)%>%
  glimpse()
d81<-read_csv("./MLR Data Extract_1981.csv")%>%
  mutate(year=1981)%>%
  glimpse()
d82<-read_csv("./MLR Data Extract_1982.csv")%>%
  mutate(year=1982)%>%
  glimpse()
d83<-read_csv("./MLR Data Extract_1983.csv")%>%
  mutate(year=1983)%>%
  glimpse()
d84<-read_csv("./MLR Data Extract_1984.csv")%>%
  mutate(year=1984)%>%
  glimpse()
d85<-read_csv("./MLR Data Extract_1985.csv")%>%
  mutate(year=1985)%>%
  glimpse()
d86<-read_csv("./MLR Data Extract_1986.csv")%>%
  mutate(year=1986)%>%
  glimpse()
d87<-read_csv("./MLR Data Extract_1987.csv")%>%
  mutate(year=1987)%>%
  glimpse()
d88<-read_csv("./MLR Data Extract_1988.csv")%>%
  mutate(year=1988)%>%
  glimpse()
d89<-read_csv("./MLR Data Extract_1989.csv")%>%
  mutate(year=1989)%>%
  glimpse()

# new years - 70s
d73<-read_csv("./MLR Data Extract_1973.csv")%>%
  mutate(year=1973)%>%
  glimpse()
d74<-read_csv("./MLR Data Extract_1974.csv")%>%
  mutate(year=1974)%>%
  glimpse()
d75<-read_csv("./MLR Data Extract_1975.csv")%>%
  mutate(year=1975)%>%
  glimpse()
d76<-read_csv("./MLR Data Extract_1976.csv")%>%
  mutate(year=1976)%>%
  glimpse()
d77<-read_csv("./MLR Data Extract_1977.csv")%>%
  mutate(year=1977)%>%
  glimpse()
d78<-read_csv("./MLR Data Extract_1978.csv")%>%
  mutate(year=1978)%>%
  glimpse()
d79<-read_csv("./MLR Data Extract_1979.csv")%>%
  mutate(year=1979)%>%
  glimpse()

# new years - 20s
d21<-read_csv("./MLR Data Extract_2021.csv")%>%
  mutate(year=2021)%>%
  glimpse()
d22<-read_csv("./MLR Data Extract_2022.csv")%>%
  mutate(year=2022)%>%
  glimpse()
d23<-read_csv("./MLR Data Extract_2023.csv")%>%
  mutate(year=2023)%>%
  glimpse()
d24<-read_csv("./MLR Data Extract_2024.csv")%>%
  mutate(year=2024)%>%
  glimpse()

# combine 
d40<-rbind(d0,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d21,d22,d23,d24)%>%
  glimpse()
unique(d40$year)

d41<-rbind(d00,d11,d12,d13,d14,d15,d16,d17,d18,d19)%>%
  glimpse()
unique(d41$year)

d42<-rbind(d90,d91,d92,d93,d94,d95,d96,d97,d98,d99)%>%
  glimpse()
unique(d42$year)

d43<-rbind(d80,d81,d82,d83,d84,d85,d86,d87,d88,d89)%>%
  glimpse()
unique(d43$year)

d44<-rbind(d73,d74,d75,d76,d77,d78,d79)%>%
  glimpse()
unique(d44$year)

# all
d50<-rbind(d40,d41,d42,d43,d44)%>%
  glimpse()
unique(d50$year)

# estimate of unique spp from all years (2073-2020) - includes freshwater, roe, algae
length(unique(d50$SpeciesID))%>% 
  glimpse() # 395

# estimate of unique spp from all years (2010-2024) - includes freshwater, roe, algae
length(unique(d40$SpeciesID))%>% 
  glimpse() # 344

# unique names and IDs for all species
d51<-d50%>%
  select(SpeciesID,SpeciesName)%>%
  unique()%>%
  arrange(SpeciesID)%>%
  glimpse()

# remove_pii -------------------------
d53<-d50%>%
  select(-FisherID,-ALDSFisherName,-ETixFisherName,-BusinessID,-FishBusinessName)%>%
  glimpse()

d54<-d40%>%
  select(-FisherID,-ALDSFisherName,-ETixFisherName,-BusinessID,-FishBusinessName)%>%
  glimpse()
  

# save
setwd("C:/Users/Jennifer.Selgrath/Documents/research/R_projects/dsc_valuation/")

write_csv(d50,"./results/fishtix_1973_2024.csv")
write_csv(d51,"./doc/fishtix_spp_1973_2024.csv")
write_csv(d53,"./results/fishtix_no_pii_1973_2024.csv")


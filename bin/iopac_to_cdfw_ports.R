# Jennifer Selgrath
# NOAA CINMS
# Deep sea coral valuation
#
# GOAL: combine port codes to link cdfw, pacfin, iopac codes
#----------------------------------------------------------
library(tidyverse); library(ggplot2)


# github: https://github.com/jselgrath/deep_sea_coral_valuation

#----------------------------------------------------------
# pacfin translation codes from Brad Stenberg <bstenberg@psmfc.org> -------------------



#----------------------------------------------------------
remove(list=ls())
setwd("G:/My Drive/research/r_projects/dsc_valuation")

# pacfin translation codes from psmfc Brad Stenberg <bstenberg@psmfc.org> jan/feb 2026
# port_translations_pacfin.csv - pacfin ports - pacfin_port_translations.xlsx
# port_translations_pacfin2 - agency ports - pacfin_port_translations.xlsx

d1<-read_csv("./data/port_translations_pacfin.csv")%>%
  filter(AGENCY_CODE=="C")%>% #California
  filter(PORT_TYPE_CODE=="1")%>% #California & only ports (1) not groups (2) or all (3)
  mutate(pacfin_port_code=PACFIN_PORT_CODE)%>%
  select(-AGENCY_CODE,-EXPIRE_DATE,-PACFIN_PORT_CODE)%>%
  unique()%>%
  glimpse()
d1
# d1%>%filter(pacfin_port_code=="MCR"|pacfin_port_code=="BCR")%>%view() # MNA = PACFIN CODE


d2<-read_csv("./data/port_translations_pacfin2.csv")%>%
  filter(AGENCY_CODE=="C")%>% #California
  mutate(cdfw_port_id=as.numeric(PORT_CODE),
         cdfw_port_name=PORT_NAME)%>%
  select(cdfw_port_id,cdfw_port_name,pacfin_port_code=PACFIN_PORT_CODE,county_code=COUNTY_CODE,congressional_district=CONGRESSIONAL_DISTRICT,active_date=ACTIVE_DATE, latitude=LATITUDE, longitude=LONGITUDE)%>%
  unique()%>%
  arrange(cdfw_port_id)%>%
  glimpse()
d2

# original port list
d3 <- read_csv("./data/portlist_allCA3.csv")%>% 
  select(iopac_port_complex=PortGroup_IOPAC,iopac_port=PortName,
         cdfw_port_id=PortID)%>%
  unique()%>%
  filter(!is.na(iopac_port_complex))%>%
  arrange(cdfw_port_id)%>%
  glimpse()
d3

# from chris free
d4<-read_csv("./data/port_key_cdfw.csv")%>%
  # mutate(clean_key = str_replace_all(cdfw_port, "[^[:alnum:]]", "") %>% tolower())%>%
  mutate(cdfw_port_id=as.numeric(cdfw_port_code))%>%
  arrange(cdfw_port_id)%>%
  select(cdfw_port_id,cdfw_port_complex,  cdfw_port_complex_code, cdfw_port_code, cdfw_county_code, discontinued_date)%>% #cdfw_port,
  filter(discontinued_date!="1/1/1996"| is.na(discontinued_date))%>%
  filter(discontinued_date != "1/1/1997" | is.na(discontinued_date))%>%
  glimpse()
d4

# using this for names of pacfin port complex 
d5<-read_csv("./data/pacfin_port_complex_names.csv")%>%
  glimpse()
d5


# merge pacfin tables from pfmc ----------------------------
d6<-d2%>%
  full_join(d1)%>%
  
  # fill in missing info
  mutate(SAMPLING_GROUP_CODE=if_else(cdfw_port_id ==-1| cdfw_port_id ==999,"X",SAMPLING_GROUP_CODE))%>%
  mutate(PORT_TYPE_CODE=if_else(cdfw_port_id ==-1| cdfw_port_id ==999,"1",PORT_TYPE_CODE))%>%
  mutate(PACFIN_GROUP_PORT_CODE=if_else(cdfw_port_id ==-1| cdfw_port_id ==999,"CA2",PACFIN_GROUP_PORT_CODE))%>%
  mutate(PACFIN_PORT_NAME=if_else(cdfw_port_id ==-1| cdfw_port_id ==999,"OTHER CAL",PACFIN_PORT_NAME))%>%
  mutate(PACFIN_PORT_DESCRIPTION=if_else(cdfw_port_id ==-1| cdfw_port_id ==999,"OTHER OR UNKNOWN CALIFORNIA PORTS",PACFIN_PORT_DESCRIPTION))%>%
  mutate(PORT_ORDER =if_else(cdfw_port_id ==-1| cdfw_port_id ==999,99,PORT_ORDER ))%>%
  mutate(county_code =if_else(cdfw_port_id ==-1| cdfw_port_id ==999,"UCA",county_code ))%>%
  mutate(congressional_district  =if_else(cdfw_port_id ==-1| cdfw_port_id ==999,"000",congressional_district  ))%>%
  mutate(IS_SUMMARIZED=if_else(cdfw_port_id ==-1| cdfw_port_id ==999,TRUE,IS_SUMMARIZED))%>%
  select(cdfw_port_id,cdfw_port_name,     pacfin_port_code, pacfin_port_complex_code=PACFIN_GROUP_PORT_CODE, pacfin_county_code=county_code,  congressional_district, ACTIVE_DATE, latitude, longitude,  SAMPLING_GROUP_CODE, PORT_TYPE_CODE,PORT_ORDER)%>%
  
  mutate(cdfw_port_id=as.numeric(cdfw_port_id))%>%
  arrange(cdfw_port_id)%>%
  select(-SAMPLING_GROUP_CODE,-PORT_TYPE_CODE)%>%
  select(cdfw_port_id:pacfin_county_code,congressional_district,active_date=ACTIVE_DATE, latitude, longitude, port_order=PORT_ORDER)%>%
  glimpse()
d6


# join with pacfin complex codes and names
d7<-d6%>%
  full_join(d5)%>%
  glimpse()


# # join with iopac groups from ONMS
d8<-d7%>%
  full_join(d3)%>% # iopac groups from ONMS and IOPAC report
  filter(cdfw_port_id!=343)%>% #inland, near sacramento
  filter(pacfin_county_code!="ICA")%>% # remove inland areas
  arrange(cdfw_port_id)%>%
  mutate(iopac_port=if_else(is.na(iopac_port),cdfw_port_name,iopac_port))%>%
  filter(iopac_port_complex!="California"| is.na(iopac_port_complex))%>% # remove inland areas2, keep NAs
  mutate(iopac_port_complex=if_else(clean_pacfin_port_complex=="bodegabay","BodegaBay",iopac_port_complex))%>%
  mutate(iopac_port_complex=if_else(clean_pacfin_port_complex=="crescentcity","CrescentCity",iopac_port_complex))%>%
  mutate(iopac_port_complex=if_else(clean_pacfin_port_complex=="eureka","Eureka",iopac_port_complex))%>%
  mutate(iopac_port_complex=if_else(clean_pacfin_port_complex=="fortbragg","Ft_Bragg",iopac_port_complex))%>%
  mutate(iopac_port_complex=if_else(clean_pacfin_port_complex=="losangeles","LosAngeles",iopac_port_complex))%>%
  mutate(iopac_port_complex=if_else(clean_pacfin_port_complex=="monterey","Monterey",iopac_port_complex))%>%
  mutate(iopac_port_complex=if_else(clean_pacfin_port_complex=="morrobay","MorroBay",iopac_port_complex))%>%
  mutate(iopac_port_complex=if_else(clean_pacfin_port_complex=="sanfrancisco","SanFrancisco",iopac_port_complex))%>%
  mutate(iopac_port_complex=if_else(clean_pacfin_port_complex=="santabarbara","SantaBarbara",iopac_port_complex))%>%
  mutate(iopac_port_complex=if_else(clean_pacfin_port_complex=="sandiego","SanDiego",iopac_port_complex))%>%
  mutate(iopac_port_complex=if_else(is.na(iopac_port_complex),"Unknown",iopac_port_complex))%>%
  mutate(pacfin_port_complex=if_else(is.na(clean_pacfin_port_complex),"CALIFORNIA2 AREA PORTS",clean_pacfin_port_complex))%>%
  mutate(clean_pacfin_port_complex=if_else(is.na(clean_pacfin_port_complex),"California2",clean_pacfin_port_complex))%>%
  select(cdfw_port_id, cdfw_port_name,pacfin_port_code,pacfin_port_complex_code,pacfin_port_complex,clean_pacfin_port_complex,iopac_port,iopac_port_complex,pacfin_county_code,congressional_district,active_date,latitude, longitude,port_order)%>%
  glimpse()
glimpse(d8)
d8
unique(d8$pacfin_port_complex)

names(d8)


# save
write_csv(d8,"./results/port_key_final.csv")

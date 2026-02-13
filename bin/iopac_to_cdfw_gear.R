# Jennifer Selgrath
# NOAA CINMS
# Deep sea coral valuation
#
# GOAL: combine gear codes
#----------------------------------------------------------
library(tidyverse); library(ggplot2)


# github: https://github.com/jselgrath/deep_sea_coral_valuation



#----------------------------------------------------------
remove(list=ls())
setwd("G:/My Drive/research/r_projects/dsc_valuation")

# pacfin translation codes from Brad Stenberg <bstenberg@psmfc.org> -------------------
# gear_translations_pacfin2.csv - agency gear code tab
# gear_translations_pacfin.csv - 

d00<-read_csv("./data/gear_translations_pacfin.csv")%>%
  mutate(pacfin_gear_code=PACFIN_GEAR_CODE,pacfin_gear_type_code=GEAR_TYPE_CODE, pacfin_gear_group_code=PACFIN_GROUP_GEAR_CODE, pacfin_gear_name=PACFIN_GEAR_NAME, pacfin_gear_description=PACFIN_GEAR_DESCRIPTION)%>%
  select(pacfin_gear_code:pacfin_gear_description,pacfin_gear_active_date=ACTIVE_DATE,pacfin_gear_expire_date=EXPIRE_DATE)%>%
  unique()%>%
  glimpse()


d0<-read_csv("./data/gear_translations_pacfin2.csv")%>%
  mutate(cdfw_gear_id=as.numeric(GEAR_CODE))%>%
  mutate(cdfw_gear_name=GEAR_NAME,pacfin_gear_code=PACFIN_GEAR_CODE,pacfin_gear_description=GEAR_DESCRIPTION)%>%
  filter(AGENCY_CODE=="C")%>% #California
  select(cdfw_gear_id:pacfin_gear_code,cdfw_gear_active_date=ACTIVE_DATE,cdfw_gear_expire_date=EXPIRE_DATE)%>%
  unique()%>%
  glimpse()



# select only type 1 codes and join two tables from pacfin
d000<-d00%>%
  filter(pacfin_gear_type_code==1)%>% # specific gears (1), not groups (2) or all gears
  select(-pacfin_gear_type_code)%>%
  right_join(d0)%>%
  select(cdfw_gear_id,pacfin_gear_code, pacfin_gear_group_code, pacfin_gear_description, cdfw_gear_active_date)%>% #, EXPIRE_DATE - has no info
  unique()%>%
  arrange(cdfw_gear_id)%>%
  glimpse()
d000


# iopac codes from ONMS
d1<-read_csv("./data/gear_key_iopac.csv")%>%
  select(iopac_gear_name=GearName,iopac_gear_group=GEAR_SUM)%>%
  glimpse()


# from chris free - jenny added pacfin gear group based on best estimation (=pacfin_gear_group1)
d2<-read_csv("./data/gear_key_cdfw.csv")%>% 
  # filter(as.character(discontinued_date) != "4/1/1996"| is.na(discontinued_date))%>% #before study period
  select(cdfw_gear_id=gear_code,cdfw_gear_name=gear,cdfw_gear_discontinued_date=discontinued_date,gear_type_free,pacfin_gear_group1)%>% # ,gear_type_free,pacfin_gear_group1
  # filter(cdfw_gear_id!=95|cdfw_gear_id!=17)%>% #Raft/Lines For Herring Roe On Kelp & kelp barge
  arrange(cdfw_gear_id)%>%
  unique()%>%
  glimpse()



d3<-read_csv("./data/gear_dat.csv")%>% # iopac codes from noaa nwfsc
  select(pacfin_gear_group_id=ID,pacfin_gear_group_code=GEAR,pacfin_gear_group_name=DESCRIPTION,iopac_gear_group=GEAR_SUM)%>%
  glimpse()

d000;d1;d2;d3

unique(d0$pacfin_gear_code)


# join csfw:pacfin (d000)  and cdfw names and code discontinue date (d2)
d4<-d000%>%
  full_join(d2)%>%
  
  mutate(pacfin_gear_group_code=if_else(cdfw_gear_id ==-1| cdfw_gear_id ==99,"MSC",pacfin_gear_group_code))%>%
  mutate(pacfin_gear_description=if_else(cdfw_gear_id ==-1| cdfw_gear_id ==99,"UNKNOWN OR UNSPECIFIED GEAR",pacfin_gear_description))%>% 
  mutate(cdfw_gear_name=if_else(cdfw_gear_id ==-1,"Unknown",cdfw_gear_name))%>% 
  
  mutate(pacfin_gear_group_code=if_else(cdfw_gear_id ==63|cdfw_gear_id ==64|cdfw_gear_id ==67|cdfw_gear_id ==68|cdfw_gear_id ==69,"NET",pacfin_gear_group_code))%>%
  mutate(pacfin_gear_description=if_else(cdfw_gear_id ==63|cdfw_gear_id ==64,"DRIFT GILL NET",pacfin_gear_description))%>%
  mutate(pacfin_gear_description=if_else(cdfw_gear_id ==67|cdfw_gear_id ==68|cdfw_gear_id ==69,"GILL NET",pacfin_gear_description))%>%
  
  mutate(pacfin_gear_group_code=if_else(cdfw_gear_id ==37|cdfw_gear_id ==38,"POT",pacfin_gear_group_code))%>%
  mutate(pacfin_gear_description=if_else(cdfw_gear_id ==37|cdfw_gear_id ==38,"OTHER POT GEAR",pacfin_gear_description))%>% 
  
  mutate(pacfin_gear_code=if_else(cdfw_gear_id ==31,"VHL",pacfin_gear_code))%>%
  mutate(pacfin_gear_group_code=if_else(cdfw_gear_id ==31,"HKL",pacfin_gear_group_code))%>%
  mutate(pacfin_gear_description=if_else(cdfw_gear_id ==31,"LONGLINE OR SETLINE",pacfin_gear_description))%>% 
  
  mutate(pacfin_gear_code=if_else(cdfw_gear_id ==30,"LGL",pacfin_gear_code))%>%
  mutate(pacfin_gear_group_code=if_else(cdfw_gear_id ==30|cdfw_gear_id ==82|cdfw_gear_id ==81,"HKL",pacfin_gear_group_code))%>%
  mutate(pacfin_gear_description=if_else(cdfw_gear_id ==30|cdfw_gear_id ==82|cdfw_gear_id ==81,"LONGLINE OR SETLINE",pacfin_gear_description))%>% 
  
  mutate(pacfin_gear_code=if_else(cdfw_gear_id ==32,"TRL",pacfin_gear_code))%>%
  mutate(pacfin_gear_group_code=if_else(cdfw_gear_id ==32|cdfw_gear_id ==86,"TLS",pacfin_gear_group_code))%>%
  mutate(pacfin_gear_description=if_else(cdfw_gear_id ==32|cdfw_gear_id ==86,"TROLL",pacfin_gear_description))%>% 
  
  mutate(pacfin_gear_code=if_else(cdfw_gear_id ==33,"OTW",pacfin_gear_code))%>%
  mutate(pacfin_gear_group_code=if_else(cdfw_gear_id ==33,"TWL",pacfin_gear_group_code))%>%
  mutate(pacfin_gear_description=if_else(cdfw_gear_id ==33,"OTHER TRAWL GEAR",pacfin_gear_description))%>% 
  
  mutate(pacfin_gear_code=if_else(cdfw_gear_id ==34,"PRT",pacfin_gear_code))%>%
  mutate(pacfin_gear_group_code=if_else(cdfw_gear_id ==34,"TWL",pacfin_gear_group_code))%>%
  mutate(pacfin_gear_description=if_else(cdfw_gear_id ==34,"PAIR TRAWL",pacfin_gear_description))%>%
  
  mutate(pacfin_gear_code=if_else(cdfw_gear_id ==36 |cdfw_gear_id ==85,"JIG",pacfin_gear_code))%>%
  mutate(pacfin_gear_group_code=if_else(cdfw_gear_id ==36|cdfw_gear_id ==85,"HKL",pacfin_gear_group_code))%>%
  mutate(pacfin_gear_description=if_else(cdfw_gear_id ==36|cdfw_gear_id ==85,"POLE (COMMERCIAL)",pacfin_gear_description))%>%
  
  # deep set buoy gear
  mutate(pacfin_gear_group_code=if_else(cdfw_gear_id ==83|cdfw_gear_id ==84,"HKL",pacfin_gear_group_code))%>%
  mutate(pacfin_gear_description=if_else(cdfw_gear_id ==83|cdfw_gear_id ==84,"VERTICAL HOOK AND LINE GEAR",pacfin_gear_description))%>%



  arrange(cdfw_gear_id)%>%
  glimpse()


# view(d4)
d4

d5<-d4%>%
  left_join(d3)%>% # drops Non-trawl gear id=2 (1 line)
  select(cdfw_gear_id, cdfw_gear_name, pacfin_gear_code,pacfin_gear_description, pacfin_gear_group_id, pacfin_gear_group_code, pacfin_gear_group_name, iopac_gear_group,cdfw_gear_active_date,cdfw_gear_discontinued_date)%>%
  
  # add names for new gears
  mutate(cdfw_gear_name=if_else(cdfw_gear_id==85,"Stationary Vertical Midwater Jig",cdfw_gear_name))%>%
  mutate(cdfw_gear_name=if_else(cdfw_gear_id==86,"Midwater Troll",cdfw_gear_name))%>%
  # didnt find name for cdfw_gear_id==75 which is also missing
  glimpse()
d5


# save
write_csv(d5,"./results/gear_key_final.csv")


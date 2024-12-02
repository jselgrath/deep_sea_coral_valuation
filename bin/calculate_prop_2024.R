# Jennifer Selgrath
# NOAA CINMS
# project: economic valuation of commercial fisheries associated with deep sea corals
#
# GOAL: join tables of same data type and calculate percent
# ------------
# METADATA
#   catch_nomults - Catch that did not have associated multipliers
#   COMMDC (commodity sector = species + gear)
#   total values = vessel + processor


#----------------------------------------------------------
library(tidyverse); library(ggplot2)

#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/Jennifer.Selgrath/Documents/research/R_projects/dsc_valuation/")


d1<-read_csv("./results/ec_port_bl_long.csv")%>% glimpse
d2<-read_csv("./results/ec_port_pr_long.csv")%>% glimpse()
d3<-read_csv("./results/ec_port_ha12_long.csv")%>% glimpse
d4<-read_csv("./results/ec_port_ha2_long.csv")%>%glimpse
d5<-read_csv("./results/ec_port_all_long.csv")%>%glimpse

d1_d<-read_csv("./results/ec_bl_noMult_long.csv")%>%glimpse
d2_d<-read_csv("./results/ec_pr_noMult_long.csv")%>%glimpse
d3_d<-read_csv("./results/ec_ha12_noMult_long.csv")%>%glimpse
d4_d<-read_csv("./results/ec_ha2_noMult_long.csv")%>%glimpse
d5_d<-read_csv("./results/ec_allSp_noMult_long.csv")%>%glimpse


unique(d1$Sector)

# plot values without multipliers ----------------------------------
ggplot(d1_d,aes(x=year,y=value_noMult_bl/1000000,color=metric))+geom_point()+ylab("revenue (mil)")

# summarize values-------------------------------------------------------------------------
# summarize - port data by sector ----------------------------

# body length (bl) --------------------------------------
d1_ps<-d1%>%
  group_by(PortGroup_IOPAC,COMMCD, COMMCD2, year, assoc, metric)%>%
  filter(metric=="TotOut"|metric== "TotInc"|metric== "TotEmp"|metric==  "revenue")%>%
  summarise(
    port_value_total=sum(value_port_bl,na.rm=T),
    port_value_assoc=sum(value_port_bl[dsc_b_v3 =="1"],na.rm=T),
    port_value_unkn=sum(value_port_bl[dsc_b_v3 =="unknown"],na.rm=T),
    port_value_not_assoc=sum(value_port_bl[dsc_b_v3 =="0"],na.rm=T),
    port_prp_assoc=(port_value_assoc/port_value_total),
    port_prp_assoc_unkn=(port_value_assoc+port_value_unkn)/port_value_total)%>%
  mutate(port_prp_assoc=ifelse(is.nan(port_prp_assoc),0,port_prp_assoc),
         port_prp_assoc_unkn=ifelse(is.nan(port_prp_assoc_unkn),0,port_prp_assoc_unkn))%>%
  glimpse()

# summarize - port data no sector----------------------------
d2_p<-d2%>%
  group_by(PortGroup_IOPAC,year, metric)%>%
  filter(metric=="TotOut"|metric== "TotInc"|metric== "TotEmp"|metric==  "revenue")%>%
  summarise(
    port_value_total=sum(value_port,na.rm=T),
    port_value_assoc=sum(value_port[dsc_b_v3 =="1"],na.rm=T),
    port_value_unkn=sum(value_port[dsc_b_v3 =="unknown"],na.rm=T),
    port_value_not_assoc=sum(value_port[dsc_b_v3 =="0"],na.rm=T),
    port_prp_assoc=(port_value_assoc/port_value_total),
    port_prp_assoc_unkn=(port_value_assoc+port_value_unkn)/port_value_total)%>%
  mutate(port_prp_assoc=ifelse(is.nan(port_prp_assoc),0,port_prp_assoc),
         port_prp_assoc_unkn=ifelse(is.nan(port_prp_assoc_unkn),0,port_prp_assoc_unkn))%>%
  glimpse()

# annual state data by sector----------------------------
d3_as<-d3%>%
  group_by(COMMCD, year, metric)%>%
  filter(metric=="TotOut"|metric== "TotInc"|metric== "TotEmp"|metric==  "revenue")%>%
  summarise(
    state_value_total=sum(value_state,na.rm=T),
    state_value_assoc=sum(value_state[dsc_b_v3 =="1"],na.rm=T),
    state_value_unkn=sum(value_state[dsc_b_v3 =="unknown"],na.rm=T),
    state_value_not_assoc=sum(value_state[dsc_b_v3 =="0"],na.rm=T),
    state_prp_assoc=(state_value_assoc/state_value_total),
    state_prp_assoc_unkn=(state_value_assoc+state_value_unkn)/state_value_total)%>%
  mutate(state_prp_assoc=ifelse(is.nan(state_prp_assoc),0,state_prp_assoc),
         state_prp_assoc_unkn=ifelse(is.nan(state_prp_assoc_unkn),0,state_prp_assoc_unkn))%>%
  glimpse()

# annual data----------------------------
d4_a<-d4%>%
  group_by(year, metric)%>%
  filter(metric=="TotOut"|metric== "TotInc"|metric== "TotEmp"|metric==  "revenue")%>%
  summarise(
    annual_value_total=sum(value_annual,na.rm=T),
    annual_value_assoc=sum(value_annual[dsc_b_v3 =="1"],na.rm=T),
    annual_value_unkn=sum(value_annual[dsc_b_v3 =="unknown"],na.rm=T),
    annual_value_not_assoc=sum(value_annual[dsc_b_v3 =="0"],na.rm=T),
    annual_prp_assoc=(annual_value_assoc/annual_value_total),
    annual_prp_assoc_unkn=(annual_value_assoc+annual_value_unkn)/annual_value_total)%>%
  mutate(annual_prp_assoc=ifelse(is.nan(annual_prp_assoc),0,annual_prp_assoc),
         annual_prp_assoc_unkn=ifelse(is.nan(annual_prp_assoc_unkn),0,annual_prp_assoc_unkn))%>%
  glimpse()


# save ###################
write_csv(d1_ps,"./results/val_port_sector.csv")
write_csv(d1_p,"./results/val_port.csv")
write_csv(d2_s,"./results/val_state_sector.csv")
write_csv(d3_s,"./results/val_annual.csv")
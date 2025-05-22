# Jennifer Selgrath
# NOAA CINMS
#
# GOAL: summary statistics at state level for deep sea coral associated species
# -----------------------------------------------
# METADATA
#   catch_nomults - Catch that did not have associated multipliers
#   COMMDC (commodity sector = species + gear)
#   total values = vessel + processor
#   Total output = vessel output + processor output
#    -  Output =  similar to GDP (production) - gross sales
#   Total income = vessel income + processor income
#    - Income = income paid to employees, income to people working in bake shop, or gas station supplying vessel
#   Total employment = vessel employment + processor employment
#     - Employment = number of people that are employed by virtue of vessel operating

# Note: State file is for commodity data- names are interchanged
# value2 is regular value for emp and million dollars for inc and out
#----------------------------------------------------------
library(tidyverse); library(ggplot2);library(RColorBrewer); library(colorspace)

#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/Jennifer.Selgrath/Documents/research/R_projects/dsc_valuation/")

#----------------------------------------------------------
# STATE LEVEL 
#----------------------------------------------------------
d1<-read_csv("./results/ec_annual_all_long.csv")%>% 
  filter(metric=="TotOut_CA2"|metric=="TotInc_CA2"| metric=="TotEmp_CA2")%>%
  mutate(
    metric2=str_replace_all(metric,pattern="TotEmp_CA2",replacement="Employment"),
    metric2=str_replace_all(metric2,pattern="TotOut_CA2",replacement="Output (Million USD)"),
    metric2=str_replace_all(metric2,pattern="TotInc_CA2",replacement="Income (Million USD)"),
    value_annual_1_mil=value/1000000)%>%
  mutate(Association=factor(association, levels = c("Associated","Definite Association", "Probable Association","Not Associated","Multispecies Group","No Data")))%>%
  # filter(Association!="Multispecies Group")%>%
  mutate(value2=ifelse(metric=="TotEmp_CA2",value,value_annual_1_mil))%>%
  glimpse()

# summarize for all years ---------------------
# value2 is regular value for emp and million dollars for inc and out
d1a<-d1%>%
  filter(assoc_type=="pr")%>%
  # filter(metric=="TotOut_CA2")%>%
  group_by(assoc_type,Association,metric,metric2)%>%
  summarize(
    n=n(),
    value2_tot=sum(value2),
    value2_u=mean(value2),
    value2_sd=sd(value2),
    value2_sem=sd(value2)/sqrt(n),
    value2_min=round(min(value2),2),
    value2_max=round(max(value2),2),
    value2_range=paste0(value2_min,"-", value2_max)
  )%>%
  arrange(assoc_type,metric)%>%
  # filter(assoc_type!="bl")%>%
  glimpse()
d1a


write_csv(d1a,"./doc/summary_stats_all_yr_state.csv")

#  for all years ---------------------
# value2 is regular value for emp and million dollars for inc and out
d1b<-d1%>%
  filter(assoc_type=="pr")%>%
  # filter(metric=="TotOut_CA2")%>%
  group_by(assoc_type,Association,metric,metric2)%>%
  summarize(
    # same as above
    n=n(),
    value2_tot=sum(value2),
    value2_u=mean(value2),
    value2_sd=sd(value2),
    value2_sem=sd(value2)/sqrt(n),
    value2_min=round(min(value2),2),
    value2_max=round(max(value2),2),
    value2_range=paste0(value2_min,"-", value2_max)
  )%>%
  arrange(assoc_type,metric)%>%
  ungroup()%>%
  group_by(metric2)%>%
  mutate(tot_sum=sum(value2_tot))%>%
  # mutate(tot_u=sum(value2_u))%>%
  mutate(tot_prpn=value2_tot/tot_sum)%>%
  glimpse()
d1b

write_csv(d1b,"./doc/summary_stats_all_yr_state2.csv")




#----------------------------------------------------------
# COMMODITY LEVEL 
#----------------------------------------------------------
# commodity<-read_csv("./data/ports_n_to_s.csv")%>% 
#   filter(port_group!="UNSP")%>%
#   glimpse() # to reorder factor

d3<-read_csv("./results/ec_port_all_long.csv")%>%
  left_join(ports)%>%
  filter(metric=="TotOut_port"|metric=="TotInc_port"| metric=="TotEmp_port")%>% # these are the three most useful values, revenue includes values with no multipliers
  mutate(
    metric2=str_replace_all(metric,pattern="TotEmp_port",replacement="Total Employment"),
    metric2=str_replace_all(metric2,pattern="TotOut_port",replacement="Output (Million USD)"),
    metric2=str_replace_all(metric2,pattern="TotInc_port",replacement="Income (Million USD)"),
    value_annual_mil=value/1000000)%>%
  mutate(Association=factor(association, levels = c("Associated","Definite Association", "Probable Association","Not Associated","Multispecies Group","No Data")))%>%
  mutate(PortGroup_IOPAC=factor(PortGroup_IOPAC),
         PortGroup_IOPAC=fct_reorder(PortGroup_IOPAC,port_order))%>%
  mutate(port_group=factor(port_group),
         port_group=fct_reorder(port_group,port_order))%>%
  mutate(value2=ifelse(metric=="TotEmp_port",value,value_annual_mil))%>%
  glimpse()
d3

# summarize for all years ---------------------
# value2 is regular value for emp and million dollars for inc and out
d3a<-d3%>%
  # filter(assoc_type=="pr")%>%
  # filter(metric=="TotOut_port")%>%
  group_by(assoc_type,Association,metric,metric2, PortGroup_IOPAC,port_group,region)%>%
  summarize(
    n=n(),
    value2_tot=sum(value2),
    value2_u=mean(value2),
    value2_sd=sd(value2),
    value2_sem=sd(value2)/sqrt(n),
    value2_min=round(min(value2),2),
    value2_max=round(max(value2),2),
    value2_range=paste0(value2_min,"-", value2_max)
  )%>%
  arrange(port_group,assoc_type,metric)%>%
  # filter(assoc_type!="bl")%>%
  glimpse()
d3a

write_csv(d3a,"./doc/summary_stats_all_yr_port.csv")



#  for all years ---------------------
# value2 is regular value for emp and million dollars for inc and out
d3b<-d3%>%
  filter(assoc_type=="pr")%>%
  filter(metric=="TotOut_port")%>%
  group_by(assoc_type,Association,metric,metric2, PortGroup_IOPAC,port_group,region)%>%
  summarize(
    n=n(),
    value2_tot=sum(value2),
    value2_u=mean(value2),
    value2_sd=sd(value2),
    value2_sem=sd(value2)/sqrt(n),
    value2_min=round(min(value2),2),
    value2_max=round(max(value2),2),
    value2_range=paste0(value2_min,"-", value2_max)
  )%>%
  arrange(port_group,assoc_type,metric)%>%
  ungroup()%>%
  group_by(port_group,metric2)%>%
  mutate(tot_sum=sum(value2_tot))%>%
  mutate(tot_u_yr=sum(value2_u))%>%
  ungroup()%>%
  group_by(metric2)%>%
  mutate(tot_sum=sum(value2_tot))%>%
  mutate(tot_prpn=value2_tot/tot_sum)%>%
  glimpse()
d3b

write_csv(d3b,"./doc/summary_stats_all_yr_commodity2.csv")





#----------------------------------------------------------
# PORT LEVEL 
#----------------------------------------------------------
ports<-read_csv("./data/ports_n_to_s.csv")%>% 
  filter(port_group!="UNSP")%>%
  glimpse() # to reorder factor
ports$region<-c("N","N","N","N","C","C","C","S","S","S") # https://wildlife.ca.gov/Fishing/Ocean - SF could go either
ports

d3<-read_csv("./results/ec_port_all_long.csv")%>%
  left_join(ports)%>%
  filter(metric=="TotOut_port"|metric=="TotInc_port"| metric=="TotEmp_port")%>% # these are the three most useful values, revenue includes values with no multipliers
  mutate(
    metric2=str_replace_all(metric,pattern="TotEmp_port",replacement="Total Employment"),
    metric2=str_replace_all(metric2,pattern="TotOut_port",replacement="Output (Million USD)"),
    metric2=str_replace_all(metric2,pattern="TotInc_port",replacement="Income (Million USD)"),
    value_annual_mil=value/1000000)%>%
  mutate(Association=factor(association, levels = c("Associated","Definite Association", "Probable Association","Not Associated","Multispecies Group","No Data")))%>%
  mutate(PortGroup_IOPAC=factor(PortGroup_IOPAC),
         PortGroup_IOPAC=fct_reorder(PortGroup_IOPAC,port_order))%>%
  mutate(port_group=factor(port_group),
         port_group=fct_reorder(port_group,port_order))%>%
  mutate(value2=ifelse(metric=="TotEmp_port",value,value_annual_mil))%>%
  glimpse()
d3

# summarize for all years ---------------------
# value2 is regular value for emp and million dollars for inc and out
d3a<-d3%>%
  # filter(assoc_type=="pr")%>%
  # filter(metric=="TotOut_port")%>%
  group_by(assoc_type,Association,metric,metric2, PortGroup_IOPAC,port_group,region)%>%
  summarize(
    n=n(),
    value2_tot=sum(value2),
    value2_u=mean(value2),
    value2_sd=sd(value2),
    value2_sem=sd(value2)/sqrt(n),
    value2_min=round(min(value2),2),
    value2_max=round(max(value2),2),
    value2_range=paste0(value2_min,"-", value2_max)
  )%>%
  arrange(port_group,assoc_type,metric)%>%
  # filter(assoc_type!="bl")%>%
  glimpse()
d3a

write_csv(d3a,"./doc/summary_stats_all_yr_port.csv")



#  for all years ---------------------
# value2 is regular value for emp and million dollars for inc and out
d3b<-d3%>%
  filter(assoc_type=="pr")%>%
  filter(metric=="TotOut_port")%>%
  group_by(assoc_type,Association,metric,metric2, PortGroup_IOPAC,port_group,region)%>%
  summarize(
    n=n(),
    value2_tot=sum(value2),
    value2_u=mean(value2),
    value2_sd=sd(value2),
    value2_sem=sd(value2)/sqrt(n),
    value2_min=round(min(value2),2),
    value2_max=round(max(value2),2),
    value2_range=paste0(value2_min,"-", value2_max)
  )%>%
  arrange(port_group,assoc_type,metric)%>%
  ungroup()%>%
  group_by(port_group,metric2)%>%
  mutate(tot_sum=sum(value2_tot))%>%
  mutate(tot_u_yr=sum(value2_u))%>%
  ungroup()%>%
  group_by(metric2)%>%
  mutate(tot_sum=sum(value2_tot))%>%
  mutate(tot_prpn=value2_tot/tot_sum)%>%
  glimpse()
d3b

write_csv(d3b,"./doc/summary_stats_all_yr_port2.csv")

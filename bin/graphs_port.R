# Jennifer Selgrath
# NOAA CINMS
#
# GOAL: graph port data
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

#----------------------------------------------------------
library(tidyverse); library(ggplot2);library(RColorBrewer); library(colorspace)

#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/Jennifer.Selgrath/Documents/research/R_projects/dsc_valuation/")

d1_a<-read_csv("./results/ec_port_all_long.csv")%>%glimpse() # this version does not have commodity info
d1_a


d1_a%>%
  filter(metric =="TotInc_port")%>%
  summarize(rng=range(value,na.rm=T))

unique(d1_a$assoc_type)

ports<-read_csv("./data/ports_n_to_s.csv")%>% glimpse() # to reorder factor

# check for NA category - should be empty
d1_a%>%
  filter(is.na(association))%>%
  glimpse()


# ----------------------------------------------------
# PORT LEVEL DATA, with commodity data ####--------------------------------
# ----------------------------------------------------
# add order for ports to be grouped geographically ----------------
d1_ap<-d1_a%>%
  left_join(ports)%>%glimpse()

filter(d1_ap,!is.na(port_order))%>%glimpse() # check

unique(d1_ap$metric)
unique(d1_ap$port_group)


d1<-d1_ap%>%
  filter(metric=="TotOut_port"|metric=="TotInc_port"| metric=="TotEmp_port")%>% # these are the three most useful values, revenue includes values with no multipliers
  mutate(
    metric2=str_replace_all(metric,pattern="TotEmp_port",replacement="Total Employment"),
    metric2=str_replace_all(metric2,pattern="TotOut_port",replacement="Output (Total)"),
    metric2=str_replace_all(metric2,pattern="TotInc_port",replacement="Income (Total)"),
    value_annual_mil=value/1000000)%>%
  mutate(Association=factor(association, levels = c("Associated","Definite Association", "Probable Association","Not Associated","Multispecies Group","No Data")))%>%
  mutate(PortGroup_IOPAC=factor(PortGroup_IOPAC),
         PortGroup_IOPAC=fct_reorder(PortGroup_IOPAC,port_order))%>%
  mutate(port_group=factor(port_group),
         port_group=fct_reorder(port_group,port_order))%>%
  mutate(value2=ifelse(metric=="TotEmp_port",value,value_annual_1_mil))%>%
  glimpse()
d1

levels(d1$PortGroup_IOPAC)
levels(d1$port_group)
unique(d1$metric)
unique(d1$assoc_type)
unique(d1$scale)



# summarize by port -----------------------------------
glimpse(d1)
names(d1)

# # here b is to distinguish new calculations
# # proportion does not work when summarized because existing prop values are for commodities
# d1_b<-d1%>%
#   group_by(year, PortGroup_IOPAC, port_order, port_group,scale, assoc_type,association,metric, metric2)%>%
#   summarize(
#     n=n(),
#     port_value=sum(value,na.rm=T),
#     port_value_u=mean(value,na.rm=T),
#     port_value_sd=sd(value,na.rm=T),
#     port_value_mil=port_value/1000000)%>%
#   ungroup()%>%
#   glimpse()
# d1_b
# 
# range(d1_b$port_value)





# --------------------------------------------
# GRAPHS ##### -------------------------------
# --------------------------------------------
source("./bin/deets.R")
metric_labs_t<-c("TotEmp_port"="Employment", "TotInc_port"="Income (Million USD)", "TotOut_port"= "Output (Million USD)")
metric_labs<-c("TotEmp_port"="Employment", "TotInc_port"="Income", "TotOut_port"= "Output")
metric_labs_p<-c("TotEmp_port"="Employment", "TotInc_port"="Income", "TotOut_port"= "Output")

# split data by metric so scales are easier to interpret ---------------------------

# port by metric
d1_be<-d1%>%filter(metric=="TotEmp_port")%>%
  filter(Association!="Multispecies Group")%>%glimpse()
d1_bi<-d1%>%filter(metric=="TotInc_port")%>%
  filter(Association!="Multispecies Group")%>%glimpse()
d1_bo<-d1%>%filter(metric=="TotOut_port")%>%
  filter(Association!="Multispecies Group")%>%glimpse()

# range
d1_be%>%
  filter(Association=="Multispecies Group")%>%
  summarize(rng=range(value))%>%
  glimpse()
# 0.000000, 2.809886

d1_bi%>%
  filter(Association=="Multispecies Group")%>%
  summarize(rng=range(value_annual_mil))%>%
  glimpse()
# 0.00, .8891025

d1_bo%>%
  filter(Association=="Multispecies Group")%>%
  summarize(rng=range(value_annual_mil))%>%
  glimpse()
# 0.0, .1793293


# --------------------------------------------------------------
# PORT LEVEL DATA ####------------------------------------------
# --------------------------------------------------------------

# ------------------------------------------------
# EMPLOYMENT
# ------------------------------------------------

# area graphs -----------------------------
f1a<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value, fill=Association))+
    geom_area(size=1.2)+ #geom_area()+ #geom_smooth
    scale_fill_discrete_sequential(palette = "Batlow",nmax=4,order=(c(4:2)))+ #TealGrn
    geom_hline(yintercept=0,color="lightgrey")+
    deets10+ #8
    facet_grid(rows=vars(port_group))+
    # facet_grid(port_group~Association)+
    scale_y_continuous("Employment",limits=c(-5,2200),breaks=c(0,1000,2000))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}

# line graphs - assoc-----------------------------
f1b<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value, color=Association))+ # Association
    geom_line(size=1.2)+ #geom_area()+ #geom_smooth
    scale_color_discrete_sequential(palette = "Batlow",nmax=4,order=(c(4:2)))+ 
    geom_hline(yintercept=0,color="lightgrey")+
    deets10+ 
    facet_grid(rows=vars(port_group))+
    # facet_grid(port_group~Association)+
    scale_y_continuous("Total Employment",limits=c(-5,2000),breaks=c(0,1000,2000))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}


# line graphs - port -----------------------------
f1c<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value, color=port_group))+ # Association
    geom_line(size=1.2)+ #geom_area()+ #geom_smooth
    scale_color_discrete_sequential(palette = "TealGrn")+ #TealGrn
    geom_hline(yintercept=0,color="lightgrey")+
    deets8+
    facet_grid(port_group~Association)+
    # facet_wrap(vars(port_group), ncol=1)+
    scale_y_continuous("Total Employment",limits=c(-5,2000),breaks=c(0,1000,2000))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}

# proximity --------------
f1a(dta=d1_be,assoc="pr") # Association
ggsave("./doc/graph_port_pr_e1.jpg",height=8,width=5)

f1b(dta=d1_be,assoc="pr") # port, assoc colors
ggsave("./doc/graph_port_pr_e2.jpg",height=8,width=5)

f1c(dta=d1_be,assoc="pr") # port, port colors
ggsave("./doc/graph_port_pr_e3.jpg",height=8,width=8)


# habitat
f1a(dta=d1_be,assoc="ha12") # association
ggsave("./doc/graph_port_ha12_e1.jpg",height=8,width=5)

f1b(dta=d1_be,assoc="ha12") # port, assoc colors
ggsave("./doc/graph_port_ha12_e2.jpg",height=8,width=5)

f1c(dta=d1_be,assoc="ha12") # port, port colors
ggsave("./doc/graph_port_ha12_e3.jpg",height=8,width=8)





# ------------------------------------------------
# INCOME
# ------------------------------------------------

# area graphs -----------------------------
f3<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value_annual_mil, fill=Association))+
    geom_area(size=1.2)+ #geom_area()+ #geom_smooth
    scale_fill_discrete_sequential(palette = "Batlow",nmax=4,order=(c(4:2)))+ #TealGrn
    geom_hline(yintercept=0,color="lightgrey")+
    deets10+ #8
    facet_grid(rows=vars(port_group))+
    # facet_grid(port_group~Association)+
    scale_y_continuous("Total Income (Mil)",limits=c(-5,120),breaks=c(0,60,120))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}

# line graphs - assoc-----------------------------
f4<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value_annual_mil, color=Association))+ # Association
    geom_line(size=1.2)+ #geom_area()+ #geom_smooth
    scale_color_discrete_sequential(palette = "Batlow",nmax=4,order=(c(4:2)))+ 
    geom_hline(yintercept=0,color="lightgrey")+
    deets10+ 
    facet_grid(rows=vars(port_group))+
    # facet_grid(port_group~Association)+
    scale_y_continuous("Total Income (Mil)",limits=c(-5,120),breaks=c(0,60,120))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}


# line graphs - port -----------------------------
f5<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value_annual_mil, color=port_group))+ # Association
    geom_line(size=1.2)+ #geom_area()+ #geom_smooth
    scale_color_discrete_sequential(palette = "TealGrn")+ #TealGrn
    geom_hline(yintercept=0,color="lightgrey")+
    deets8+
    facet_grid(port_group~Association)+
    # facet_wrap(vars(port_group), ncol=1)+
    scale_y_continuous("Total Income (Mil)",limits=c(-5,120),breaks=c(0,60,120))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}

# proximity --------------
f3(dta=d1_bi,assoc="pr") # Association
ggsave("./doc/graph_port_pr_i1.jpg",height=8,width=5)

f4(dta=d1_bi,assoc="pr") # port, assoc colors
ggsave("./doc/graph_port_pr_i2.jpg",height=8,width=5)

f5(dta=d1_bi,assoc="pr") # port, port colors
ggsave("./doc/graph_port_pr_i3.jpg",height=8,width=8)


# habitat
f3(dta=d1_bi,assoc="ha12") # association
ggsave("./doc/graph_port_ha12_i1.jpg",height=8,width=5)

f4(dta=d1_bi,assoc="ha12") # port, assoc colors
ggsave("./doc/graph_port_ha12_i2.jpg",height=8,width=5)

f5(dta=d1_bi,assoc="ha12") # port, port colors
ggsave("./doc/graph_port_ha12_i3.jpg",height=8,width=8)







# ------------------------------------------------
# OUTPUT
# ------------------------------------------------

# area graphs -----------------------------
f6<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value_annual_mil, fill=Association))+
    geom_area(size=1.2)+ #geom_area()+ #geom_smooth
    scale_fill_discrete_sequential(palette = "Batlow",nmax=4,order=(c(4:2)))+ #TealGrn
    geom_hline(yintercept=0,color="lightgrey")+
    deets10+ #8
    facet_grid(rows=vars(port_group))+
    # facet_grid(port_group~Association)+
    scale_y_continuous("Total Output (Mil)",limits=c(-5,250),breaks=c(0,100,200))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}

# line graphs - assoc-----------------------------
f7<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value_annual_mil, color=Association))+ # Association
    geom_line(size=1.2)+ #geom_area()+ #geom_smooth
    scale_color_discrete_sequential(palette = "Batlow",nmax=4,order=(c(4:2)))+ 
    geom_hline(yintercept=0,color="lightgrey")+
    deets10+ 
    facet_grid(rows=vars(port_group))+
    # facet_grid(port_group~Association)+
    scale_y_continuous("Total Output (Mil)",limits=c(-5,250),breaks=c(0,100,200))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}


# line graphs - port -----------------------------
f8<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value_annual_mil, color=port_group))+ # Association
    geom_line(size=1.2)+ #geom_area()+ #geom_smooth
    scale_color_discrete_sequential(palette = "TealGrn")+ #TealGrn
    geom_hline(yintercept=0,color="lightgrey")+
    deets8+
    facet_grid(port_group~Association)+
    scale_y_continuous("Total Output (Mil)",limits=c(-5,250),breaks=c(0,100,200))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}

# proximity --------------
f6(dta=d1_bo,assoc="pr") # Association
ggsave("./doc/graph_port_pr_o1.jpg",height=8,width=5)

f7(dta=d1_bo,assoc="pr") # port, assoc colors
ggsave("./doc/graph_port_pr_o2.jpg",height=8,width=5)

f8(dta=d1_bo,assoc="pr") # port, port colors
ggsave("./doc/graph_port_pr_o3.jpg",height=8,width=8)


# habitat
f6(dta=d1_bo,assoc="ha12") # association
ggsave("./doc/graph_port_ha12_o1.jpg",height=8,width=5)

f7(dta=d1_bo,assoc="ha12") # port, assoc colors
ggsave("./doc/graph_port_ha12_o2.jpg",height=8,width=5)

f8(dta=d1_bo,assoc="ha12") # port, port colors
ggsave("./doc/graph_port_ha12_o3.jpg",height=8,width=8)



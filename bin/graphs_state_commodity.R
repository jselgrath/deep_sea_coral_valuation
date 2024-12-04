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

# Note: State file is for commodity data- names are interchanged
#----------------------------------------------------------
library(tidyverse); library(ggplot2);library(RColorBrewer); library(colorspace)

#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/Jennifer.Selgrath/Documents/research/R_projects/dsc_valuation/")

d1_a<-read_csv("./results/ec_state_all_long.csv")%>%glimpse()
d1_a
unique(d1_a$assoc_type)

# check for NA category
d1_a%>%
  filter(is.na(association))%>%
  glimpse()

# check range
d1_a%>%
  filter(metric =="TotInc_CA")%>%
  summarize(rng=range(value,na.rm=T))

# make codes alphabetical with descriptions ----------------
d1_b<-d1_a%>%
  mutate(COMMCD=str_replace_all(COMMCD,pattern="SAM",replacement="SAL"),
         COMMCD=str_replace_all(COMMCD,pattern="SBL",replacement="SAB"),
         COMMCD2=str_replace_all(COMMCD2,pattern="SAM",replacement="SAL"),
         COMMCD2=str_replace_all(COMMCD2,pattern="SBL",replacement="SAB"),
         COMMCD2=str_replace_all(COMMCD2,pattern="_",replacement=",\n"),
         DESCRIPTION=str_replace_all(DESCRIPTION,pattern="CPS",replacement="Coastal Pelagics"),
         DESCRIPTION=str_replace_all(DESCRIPTION,pattern="HMS",replacement="Highly Migratory"))%>%
         glimpse()

unique(d1_b$COMMCD)

# filter for 3 metrics
d1<-d1_b%>%
  filter(metric=="TotOut_CA"|metric=="TotInc_CA"| metric=="TotEmp_CA")%>% # these are the three most useful values, revenue includes values with no multipliers
  mutate(
    metric2=str_replace_all(metric,pattern="TotEmp_CA",replacement="Total Employment"),
    metric2=str_replace_all(metric2,pattern="TotOut_CA",replacement="Output (Total)"),
    metric2=str_replace_all(metric2,pattern="TotInc_CA",replacement="Income (Total)"),
    value_commodity_mil=value/1000000,
    Commodity=DESCRIPTION)%>%
  mutate(Association=factor(association, levels = c("Associated","Definite Association", "Probable Association","Not Associated","Multispecies Group","No Data")))%>%
  glimpse()
d1

unique(d1$metric)
unique(d1$assoc_type)
unique(d1$scale)



# # summarize by commodity -----------------------------------
# glimpse(d1)
# names(d1)
# 
# # here b is to distinguish new calculations
# # proportion does not work when summarized because existing prop values are for commodities
# d1_b<-d1%>%
#   group_by(year, COMMCD, COMMCD2,DESCRIPTION, scale, assoc_type,association,metric, metric2)%>%
#   summarize(
#     n=n(),
#     commodity_value=sum(value,na.rm=T),
#     commodity_value_u=mean(value,na.rm=T),
#     commodity_value_sd=sd(value,na.rm=T),
#     commodity_value_mil=sum(value,na.rm=T)/1000000)%>%
#   ungroup()%>%
#   glimpse()
# d1_b
# 
# range(d1_b$commodity_value)





# --------------------------------------------
# GRAPHS ##### -------------------------------
# --------------------------------------------
source("./bin/deets.R")
metric_labs_t<-c("TotEmp_CA"="Employment (Total)", "TotInc_CA"="Income (Total)", "TotOut_CA"= "Output (Total)")
metric_labs<-c("TotEmp_CA"="Employment", "TotInc_CA"="Income", "TotOut_CA"= "Output")
metric_labs_p<-c("TotEmp_CA"="Employment (Proportion)", "TotInc_CA"="Income (Proportion)", "TotOut_CA"= "Output (Proportion)")


#  commodity total - all metrics in one graph  --------------------------------------------
ggplot(data=d1,aes(x=year,y=value_commodity_mil, color=Commodity))+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "SunsetDark")+
  geom_hline(yintercept=0,color="lightgrey")+
  deets9+
  facet_grid(Commodity~metric2,scales="free")+
  ylab("Deep Sea Coral Contributions By Commodity")+
  # scale_y_continuous("Port-Level Deep Sea Coral Associated Spp.\nTotal (CA)", limits=c(-.1,1),breaks=c(0,.5,1))+
  scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))

ggsave("./doc/graph_comm.jpg",height=30,width=5)


# split data by metric so scales are easier to interpret ---------------------------
# remove commodities with low value (visual)

d1_be<-d1%>%filter(metric=="TotEmp_CA")%>%
  filter(Association!="Multispecies Group")%>%
  glimpse()
d1_be
# range(d1_be$value,na.rm=T) #0.000 2067.045

d1_bi<-d1%>%filter(metric=="TotInc_CA")%>%
  filter(Association!="Multispecies Group")%>%
  glimpse()
# range(d1_bi$value,na.rm=T) #0 138118350

d1_bo<-d1%>%filter(metric=="TotOut_CA")%>%
  filter(Association!="Multispecies Group")%>%
  glimpse()
# range(d1_bi$value_commodity_mil,na.rm=T)  # 0.0000 138.1184



# subset so easier to visualize  ------------------------------------------

# employment -------------------
d1_be2<-d1_be%>%
  group_by(COMMCD)%>%
  summarize(
    mx=max(value))%>%
  filter(mx>=100)%>%
  select(COMMCD)%>%
  glimpse()

d1_be3<-d1_be2%>%
  left_join(d1_be)%>%
  glimpse()


# income -------------------
# 120 commcd, assoc combinations; 40 commcd; 11 >= 5 mil; 20 >=1 mil
d1_bi2<-d1_bi%>%
  group_by(COMMCD)%>%
  summarize(
    mx=max(value_commodity_mil))%>%
  filter(mx>=1)%>%
  select(COMMCD)%>%
  glimpse()

d1_bi3<-d1_bi2%>%
  left_join(d1_bi)%>%
  glimpse()

# output -------------------
d1_bo2<-d1_bo%>%
  group_by(COMMCD)%>%
  summarize(
    mx=max(value_commodity_mil))%>%
  filter(mx>=1)%>%
  select(COMMCD)%>%
  glimpse()

d1_bo3<-d1_bo2%>%
  left_join(d1_bo)%>%
  glimpse()
  

#  commodity total - all metrics in one graph  --------------------------------------------
# for subset where income > 1 mil
d1_bi3%>%
filter(assoc_type=="pr")%>%
ggplot(aes(x=year,y=value_commodity_mil, color=Commodity))+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "SunsetDark")+
  geom_hline(yintercept=0,color="lightgrey")+
  deets9+
  facet_grid(COMMCD~Association,scales="free")+
  # ylab("Deep Sea Coral Contributions By Commodity")+
  scale_y_continuous("Deep Sea Coral Contributions By Commodity\nIncome (Mil)", limits=c(-1,150),breaks=c(0,75,150))+
  scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))

ggsave("./doc/graph_comm_inc_1m_pr.jpg",height=30,width=10)


# --------------------------------------------------------------
# EMPLOYMENT ####------------------------------------------
# --------------------------------------------------------------


# ------------------------------------------------
# area graphs -----------------------------
f1e<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value, fill=Association))+
    geom_area(size=1.2)+ #geom_area()+ #geom_smooth
    scale_fill_discrete_sequential(palette = "Hawaii")+ #TealGrn
    geom_hline(yintercept=0,color="lightgrey")+
    deets9+ #8
    facet_grid(COMMCD2~Association)+
    scale_y_continuous("Total Employment", limits=c(-1,2100),breaks=c(0,1000,2000))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}

# line graphs - assoc-----------------------------
f2e<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value, color=Association))+ # association
    geom_line(size=1.2)+ #geom_area()+ #geom_smooth
    scale_color_discrete_sequential(palette = "Hawaii")+ 
    geom_hline(yintercept=0,color="lightgrey")+
    deets8+ #8
    facet_grid(COMMCD2~Association)+
    scale_y_continuous("Total Employment", limits=c(-1,2100),breaks=c(0,1000,2000))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}


# line graphs - port -----------------------------
f3e<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value, color=Commodity))+ # association
    geom_line(size=1.2)+ #geom_area()+ #geom_smooth
    scale_color_discrete_sequential(palette = "SunsetDark")+ #TealGrn
    geom_hline(yintercept=0,color="lightgrey")+
    deets9+ #8
    facet_grid(COMMCD2~Association)+
    scale_y_continuous("Total Employment", limits=c(-1,2100),breaks=c(0,1000,2000))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}

# proximity --------------
f1e(dta=d1_be3,assoc="pr") # association
ggsave("./doc/graph_comm_pr_e1.jpg",height=8,width=5)

f2e(dta=d1_be3,assoc="pr") # port, assoc colors
ggsave("./doc/graph_comm_pr_e2.jpg",height=8,width=5)

f3e(dta=d1_be3,assoc="pr") # port, port colors
ggsave("./doc/graph_comm_pr_e3.jpg",height=8,width=5)


# habitat
f1e(dta=d1_be3,assoc="ha12") # association
ggsave("./doc/graph_comm_ha12_i1.jpg",height=8,width=5)

f2e(dta=d1_be3,assoc="ha12") # port, assoc colors
ggsave("./doc/graph_comm_ha12_i2.jpg",height=8,width=5)

f3e(dta=d1_be3,assoc="ha12") # port, port colors
ggsave("./doc/graph_comm_ha12_i3.jpg",height=8,width=5)




# --------------------------------------------------------------
# INCOME ####------------------------------------------
# --------------------------------------------------------------

# ------------------------------------------------
# area graphs -----------------------------
f1i<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value_commodity_mil, fill=Association))+
    geom_area(size=1.1)+ #geom_area()+ #geom_smooth
    scale_fill_discrete_sequential(palette = "Hawaii")+ #TealGrn
    geom_hline(yintercept=0,color="lightgrey")+
    deets9+ #8
    facet_grid(rows = vars(COMMCD2))+ #     facet_grid(COMMCD2~Association)+
    scale_y_continuous("Total Income (Mil)", limits=c(-1,150),breaks=c(0,75,150))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}

# line graphs - assoc-----------------------------
f2i<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value_commodity_mil, color=Association))+ # association
    geom_line(size=1.1)+ #geom_area()+ #geom_smooth
    scale_color_discrete_sequential(palette = "Hawaii")+ 
    geom_hline(yintercept=0,color="lightgrey")+
    deets8+ #8
    facet_grid(rows = vars(COMMCD2))+ #     facet_grid(COMMCD2~Association)+
    scale_y_continuous("Total Income (Mil)", limits=c(-1,150),breaks=c(0,75,150))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}


# line graphs - port -----------------------------
f3i<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value_commodity_mil, color=Commodity))+ # association
    geom_line(size=1.1)+ #geom_area()+ #geom_smooth
    scale_color_discrete_sequential(palette = "SunsetDark")+ #TealGrn
    geom_hline(yintercept=0,color="lightgrey")+
    deets9+ #8
    facet_grid(COMMCD2~Association)+
    scale_y_continuous("Total Income (Mil)", limits=c(-1,150),breaks=c(0,75,150))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}

# proximity --------------
f1i(dta=d1_bi3,assoc="pr") # association
ggsave("./doc/graph_comm_pr_i1.jpg",height=8,width=5)

f2i(dta=d1_bi3,assoc="pr") # port, assoc colors
ggsave("./doc/graph_comm_pr_i2.jpg",height=8,width=5)

f3i(dta=d1_bi3,assoc="pr") # port, port colors
ggsave("./doc/graph_comm_pr_i3.jpg",height=8,width=5)


# habitat
f1i(dta=d1_bi3,assoc="ha12") # association
ggsave("./doc/graph_comm_ha12_i1.jpg",height=8,width=5)

f2i(dta=d1_bi3,assoc="ha12") # port, assoc colors
ggsave("./doc/graph_comm_ha12_i2.jpg",height=8,width=5)

f3i(dta=d1_bi3,assoc="ha12") # port, port colors
ggsave("./doc/graph_comm_ha12_i3.jpg",height=8,width=5)





# --------------------------------------------------------------
# OUTPUT ####------------------------------------------
# --------------------------------------------------------------

# ------------------------------------------------
# area graphs -----------------------------
f1o<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value_commodity_mil, fill=Association))+
    geom_area(size=1.2)+ #geom_area()+ #geom_smooth
    scale_fill_discrete_sequential(palette = "Hawaii")+ #TealGrn
    geom_hline(yintercept=0,color="lightgrey")+
    deets9+ #8
    facet_grid(rows = vars(COMMCD2))+ #facet_grid(COMMCD2~Association)+
    scale_y_continuous("Total Output (Mil)", limits=c(-1,350),breaks=c(0,150,300))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}

# line graphs - assoc-----------------------------
f2o<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value_commodity_mil, color=Association))+ # association
    geom_line(size=1.2)+ #geom_area()+ #geom_smooth
    scale_color_discrete_sequential(palette = "Hawaii")+ 
    geom_hline(yintercept=0,color="lightgrey")+
    deets8+ #8
    facet_grid(rows = vars(COMMCD2))+ #facet_grid(COMMCD2~Association)+
    scale_y_continuous("Total Output (Mil)", limits=c(-1,350),breaks=c(0,150,300))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}


# line graphs - port -----------------------------
f3o<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value_commodity_mil, color=Commodity))+ # association
    geom_line(size=1.2)+ #geom_area()+ #geom_smooth
    scale_color_discrete_sequential(palette = "SunsetDark")+ #TealGrn
    geom_hline(yintercept=0,color="lightgrey")+
    deets9+ #8
    facet_grid(COMMCD2~Association)+
    scale_y_continuous("Total Output (Mil)", limits=c(-1,350),breaks=c(0,150,300))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}

# proximity --------------
f1o(dta=d1_bo3,assoc="pr") # association
ggsave("./doc/graph_comm_pr_o1.jpg",height=8,width=5)

f2o(dta=d1_bo3,assoc="pr") # port, assoc colors
ggsave("./doc/graph_comm_pr_o2.jpg",height=8,width=5)

f3o(dta=d1_bo3,assoc="pr") # port, port colors
ggsave("./doc/graph_comm_pr_o3.jpg",height=8,width=5)


# habitat
f1o(dta=d1_bo3,assoc="ha12") # association
ggsave("./doc/graph_comm_ha12_o1.jpg",height=8,width=5)

f2o(dta=d1_bo3,assoc="ha12") # port, assoc colors
ggsave("./doc/graph_comm_ha12_o2.jpg",height=8,width=5)

f3o(dta=d1_bo3,assoc="ha12") # port, port colors
ggsave("./doc/graph_comm_ha12_o3.jpg",height=8,width=5)
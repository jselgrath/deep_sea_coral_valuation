# Jennifer Selgrath
# NOAA CINMS
# Deep sea coral valuation
#
# GOAL: graph state annual data
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

d3_a<-read_csv("./results/ec_annual_all_long.csv")%>% glimpse()


# check for NA category - should be empty
d3_a%>%
  filter(is.na(association))%>%
  glimpse()




# --------------------------------------------------------------
# STATE LEVEL SUMMARIZED DATA ###### ---------------------------------
# --------------------------------------------------------------

# select values for graphing -------------------------
d3<-d3_a%>%
  # filter(metric=="TotOut_CA2"|metric=="TotInc_CA2"| metric=="TotEmp_CA2"| metric=="Revenue")%>%
  filter(metric=="TotOut_CA2"|metric=="TotInc_CA2"| metric=="TotEmp_CA2")%>%
  mutate(
    metric2=str_replace_all(metric,pattern="TotEmp_CA2",replacement="Employment"),
    metric2=str_replace_all(metric2,pattern="TotOut_CA2",replacement="Output (million)"),
    metric2=str_replace_all(metric2,pattern="TotInc_CA2",replacement="Income (million)"),
    value_annual_1_mil=value/1000000)%>%
  mutate(Association=factor(association, levels = c("Associated","Definite Association", "Probable Association","Not Associated","Multispecies Group","No Data")))%>%
  filter(Association!="Multispecies Group")%>%
  mutate(value2=ifelse(metric=="TotEmp_CA2",value,value_annual_1_mil))%>%
  glimpse()

# d3$association




# --------------------------------------------
# GRAPHS ##### -------------------------------
# --------------------------------------------
source("./bin/deets.R")


metric_labs_t<-c("TotEmp_CA2"="Employment", "TotInc_CA2"="Income (Million USD)", "TotOut_CA2"= "Output (Million USD)")
metric_labs<-c("TotEmp_CA2"="Employment", "TotInc_CA2"="Income", "TotOut_CA2"= "Output")
metric_labs_p<-c("TotEmp_CA2"="Employment", "TotInc_CA2"="Income", "TotOut_CA2"= "Output")




# --------------------------------------------------------------
# STATE LEVEL SUMMARIZED DATA ###### ---------------------------
# --------------------------------------------------------------

# --------------------------------------------------------------
# graph annual totals - area ---------------------------
f1<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value2, fill=Association))+
      geom_area()+ #geom_line(size=1.2)+ #geom_smooth
      scale_fill_discrete_sequential(palette = "Batlow",nmax=4,order=(c(4:2)))+
      geom_hline(yintercept=0,color="lightgrey")+
      deets10+
      facet_wrap(vars(metric), nrow=3,scales="free_y", labeller = labeller(metric=metric_labs_t))+
      ylab("Deep Sea Coral Contributions to California")+
      scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}

  
f1(dta=d3,assoc="bl") 
ggsave("./doc/graph_annual1_bl.jpg",height=8,width=7)

f1(dta=d3,assoc="pr") 
ggsave("./doc/graph_annual1_pr.jpg",height=8,width=7)

f1(dta=d3,assoc="ha12") 
ggsave("./doc/graph_annual1_ha12.jpg",height=8,width=7)




# --------------------------------------------------------------
# graph percent of annual totals ---------------------------
f2<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=proportion, fill=Association))+
    geom_area()+ #geom_line(size=1.2)+ #geom_smooth
    scale_fill_discrete_sequential(palette = "Batlow",nmax=4,order=(c(4:2)))+
    geom_hline(yintercept=0,color="lightgrey")+
    deets10+
    facet_wrap(vars(metric2), nrow=3,scales="free_y", labeller = labeller(metric=metric_labs_p))+
    scale_y_continuous("Deep Sea Coral Contributions to California\n(proportion of fisheries)", limits=c(-.1,1.1),breaks=c(0,.5,1))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}

f2(dta=d3,assoc="bl") 
ggsave("./doc/graph_annual2_bl_p.jpg",height=8,width=7)

f2(dta=d3,assoc="pr") 
ggsave("./doc/graph_annual2_pr_p.jpg",height=8,width=7)

f2(dta=d3,assoc="ha12") 
ggsave("./doc/graph_annual2_ha12_p.jpg",height=8,width=7)



# --------------------------------------------------------------
# graph annual totals - smooth ---------------------------
f3<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value2, color=Association))+
    geom_smooth(size=1.2)+ #geom_line(size=1.2)+ #geom_smooth
    scale_color_discrete_sequential(palette = "Batlow",nmax=4,order=(c(4:2)))+
    geom_hline(yintercept=0,color="lightgrey")+
    deets10+
    facet_wrap(vars(metric), nrow=3,scales="free_y", labeller = labeller(metric=metric_labs_t))+
    # ylab("Deep Sea Coral Contributions to California")+
    scale_y_continuous("Deep Sea Coral Contributions to California")+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}


f3(dta=d3,assoc="bl") 
ggsave("./doc/graph_annual3_bl.jpg",height=8,width=7)

f3(dta=d3,assoc="pr") 
ggsave("./doc/graph_annual3_pr.jpg",height=8,width=7)

f3(dta=d3,assoc="ha12") 
ggsave("./doc/graph_annual3_ha12.jpg",height=8,width=7)





# --------------------------------------------------------------
# graph annual totals - line ---------------------------
f4<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value2, color=Association))+
    geom_line(size=1.2)+ #geom_line(size=1.2)+ #geom_smooth
    scale_color_discrete_sequential(palette = "Batlow",nmax=4,order=(c(4:2)))+
    geom_hline(yintercept=0,color="lightgrey")+
    deets10+
    facet_wrap(vars(metric), nrow=3,scales="free_y", labeller = labeller(metric=metric_labs_t))+
    # ylab("Deep Sea Coral Contributions to California")+
    scale_y_continuous("Deep Sea Coral Contributions to California")+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}


f4(dta=d3,assoc="bl") 
ggsave("./doc/graph_annual4_bl.jpg",height=8,width=7)

f4(dta=d3,assoc="pr") 
ggsave("./doc/graph_annual4_pr.jpg",height=8,width=7)

f4(dta=d3,assoc="ha12") 
ggsave("./doc/graph_annual4_ha12.jpg",height=8,width=7)




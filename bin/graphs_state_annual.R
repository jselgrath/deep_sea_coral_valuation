# Jennifer Selgrath
# NOAA CINMS
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
  glimpse()

# d3$association




# --------------------------------------------
# GRAPHS ##### -------------------------------
# --------------------------------------------
source("./bin/deets.R")


metric_labs_t<-c("TotEmp"="Employment (Total)", "TotInc"="Income (Million)", "TotOut"= "Output (Million)")
metric_labs<-c("TotEmp"="Employment", "TotInc"="Income", "TotOut"= "Output")
metric_labs_p<-c("TotEmp"="Employment (Proportion)", "TotInc"="Income (Proportion)", "TotOut"= "Output (Proportion)")




# --------------------------------------------------------------
# STATE LEVEL SUMMARIZED DATA ###### ---------------------------
# --------------------------------------------------------------

# graph annual totals - area ---------------------------
f1<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value_annual_1_mil, fill=Association))+
      geom_area()+ #geom_line(size=1.2)+ #geom_smooth
      scale_fill_discrete_sequential(palette = "Hawaii")+
      geom_hline(yintercept=0,color="lightgrey")+
      deets9+
      facet_wrap(vars(metric2), nrow=3,scales="free_y", labeller = labeller(metric=metric_labs_t))+
      scale_y_continuous("Total Contributions to California")+
      scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}

  
f1(dta=d3,assoc="bl") 
ggsave("./doc/graph_annual1_bl.jpg",height=8,width=4)

f1(dta=d3,assoc="pr") 
ggsave("./doc/graph_annual1_pr.jpg",height=8,width=4)

f1(dta=d3,assoc="ha12") 
ggsave("./doc/graph_annual1_ha12.jpg",height=8,width=4)



# graph annual totals - line ---------------------------
f1<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=value_annual_1_mil, color=Association))+
    geom_line()+ #geom_line(size=1.2)+ #geom_smooth
    scale_color_discrete_sequential(palette = "Hawaii")+
    geom_hline(yintercept=0,color="lightgrey")+
    deets9+
    facet_wrap(vars(metric2), nrow=3,scales="free_y", labeller = labeller(metric=metric_labs_t))+
    ylab("Total Contributions to California")+
    scale_y_continuous("Total Income (Mil)")+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}


f1(dta=d3,assoc="bl") 
ggsave("./doc/graph_annual2_bl.jpg",height=8,width=4)

f1(dta=d3,assoc="pr") 
ggsave("./doc/graph_annual2_pr.jpg",height=8,width=4)

f1(dta=d3,assoc="ha12") 
ggsave("./doc/graph_annual2_ha12.jpg",height=8,width=4)





# graph percent of annual totals ---------------------------
f2<-function(dta,assoc){
  dta%>%
    filter(assoc_type==assoc)%>%
    ggplot(aes(x=year,y=proportion, fill=Association))+
    geom_area()+ #geom_line(size=1.2)+ #geom_smooth
    scale_fill_discrete_sequential(palette = "Hawaii")+
    geom_hline(yintercept=0,color="lightgrey")+
    deets9+
    facet_wrap(vars(metric2), nrow=3,scales="free_y", labeller = labeller(metric=metric_labs_p))+
    # ylab("Proportional Contributions to California Fisheries")+ #, limits=c(-.1,1),breaks=c(0,.5,1)
    scale_y_continuous("Annual Deep Sea Coral Contributions \n(proportion of fisheries)", limits=c(-.1,1.1),breaks=c(0,.5,1))+
    scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
}

f2(dta=d3,assoc="bl") 
ggsave("./doc/graph_annual3_bl_p.jpg",height=8,width=4)

f2(dta=d3,assoc="pr") 
ggsave("./doc/graph_annual3_pr_p.jpg",height=8,width=4)

f2(dta=d3,assoc="ha12") 
ggsave("./doc/graph_annual3_ha12_p.jpg",height=8,width=4)








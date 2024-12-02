# Jennifer Selgrath
# NOAA CINMS
#
# GOAL: graph data
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

d1_a<-read_csv("./results/ec_port_all_long.csv")%>%glimpse()
d1_a
d2_a<-read_csv("./results/ec_state_all_long.csv")%>%glimpse()
d3_a<-read_csv("./results/ec_annual_all_long.csv")%>% glimpse()

ports<-read_csv("./data/ports_n_to_s.csv")%>% glimpse() # to reorder factor


# add order for ports to be grouped geographically ----------------
d1_ap<-d1_a%>%
  left_join(ports)%>%glimpse()

filter(d1_ap,!is.na(port_order))%>%glimpse() # check

# ----------------------------------------------------
# PORT LEVEL DATA ####--------------------------------
# ----------------------------------------------------
unique(d1_ap$metric)


d1<-d1_ap%>%
  filter(metric=="TotOut"|metric=="TotInc"| metric=="TotEmp")%>% # these are the three most useful values
  filter(!is.na(PortGroup_IOPAC))%>% # confirm but I think these are all 0s
  mutate(
    metric2=str_replace_all(metric,pattern="TotEmp",replacement="Total Employment"),
    metric2=str_replace_all(metric2,pattern="TotOut",replacement="Total Output (million)"),
    metric2=str_replace_all(metric2,pattern="TotInc",replacement="Total Income (million)"),
    value_port_1_mil=value/1000000)%>%
  filter(port_group!="UNSP")%>%   # remove catches from unspecified ports
  mutate(PortGroup_IOPAC=factor(PortGroup_IOPAC),
         PortGroup_IOPAC=fct_reorder(PortGroup_IOPAC,port_order))%>%
  mutate(port_group=factor(port_group),
         port_group=fct_reorder(port_group,port_order))%>%
  glimpse()
d1

levels(d1$PortGroup_IOPAC)
levels(d1$port_group)
unique(d1$metric)



# summarize by port -----------------------------------
glimpse(d1)
names(d1)

# here b is to distinguish new calculations
# proportion does not work when summarized because existing prop values are for commodities
d1_b<-d1%>%
  group_by(year, PortGroup_IOPAC, port_order, port_group,scale, assoc_type,association,metric, metric2)%>%
  summarize(
    n=n(),
    port_value=sum(value,na.rm=T),
    port_value_u=mean(value,na.rm=T),
    port_value_sd=sd(value,na.rm=T),
    port_value_mil=sum(value,na.rm=T)/1000000)%>%
  ungroup()%>%
  glimpse()
d1_b

range(d1_b$port_value)

# set finances to mil and employment to all
d1_b$value_1<-d1_b$port_value_mil
d1_b$value_1[d1_b$metric=="Employment"]<-d1_b$value_port_1_b[d1_b$metric=="Employment"]

glimpse(d1_b)





# ----------------------------------------------------
# COMMODITY/STATE LEVEL DATA ####--------------------------------
# ----------------------------------------------------
d2<-d2_a%>%
  filter(metric=="TotOut"|metric=="TotInc"| metric=="TotEmp")%>% # these are the three most useful values
  filter(!is.na(value_state_allSp))%>% # remove NAs - confirm but should all  be 0
  mutate(
    metric2=str_replace_all(metric,pattern="TotEmp",replacement="Total Employment"),
    metric2=str_replace_all(metric2,pattern="TotOut",replacement="Total Output (million)"),
    metric2=str_replace_all(metric2,pattern="TotInc",replacement="Total Income (million)"),
    value_comm_1_mil=value_state_1/1000000,
    value_comm_1NA_mil=value_state_1NA/1000000)%>%
  mutate(commodity=factor(COMMCD))%>%
  mutate(desc=factor(DESCRIPTION))%>%
  glimpse()

d2

# summarize by commodity -----------------------------------
glimpse(d2)
names(d2)

d2_b<-d2%>%
  group_by(year, commodity, desc, metric, metric2,Sector)%>%
  summarize(
    n=n(),
    value_comm_1_b=      sum(value_state_1, na.rm=T),
    value_comm_allSp_b=  sum(value_state_allSp, na.rm=T),
    value_comm_1NA_b=    sum(value_state_1NA, na.rm=T),
    value_comm_1_prp_b=  value_comm_1_b/value_comm_allSp_b,
    value_comm_1NA_prp_b=value_comm_1NA_b/value_comm_allSp_b,
    value_comm_1_mil_b=  value_comm_1_b/1000000,
    value_comm_1NA_mil_b=value_comm_1NA_b/1000000)%>%
  ungroup()%>%
  glimpse()

range(d2_b$value_comm_1_prp_b)

# set finances to mil and employment to all
d2_b$value_1<-d2_b$value_comm_1_mil_b
d2_b$value_1[d2_b$metric=="Employment"]<-d2_b$value_comm_1_b[d2_b$metric=="Employment"]

glimpse(d2_b)








# --------------------------------------------------------------
# STATE LEVEL SUMMARIZED DATA ###### ---------------------------------
# --------------------------------------------------------------

# select values for graphing -------------------------
d3<-d3_a%>%
  filter(metric=="TotOut"|metric=="TotInc"| metric=="TotEmp")%>%
  mutate(
    metric2=str_replace_all(metric,pattern="TotEmp",replacement="Employment"),
    metric2=str_replace_all(metric2,pattern="TotOut",replacement="Output (million)"),
    metric2=str_replace_all(metric2,pattern="TotInc",replacement="Income (million)"),
    value_1_mil=value_annual_1/1000000,
    value_1NA_mil=value_annual_1NA/1000000)%>%
  glimpse()

unique(d3$metric)

# set finances to mil and employment to all
d3$value_1<-d3$value_1_mil
d3$value_1[d3$metric=="TotEmp"]<-d3$value_annual_1[d3$metric=="TotEmp"]

d3






# --------------------------------------------
# GRAPHS ##### -------------------------------
# --------------------------------------------
source("./bin/deets.R")
metric_labs_t<-c("TotEmp"="Employment (Total)", "TotInc"="Income (Total)", "TotOut"= "Output (Total)")
metric_labs<-c("TotEmp"="Employment", "TotInc"="Income", "TotOut"= "Output")
metric_labs_p<-c("TotEmp"="Employment (Proportion)", "TotInc"="Income (Proportion)", "TotOut"= "Output (Proportion)")

# split data by metric so scales are easier to interpret ---------------------------
d1_be<-d1_b%>%filter(metric=="TotEmp")
d1_bi<-d1_b%>%filter(metric=="TotInc")
d1_bo<-d1_b%>%filter(metric=="TotOut")

d2_be<-d2_b%>%filter(metric=="TotEmp")
d2_bi<-d2_b%>%filter(metric=="TotInc")
d2_bo<-d2_b%>%filter(metric=="TotOut")



# --------------------------------------------------------------
# STATE LEVEL SUMMARIZED DATA ###### ---------------------------
# --------------------------------------------------------------

# graph annual totals
ggplot(data=d3,aes(x=year,y=value_1, color=metric2))+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "Hawaii")+
  geom_hline(yintercept=0,color="lightgrey")+
  deets8+
  facet_wrap(vars(metric2), nrow=3,scales="free_y", labeller = labeller(metric=metric_labs_t))+
  ylab("Total Contributions to California")+
  # scale_y_continuous("Deep Sea Coral Associated Spp.\nTotal (CA)", limits=c(-.1,1),breaks=c(0,.5,1))+
  scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))

ggsave("./doc/graph_annual.jpg",height=8,width=4)


# graph percent of annual totals
ggplot(data=d3,aes(x=year,y=value_annual_1_prp, color=metric))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_hline(yintercept=.75,color="lightgrey")+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "Hawaii")+
  deets8+
  facet_wrap(vars(metric), nrow=3,scales="free_y", labeller = labeller(metric=metric_labs_p))+
  # ylab("Proportion of Contributions to California Fisheries")+
  # facet_wrap(vars(metric), nrow=3, labeller = labeller(metric=metric_labs_p))+
  # ylab("Proportion of State Fisheries")+
  scale_y_continuous("Proportional Contributions to California Fisheries", limits=c(-.1,1),breaks=c(0,.5,1))+
  # scale_y_continuous("Annual Deep Sea Coral Contributions \n(proportion of fisheries)", limits=c(-.1,1),breaks=c(0,.5,1))+
  scale_x_continuous("Year", limits=c(2010,2020),breaks=c(2010,2015,2020))

ggsave("./doc/graph_annual_prop.jpg",height=8,width=4)



# --------------------------------------------------------------
# PORT LEVEL DATA ####------------------------------------------
# --------------------------------------------------------------


# ------------------------------------------------
# Port level graphs for TOTAL values
# ------------------------------------------------


#  port total - all metrics in one graph  --------------------------------------------
ggplot(data=d1_b,aes(x=year,y=value_port_1_mil_b, color=metric2))+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "TealGrn")+
  geom_hline(yintercept=0,color="lightgrey")+
  deets8+
  facet_grid(port_group~metric,scales="free")+
  ylab("Contributions by Port")+
  # scale_y_continuous("Port-Level Deep Sea Coral Associated Spp.\nTotal (CA)", limits=c(-.1,1),breaks=c(0,.5,1))+
  scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))

ggsave("./doc/graph_port.jpg",height=8,width=4)



# port employment - total ----------------------------------------------

ggplot(data=d1_be,aes(x=year,y=value_port_1_b, color=port_group))+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "TealGrn")+
  geom_hline(yintercept=0,color="lightgrey")+
  deets8+
  facet_grid(port_group~metric2,scales="free")+
  scale_y_continuous("Total Employment", limits=c(-1,1800),breaks=c(0,750,1500))+
  scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
ggsave("./doc/graph_port_emp.jpg",height=8,width=4)


# port income - total by million ----------------------------------------

ggplot(data=d1_bi,aes(x=year,y=value_port_1_mil_b, color=port_group))+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "TealGrn")+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_hline(yintercept=50,color="lightgrey")+
  deets8+
  facet_grid(port_group~metric2,scales="free")+
  # ylab("Deep Sea Coral Contributions By Port")+
  scale_y_continuous("Total Income (mil)", limits=c(-1,100),breaks=c(0,50,100))+
  scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
ggsave("./doc/graph_port_inc.jpg",height=8,width=4)



# port output - total by million ---------------------------------------

ggplot(data=d1_bo,aes(x=year,y=value_port_1_mil_b, color=port_group))+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "TealGrn")+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_hline(yintercept=100,color="lightgrey")+
  deets8+
  facet_grid(port_group~metric2,scales="free")+
  ylab("Deep Sea Coral Contributions By Port")+
  scale_y_continuous("Total Output (mil)", limits=c(-1,200),breaks=c(0,100,200))+
  scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
ggsave("./doc/graph_port_out.jpg",height=8,width=4)







# ------------------------------------------------
# Port level graphs for PROPORTIONAL values
# ------------------------------------------------


# port porportional all metrics --------------------------------------
ggplot(data=d1_b,aes(x=year,y=value_port_1_prp_b, color=metric))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_hline(yintercept=.75,color="lightgrey")+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "TealGrn")+
  deets8+
  facet_grid(port_group~metric, labeller = labeller(metric=metric_labs_p))+
  scale_y_continuous("Proportion of Total Port", limits=c(-.1,1),breaks=c(0,.5,1))+
  # scale_y_continuous("Deep Sea Coral Contributions by Port\n(proportion of fisheries total)", limits=c(-.1,1),breaks=c(0,.5,1))+
  scale_x_continuous("Year", limits=c(2010,2020),breaks=c(2010,2015,2020))

 ggsave("./doc/graph_port_p.jpg",height=8,width=4)


 
 
# port employment - proportional ----------------------------------------------
ggplot(data=d1_be,aes(x=year,y=value_port_1_prp_b, color=port_group))+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "TealGrn")+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_hline(yintercept=.75,color="lightgrey")+
  deets8+
  facet_grid(port_group~metric, labeller = labeller(metric=metric_labs_p))+
  scale_y_continuous("Proportion of Total Port", limits=c(-.1,1),breaks=c(0,.5,1))+
  # scale_y_continuous("Proportion of Total Fisheries", limits=c(-.1,1),breaks=c(0,.5,1))+le_y_continuous("Deep Sea Coral Contributions by Port\n(proportion of fisheries total)", limits=c(-.1,1),breaks=c(0,.5,1))+
  scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
ggsave("./doc/graph_port_emp_p.jpg",height=8,width=4)



# port income - proportional by million ----------------------------------------------
ggplot(data=d1_bi,aes(x=year,y=value_port_1_prp_b, color=port_group))+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "TealGrn")+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_hline(yintercept=.75,color="lightgrey")+
  deets8+
  facet_grid(port_group~metric, labeller = labeller(metric=metric_labs_p))+
  scale_y_continuous("Proportion of Total Fisheries", limits=c(-.1,1),breaks=c(0,.5,1))+
  # scale_y_continuous("Deep Sea Coral Contributions by Port\n(proportion of fisheries total)", limits=c(-.1,1),breaks=c(0,.5,1))+
  scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
ggsave("./doc/graph_port_inc_p.jpg",height=8,width=4)


# port output - proportional by million ---------------------------------------------
ggplot(data=d1_bo,aes(x=year,y=value_port_1_prp_b, color=port_group))+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "TealGrn")+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_hline(yintercept=.75,color="lightgrey")+
  deets8+
  facet_grid(port_group~metric, labeller = labeller(metric=metric_labs_p))+
  # ylab("Deep Sea Coral Contributions By Port")+
  scale_y_continuous("Proportion of Total Fisheries", limits=c(-.1,1),breaks=c(0,.5,1))+
  # scale_y_continuous("Deep Sea Coral Contributions By Port\n(Total Output (proportion))", limits=c(-.1,1),breaks=c(0,0.5,1))+
  scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))

ggsave("./doc/graph_port_out_p.jpg",height=8,width=4)





# ------------------------------------------------
# Commodity level graphs for TOTAL values
# ------------------------------------------------
d2_b
glimpse(d2_b)
unique(d2_b$PortGroup_IOPAC)
unique(d2_b$COMMCD)

cbind(levels(d2_b$commodity),levels(d2_b$desc))

#  commodity total - all metrics in one graph  --------------------------------------------
ggplot(data=d2_b,aes(x=year,y=value_comm_1_mil_b, color=commodity))+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "SunsetDark")+
  geom_hline(yintercept=0,color="lightgrey")+
  deets8+
  facet_grid(commodity~metric2,scales="free")+
  ylab("Deep Sea Coral Contributions By Commodity")+
  # scale_y_continuous("Port-Level Deep Sea Coral Associated Spp.\nTotal (CA)", limits=c(-.1,1),breaks=c(0,.5,1))+
  scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))

ggsave("./doc/graph_comm.jpg",height=30,width=4)


# remove species that are all 0s for easier reading of graph
#  0 - coastal pelagics, Highly Migratory Sp, Salmon, Whiting (aka hake), 
# >0 - Crabs, Dover/Thornyhead, Halibut (may be all groundfish), Other Groundfish, Sablefish, Shrimp



# commodity employment - total ----------------------------------------------

ggplot(data=d2_be,aes(x=year,y=value_comm_1_b, color=commodity))+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "SunsetDark")+
  geom_hline(yintercept=0,color="lightgrey")+
  deets8+
  facet_grid(commodity~metric2,scales="free")+
  # ylab("Deep Sea Coral Contributions By Port")+
  scale_y_continuous("Deep Sea Coral Contributions By Commodity\n(total employment)", limits=c(-1,2500),breaks=c(0,1250,2500))+
  scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
ggsave("./doc/graph_com_emp.jpg",height=20,width=4)


# commodity income - total by million ----------------------------------------

ggplot(data=d2_bi,aes(x=year,y=value_comm_1_mil_b, color=commodity))+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "SunsetDark")+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_hline(yintercept=100,color="lightgrey")+
  deets8+
  facet_grid(commodity~metric2,scales="free")+
  # ylab("Deep Sea Coral Contributions By Port")+
  scale_y_continuous("Deep Sea Coral Contributions By Commodity\n(total Income (mil))", limits=c(-1,200),breaks=c(0,100,200))+
  scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
ggsave("./doc/graph_com_inc.jpg",height=20,width=4)



# commodity output - total by million ---------------------------------------

ggplot(data=d2_bo,aes(x=year,y=value_comm_1_mil_b, color=commodity))+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "SunsetDark")+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_hline(yintercept=100,color="lightgrey")+
  deets8+
  facet_grid(commodity~metric2,scales="free")+
  ylab("Deep Sea Coral Contributions By Port")+
  scale_y_continuous("Deep Sea Coral Contributions By Commodity\n(total output (mil))", limits=c(-1,300),breaks=c(0,150,300))+
  scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
ggsave("./doc/graph_com_out.jpg",height=20,width=4)






# ------------------------------------------------
# Commodity level graphs for PROPORTIONAL values
# ------------------------------------------------


# commodity porportional all metrics --------------------------------------
ggplot(data=d2_b,aes(x=year,y=value_comm_1_prp_b, color=commodity))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_hline(yintercept=.75,color="lightgrey")+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "SunsetDark")+
  deets8+
  facet_grid(commodity~metric, labeller = labeller(metric=metric_labs_p))+
  scale_y_continuous("Proportion of Total Commodity", limits=c(-.1,1),breaks=c(0,.5,1))+
  # scale_y_continuous("Deep Sea Coral Contributions By Commodity\n(proportion of fisheries total)", limits=c(-.1,1),breaks=c(0,.5,1))+
  scale_x_continuous("Year", limits=c(2010,2020),breaks=c(2010,2015,2020))

ggsave("./doc/graph_com_p.jpg",height=20,width=5)




# commodity employment - proportional ----------------------------------------------
ggplot(data=d2_be,aes(x=year,y=value_comm_1_prp_b, color=commodity))+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "SunsetDark")+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_hline(yintercept=.75,color="lightgrey")+
  deets8+
  facet_grid(commodity~metric, labeller = labeller(metric=metric_labs_p))+
  scale_y_continuous("Proportion of Total Commodity", limits=c(-.1,1),breaks=c(0,.5,1))+
  # scale_y_continuous("Deep Sea Coral Contributions By Commodity\n(proportion of fisheries)", limits=c(-.1,1),breaks=c(0,.5,1))+
  scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))

ggsave("./doc/graph_com_emp_p.jpg",height=20,width=4)



# commodity income - proportional by million ----------------------------------------------
ggplot(data=d2_bi,aes(x=year,y=value_comm_1_prp_b, color=commodity))+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "SunsetDark")+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_hline(yintercept=.75,color="lightgrey")+
  deets8+
  facet_grid(commodity~metric, labeller = labeller(metric=metric_labs_p))+
  scale_y_continuous("Proportion of Total Commodity", limits=c(-.1,1),breaks=c(0,.5,1))+
  # scale_y_continuous("Deep Sea Coral Contributions By Commodity\n(proportion of fisheries)", limits=c(-.1,1),breaks=c(0,.5,1))+
  scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))
ggsave("./doc/graph_com_inc_p.jpg",height=20,width=4)


# commodity output - proportional by million ---------------------------------------------
ggplot(data=d2_bo,aes(x=year,y=value_comm_1_prp_b, color=commodity))+
  geom_line(size=1.2)+
  scale_color_discrete_sequential(palette = "SunsetDark")+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_hline(yintercept=.75,color="lightgrey")+
  deets8+
  facet_grid(commodity~metric, labeller = labeller(metric=metric_labs_p))+
  # ylab("Deep Sea Coral Contributions By Commodity")+
  scale_y_continuous("Proportion of Total Commodity", limits=c(-.1,1),breaks=c(0,.5,1))+
  # scale_y_continuous("Deep Sea Coral Contributions By Commodity\n(Total Output (proportion of fisheries))", limits=c(-.1,1),breaks=c(0,0.5,1))+
  scale_x_continuous("Year",limits=c(2010,2020),breaks=c(2010,2015,2020))

ggsave("./doc/graph_com_out_p.jpg",height=20,width=4)




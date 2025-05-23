# Jennifer Selgrath
# NOAA CINMS
# project: economic valuation of commercial fisheries associated with deep sea corals
#
# GOAL: Load files and change them to long format, merge with descriptions of commodity codes
# ---------------------------------------------------------
# FILE NAMES - MEANINGS & PURPOSE

# economics ------------------------------------
#   catch_nomults
#    ●	Catch that did not have associated multipliers
#   econcontributions_portlevel
#    ●	Results - broken down by 
#     ○	year, port group, COMMDC (commodity sector = species + gear)
#    ●	total values = vessel + processor
#   econcontributions_statelevel_ann
#    ●	Results - broken down by year
#   econcontributions_statelevel
#    ●	Results - broken down by year, COMMDC (commodity sector = species + gear)


# associations ------------------------------------
# bl = body length
# pr = proximity
# ha12 = probably (1) or definitely (2) associated with depth and habitat
# ha2 =  definitely associated with depth and habitat
# all = all species

#----------------------------------------------------------
library(tidyverse); library(ggplot2)

#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/Jennifer.Selgrath/Documents/research/R_projects/dsc_valuation/")


# load files - DSC associated sp by body length (bl) -------------------------------
d0_a<-read_csv("./results/econcontributions_portlevel_bl.csv")%>%  glimpse()
d0_b<-read_csv("./results/econcontributions_statelevel_bl.csv")%>%  
  glimpse()
d0_c<-read_csv("./results/econcontributions_statelevel_ann_bl.csv")%>%  glimpse()

# load files - DSC associated sp by proximity (pr)-------------------------------
d1_a<-read_csv("./results/econcontributions_portlevel_pr.csv")%>%  glimpse()
d1_b<-read_csv("./results/econcontributions_statelevel_pr.csv")%>%  
  glimpse()
d1_c<-read_csv("./results/econcontributions_statelevel_ann_pr.csv")%>%  glimpse()

# load files - DSC associated sp by depth and habitat (ha) - probably, definetly-------------------------------
d2_a<-read_csv("./results/econcontributions_portlevel_ha12.csv")%>%  glimpse()
d2_b<-read_csv("./results/econcontributions_statelevel_ha12.csv")%>%  
  glimpse()
d2_c<-read_csv("./results/econcontributions_statelevel_ann_ha12.csv")%>%  glimpse()

# load files - all spp --------------------------------------------------------------
d3_a<-read_csv("./results/econcontributions_portlevel_all.csv")%>%  glimpse()
d3_b<-read_csv("./results/econcontributions_statelevel_all.csv")%>%  
  glimpse()
d3_c<-read_csv("./results/econcontributions_statelevel_ann_all.csv")%>%  glimpse()




# ----------------------------------------------------------
# wide to long functions


# port data ------------------
f_longer_port<-function(dta){
  dta%>%
    pivot_longer(cols=-c(year,assoc,PortGroup_IOPAC,COMMCD,COMMCD2,Sector),names_to="metric",values_to="value_port")
}

# pivot port files ------------------
d0_aL<-f_longer_port(d0_a)%>%
  mutate(value_port_bl=value_port,assoc_port_bl=assoc)%>%select(!value_port,-assoc)%>%  glimpse()
d1_aL<-f_longer_port(d1_a)%>%
  mutate(value_port_pr=value_port,assoc_port_pr=assoc)%>%select(!value_port,-assoc)%>%  glimpse()
d2_aL<-f_longer_port(d2_a)%>%
  mutate(value_port_ha12=value_port,assoc_port_ha12=assoc)%>%select(!value_port,-assoc)%>%  glimpse()
d3_aL<-f_longer_port(d3_a)%>%
  mutate(value_port_allSp=value_port,assoc_port_allSp=assoc)%>%select(!value_port,-assoc)%>%
  glimpse()


# state data ------------------
f_longer_state<-function(dta){
  dta%>%
    pivot_longer(cols=-c(year,assoc,PortGroup_IOPAC,COMMCD,COMMCD2,Sector),names_to="metric",values_to="value_state")
}

# pivot files
d0_bL<-f_longer_state(d0_b)%>%
  mutate(value_state_bl=value_state,assoc_state_bl=assoc)%>%select(!value_state,-assoc)%>%
  glimpse()
d1_bL<-f_longer_state(d1_b)%>%
  mutate(value_state_pr=value_state,assoc_state_pr=assoc)%>%select(!value_state,-assoc)%>%
  glimpse()
d2_bL<-f_longer_state(d2_b)%>%
  mutate(value_state_ha12=value_state,assoc_state_ha12=assoc)%>%select(!value_state,-assoc)%>%
  glimpse()
d3_bL<-f_longer_state(d3_b)%>%
  mutate(value_state_allSp=value_state,assoc_state_allSp=assoc)%>%select(!value_state,-assoc)%>%
  glimpse()

# annual ------------------
f_longer_yr<-function(dta){
  dta%>%
    pivot_longer(cols=-c(year,assoc,PortGroup_IOPAC),names_to="metric",values_to="value_annual")
}

# pivot files
d0_cL<-f_longer_yr(d0_c)%>%
  mutate(value_annual_bl=value_annual,assoc_annual_bl=assoc)%>%select(!value_annual,-assoc)%>%  glimpse()
d1_cL<-f_longer_yr(d1_c)%>%
  mutate(value_annual_pr=value_annual,assoc_annual_pr=assoc)%>%select(!value_annual,-assoc)%>%  glimpse()
d2_cL<-f_longer_yr(d2_c)%>%
  mutate(value_annual_ha12=value_annual,assoc_annual_ha12=assoc)%>%select(!value_annual,-assoc)%>%  glimpse()
d3_cL<-f_longer_yr(d3_c)%>%
  mutate(value_annual_allSp=value_annual,assoc_annual_allSp=assoc)%>%select(!value_annual,-assoc)%>%  glimpse()



# --------------------------------------------------------------------------
# NO MULTIPLIER DATA ALREADY IN LONG FORM. REMOVED CODE 
# --------------------------------------------------------------------------

# save ########### ----------------------------------------
write_csv(d0_aL,"./results/ec_port_bl_long.csv")
write_csv(d1_aL,"./results/ec_port_pr_long.csv")
write_csv(d2_aL,"./results/ec_port_ha12_long.csv")
write_csv(d3_aL,"./results/ec_port_allSp_long.csv")

write_csv(d0_bL,"./results/ec_state_bl_long.csv")
write_csv(d1_bL,"./results/ec_state_pr_long.csv")
write_csv(d2_bL,"./results/ec_state_ha12_long.csv")
write_csv(d3_bL,"./results/ec_state_allSp_long.csv")

write_csv(d0_cL,"./results/ec_annual_bl_long.csv")
write_csv(d1_cL,"./results/ec_annual_pr_long.csv")
write_csv(d2_cL,"./results/ec_annual_ha12_long.csv")
write_csv(d3_cL,"./results/ec_annual_allSp_long.csv")

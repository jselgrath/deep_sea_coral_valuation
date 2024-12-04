# Jennifer Selgrath
# NOAA CINMS
#
# GOAL: join tables of same data type and calculate percent
# ------------
# METADATA
#   catch_nomults - Catch that did not have associated multipliers
#   COMMDC (commodity sector = species + gear)
#   total values = vessel + processor

# no multiplier data included in other data, so 
#----------------------------------------------------------
library(tidyverse); library(ggplot2)

#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/Jennifer.Selgrath/Documents/research/R_projects/dsc_valuation/")


# commodity code descriptions
d_cc<-read_csv("./results/commcd_dat2.csv")%>%glimpse()

# body length
d1_a<-read_csv("./results/ec_port_bl_long.csv")%>%glimpse()
d2_a<-read_csv("./results/ec_state_bl_long.csv")%>%glimpse()
d3_a<-read_csv("./results/ec_annual_bl_long.csv")%>%glimpse()


# proximity
d1_b<-read_csv("./results/ec_port_pr_long.csv")%>%glimpse()
d2_b<-read_csv("./results/ec_state_pr_long.csv")%>%glimpse()
d3_b<-read_csv("./results/ec_annual_pr_long.csv")%>%glimpse()


# habitat 12
d1_b1<-read_csv("./results/ec_port_ha12_long.csv")%>%glimpse()
d2_b1<-read_csv("./results/ec_state_ha12_long.csv")%>%glimpse()
d3_b1<-read_csv("./results/ec_annual_ha12_long.csv")%>%glimpse()


# all species
d1_c<-read_csv("./results/ec_port_allSp_long.csv")%>%glimpse()
d2_c<-read_csv("./results/ec_state_allSp_long.csv")%>%glimpse()
d3_c<-read_csv("./results/ec_annual_allSp_long.csv")%>%glimpse()




# join data with all species, calc proportions    #################################################

#----------------------------------------------------------
# PORT - WITH COMMODITIES ----------------------------------------
#----------------------------------------------------------
# port, bl ---------------------------
d5a<-d1_a%>%
  full_join(d1_c)%>% 
  mutate(value_port_bl_prp=value_port_bl/value_port_allSp)%>%
  select(-assoc_port_allSp,-value_port_allSp)%>%
  select(PortGroup_IOPAC:value_port_bl_prp,association=assoc_port_bl,value=value_port_bl,proportion=value_port_bl_prp)%>%
  mutate(scale="port",assoc_type="bl")%>%
  select(PortGroup_IOPAC:year,sector=Sector,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d5a


# port, pr ---------------------------
d5b<-d1_b%>%
  full_join(d1_c)%>% #(28160 records)
  mutate(
    value_port_pr_prp=value_port_pr/value_port_allSp)%>%
  select(-assoc_port_allSp, -value_port_allSp)%>%
  select(PortGroup_IOPAC:value_port_pr_prp,association=assoc_port_pr,value=value_port_pr,proportion=value_port_pr_prp)%>%
  mutate(scale="port",assoc_type="pr")%>%
  select(PortGroup_IOPAC:year,sector=Sector,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d5b


# port, ha12 ------------------------
d5c<-d1_b1%>%
  full_join(d1_c)%>% #(28160 records)
  mutate(
     value_port_ha12_prp=value_port_ha12/value_port_allSp)%>%
  select(-assoc_port_allSp, -value_port_allSp)%>%
  select(PortGroup_IOPAC:value_port_ha12_prp,association=assoc_port_ha12,value=value_port_ha12,proportion=value_port_ha12_prp)%>%
  mutate(scale="port",assoc_type="ha12")%>%
  select(PortGroup_IOPAC:year,sector=Sector,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d5c


# join port records -------------------------
d5d<-d5a%>%
  rbind(d5b)%>%
  rbind(d5c)%>%
  left_join(d_cc)%>%
  glimpse()

d5d


#----------------------------------------------------------
# PORT - NO COMMODITIES ----------------------------------------
#----------------------------------------------------------
d1_c2<-d1_c%>%
  select(-COMMCD,-COMMCD2,-Sector)%>%
  group_by(PortGroup_IOPAC,year,metric)%>%
  summarize(
    value_port_allSp2=sum(value_port_allSp,na.rm=T)
  )%>%
  glimpse()

# port, bl ---------------------------
d5a2<-d1_a%>%
  select(-COMMCD,-COMMCD2,-Sector)%>%
  group_by(PortGroup_IOPAC,year,metric,assoc_port_bl)%>%
  summarize(
    value_port_bl2=sum(value_port_bl,na.rm=T)
  )%>%
  full_join(d1_c2)%>% 
  mutate(value_port_bl_prp2=value_port_bl2/value_port_allSp2)%>%
  # select(-value_port_allSp2)%>%
  select(PortGroup_IOPAC:value_port_bl_prp2,association=assoc_port_bl,value=value_port_bl2,proportion=value_port_bl_prp2,value_port_allSp2)%>%
  mutate(scale="port",assoc_type="bl")%>%
  select(PortGroup_IOPAC:year,scale,assoc_type,association,metric,value,proportion,value_port_allSp2)%>%
  glimpse()
d5a2


# port, pr ---------------------------
d5b2<-d1_b%>%  
  select(-COMMCD,-COMMCD2,-Sector)%>%
  group_by(PortGroup_IOPAC,year,metric,assoc_port_pr)%>%
  summarize(
    value_port_pr2=sum(value_port_pr,na.rm=T)
  )%>%
  full_join(d1_c2)%>% 
  mutate(value_port_pr_prp2=value_port_pr2/value_port_allSp2)%>%
  # select(-value_port_allSp2)%>%
  select(PortGroup_IOPAC:value_port_pr_prp2,association=assoc_port_pr,value=value_port_pr2,proportion=value_port_pr_prp2,value_port_allSp2)%>%
  mutate(scale="port",assoc_type="pr")%>%
  select(PortGroup_IOPAC:year,scale,assoc_type,association,metric,value,proportion,value_port_allSp2)%>%
  glimpse()
d5b2


# port, ha12 ------------------------
d5c2<-d1_b1%>%
  select(-COMMCD,-COMMCD2,-Sector)%>%
  group_by(PortGroup_IOPAC,year,metric,assoc_port_ha12)%>%
  summarize(
    value_port_ha122=sum(value_port_ha12,na.rm=T)
  )%>%
  full_join(d1_c2)%>% 
  mutate(value_port_ha12_prp2=value_port_ha122/value_port_allSp2)%>%
  # select(-value_port_allSp2)%>%
  select(PortGroup_IOPAC:value_port_ha12_prp2,association=assoc_port_ha12,value=value_port_ha122,proportion=value_port_ha12_prp2,value_port_allSp2)%>%
  mutate(scale="port",assoc_type="ha12")%>%
  select(PortGroup_IOPAC:year,scale,assoc_type,association,metric,value,proportion,value_port_allSp2)%>%
  glimpse()
d5c2


# join port records -------------------------
d5d2<-d5a2%>%
  rbind(d5b2)%>%
  rbind(d5c2)%>%
  glimpse()

d5d2


# ---------------------------------------------------------------
# join all records for the state, calc proportions --------------------------------

# bl --------------------------------
d6a<-d2_a%>%
  full_join(d2_c)%>% 
  mutate(
    value_state_bl_prp=value_state_bl/value_state_allSp)%>%
  select(-assoc_state_allSp,-value_state_allSp)%>%
  select(COMMCD:value_state_bl_prp,association=assoc_state_bl,value=value_state_bl,proportion=value_state_bl_prp)%>%
  mutate(scale="state",assoc_type="bl")%>%
  select(COMMCD:year,sector=Sector,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d6a


# pr --------------------------
d6b<-d2_b%>%
  full_join(d2_c)%>% 
  mutate(
    value_state_pr_prp=value_state_pr/value_state_allSp)%>%
  select(-assoc_state_allSp,-value_state_allSp)%>%
  select(COMMCD:value_state_pr_prp,association=assoc_state_pr,value=value_state_pr,proportion=value_state_pr_prp)%>%
  mutate(scale="state",assoc_type="pr")%>%
  select(COMMCD:year,sector=Sector,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d6b



# ha12 ------------------------------------
d6c<-d2_b1%>%
  full_join(d2_c)%>% 
  mutate(
    value_state_ha12_prp=value_state_ha12/value_state_allSp)%>%
  select(-assoc_state_allSp,-value_state_allSp)%>%
  select(COMMCD:value_state_ha12_prp,association=assoc_state_ha12,value=value_state_ha12,proportion=value_state_ha12_prp)%>%
  mutate(scale="state",assoc_type="ha12")%>%
  select(COMMCD:year,sector=Sector,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d6c



# join state records -----------------
d6d<-d6a%>%
  rbind(d6b)%>%
  rbind(d6c)%>%
  left_join(d_cc)%>%
  glimpse()

d6d


# ----------------------------------------------------------
# join all annual records --------------------------------

# bl -------------------------------
d7a<-d3_a%>%
  full_join(d3_c)%>% 
  mutate(
    value_annual_bl_prp=value_annual_bl/value_annual_allSp)%>%
  select(-assoc_annual_allSp,-value_annual_allSp)%>%
  select(year:value_annual_bl_prp,association=assoc_annual_bl,value=value_annual_bl,proportion=value_annual_bl_prp)%>%
  mutate(scale="annual",assoc_type="bl")%>%
  select(year,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d7a



# pr -------------------------------
d7b<-d3_b%>%
  full_join(d3_c)%>% 
  mutate(
    value_annual_pr_prp=value_annual_pr/value_annual_allSp  )%>%
  select(-assoc_annual_allSp,-value_annual_allSp)%>%
  select(year:value_annual_pr_prp,association=assoc_annual_pr,value=value_annual_pr,proportion=value_annual_pr_prp)%>%
  mutate(scale="annual",assoc_type="pr")%>%
  select(year,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d7b



# ha12 -------------------------------
d7c<-d3_b1%>%
  full_join(d3_c)%>% 
  mutate(
    value_annual_ha12_prp=value_annual_ha12/value_annual_allSp
  )%>%
  select(-assoc_annual_allSp,-value_annual_allSp)%>%
  select(year:value_annual_ha12_prp,association=assoc_annual_ha12,value=value_annual_ha12,proportion=value_annual_ha12_prp)%>%
  mutate(scale="annual",assoc_type="ha12")%>%
  select(year,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d7c


# join annual records
d7d<-d7a%>%
  rbind(d7b)%>%
  rbind(d7c)%>%
  glimpse()

d7d


# save ###################
write_csv(d5d,"./results/ec_port_com_all_long.csv")
write_csv(d5d2,"./results/ec_port_all_long.csv")
write_csv(d6d,"./results/ec_state_all_long.csv")
write_csv(d7d,"./results/ec_annual_all_long.csv")



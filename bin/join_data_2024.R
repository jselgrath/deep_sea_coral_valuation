# Jennifer Selgrath
# NOAA CINMS
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


# commodity code descriptions
d_cc<-read_csv("./results/commcd_dat2.csv")%>%glimpse()

# body length
d1_a<-read_csv("./results/ec_port_bl_long.csv")%>%glimpse()
d2_a<-read_csv("./results/ec_state_bl_long.csv")%>%glimpse()
d3_a<-read_csv("./results/ec_annual_bl_long.csv")%>%glimpse()
d4_a<-read_csv("./results/ec_noMult_bl_long.csv")%>%glimpse()

# proximity
d1_b<-read_csv("./results/ec_port_pr_long.csv")%>%glimpse()
d2_b<-read_csv("./results/ec_state_pr_long.csv")%>%glimpse()
d3_b<-read_csv("./results/ec_annual_pr_long.csv")%>%glimpse()
d4_b<-read_csv("./results/ec_noMult_pr_long.csv")%>%glimpse()

# habitat 12
d1_b1<-read_csv("./results/ec_port_ha12_long.csv")%>%glimpse()
d2_b1<-read_csv("./results/ec_state_ha12_long.csv")%>%glimpse()
d3_b1<-read_csv("./results/ec_annual_ha12_long.csv")%>%glimpse()
d4_b1<-read_csv("./results/ec_noMult_ha12_long.csv")%>%glimpse()

# habitat 2 (definitely associated)
# d1_b2<-read_csv("./results/ec_port_ha2_long.csv")%>%glimpse()
# d2_b2<-read_csv("./results/ec_state_ha2_long.csv")%>%glimpse()
# d3_b2<-read_csv("./results/ec_annual_ha2_long.csv")%>%glimpse()
# d4_b2<-read_csv("./results/ec_noMult_ha2_long.csv")%>%glimpse()

# all species
d1_c<-read_csv("./results/ec_port_allSp_long.csv")%>%glimpse()
d2_c<-read_csv("./results/ec_state_allSp_long.csv")%>%glimpse()
d3_c<-read_csv("./results/ec_annual_allSp_long.csv")%>%glimpse()
d4_c<-read_csv("./results/ec_noMult_allSp_long.csv")%>%glimpse()



# join data with all species and pivot longer #################################################

# join all records for ports  ---------------------------
d1_a[324,]

# port, bl ---------------------------
d5a<-d1_a%>%
  full_join(d1_c)%>% 
  mutate(  value_port_bl_prp=value_port_bl/value_port_allSp)%>%
  select(-assoc_port_allSp)%>%
  glimpse()
d5a

mean(d5a$value_port_bl_prp,na.rm=T)

# pivot long 
d5a2<-d5a%>%
  select(-value_port_allSp)%>%
  select(PortGroup_IOPAC:value_port_bl_prp,association=assoc_port_bl,value=value_port_bl,proportion=value_port_bl_prp)%>%
  mutate(scale="port",assoc_type="bl")%>%
  select(PortGroup_IOPAC:year,sector=Sector,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d5a2


# port, pr ---------------------------
d5b<-d1_b%>%
  full_join(d1_c)%>% #(28160 records)
  mutate(
    value_port_pr_prp=value_port_pr/value_port_allSp)%>%
  select(-assoc_port_allSp)%>%
  glimpse()
d5b

mean(d5b$value_port_pr_prp,na.rm=T)


# pivot long
d5b2<-d5b%>%
  select(-value_port_allSp)%>%
  select(PortGroup_IOPAC:value_port_pr_prp,association=assoc_port_pr,value=value_port_pr,proportion=value_port_pr_prp)%>%
  mutate(scale="port",assoc_type="pr")%>%
  select(PortGroup_IOPAC:year,sector=Sector,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d5b2


# port, ha12 ------------------------
d5c<-d1_b1%>%
  full_join(d1_c)%>% #(28160 records)
  mutate(
     value_port_ha12_prp=value_port_ha12/value_port_allSp)%>%
  select(-assoc_port_allSp)%>%
  glimpse()
d5c
mean(d5c$value_port_ha12_prp,na.rm=T)

# pivot long
d5c2<-d5c%>%
  select(-value_port_allSp)%>%
  select(PortGroup_IOPAC:value_port_ha12_prp,association=assoc_port_ha12,value=value_port_ha12,proportion=value_port_ha12_prp)%>%
  mutate(scale="port",assoc_type="ha12")%>%
  select(PortGroup_IOPAC:year,sector=Sector,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d5c2


# join port records -------------------------
d5d<-d5a2%>%
  rbind(d5b2)%>%
  rbind(d5c2)%>%
  left_join(d_cc)%>%
  glimpse()

d5d


# ---------------------------------------------------------------
# join all records for the state and pivot longer --------------------------------

# bl --------------------------------
d6a<-d2_a%>%
  full_join(d2_c)%>% 
  mutate(
    value_state_bl_prp=value_state_bl/value_state_allSp)%>%
  select(-assoc_state_allSp)%>%
  glimpse()

mean(d6a$value_state_bl_prp,na.rm=T)

# pivot long 
d6a2<-d6a%>%
  select(-value_state_allSp)%>%
  select(COMMCD:value_state_bl_prp,association=assoc_state_bl,value=value_state_bl,proportion=value_state_bl_prp)%>%
  mutate(scale="state",assoc_type="bl")%>%
  select(COMMCD:year,sector=Sector,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d6a2


# pr --------------------------
d6b<-d2_b%>%
  full_join(d2_c)%>% 
  mutate(
    value_state_pr_prp=value_state_pr/value_state_allSp)%>%
  select(-assoc_state_allSp)%>%
  glimpse()

mean(d6b$value_state_pr_prp,na.rm=T)

# pivot long 
d6b2<-d6b%>%
  select(-value_state_allSp)%>%
  select(COMMCD:value_state_pr_prp,association=assoc_state_pr,value=value_state_pr,proportion=value_state_pr_prp)%>%
  mutate(scale="state",assoc_type="pr")%>%
  select(COMMCD:year,sector=Sector,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d6b2



# ha12 ------------------------------------
d6c<-d2_b1%>%
  full_join(d2_c)%>% 
  mutate(
    value_state_ha12_prp=value_state_ha12/value_state_allSp)%>%
  select(-assoc_state_allSp)%>%
  glimpse()

mean(d6c$value_state_ha12_prp,na.rm=T)

# pivot long 
d6c2<-d6c%>%
  select(-value_state_allSp)%>%
  select(COMMCD:value_state_ha12_prp,association=assoc_state_ha12,value=value_state_ha12,proportion=value_state_ha12_prp)%>%
  mutate(scale="state",assoc_type="ha12")%>%
  select(COMMCD:year,sector=Sector,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d6c2



# join state records -----------------
d6d<-d6a2%>%
  rbind(d6b2)%>%
  rbind(d6c2)%>%
  left_join(d_cc)%>%
  glimpse()

d6d



# join all annual records --------------------------------

# bl -------------------------------
d7a<-d3_a%>%
  full_join(d3_c)%>% 
  mutate(
    value_annual_bl_prp=value_annual_bl/value_annual_allSp)%>%
  select(-assoc_annual_allSp)%>%
  glimpse()

# pivot long 
d7a2<-d7a%>%
  select(-value_annual_allSp)%>%
  select(year:value_annual_bl_prp,association=assoc_annual_bl,value=value_annual_bl,proportion=value_annual_bl_prp)%>%
  mutate(scale="annual",assoc_type="bl")%>%
  select(year,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d7a2



# pr -------------------------------
d7b<-d3_b%>%
  full_join(d3_c)%>% 
  mutate(
    value_annual_pr_prp=value_annual_pr/value_annual_allSp  )%>%
  select(-assoc_annual_allSp)%>%
  glimpse()

# pivot long 
d7b2<-d7b%>%
  select(-value_annual_allSp)%>%
  select(year:value_annual_pr_prp,association=assoc_annual_pr,value=value_annual_pr,proportion=value_annual_pr_prp)%>%
  mutate(scale="annual",assoc_type="pr")%>%
  select(year,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d7b2

# ha12 -------------------------------
d7c<-d3_b1%>%
  full_join(d3_c)%>% 
  mutate(
    value_annual_ha12_prp=value_annual_ha12/value_annual_allSp
  )%>%
  select(-assoc_annual_allSp)%>%
  glimpse()


# pivot long 
d7c2<-d7c%>%
  select(-value_annual_allSp)%>%
  select(year:value_annual_ha12_prp,association=assoc_annual_ha12,value=value_annual_ha12,proportion=value_annual_ha12_prp)%>%
  mutate(scale="annual",assoc_type="ha12")%>%
  select(year,scale,assoc_type,association,metric,value,proportion)%>%
  glimpse()
d7c2



# join annual records
d7d<-d7a2%>%
  rbind(d7b2)%>%
  rbind(d7c2)%>%
  glimpse()

d7d


# no mult join -------------------------------------
# d8<-d4_a%>%
#   full_join(d4_b)%>%
#   full_join(d4_b1)%>%
#   # full_join(d4_b2)%>%
#   full_join(d4_c)%>% 
#   unique()%>%
#   glimpse()
# 
# d8%>%
#   filter(value_noMult_bl!=value_noMult_allSp)

# annual no multiplier
# note 0s are really NAs
# d9<-d8%>%
#   select(-PortGroup_IOPAC)%>% 
#   unique()%>%
#   group_by(year,metric)%>%
#   # filter(metric=="revenue")%>%
#   summarize(
#     value_noMult_annual_bl=sum(value_noMult_bl,na.rm=T),
#     value_noMult_annual_pr=sum(value_noMult_pr,na.rm=T),
#     value_noMult_annual_ha12=sum(value_noMult_ha12,na.rm=T),
#     value_noMult_annual_ha2=sum(value_noMult_ha2,na.rm=T),
#     value_noMult_annual_allSp=sum(value_noMult_allSp,na.rm=T)
#   )%>%
#   ungroup()%>%
#   glimpse()
# d9  
  
# join no mult to other datasets
# d15<-d5%>%
#   left_join(d8)%>% glimpse()
# 
# d16<-d6%>%
#   left_join(d8)%>% glimpse() # only 2 are not matched
# 
# # note 0s are really NAs
# d17<-d7%>%
#   left_join(d9)%>% 
#   glimpse()
# 
# 
# d17%>%filter(value_noMult_annual_bl>=1)

# save ###################
write_csv(d5d,"./results/ec_port_all_long.csv")
write_csv(d6d,"./results/ec_state_all_long.csv")
write_csv(d7d,"./results/ec_annual_all_long.csv")
# write_csv(d8,"./results/ec_noMult_all_long.csv")


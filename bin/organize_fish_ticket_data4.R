# Jennifer Selgrath
# NOAA CINMS
# Deep sea coral valuation
#
# GOAL: add port codes
#----------------------------------------------------------
library(tidyverse); library(ggplot2)


# github: https://github.com/jselgrath/deep_sea_coral_valuation
# ## See Table 1.3 in 2013 Technical Appendix

#----------------------------------------------------------
remove(list=ls())

# fishtix wd
setwd("C:/Users/jennifer.selgrath/Documents/research/r_results/dsc_val_fishticket")

# fishing data -----------------
d1 <- read_csv("./results/fishtix_2010_2024_no_pii3.csv")%>%
  mutate(clean_key = str_replace_all(cdfw_port_name, "[^[:alnum:]]", "") %>% tolower())%>%
  mutate(LandingDate = as.Date(LandingDate, format = "%Y-%m-%d"))%>% # Setting Landing Date variable as class "date"
  glimpse()

d1p<-d1%>%
  select(cdfw_port_id,cdfw_port_name,clean_key)%>%
  unique()%>%
  glimpse()
d1p

# Reading in port key # --------------------------------------------------
# ** NOTE: manually grouped ports into port complexes according to above NOAA Tech Memo
# ** For ports in CDFW triptix without corresponding IOPAC port complex, grouped as "California" (and CA mults applied)
# note portlist3 fixes mill creek and big creek and assigns them to monterey - check with Robert about ONMS practices

setwd("G:/My Drive/research/r_projects/dsc_valuation")

# original port list
d2 <- read_csv("./data/portlist_allCA3.csv")%>% 
  select(c("PortName", "PortGroup_IOPAC"))%>%
  select(iopac_port_complex=PortGroup_IOPAC,iopac_port=PortName)%>%
  mutate(clean_key = str_replace_all(iopac_port, "[^[:alnum:]]", "") %>% tolower())%>%
  unique()%>%
  filter(!is.na(iopac_port_complex))%>%
  mutate(clean_ports = str_replace_all(iopac_port_complex, "[^[:alnum:]]", "") %>% tolower())%>%
  glimpse()


# pacfin code list - lots are grouped under "other"
d3<-read_csv("./data/port_code_pacfin.csv")%>%
  mutate(clean_key = str_replace_all(port, "[^[:alnum:]]", "") %>% tolower())%>%
  select(pacfin_port_code=port_code,pacfin_portcomplex=portcomplex_code,pacfin_port_complex_code=port_complex,pacfin_port=port,clean_key)%>%
  filter(!is.na(clean_key))%>%
  mutate(clean_ports = str_replace_all(pacfin_portcomplex, "[^[:alnum:]]", "") %>% tolower())%>%
  mutate(clean_ports = str_remove(clean_ports, "areaports"))%>%
  glimpse()
d3

d3b<-d3%>%
  select(pacfin_port_complex_code,clean_ports)%>%
  unique()%>%
  glimpse()

d3c<-rbind(d3b,c("SBA","santabarbara"))%>%
  mutate(clean_ports=if_else(clean_ports=="fortbragg","ftbragg",clean_ports))%>%
  glimpse()
d3c

d4<-read_csv("./data/port_key_cdfw.csv")%>%
  mutate(clean_key = str_replace_all(cdfw_port, "[^[:alnum:]]", "") %>% tolower())%>%
  mutate(clean_ports = str_replace_all(cdfw_port_complex, "[^[:alnum:]]", "") %>% tolower())%>%
  glimpse()
d4

# to do - make d3 just port codes from pacfin and then add iopac codes 

# join fishtix and iopac code list (from chris free)
d5<-d1p%>%
  left_join(d2, by="clean_key")%>%
  # filter(!is.na(clean_key))%>%
  # arrange(cdfw_port_code)%>%
  # filter(discontinued_date != "1/1/1996" | is.na(discontinued_date))%>%
  # filter(discontinued_date != "1/1/1997" | is.na(discontinued_date))%>%
  # filter(cdfw_port_complex_code!=10)%>% # sacramento delta
  # mutate(clean_ports = str_replace_all(pacfin_port, "[^[:alnum:]]", "") %>% tolower())%>%
  # mutate(clean_ports = str_remove(clean_ports, "areaports"))%>%
  glimpse()
d5
view(d5)



# join fishtix and cdfw code list (from chris free)
d6<-d5%>%
  left_join(d3c, by=c("clean_ports"))%>%
  # filter(!is.na(clean_key))%>%
  # arrange(cdfw_port_code)%>%
  # filter(discontinued_date != "1/1/1996" | is.na(discontinued_date))%>%
  # filter(discontinued_date != "1/1/1997" | is.na(discontinued_date))%>%
  # filter(cdfw_port_complex_code!=10)%>% # sacramento delta
  # mutate(clean_ports = str_replace_all(pacfin_port, "[^[:alnum:]]", "") %>% tolower())%>%
  # mutate(clean_ports = str_remove(clean_ports, "areaports"))%>%
  filter(clean_ports!="california")%>%
  filter(!is.na(clean_ports))%>%
  unique()%>%
  glimpse()
d6
view(d6)


d3
d5




# with codes from iopac from jack
d6<-d5%>%
  full_join(d3c, by="clean_ports")%>%
  filter(!is.na(clean_key))%>%unique()%>%
  filter(clean_ports!="california")%>%
  mutate(pacfin_port_complex_code=if_else(clean_ports=="invalidorunknownport","UNK",pacfin_port_complex_code))%>%
  glimpse()
d6
view(d6)


d7<-d1%>%
  left_join(d6)%>%
  glimpse()
d7


# Save
write_csv(d6,"./results/port_key_final.csv")

setwd("C:/Users/jennifer.selgrath/Documents/research/r_results/dsc_val_fishticket")
write_csv(d7,"./results/fishtix_2010_2024_no_pii4.csv")
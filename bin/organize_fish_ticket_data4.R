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
  mutate(clean_port = str_replace_all(cdfw_port_name, "[^[:alnum:]]", "") %>% tolower())%>%
  # mutate(LandingDate = as.Date(LandingDate, format = "%Y-%m-%d"))%>% # Setting Landing Date variable as class "date"
  unique()%>%
  glimpse()


# Reading in port key # --------------------------------------------------
# ** NOTE: manually grouped ports into port complexes according to above NOAA Tech Memo
# ** For ports in CDFW triptix without corresponding IOPAC port complex, grouped as "California" (and CA mults applied)
# note portlist3 fixes mill creek and big creek and assigns them to monterey - check with Robert about ONMS practices

setwd("G:/My Drive/research/r_projects/dsc_valuation")

d2<-read_csv("./results/port_key_final2.csv")%>% # 2 is limited by non-discontinued ports from Chris Free's list
  select(-cdfw_port_name)%>%
  filter(!is.na(cdfw_port_active_date))%>%
  unique()%>%
  glimpse()


# join port codes
d3<-d1%>%
  left_join(d2)%>%
  # select(iopac_commcd,COMMCD3,cdfw_species_id: cdfw_gear_id,cdfw_gear_name,cdfw_landing_receipt_num :pacfin_species_code,pacfin_species_id:pacfin_species_group_id,pacfin_management_group_code ,pacfin_species_group_code:pacfin_species_sort_order, pacfin_gear_code: iopac_gear_group,
  #        cdfw_port_discontinued_date,clean_port:iopac_port_complex,
  #        pacfin_county_code:port_order)%>%
  select(-clean_pacfin_port_complex,-pacfin_port_active_date)%>%
  filter(!is.na((pacfin_port_code)))%>% # these are all inland ports - below is code to check
  glimpse()

# check out NAs
d3%>%filter(is.na(pacfin_port_code))%>%select(cdfw_port_id, cdfw_port_name)%>%unique()
d3%>%filter(is.na(iopac_port))%>%select(cdfw_port_id, cdfw_port_name)%>%unique()
d3%>%filter(is.na(iopac_port_complex))%>%select(cdfw_port_id, cdfw_port_name)%>%unique()
d3%>%filter(is.na(cdfw_total_price))%>%select(cdfw_port_id, cdfw_port_name,cdfw_landing_receipt_num :cdfw_total_price) # only one missing a price
view(d3)

# reduce columns
d4<-d3%>%
  # select(iopac_commcd,COMMCD3:cdfw_landing_receipt_num ,cdfw_total_price,year:congressional_district,latitude:port_order)%>%
  filter(!is.na(total_price))%>%
  glimpse()

# Save ------------------------------------
setwd("C:/Users/jennifer.selgrath/Documents/research/r_results/dsc_val_fishticket")
write_csv(d4,"./results/fishtix_2010_2024_no_pii4.csv")

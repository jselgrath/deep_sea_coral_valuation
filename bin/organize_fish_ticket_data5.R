# Jennifer Selgrath
# NOAA CINMS
# Deep sea coral valuation
#
# GOAL: join multipliers & fishtix data

## References:
## Technical Appendix for 2013 Commercial Fisheries report
# https://nmssanctuaries.blob.core.windows.net/sanctuaries-prod/media/archive/science/socioeconomic/farallones/pdfs/techapp13.pdf

# github: https://github.com/jselgrath/deep_sea_coral_valuation
# ## See Table 1.3 in 2013 Technical Appendix

# NOTES:
# 2 version has averaged values for NAs

# multipliers for SB larger than multipliers for CA except for "halibut, trawl", "HMS, fixed gear", "HMS, net",
# "other species, trawl", "salmon, fixed gear", 
# No SB mults for Sablefish trawl, whiting trawl

# will need to create separate tables for two spatial resolutions, state and by port - two sets of multipliers
# ---------------------------------------------------------------------------------------------------
library(tidyverse); library(ggplot2)
library(lubridate)


#----------------------------------------------------------
remove(list=ls())

# fishtix wd
setwd("C:/Users/jennifer.selgrath/Documents/research/r_results/dsc_val_fishticket")

# fishing data -----------------
d1 <- read_csv("./results/fishtix_2010_2024_no_pii4.csv")%>%
  mutate(iopac_port_complex=if_else(iopac_port_complex=="Ft_Bragg","FtBragg",iopac_port_complex))%>%
  # select(-cdfw_port_name)%>%
  arrange(iopac_port)%>%
  glimpse()


# check out NAs
d1%>%filter(is.na(pacfin_port_code))%>%select(cdfw_port_id, cdfw_port_name)%>%unique()
# d1%>%filter(is.na(pacfin_port_code))%>%view()
# 

# -------------------------
setwd("G:/My Drive/research/r_projects/dsc_valuation")

# note: columns ending in 2 use mean values to fill in empty multipliers 
# updating column names with _m for multiplier

# state/commodity level multipliers (sm)
d2<-read_csv("./results/multipliers_2023_ca3.csv")%>% 
  select(iopac_species=SPECIES_NAME, iopac_commcd=commcd, iopac_commcd_long=Name, iopac_sector=Sector,# Selecting relevant columns
         vessel_output_sm=Vessel_output2,
         vessel_income_sm=Vessel_income2,
         vessel_employment_sm=Vessel_employment2,
         processor_output_sm=Processor_output2,
         processor_income_sm=Processor_income2,
         processor_employment_sm=Processor_employment2,
         tot_out_sm=TotOut2,
         tot_inc_sm=TotInc2,
         tot_emp_sm=TotEmp2)%>%
  arrange(iopac_commcd)%>%
  glimpse()
glimpse(d2)

# port multipliers (pm). Region = port complex
d3<-read_csv("./results/multipliers_2023_port3.csv")%>% 
  # mutate(clean_ports = str_replace_all(Region, "[^[:alnum:]]", "") %>% tolower())%>%
  select(iopac_port_complex=Region, iopac_species=SPECIES_NAME, iopac_commcd=commcd, iopac_commcd_long=Name, iopac_sector=Sector,# Selecting relevant columns
         vessel_output_pm=Vessel_output2,
         vessel_income_pm=Vessel_income2,
         vessel_employment_pm=Vessel_employment2,
         processor_output_pm=Processor_output2,
         processor_income_pm=Processor_income2,
         processor_employment_pm=Processor_employment2,
         tot_out_pm=TotOut2,
         tot_inc_pm=TotInc2,
         tot_emp_pm=TotEmp2)%>%
  arrange(iopac_port_complex)%>%
  glimpse()
d3  



# JOIN MULTIPLIERS WITH FISHTIX DATA  ----------------------------

# state multipliers -------------------
# - join_by(iopac_commcd) after removing port info and other misc info
glimpse(d1) #fishtix
glimpse(d2) # state multipliers

# join fishtix to state multipliers
d4<-d1%>%
  select(iopac_commcd,cdfw_species_id,cdfw_species_name,cdfw_gear_id,cdfw_gear_name,total_price ,year,species_group2,level,depth_min_m:assoc_habitat2,iopac_gear_group)%>%
  left_join(d2)%>%
  glimpse()



# check out NAs _ checking this to confirm all NAs were addressed in "iopac_multipliers3.R"--------------------------------
d4%>%filter(is.na(tot_emp_sm))%>%select(iopac_commcd, total_price,year,tot_emp_sm)%>%unique()
temp1<-d4%>%filter(is.na(tot_emp_sm))%>%select(iopac_commcd)%>%unique()%>%arrange(iopac_commcd)%>%print(n=26)
# d4%>%filter(is.na(pacfin_port_code))
glimpse(temp1)



# port  multipliers ----------------------------------------------------
# Prepare the California fallback data for when port is NA
# We only need the commcd and the '2' columns (the filled multipliers)
ca_fallback <- d3%>%
  filter(iopac_port_complex=="California")%>%
  select(iopac_commcd, 
         vessel_output_ca=vessel_output_pm, 
         vessel_income_ca=vessel_income_pm,
         vessel_employment_ca=vessel_employment_pm,
         processor_output_ca=processor_output_pm,
         processor_income_ca=processor_income_pm,
         processor_employment_ca=processor_employment_pm,
         tot_out_ca=tot_out_pm,
         tot_inc_ca=tot_inc_pm,
         tot_emp_ca=tot_emp_pm)%>%
  glimpse()


d5<-d1%>%
  select(iopac_commcd,iopac_port,iopac_port_complex,cdfw_species_id,cdfw_species_name,cdfw_gear_id,cdfw_gear_name,total_price ,year,species_group2,level,depth_min_m:assoc_habitat2,iopac_gear_group)%>%
  left_join(d3)%>%
  
# Join with the California Fallback
left_join(ca_fallback, by = "iopac_commcd")%>%
  
# Use coalesce to fill NAs
mutate(
  vessel_output_pm = coalesce(vessel_output_pm, vessel_output_ca),
  vessel_income_pm = coalesce(vessel_income_pm, vessel_income_ca),
  vessel_employment_pm = coalesce(vessel_employment_pm, vessel_employment_ca),
  processor_output_pm = coalesce(processor_output_pm, processor_output_ca),
  processor_income_pm = coalesce(processor_income_pm, processor_income_ca),
  processor_employment_pm = coalesce(processor_employment_pm, processor_employment_ca),
  tot_out_pm = coalesce(tot_out_pm, tot_out_ca),
  tot_inc_pm = coalesce(tot_inc_pm, tot_inc_ca),
  tot_emp_pm = coalesce(tot_emp_pm, tot_emp_ca)
) %>%
  
# Cleanup the temporary fallback columns
  select(-ends_with("_ca"))%>%
  glimpse()

glimpse(d5)

# check out NAs _ checking this to confirm all NAs were addressed in "iopac_multipliers3.R" and coalesce code above--------------------------------
d5%>%filter(is.na(tot_emp_pm))%>%select(iopac_commcd, total_price,year,tot_emp_pm)%>%unique()
temp2<-d5%>%filter(is.na(tot_emp_pm))%>%select(iopac_commcd)%>%unique()%>%arrange(iopac_commcd)%>%print(n=26)
d5%>%filter(is.na(iopac_port_complex))
glimpse(temp2)

# previously 446 combinations with NA 



# save
setwd("C:/Users/jennifer.selgrath/Documents/research/r_results/dsc_val_fishticket")
write_csv(d4,"./results/fishtix_2010_2024_no_pii5_ca.csv")
write_csv(d5,"./results/fishtix_2010_2024_no_pii5_port.csv")

## Jack Eynon & Jenny Selgrath
## NOAA ONMS
# project: economic valuation of commercial fisheries associated with deep sea corals
# --------------------------------------------------------------------------------------
## Script to analyze commercial logbook records for ca fisheries and deep sea corals and extract indicators

## References:
## Technical Appendix for 2013 Commercial Fisheries report
# https://nmssanctuaries.blob.core.windows.net/sanctuaries-prod/media/archive/science/socioeconomic/farallones/pdfs/techapp13.pdf

# NOTES:
# multipliers for SB larger than multipliers for CA except for "halibut, trawl", "HMS, fixed gear", "HMS, net",
# "other species, trawl", "salmon, fixed gear", 
# No SB mults for Sablefish trawl, whiting trawl

# ---------------------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(scales)
library(cowplot)
library(Kendall)
library(zoo)
library(imputeTS)
# ---------------------------------------------------------------------------------------------------

remove(list = ls())
# setwd("//aamb-s-clust01/Shared_Data/ONMS/Socioeconomic/California Work/CA Fish & Wildlife Data Agreement 2021/2021 Data Request/2021 CDFW Data Analysis/CDFW R Project 2021/CommercialTripTix/Data/")
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/dsc_valuation/jacks_files_from_danielle/CommercialTripTix")
setwd("C:/Users/Jennifer.Selgrath/Documents/research/R_projects/dsc_valuation/")

# read fishing econ, dsc, io-pac, port data -----------------
# ADJUST THIS INFO FOR BLOCK AND VMS ANALYSES
d1 <- read_csv("./results/triptix_allCA3_io_pac.csv")%>%
  select(-LandingReceiptNum,-LandingDate,-FisherID,-VesselID,-CDFWBlockID,-BlockName)%>%
  glimpse


# --------------------------------------------------
# catch summary table
### Summarizing catch by commodity multiplier groups (i.e. COMMCD)
# --------------------------------------------------
fxn_commsector_catch1 <- function(dta,col_name){
  colmn <- deparse(substitute(col_name))
  
  dta%>% 
    mutate(assoc=dta[[colmn]])%>%
    filter(year >= 2010) %>%
    drop_na(Value) %>%
    group_by(PortGroup_IOPAC, COMMCD, COMMCD2,year,assoc) %>%  #LandingReceiptNum, LandingDate, FisherID, VesselID, ,PortName,SpeciesName,LandingReceiptNum
    dplyr::summarise(revenue = sum(Value))
  
  # dta[[col_name]]<-col_name
}

# run fxn for three types of associations and all data ------------------

# body length associations
cc_bl<-fxn_commsector_catch1(d1,assoc_body_length2)%>%  
  glimpse()


# proximity associations
cc_pr<-fxn_commsector_catch1(d1,assoc_proximity2)%>%  
  glimpse()

# habitat associations 
cc_ha12<-fxn_commsector_catch1(d1,assoc_habitat2)%>% 
  glimpse()

# all
cc_all<-  d1%>%
  filter(year >= 2010) %>%
  drop_na(Value) %>%
  mutate(assoc="allSp")%>% #assoc_all
  group_by(PortGroup_IOPAC, COMMCD, COMMCD2,year,assoc) %>%  #LandingReceiptNum, LandingDate, FisherID, VesselID, 
  dplyr::summarise(revenue = sum(Value))%>%
  arrange(year,COMMCD2,PortGroup_IOPAC)%>%
  glimpse()

# commsector_catch <- commsector_catch %>% group_by(LandingReceiptNum, LandingDate, FisherID, VesselID, PortGroup_IOPAC, COMMCD) %>%
#   complete(year = 2010:2020, fill = list(revenue = 0))

# --------------------------------------------------
### Linking 2020 commodity multiplier table to catch summary table
# --------------------------------------------------

# Reading in table
comm_mults_2020 <- read.csv(file = "./data/IMPLAN/comm_mults_2020.csv")%>%
  select(PortGroup_IOPAC=PortGroup_iopac,Name:TotEmp)%>% # Renaming PortGroup_IOPAC for consistency in join
  select(c("PortGroup_IOPAC", "COMMCD", # Selecting relevant columns
           "Sector", "Vessel_output",
           "Vessel_income", "Vessel_employment",
           "Processor_output", "Processor_income",
           "Processor_employment", "TotOut",
           "TotInc", "TotEmp"))%>%
  glimpse()



# will need to create separate tables for two spatial resolutions, state and by port

# --------------------------------------------------
# Joining tables - function - port
#  HERE USING PORT MULTIPLIERS 
# --------------------------------------------------
fxn2_commsector_catch<-function(dta){
  dta%>%
    left_join(comm_mults_2020, by = c("PortGroup_IOPAC", "COMMCD"))%>%
    glimpse()
} 

# run fxn for four types of associations ------------------

# note: this has all data including values with no multipliers and port and state multipliers 
cc2_bl<-fxn2_commsector_catch(cc_bl)
cc2_pr<-fxn2_commsector_catch(cc_pr)
cc2_ha12<-fxn2_commsector_catch(cc_ha12)
cc2_all<-fxn2_commsector_catch(cc_all)


# --------------------------------------------------
# economic contribution results function - multiplies revenue by multiplier from IMPLAN
# PORT MULTIPLIERS
# -------------------------------------------------
# note originally columns that are removed below were overwritten instead of written to a column with a new name
fxn_econ_contr<-function(dta){
  dta %>%
    mutate(Vessel_output2 = revenue*Vessel_output,
           Vessel_income2 = revenue*Vessel_income,
           Vessel_employment2 = revenue*Vessel_employment,
           Processor_output2 = revenue*Processor_output,
           Processor_income2 = revenue*Processor_income,
           Processor_employment2 = revenue*Processor_employment,
           TotOut2 = revenue*TotOut,
           TotInc2 = revenue*TotInc,
           TotEmp2 = revenue*TotEmp)%>%
    # select(-Vessel_output,-Vessel_income,-Vessel_employment,-Processor_output,-Processor_income,-Processor_employment-TotOut,-TotInc,-TotEmp)%>%
    glimpse()
}


# run function ----------------------
cc3_port_bl<-fxn_econ_contr(cc2_bl)%>%glimpse()
cc3_port_pr<-fxn_econ_contr(cc2_pr)
cc3_port_ha12<-fxn_econ_contr(cc2_ha12)
cc3_port_all<-fxn_econ_contr(cc2_all)


# cc3_port_bl%>%filter(TotEmp>0)



# --------------------------------------------------
## Creating summary of economic contribution of landings for state of CA
#  STATE MULTIPLIERS 
# --------------------------------------------------

# Aggregating revenue by year and commodity sector across all ports
# Note: for CHNMS, all ports happen to be in CA
fxn_commsector_catch3_ca<-function(dta){
  dta%>%
    group_by(COMMCD, COMMCD2,year,assoc) %>% #LandingReceiptNum, LandingDate, FisherID, VesselID, 
    dplyr::summarise(revenue = sum(revenue, na.rm = T)) %>%
    mutate(PortGroup_IOPAC = "California")%>% # rewrite all ports to CA to join state multipliers
    left_join(comm_mults_2020, # Adding commodity multipliers to table
              by = c("PortGroup_IOPAC", "COMMCD"))%>%
    glimpse()  
}

# run function
cc3_ca_bl<-fxn_commsector_catch3_ca(cc2_bl)
cc3_ca_pr<-fxn_commsector_catch3_ca(cc2_pr)
cc3_ca_ha12<-fxn_commsector_catch3_ca(cc2_ha12)
cc3_ca_all<-fxn_commsector_catch3_ca(cc2_all)


# --------------------------------------------------
# Computing economic contributions for all CA
# STATE MULTIPLIERS 
# --------------------------------------------------
# note originally columns that are removed below were overwritten instead of written to a column with a new name
fxn_econcontr_results_CA <- function(dta){
  dta%>%
    mutate(Vessel_output_CA = revenue*Vessel_output,
           Vessel_income_CA = revenue*Vessel_income,
           Vessel_employment_CA = revenue*Vessel_employment,
           Processor_output_CA = revenue*Processor_output,
           Processor_income_CA = revenue*Processor_income,
           Processor_employment_CA = revenue*Processor_employment,
           TotOut_CA = revenue*TotOut,
           TotInc_CA = revenue*TotInc,
           TotEmp_CA = revenue*TotEmp)%>%
    select(-Vessel_output,-Vessel_income,-Vessel_employment,-Processor_output,-Processor_income,-Processor_employment,-TotOut,-TotInc,-TotEmp)%>%
    glimpse()
}


# run function
cc4_ca_bl<-fxn_econcontr_results_CA(cc3_ca_bl)
cc4_ca_pr<-fxn_econcontr_results_CA(cc3_ca_pr)
cc4_ca_ha12<-fxn_econcontr_results_CA(cc3_ca_ha12)
cc4_ca_all<-fxn_econcontr_results_CA(cc3_ca_all)


# --------------------------------------------------
# Computing economic contribution summary for all CA
# STATE MULTIPLIERS
# --------------------------------------------------
fxn_econcontr_summary_CA <-function(dta){
  dta%>%
    group_by(year,assoc) %>%
    dplyr::summarise(Revenue = sum(revenue, na.rm = T),
                     Vessel_output_CA2 = sum(Vessel_output_CA, na.rm = T),
                     Vessel_income_CA2 = sum(Vessel_income_CA, na.rm = T),
                     Vessel_employment_CA2 = sum(Vessel_employment_CA, na.rm = T),
                     Processor_output_CA2= sum(Processor_output_CA, na.rm = T),
                     Processor_income_CA2 = sum(Processor_income_CA, na.rm = T),
                     Processor_employment_CA2 = sum(Processor_employment_CA, na.rm = T),
                     TotOut_CA2 = sum(TotOut_CA, na.rm = T),
                     TotInc_CA2 = sum(TotInc_CA, na.rm = T),
                     TotEmp_CA2 = sum(TotEmp_CA, na.rm = T))%>%
    glimpse()
}


# run function
cc5_ca_bl<-fxn_econcontr_summary_CA (cc4_ca_bl)
cc5_ca_pr<-fxn_econcontr_summary_CA (cc4_ca_pr)
cc5_ca_ha12<-fxn_econcontr_summary_CA (cc4_ca_ha12)
cc5_ca_all<-fxn_econcontr_summary_CA (cc4_ca_all)

# --------------------------------------------------
# report - a subset of the summary values
# --------------------------------------------------
fxn_econcontr_CA_report <- function(dta){
  dta%>%
    select(year, assoc, Revenue,TotOut_CA2,TotInc_CA2,TotEmp_CA2)%>%
    glimpse()
}

# run function
cc6_ca_bl<-fxn_econcontr_CA_report    (cc5_ca_bl)
cc6_ca_pr<-fxn_econcontr_CA_report    (cc5_ca_pr)
cc6_ca_ha12<-fxn_econcontr_CA_report  (cc5_ca_ha12)
cc6_ca_all<-fxn_econcontr_CA_report   (cc5_ca_all)



# --------------------------------------------------
# NO MULTILPIER CALCULATIONS - USUALLY WHEN SPECIES OR GEAR IS NA
# --------------------------------------------------


# --------------------------------------------------
# Creating table for catch groups by port with no matching multipliers
# --------------------------------------------------
fxn_no_mult0<-function(dta){
  dta %>% 
    filter(is.na(Vessel_output))%>%
    select(PortGroup_IOPAC:assoc,Sector,revenue)%>%
    glimpse()
}


# run no multipliers fxn for four types of associations at port/commodity level ------------------
nomults_port_bl   <- fxn_no_mult0(cc2_bl)
nomults_port_pr   <- fxn_no_mult0(cc2_pr)
nomults_port_ha12 <- fxn_no_mult0(cc2_ha12)
nomults_port_all  <- fxn_no_mult0(cc2_all)


# summarize catch no multipliers at state/commodity level ---------------------------------
fxn_no_mult<-function(dta){
  dta %>% 
    group_by(COMMCD, COMMCD2,year,assoc) %>% 
    drop_na(revenue) %>%
    dplyr::summarise(revenue2 = sum(revenue))%>% # revenue no mult
    glimpse()
}


# run no multipliers fxn for four types of associations - summarizes at state/commodity level ------------------
nomults_commodity_bl   <- fxn_no_mult(nomults_port_bl)
nomults_commodity_pr   <- fxn_no_mult(nomults_port_pr)
nomults_commodity_ha12 <- fxn_no_mult(nomults_port_ha12)
nomults_commodity_all  <- fxn_no_mult(nomults_port_all)
nomults_commodity_all

# --------------------------------------------------
# Summary of annual landings revenue without a corresponding multiplier
# --------------------------------------------------
fxn_no_mult2<-function(dta){
  dta%>% 
    group_by(year,assoc) %>%
    dplyr::summarise(Revenue = round(sum(revenue),0))%>%
    glimpse()
}

# run no mult annual landings revenue fxn for four types of associations ------------------
# no sector or no species
nomults_annrev_bl     <- fxn_no_mult2(nomults_port_bl)
nomults_annrev_pr     <- fxn_no_mult2(nomults_port_pr)
nomults_annrev_ha12   <- fxn_no_mult2(nomults_port_ha12)
nomults_annrev_all    <- fxn_no_mult2(nomults_port_all)
nomults_annrev_all 

# example of unmatched (no corresponding multiplier) category with high catch
# gears used were mainly UNKNOWN, diving, spear
# GRDOTRG <- d1_ha12 %>% 
#   filter(COMMCD == "GRDOTRG")%>%
#   dplyr::select(GearGroup,assoc)%>%
#   unique()%>%
#   glimpse()



# --------------------------------------------------
## Writing results to csv
# --------------------------------------------------
# can add block info above

# NO MULTIPLIERS ----------------------

# port revenue no multipliers --------------------------------------------------
write_csv(nomults_port_bl,  "./results/nomults_port_bl.csv")
write_csv(nomults_port_pr,  "./results/nomults_port_pr.csv")
write_csv(nomults_port_ha12,"./results/nomults_port_ha12.csv")
write_csv(nomults_port_all, "./results/nomults_port_all.csv")

# commodity/state revenue no multipliers --------------------------------------------------
write_csv(nomults_commodity_bl,  "./results/nomults_commodity_bl.csv")
write_csv(nomults_commodity_pr,  "./results/nomults_commodity_pr.csv")
write_csv(nomults_commodity_ha12,"./results/nomults_commodity_ha12.csv")
write_csv(nomults_commodity_all, "./results/nomults_commodity_all.csv")

# annual revenue no multipliers --------------------------------------------------
write_csv(nomults_annrev_bl,  "./results/nomults_annrev_bl.csv")
write_csv(nomults_annrev_pr,  "./results/nomults_annrev_pr.csv")
write_csv(nomults_annrev_ha12,"./results/nomults_annrev_ha12.csv")
write_csv(nomults_annrev_all, "./results/nomults_annrev_all.csv")



#  MULTIPLIERS, VALUES NOT CALCULATED -----------------------------------------

# results commercial sector catch with port multipliers
write_csv(cc2_bl,"./results/revenue_bycommsector_wportmults_bl.csv")
write_csv(cc2_pr,"./results/revenue_bycommsector_wportmults_pr.csv")
write_csv(cc2_ha12,"./results/revenue_bycommsector_wportmults_ha12.csv")
write_csv(cc2_all,"./results/revenue_bycommsector_wportmults_all.csv")

# results commercial sector catch with CA multipliers
write_csv(cc3_ca_bl,"./results/revenue_bycommsector_wCAmults_bl.csv")
write_csv(cc3_ca_pr,"./results/revenue_bycommsector_wCAmults_pr.csv")
write_csv(cc3_ca_ha12,"./results/revenue_bycommsector_wCAmults_ha12.csv")
write_csv(cc3_ca_all,"./results/revenue_bycommsector_wCAmults_all.csv")



#  MULTIPLIERS, VALUES CALCULATED -----------------------------------------

# economic contributions - port level multipliers, with commodity info  ----------------------
write_csv(cc3_port_bl,"./results/econcontributions_portlevel_bl.csv")
write_csv(cc3_port_pr,"./results/econcontributions_portlevel_pr.csv")
write_csv(cc3_port_ha12,"./results/econcontributions_portlevel_ha12.csv")
write_csv(cc3_port_all,"./results/econcontributions_portlevel_all.csv")

# economic contributions - state level multipliers, with commodity info ----------------------
write_csv(cc4_ca_bl,"./results/econcontributions_statelevel_bl.csv")
write_csv(cc4_ca_pr,"./results/econcontributions_statelevel_pr.csv")
write_csv(cc4_ca_ha12,"./results/econcontributions_statelevel_ha12.csv")
write_csv(cc4_ca_all,"./results/econcontributions_statelevel_all.csv")

# economic contributions - state level multipliers, no commodity info----------------------
write_csv(cc5_ca_bl,"./results/econcontributions_statelevel_ann_bl.csv")
write_csv(cc5_ca_pr,"./results/econcontributions_statelevel_ann_pr.csv")
write_csv(cc5_ca_ha12,"./results/econcontributions_statelevel_ann_ha12.csv")
write_csv(cc5_ca_all,"./results/econcontributions_statelevel_ann_all.csv")


# later make versions by block and by trip (vms)

## Jack Eynon & Jenny Selgrath
## NOAA ONMS
# project: economic valuation of commercial fisheries associated with deep sea corals
# --------------------------------------------------------------------------------------
## Script to analyze commercial logbook records for ca fisheries and deep sea corals and extract indicators

## References:
## Technical Appendix for 2013 Commercial Fisheries report
# https://nmssanctuaries.blob.core.windows.net/sanctuaries-prod/media/archive/science/socioeconomic/farallones/pdfs/techapp13.pdf
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


# ---------------------------------------------------------------------------------------
# SET ASSOCIATION TYPE 
# ------------------------------------------------------------------------------------
# here basing associations on body length associations 
# d1_bl <- d1 %>% 
#   filter(assoc_body_length !=0)%>% # 
#   mutate(assoc="bl")%>%
#   mutate(assoc=if_else(assoc_body_length2=="No Data","No Data",assoc))%>%
#   glimpse()
# 
# # here basing associations on proximity associations 
# d1_pr <- d1 %>% 
#   filter(assoc_proximity ==1)%>% # 
#   mutate(assoc="pr")%>%
#   glimpse()
# 
# # here basing associations on habitat associations (possible, and definite)
# d1_ha12 <- d1 %>% 
#   filter(assoc_habitat !=0)%>% # 
#   mutate(assoc="ha12")%>%
#   glimpse()
# 
# # here basing associations on habitat associations (definite)
# d1_ha2 <- d1 %>% 
#   filter(assoc_habitat ==2)%>% # 
#   mutate(assoc="ha2")%>%
#   glimpse()
# 
# d1_all<-d1%>%
#   mutate(assoc="all")%>%
#   glimpse()

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
    group_by(PortGroup_IOPAC, COMMCD, COMMCD2,year,assoc) %>%  #LandingReceiptNum, LandingDate, FisherID, VesselID, 
    dplyr::summarise(revenue = sum(Value))
  
  # dta[[col_name]]<-col_name
}

# run fxn for four types of associations and all data ------------------

# body length associations
cc_bl<-fxn_commsector_catch1(d1,assoc_body_length2)%>%  
  # select(PortGroup_IOPAC:revenue,assoc_bl=assoc)%>%
  glimpse()

# proximity associations
cc_pr<-fxn_commsector_catch1(d1,assoc_proximity2)%>%  
  # select(PortGroup_IOPAC:revenue,assoc_pr=assoc)%>%
  glimpse()

# habitat associations 
cc_ha12<-fxn_commsector_catch1(d1,assoc_habitat2)%>% 
  # select(PortGroup_IOPAC:revenue,assoc_pr=assoc)%>%
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

# Joining tables - function - port
fxn2_commsector_catch<-function(dta){
  dta%>%
    left_join(comm_mults_2020, by = c("PortGroup_IOPAC", "COMMCD"))%>%
    glimpse()
} 

# run fxn for four types of associations ------------------
cc2_bl<-fxn2_commsector_catch(cc_bl)
cc2_pr<-fxn2_commsector_catch(cc_pr)
cc2_ha12<-fxn2_commsector_catch(cc_ha12)
# cc2_ha2<-fxn2_commsector_catch(cc_ha2)
cc2_all<-fxn2_commsector_catch(cc_all)


# --------------------------------------------------
# Creating table for catch groups with no matching multipliers
# --------------------------------------------------
fxn_no_mult0<-function(dta){
  dta %>% 
    filter(is.na(Vessel_output))%>%
    glimpse()
}


# run no multipliers fxn for four types of associations ------------------
catch_nomults_bl   <- fxn_no_mult0(cc2_bl)
catch_nomults_pr   <- fxn_no_mult0(cc2_pr)
catch_nomults_ha12 <- fxn_no_mult0(cc2_ha12)
# catch_nomults_ha2  <- fxn_no_mult0(cc2_ha2)
catch_nomults_all  <- fxn_no_mult0(cc2_all)



# summarize catch no multipliers ---------------------------------
fxn_no_mult<-function(dta){
  dta %>% 
    filter(is.na(Vessel_output))%>%
    group_by(COMMCD, COMMCD2,year,assoc) %>% 
    drop_na(revenue) %>%
    dplyr::summarise(revenue = sum(revenue))%>% # revenue no mult
    glimpse()
}




# run no multipliers fxn for four types of associations ------------------
rev_nomults_bl   <- fxn_no_mult(cc2_bl)
rev_nomults_pr   <- fxn_no_mult(cc2_pr)
rev_nomults_ha12 <- fxn_no_mult(cc2_ha12)
# rev_nomults_ha2  <- fxn_no_mult(cc2_ha2)
rev_nomults_all  <- fxn_no_mult(cc2_all)

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
nomults_annrev_bl     <- fxn_no_mult2(rev_nomults_bl)
nomults_annrev_pr     <- fxn_no_mult2(rev_nomults_pr)
nomults_annrev_ha12   <- fxn_no_mult2(rev_nomults_ha12)
# nomults_annrev_ha2    <- fxn_no_mult2(rev_nomults_ha2)
nomults_annrev_all    <- fxn_no_mult2(rev_nomults_all)

# example of unmatched (no corresponding multiplier) category with high catch
# gears used were mainly UNKNOWN, diving, spear
# GRDOTRG <- d1_ha12 %>% 
#   filter(COMMCD == "GRDOTRG")%>%
#   dplyr::select(GearGroup,assoc)%>%
#   unique()%>%
#   glimpse()


# --------------------------------------------------
# economic contribution results function
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
econcontr_results_bl<-fxn_econ_contr(cc2_bl)
econcontr_results_pr<-fxn_econ_contr(cc2_pr)
econcontr_results_ha12<-fxn_econ_contr(cc2_ha12)
# econcontr_results_ha2<-fxn_econ_contr(cc2_ha2)
econcontr_results_all<-fxn_econ_contr(cc2_all)


# econcontr_results_bl%>%filter(TotEmp>0)



# --------------------------------------------------
## Creating summary of economic contribution of landings for state of CA
# --------------------------------------------------

# Aggregating revenue by year and commodity sector across all ports
# Note: for CHNMS, all ports happen to be in CA
fxn_commsector_catch3_ca<-function(dta){
  dta%>%
    group_by(COMMCD, COMMCD2,year,assoc) %>% #LandingReceiptNum, LandingDate, FisherID, VesselID, 
    dplyr::summarise(revenue = sum(revenue, na.rm = T)) %>%
    mutate(PortGroup_IOPAC = "California")%>%
    left_join(comm_mults_2020, # Adding commodity multipliers to table
              by = c("PortGroup_IOPAC", "COMMCD"))%>%
    glimpse()  
}

# run function
cc3_ca_bl<-fxn_commsector_catch3_ca(cc2_bl)
cc3_ca_pr<-fxn_commsector_catch3_ca(cc2_pr)
cc3_ca_ha12<-fxn_commsector_catch3_ca(cc2_ha12)
# cc3_ca_ha2<-fxn_commsector_catch3_ca(cc2_ha2)
cc3_ca_all<-fxn_commsector_catch3_ca(cc2_all)


# --------------------------------------------------
# Computing economic contributions for all CA
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
# cc4_ca_ha2<-fxn_econcontr_results_CA(cc3_ca_ha2)
cc4_ca_all<-fxn_econcontr_results_CA(cc3_ca_all)


# --------------------------------------------------
# Computing economic contribution summary for all CA
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
# cc5_ca_ha2<-fxn_econcontr_summary_CA (cc4_ca_ha2)
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
# cc6_ca_ha2<-fxn_econcontr_CA_report   (cc5_ca_ha2)
cc6_ca_all<-fxn_econcontr_CA_report   (cc5_ca_all)

# --------------------------------------------------
# Creating summary table of unmatched (no corresponding multiplier) annual revenue
# --------------------------------------------------
fxn_nomatch_CA_rev <- function(dta){
  dta%>%
    filter(is.na(Sector))%>% 
    group_by(year,assoc) %>%
    dplyr::summarise(Revenue = sum(revenue, na.rm = T))%>% # calc annual revenue ca
    glimpse()
}

# run function - unmatched (no corresponding multiplier) annual revenue for state
cc6_ca_bl<-fxn_nomatch_CA_rev     (cc3_ca_bl)
cc6_ca_pr<-fxn_nomatch_CA_rev     (cc3_ca_pr)
cc6_ca_ha12<-fxn_nomatch_CA_rev   (cc3_ca_ha12)
# cc6_ca_ha2<-fxn_nomatch_CA_rev    (cc3_ca_ha2)
cc6_ca_all<-fxn_nomatch_CA_rev    (cc3_ca_all)


# multipliers for SB larger than multipliers for CA except for "halibut, trawl", "HMS, fixed gear", "HMS, net",
# "other species, trawl", "salmon, fixed gear", 
# No SB mults for Sablefish trawl, whiting trawl


# --------------------------------------------------
## Writing results to csv
# --------------------------------------------------
# can add block info above



# catch no multipliers # --------------------------------------------------
write_csv(catch_nomults_bl,"./results/catch_nomults_bl.csv")
write_csv(catch_nomults_pr,"./results/catch_nomults_pr.csv")
write_csv(catch_nomults_ha12,"./results/catch_nomults_ha12.csv")
# write_csv(catch_nomults_ha2,"./results/catch_nomults_ha2.csv")
write_csv(catch_nomults_all,"./results/catch_nomults_all.csv")



# Results no multipliers --------------------------------------------------
write_csv(nomults_annrev_bl,"./results/catch_nomults_annrev_bl.csv")
write_csv(nomults_annrev_pr,"./results/catch_nomults_annrev_pr.csv")
write_csv(nomults_annrev_ha12,"./results/catch_nomults_annrev_ha12.csv")
# write_csv(nomults_annrev_ha2,"./results/catch_nomults_annrev_ha2.csv")
write_csv(nomults_annrev_all,"./results/catch_nomults_annrev_all.csv")

# results commercial sector catch with port multipliers
write_csv(cc2_bl,"./results/revenue_bycommsector_wportmults_bl.csv")
write_csv(cc2_pr,"./results/revenue_bycommsector_wportmults_pr.csv")
write_csv(cc2_ha12,"./results/revenue_bycommsector_wportmults_ha12.csv")
# write_csv(cc2_ha2,"./results/revenue_bycommsector_wportmults_ha2.csv")
write_csv(cc2_all,"./results/revenue_bycommsector_wportmults_all.csv")

# results commercial sector catch with CA multipliers
write_csv(cc3_ca_bl,"./results/revenue_bycommsector_wCAmults_bl.csv")
write_csv(cc3_ca_pr,"./results/revenue_bycommsector_wCAmults_pr.csv")
write_csv(cc3_ca_ha12,"./results/revenue_bycommsector_wCAmults_ha12.csv")
# write_csv(cc3_ca_ha2,"./results/revenue_bycommsector_wCAmults_ha2.csv")
write_csv(cc3_ca_all,"./results/revenue_bycommsector_wCAmults_all.csv")

# economic contributions - port level ----------------------
write_csv(econcontr_results_bl,"./results/econcontributions_portlevel_bl.csv")
write_csv(econcontr_results_pr,"./results/econcontributions_portlevel_pr.csv")
write_csv(econcontr_results_ha12,"./results/econcontributions_portlevel_ha12.csv")
# write_csv(econcontr_results_ha2,"./results/econcontributions_portlevel_ha2.csv")
write_csv(econcontr_results_all,"./results/econcontributions_portlevel_all.csv")

# economic contributions - state  level ----------------------
write_csv(cc4_ca_bl,"./results/econcontributions_statelevel_bl.csv")
write_csv(cc4_ca_pr,"./results/econcontributions_statelevel_pr.csv")
write_csv(cc4_ca_ha12,"./results/econcontributions_statelevel_ha12.csv")
# write_csv(cc4_ca_ha2,"./results/econcontributions_statelevel_ha2.csv")
write_csv(cc4_ca_all,"./results/econcontributions_statelevel_all.csv")

# economic summary - state  level ----------------------
write_csv(cc5_ca_bl,"./results/econcontributions_statelevel_ann_bl.csv")
write_csv(cc5_ca_pr,"./results/econcontributions_statelevel_ann_pr.csv")
write_csv(cc5_ca_ha12,"./results/econcontributions_statelevel_ann_ha12.csv")
# write_csv(cc5_ca_ha2,"./results/econcontributions_statelevel_ann_ha2.csv")
write_csv(cc5_ca_all,"./results/econcontributions_statelevel_ann_all.csv")


# later make versions by block and by trip (vms)

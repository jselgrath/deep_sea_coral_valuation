# Jennifer Selgrath
# NOAA CINMS
#
# GOAL: Driver for Deep Sea Coral Valuation project
#----------------------------------------------------------
library(tidyverse); library(ggplot2)

# github: https://github.com/jselgrath/deep_sea_coral_valuation

#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/Jennifer.Selgrath/Documents/research/R_projects/dsc_valuation/")


# LINKING Deep Sea Coral and Fisheries Data ------------------------

# ------------------------------------------------------------------------------------------
# Association Data ---------------------------------------------

# associations ------------------------------------
# bl = body length
# pr = proximity
# ha12 = probably or definitely associated with depth and habitat
# ha2 =  definitely associated with depth and habitat
# all = all species

# combine 2010-2020 fishticket data, pull species landed 2010-2020 to make species list
source("./bin/landed_sp_2010_2020.R")
#input:     Fishtix 2010-2020
#output:    ./results/fishtix_2010_2020.csv     # full dataset
#           ./doc/fishtix_spp_2010_2020.csv # just list of landed species

# merge all sp landed with existing dsc data and assign missing values
source("./bin/landed_sp_join_association_data.R")  
# input:     ./doc/all_sp_2010_2020.csv
#           C:/Users/jennifer.selgrath/Documents/research/R_projects/dsc_associations_fishery/results/association_long.csv
# output:   ./results/association_long_2010_2020.csv        # association data, just for species caught 2010-2020
    

# ------------------------------------------------------------------------------------------
# Fisheries data with economic outputs - code adapted from Jack ----------------------------

# clean fisheries economic data and make gear codes
source("./bin/fishtix_econ_clean_gear_cat.R")
# input:    ./results/triptix_allCA.csv
# output:   ./results/triptix_allCA2.csv

# join fishticket econ data with deep sea coral association data and io-pac data
source("./bin/fishtix_econ_join_dsc_iopac.R")
# input:    ./results/triptix_allCA2.csv
#           ./data/dsc_fishery_association_long.csv
#           ./data/IMPLAN/sp_key_iopac.csv
#           ./data/IMPLAN/gear_key_iopac.csv
#           ./data/portlist_allCA2.csv
# output:   ./results/triptix_allCA3_io_pac.csv

# summarize econ and iopac data by dsc association types
source("./bin/fishtix_econ_dsc_summarize.R")
# input:     ./results/triptix_allCA3_io_pac.csv
#           ./data/IMPLAN/comm_mults_2020.csv
# output:   files below, plus versions for other association types ..........

# landings without multipliers
#           ./results/nomults_port_bl.csv   
#           ./results/nomults_commodity_bl.csv
#           ./results/nomults_annrev_bl.csv

# multipliers, but values NOT calculated
#           ./results/revenue_bycommsector_wportmults_bl.csv
#           ./results/revenue_bycommsector_wCAmults_bl.csv

# values calculatd from multipliers
#           ./results/econcontributions_portlevel_bl.csv                 # economic contributions - port multipliers
#           ./results/ec_port_com_all_long.csv                          # economic contributions - port multipliers, with commodity info
#           ./results/econcontributions_statelevel_bl.csv                # economic contributions - state level multipliers, with commodity info
#           ./results/econcontributions_statelevel_ann_bl.csv            # economic contributions - state level multipliers, no commodity info



# code from DSC conference -----------------------------------------

# clean commodity code descriptions
source("./bin/commodity_codes_clean.R")
# input:     ./data/commcd_dat.csv
# output:    ./results/commcd_dat2.csv

# change data from wide to long #updated ------------------------------------
# since no multiplier data is in long form, removed those data from this code
source("./bin/wide_long_2024.R") 
# input:  ./results/econcontributions_portlevel_bl.csv  # input for all association types (bl, pr, etc).....
#         ./results/econcontributions_statelevel_bl.csv
#         ./results/econcontributions_statelevel_ann_bl.csv
#         ./results/nomults_port_bl.csv")%>%    glimpse()
#         ./results/nomults_commodity_bl.csv")%>%    glimpse()
#         ./results/nomults_annrev_bl.csv
#         ./results/commcd_dat2.csv
# output: ./results/ec_port_bl_long.csv    # for port, state, annual.
#         ./results/ec_port_pr_long.csv
#         ./results/ec_port_ha2_long.csv
#         ./results/ec_port_allSp_long.csv
#         ./results/ec_noMult_port_bl_long.csv
#         ./results/ec_noMult_port_pr_long.csv
#         ./results/ec_noMult_port_ha12_long.csv
#         ./results/ec_noMult_port_allSp_long.csv

# bind all datasets together so can calculate proportions for all three data subsets
# not including no multiplier data - gets too complicated, but also that info is still in the datasets
source("./bin/join_data_2024.R")
# input:  ./results/commcd_dat2.csv  # commodity codes
#         ./results/ec_port_bl_long.csv    # for port, state, no multipliers, etc.
#         ./results/ec_port_pr_long.csv
#         ./results/ec_port_ha2_long.csv
#         ./results/ec_port_allSp_long.csv

# output: 
# ./results/ec_port_all_long.csv
# ./results/ec_state_all_long.csv
# ./results/ec_annual_all_long.csv  



# probably can skip this
# merge commodity codes with details
# source("./bin/commodity_codes.R")
# input:
# ./data/pacfin_species_codes.csv
# ./data/pacfin_gear_codes.csv
# ./results/ec_state_all_long.csv

# output: ./results/ec_comm_all_long2.csv


# graph data --------------------------------------------------------
source("./bin/graph_state_annual.R")
# input: ./results/ec_annual_all_long.csv
# output:
#  ./doc/graph_annual_prop.jpg
#  ./doc/graph_annual.jpg

source("./bin/graphs_port.R")
# input:  ./results/ec_port_all_long.csv
# output: ./doc/graph_port_pr_i1.jpg                      # proximity, association
#         ./doc/graph_port_pr_i2.jpg                      # port, assoc colors
#         ./doc/graph_port_pr_i3.jpg                      # port, port colors
#         ./doc/graph_port_ha12_i1.jpg                    # habitat, association
#         ./doc/graph_port_ha12_i2.jpg                    # port, assoc colors
#         ./doc/graph_port_ha12_i3.jpg                    # port, port colors



source("./bin/graphs_state_commodity.R")
# input:  ./results/ec_state_all_long.csv
# output: ./doc/graph_comm.jpg
#         ./doc/graph_comm_inc_1m_pr.jpg
#         ./doc/graph_comm_pr_i1.jpg                    # e = employment, i = income, o = output
#         ./doc/graph_comm_pr_i2.jpg                    # pr = proximity, ha12 = habitat/depth
#         ./doc/graph_comm_pr_i3.jpg
#         ./doc/graph_comm_ha12_i1.jpg
#         ./doc/graph_comm_ha12_i2.jpg
#         ./doc/graph_comm_ha12_i3.jpg
#         ./doc/graph_comm_pr_o1.jpg
#         ./doc/graph_comm_pr_o2.jpg
#         ./doc/graph_comm_pr_o3.jpg
#         ./doc/graph_comm_ha12_o1.jpg                  # 1 = area, assoc colors
#         ./doc/graph_comm_ha12_o2.jpg                  # 2 = line, assoc colors
#         ./doc/graph_comm_ha12_o3.jpg                  # 3 = line, commodity colors

















# below is interm code from 2024




# bind all datasets together so can calculate proportions for all three data subsets
source("./bin/calculate_prop.R")
# input:  


# output: 
# ./results/val_port_sector.csv
# ./results/val_port.csv
# ./results/val_state_sector.csv
# ./results/val_annual.csv


# merge commodity codes with details
source("./bin/commodity_codes.R")
# input:
# ./data/pacfin_species_codes.csv
# ./data/pacfin_gear_codes.csv
# ./results/ec_state_all_long.csv

# output: ./results/ec_comm_all_long2.csv


# graph data
source("graph1b.R")
# input:
# ./results/ec_port_all_long.csv
# ./results/ec_state_all_long.csv
# ./results/ec_annual_all_long.csv

# output:
#  ./doc/graph_annual_prop.jpg
#  ./doc/graph_annual.jpg
# many...











# below is code from before I had JAck's full code



# from jack's code - organize groups and species codes from cdfw commercial fisheries data
# I combined groupings from different sanctuaries, but can change if needed. see old version of code for sanctary specific groups
source("./bin/organize_cdfw_fisheries_data.R")
# current version uses species groups for CHNMS
# input:  ./results/triptix_inflation_2022.csv
# output: ./results/trip_tix_ca_with_groups.csv
#         ./results/species_ca.csv
#         ./results/gear_ca.csv

# organizing and joining fishticket and dsc associated data. includes year, value, quantity, etc
source("./bin/join_fishtix_dsc.R")
# input:  ./results/trip_tix_ca_with_groups.csv 
#         ./results/association_long_2010_2020.csv
# output: ./results/triptix_dsc.csv

# add new species codes to species that did not have matches in IOPAC data - assigned these species to species that have similar life histories and catch patterns, so that they will be given multipliers in the code below
source("./bin/fishtix_update_codes.R")
# input:  ./results/triptix_dsc.csv
# output: ./results/triptix_dsc2.csv

# join triptix/dsc data with iopac codes and ports
source("./bin/join_fishtix_dsc_iopac.R")
# input:  ./results/triptix_dsc2.csv 
#         ./data/IMPLAN/sp_key_iopac.csv
#         ./data/IMPLAN/gear_key_iopac.csv
#         ./data/portlist_allCA2.csv
# output: ./results/triptix_dsc2.csv


# sub in codes for NAs in IMPLAN Data
source("implan_codes_sub_missing.R")
# input:  ./results/fishtix_dsc_revenue_multipliers_port.csv
#         ./results/fishtix_dsc_revenue_multipliers_ca.csv
# output: 
# 


# join triptix/dsc data with implan multipliers and revenue values
# both use 
source("./bin/join_fishtix_dsc_implan.R")
# input:  ./results/triptix_dsc2.csv
#         ./data/IMPLAN/comm_mults_2020.csv
# output: ./results/fishtix_dsc_revenue_multipliers_port.csv
#         ./results/fishtix_dsc_revenue_multipliers_ca.csv 
       

# calculate income etc values from revenue and implan multipliers ------------------
source("./bin/fishtix_dsc_implan_calc_values.R")
# input: 
# ./results/fishtix_dsc_revenue_multipliers_port.csv
# ./results/fishtix_dsc_revenue_multipliers_ca.csv
# output: 
# ./doc/econcontributions_port_sector.csv   # econ value by year, Portgroup and COMMCD, port mult
# ./doc/econcontributions_port.csv          # econ value by year, Portgroup, port mult
# ./doc/econcontributions_ca_sector.csv     # econ value by year, COMMCD, state mult
# ./doc/econcontributions_ca_ann.csv        # econ value by year, state mult
# ./doc/revenue_nomults_port_sector.csv       # revenue by year, Portgroup and COMMCD, NO port mult
# ./doc/revenue_nomults_port.csv              # revenue by year, Portgroup, NO port mult
# ./doc/revenue_nomults_CA_sector.csv         # revenue by year, COMMCD, NO state mult
# ./doc/revenue_nomults_CA.csv                # revenue by year, NO state mult




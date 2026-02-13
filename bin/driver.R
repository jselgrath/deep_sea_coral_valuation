# Jennifer Selgrath
# NOAA CINMS
# Deep sea coral valuation
#
# GOAL: Driver for Deep Sea Coral Valuation project
#----------------------------------------------------------
library(tidyverse); library(ggplot2)


# github: https://github.com/jselgrath/deep_sea_coral_valuation


# fishticket data here: C:/Users/jennifer.selgrath/Documents/research/r_data/cdfw_fishticket/MLDS_2025 and on kiteworks
# saving fisheies raw data to folder: r_results

#----------------------------------------------------------
remove(list=ls())
setwd("G:/My Drive/research/r_projects/dsc_valuation")


# IMPLAN MULTIPLIERS - using 2023 values -----------------------


# load IOPAC multipliers from NOAA NWFSC r package
# code from here: https://github.com/allen-chen-noaa-gov/IOPAC_pub
# data folder to run package from Allen Chen via email - this pulls the multipliers from iopac/implan
# io-pac - economic multipliers for ratios of how different businsses interact with each other. specific to pacific region, developed by NMFS
source("./bin/iopac_multipliers1.R")
#input:     "G:/My Drive/research/r_projects/dsc_valuation", "IOPAC_pub"
#output:    ./results/multipliers_2023.csv


# subset CA and california port multipliers for project
source("./bin/iopac_multipliers2.R")
#imput:     ./results/multipliers_2023.csv
#output:    ./results/multipliers_2023_ca.csv
#           ./results/multipliers_2023_port.csv

# fill in missing multipliers based on mean values of species, gear, port, and/or state
source("./bin/iopac_multipliers3.R")
# input:    ./results/multipliers_2023_ca.csv
#           ./results/multipliers_2023_port.csv
# output:   ./results/multipliers_2023_ca2.csv
#           ./results/multipliers_2023_port2.csv
#           ./results/multipliers_2023_ca3.csv      - 3 = version with only filled in multipliers
#           ./results/multipliers_2023_port3.csv    - 3 = version with only filled in multipliers




# FISHTICKET DATA ------------------------------

# combine 1973-2024 fish ticket data, and make species list. subset to project years
source("./bin/organize_fish_ticket_data0.R")
#input:     Fishtix 1973 to 2024 - using copy in non-R data folder so only one copy on computer. eg: ./MLR Data Extract_2010.csv
#output:    ./results/fishtix_1973_2024.csv
#           ./results/fishtix_1973_2024_no_pii.csv
#           ./results/fishtix_2010_2024_no_pii.csv

#           ./doc/fishtix_spp_1973_2024.csv
#           ./doc/fishtix_spp_2010_2024.csv

# standardize column names for fishtix data
source("organize_fish_ticket_data0b.R")
#input:     ./results/fishtix_1973_2024_no_pii.csv
#output:    ./results/fishtix_1973_2024_no_pii1.csv
#           ./results/fishtix_1973_2024_no_pii1.csv


#remove freshwater species, algae, and roe
source("./bin/organize_fish_ticket_data1.R")
#input:     ./data/dsc_val_associations_freshwater3.csv #gdrive - different from last paper because spp from all years
#           ./doc/fishtix_spp_2010_2024.csv             #gdrive
#           ./results/fishtix_1973_2024_no_pii1.csv
#           ./results/fishtix_2010_2024_no_pii.csv
#output:    ./results/fishtix_1973_2024_no_pii2.csv
#           ./results/fishtix_2010_2024_no_pii2.csv
#           ./results/fishtix_spp_2010_2024_no_fresh.csv




# ASSOCIATION DATA ---------------------------------------------


# manuscript for associations published: selgrath et al (2025) Fish and Fisheries.
# associations ------------------------------------
# bl = body length
# pr = proximity
# ha12 = probably or definitely associated with depth and habitat
# ha2 =  definitely associated with depth and habitat
# all = all species


# remove algae, agar, freshwater spp from association lists, limit to study years
source("./bin/organize_fish_ticket_data2.R")  
# input:     ./results/fishtix_spp_2010_2024_no_fresh.csv                
#            ./data/association_long2.csv                                # v2 includes additional spp for 2010
# output:    ./results/association_long_2010_2024.csv                    # association data, just for species caught 2010-2024




# ---------------------------------------------------
# ORGANIZE CODES TO JOIN WITH IOPAC DATA


# link cdfw and iopac species groups and codes
source("./bin/iopac_to_cdfw_species.R")
# input:    ./data/species_translations_pacfin.csv
#           ./data/species_translations_pacfin2.csv
#           ./data/species_key_cdfw.csv # key with cdfw to pacfin codes from chris free - see his website
#           ./data/species_dat.csv # pacfin & iopac groups from allen chen - see email
#           ./results/association_long_2010_2024.csv # association data from selgrath et al 2025
# output:   ./results/species_key_final.csv


# link cdfw and iopac gear groups and codes
source("./bin/iopac_to_cdfw_gear.R")
# input:    ./data/gear_key_iopac.csv
#           ./data/gear_key_cdfw.csv  # key with cdfw to pacfin codes from chris free
#           ./data/gear_dat.csv
# output:   ./results/gear_key_final.csv




# link cdfw and iopac port groups and codes. pacfin data from pfmc
source("./bin/iopac_to_cdfw_ports.R")
# input:    ./data/port_translations_pacfin.csv
#           ./data/port_translations_pacfin2.csv
#           ./data/portlist_allCA3.csv # from ONMS and IOPAC tech memo, fixed 2 errors
#           ./data/port_key_cdfw.csv # from chris free: https://chrismfree.com/california-fisheries-data/ca-fishing-port-codes/
#           ./data/pacfin_port_complex_names.csv
# output:   ./results/port_key_final.csv





# ---------------------------------------------------
# join fish ticket at association and species/gear key data, make commodity codes
source("./bin/organize_fish_ticket_data3.R")
# input:    ./results/fishtix_2010_2024_no_pii2.csv
#           ./results/species_key_final.csv
#           ./results/gear_key_final.csv
# output:   ./results/fishtix_2010_2024_no_pii3.csv



# join fish ticket at association and port key data
source("./bin/organize_fish_ticket_data4.R")
# input:    ./results/fishtix_2010_2024_no_pii3.csv
#           ./results/port_key_final.csv
# output:   ./results/fishtix_2010_2024_no_pii4.csv



# join iopac multipliers to fishtix data
source("./bin/organize_fish_ticket_data5.R")
# input:    ./results/fishtix_2010_2024_no_pii4.csv
#           ./results/multipliers_2023_ca3.csv
#           ./results/multipliers_2023_port3.csv
# output:   ./results/fishtix_2010_2024_no_pii5.csv



# calulate NA summaries for paper and appendix
source("./bin/organize_fish_ticket_data5_summarize.R")
# input:    ./results/fishtix_2010_2024_no_pii5.csv
# output:   ./doc/na_in_commcd_sum.csv")
#           ./doc/na_in_commcd.csv

# clean commodity code descriptions                                # fit in if decide need
# source("./bin/commodity_codes_clean.R")
# input:     ./results/fishtix_2010_2024_no_pii5_ca.csv
# output:    ./results/fishtix_2010_2024_no_pii5a_ca.csv
        

# join calculate state and port multipliers ---------------------------------------------------
# Fisheries data with economic outputs 
source("./bin/organize_fish_ticket_data6.R") # adapted from ./bin/fishtix_econ_dsc_summarize.R
# input:    ./results/fishtix_2010_2024_no_pii7_ca.csv")%>%
#           ./results/fishtix_2010_2024_no_pii7_port.csv
# output:   files below ..........

# port revenue no multipliers --------------------------------------------------
#           ./results/nomults_port_adj.csv
#           ./results/nomults_port_gp.csv
#           ./results/nomults_port_ha12.csv
#           ./results/nomults_port_all.csv

# commodity/state revenue no multipliers --------------------------------------------------
#           ./results/nomults_commodity_adj.csv
#           ./results/nomults_commodity_gp.csv
#           ./results/nomults_commodity_ha12.csv
#           ./results/nomults_commodity_all.csv

# annual revenue no multipliers --------------------------------------------------
#           ./results/nomults_annrev_adj.csv
#           ./results/nomults_annrev_gp.csv
#           ./results/nomults_annrev_ha12.csv
#           ./results/nomults_annrev_all.csv



#  MULTIPLIERS, VALUES NOT CALCULATED -----------------------------------------

# results commercial sector catch with port multipliers
#           ./results/revenue_bycommsector_wportmults_adj.csv
#           ./results/revenue_bycommsector_wportmults_gp.csv
#           ./results/revenue_bycommsector_wportmults_ha12.csv
#           ./results/revenue_bycommsector_wportmults_all.csv

# results commercial sector catch with CA multipliers
#           ./results/revenue_bycommsector_wCAmults_adj.csv
#           ./results/revenue_bycommsector_wCAmults_gp.csv
#           ./results/revenue_bycommsector_wCAmults_ha12.csv
#           ./results/revenue_bycommsector_wCAmults_all.csv



#  MULTIPLIERS, VALUES CALCULATED -----------------------------------------

# economic contributions - port level multipliers, with commodity info  ----------------------
#           ./results/econcontributions_portlevel_adj.csv
#           ./results/econcontributions_portlevel_gp.csv
#           ./results/econcontributions_portlevel_ha12.csv
#           ./results/econcontributions_portlevel_all.csv

# economic contributions - state level multipliers, with commodity info ----------------------
#           ./results/econcontributions_statelevel_adj.csv
#           ./results/econcontributions_statelevel_gp.csv
#           ./results/econcontributions_statelevel_ha12.csv
#           ./results/econcontributions_statelevel_all.csv

# economic contributions - state level multipliers, no commodity info----------------------
#           ./results/econcontributions_statelevel_ann_adj.csv
#           ./results/econcontributions_statelevel_ann_gp.csv
#           ./results/econcontributions_statelevel_ann_ha12.csv
#           ./results/econcontributions_statelevel_ann_all.csv
#           



# ANALYSES ETC ------------------------------------------------------


# code from DSC conference -----------------------------------------



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



# can skip this
# merge commodity codes with details
# source("./bin/commodity_codes.R")
# input:
# ./data/pacfin_species_codes.csv
# ./data/pacfin_gear_codes.csv
# ./results/ec_state_all_long.csv

# output: ./results/ec_comm_all_long2.csv


# graph data --------------------------------------------------------
source("./bin/graphs_state_annual.R")
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


# summary stats
source("./bin/summary_stats.R")
# input:  
# output: ./doc/summary_stats_all_yr_state.csv
#         ./doc/summary_stats_all_yr_port.csv
#         
#         
#         
#         




# old code - keep in case useful later
# sub in codes for NAs in IMPLAN Data
# source("implan_codes_sub_missing.R")
# input:  ./results/fishtix_dsc_revenue_multipliers_port.csv
#         ./results/fishtix_dsc_revenue_multipliers_ca.csv
# output: 
# 




# Jennifer Selgrath - multipleirs and code from Allen Chen
# NOAA CINMS
# Deep sea coral valuation
#
# GOAL: Pull drivers for econ data
# see: https://github.com/allen-chen-noaa-gov/IOPAC_pub
#----------------------------------------------------------
library(tidyverse)
library(devtools)
library(here)
here()

# ------------------------
remove(list=ls())

# install iopac package
#The working directory should be the top level of the package. Go up one to install.
install(here("G:/My Drive/research/r_projects/dsc_valuation", "IOPAC_pub"))

#Note package name is IOPAC
library(IOPAC)
library(ggplot2)

# ---------------------------------------
# code from github

costflist_2023 <- costflist_template

costflist_2023$vessel <- clean_cost_data(functype = "vessel")

costflist_2023$processor <- clean_cost_data(sums = costf_P_list[["y2023"]],
                                            functype = "processor")
# vessel, processer and total multipliers
multres <- iopac_wrap(costfin = costflist_2023)%>%
  arrange(Name)
head(multres)

unique(multres$Region)
unique(multres$Name)

# fix typos
multres$Region<-if_else(multres$Region=="MorrowBay","MorroBay",multres$Region)
multres$Name<-if_else(multres$Name=="Shrinp, Trawl","Shrimp, Trawl",multres$Name)


# save
write_csv(multres,"./results/multipliers_2023.csv")



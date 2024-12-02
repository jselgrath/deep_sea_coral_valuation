# Jennifer Selgrath
# NOAA CINMS
# 20230412
#
# GOAL: Load files and check them out

# FILE NAMES - MEANINGS & PURPOSE
#   catch_nomults
#    ●	Catch that did not have associated multipliers
#   econcontributions_portlevel
#    ●	Results - broken down by 
#     ○	year, port group, COMMDC (commodity sector = species + gear)
#    ●	total values = vessel + processor
#   econcontributions_statelevel_ann
#    ●	Results - broken down by year
#   econcontributions_statelevel
#    ●	Results - broken down by year, COMMDC (commodity sector = species + gear)

#======================================================
library(tidyverse); library(ggplot2)

#======================================================
remove(list=ls())
setwd("C:/Users/Jennifer.Selgrath/Documents/research/R_projects/dsc_valuation/")


# set WD to either  ------------------------------------
#   - confirmed 
# setwd("./data/DSCb_1s/")

#   - confirmed and probable species ----------------------------
# CONFIRM IF THIS IS JUST NAs OR NAs plus confirmed ######################
# setwd("./data/DSCb_NAs/")

#   - all species -------------------------
setwd("./data/all_sp/")




# load files
d1<-read_csv("./econcontributions_portlevel.csv")%>%
  glimpse()

d2<-read_csv("./econcontributions_statelevel.csv")%>%
  glimpse()

d3<-read_csv("./econcontributions_statelevel_ann.csv")%>%
  glimpse()

d4<-read_csv("./catch_nomults.csv")%>%
  glimpse()


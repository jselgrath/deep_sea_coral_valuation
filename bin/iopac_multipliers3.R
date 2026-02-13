# Jennifer Selgrath
# NOAA CINMS
# Deep sea coral valuation

# GOAL: Estimate IO-PAC multiplier values for commcd and ports for missing values. Multipliers are 2023 values.  

#----------------------------------------------------------
library(tidyverse)
library(stringr)
#----------------------------------------------------------
remove(list=ls())
setwd("G:/My Drive/research/r_projects/dsc_valuation")

# state level multipliers
d1<- read_csv("./results/multipliers_2023_ca.csv")%>% # old: #"./data/commsector_wCAmults.csv" (remove year, revenue)
  unique()%>%
  glimpse()

# port level multipliers
d2<- read_csv("./results/multipliers_2023_port.csv")%>% # old: commsector_wportmults (remove year, revenue)
  unique()%>%
  glimpse()
unique(d2$Region)
head(d2)



# List of columns to process
target_cols <- c("Vessel_output", "Vessel_income", "Vessel_employment", 
                 "Processor_output", "Processor_income", "Processor_employment", 
                 "TotOut", "TotInc", "TotEmp")

# Pre-calculate global mean
global_means <- colMeans(d1[, target_cols], na.rm = TRUE) # ca mult
global_means2 <- colMeans(d2[, target_cols], na.rm = TRUE) # port mult




# Deal with NA codes ----------------------------------

# list of NA codes that are missing from california state data - also iteratively figured out in fishtix5 code
d1a<-d1%>%
  select(Region, Name:gear_name,commcd)%>%
  unique()%>%glimpse()
tail(d1)


# create missing codes that are in fish ticket data
na1 <- tibble(
  commcd = c("NAFX", "NANA", "NANT", "NAOTRG", "NATW", 
             "CPSNA", "CRBNA", "DTNA", "GRDNA", "HALNA", 
             "HMSNA", "OTRNA", "SAMNA", "SHPNA", "WHTNA",
             "DTNT", "GRDOTRG", "HALOTRG", "HMSOTRG", "HMSTW", 
             "SAMOTRG", "SBLNT", "SBLOTRG", "SHPNT", "SHPOTRG", "WHTNT"),
  
  SPECIES_NAME = c("FN", NA, NA, NA, NA, 
                   "CPS", "Crab", "Dover/Thornyhead", "Other Groundfish", "Halibut", 
                   "HMS", "Other Species", "Salmon", "Shrimp", "Whiting",
                   "Dover/Thornyhead", "Other Groundfish", "Halibut", "HMS", "HMS",
                   "Salmon", "Sablefish", "Sablefish", "Shrimp", "Shrimp", "Whiting"),
  
  gear_name = c("Fixed Gear", NA, "Net", "Other Gear", "Trawl", 
                NA, NA, NA, NA, NA, 
                NA, NA, NA, NA, NA,
                "Net", "Other Gear", "Other Gear", "Other Gear", "Trawl",
                "Other Gear", "Net", "Other Gear", "Net", "Other Gear", "Net")
) %>%
  mutate(Region = "California",
         Name = paste0(SPECIES_NAME, ", ", gear_name))






#create shell dataframe and append to d1 - state level multipliers # ----------------------
d1_extended <- d1 %>%
  bind_rows(na1)%>%
  glimpse()

tail(d1_extended)



#create shell dataframe and append to d2 - port level multipliers # ----------------------
d2_extended <- d2 %>%
  bind_rows(na1)%>%
  glimpse()

tail(d2)
tail(d2_extended)





# California multipliers --------------------------------

# Create the new columns and calculate means
# California multipliers --------------------------------

df_updated1 <- d1_extended %>% # FIX 1: Use the extended dataset
  # Extract codes
  mutate(
    species_code = str_sub(commcd, 1, 3),
    gear_code = str_sub(commcd, 4, -1)
  ) %>%
  # FIX 2: Convert "NA" text to real NA so they don't break group means
  mutate(
    species_code = if_else(species_code == "NA", NA_character_, species_code),
    gear_code = if_else(gear_code == "NA", NA_character_, gear_code)
  ) %>%
  
  # Calculate the different grouping means
  group_by(species_code, gear_code) %>%
  mutate(across(all_of(target_cols), ~mean(.x, na.rm = TRUE), .names = "sg_mean_{.col}")) %>%
  ungroup() %>%
  
  group_by(species_code) %>%
  mutate(across(all_of(target_cols), ~mean(.x, na.rm = TRUE), .names = "s_mean_{.col}")) %>%
  ungroup() %>%
  
  group_by(gear_code) %>%
  mutate(across(all_of(target_cols), ~mean(.x, na.rm = TRUE), .names = "g_mean_{.col}")) %>%
  ungroup() %>%
  
  # Apply Imputation Waterfall
  mutate(across(
    .cols = all_of(target_cols),
    .names = "{.col}2",
    .fns = ~ case_when(
      !is.na(.x) ~ .x,
      
      # Level 2: Species + Gear (e.g., 'CPSTW')
      !is.nan(get(paste0("sg_mean_", str_remove(cur_column(), "2$")))) ~ 
        get(paste0("sg_mean_", str_remove(cur_column(), "2$"))),
      
      # Level 3: Species (e.g., 'SHPNA' grabs 'SHP' average)
      !is.nan(get(paste0("s_mean_", str_remove(cur_column(), "2$")))) ~ 
        get(paste0("s_mean_", str_remove(cur_column(), "2$"))),
      
      # Level 4: Gear (e.g., 'NAFX' grabs 'FX' average)
      !is.nan(get(paste0("g_mean_", str_remove(cur_column(), "2$")))) ~ 
        get(paste0("g_mean_", str_remove(cur_column(), "2$"))),
      
      # Level 5: Global dataset mean
      TRUE ~ global_means[str_remove(cur_column(), "2$")]
    )
  )) %>%
  select(-starts_with("sg_mean_"), -starts_with("s_mean_"), -starts_with("g_mean_"))

# Check a sample of the new columns
df_updated1 %>% select(commcd, ends_with("2")) %>% 
  head()

# View results
print(df_updated1)
# view(df_updated1)

# manually check
test1<-df_updated1%>%
  filter(species_code=="CPS")%>%
  summarise(Vessel_output_test=mean(Vessel_output, na.rm=T))%>%
  glimpse()

# Count how many NAs were present before and how many are left after
df_updated1 %>%
  summarise(across(all_of(target_cols), 
                   list(
                     orig_na = ~sum(is.na(.x)),
                     new_na  = ~sum(is.na(get(paste0(cur_column(), "2"))))
                   ), 
                   .names = "{.col}_{.fn}")) %>%
  tidyr::pivot_longer(everything(), 
                      names_to = c("Variable", ".value"), 
                      names_pattern = "(.*)_(.*)")


# check NAs -----------
df_updated1 %>%
  filter(is.na(Vessel_output2)) %>%
  select(commcd, species_code, gear_code) %>%
  unique()



# -------------------------------
# port multipliers ---------------------------------

df_updated2 <- d2_extended %>%
  # 1. Extract codes
  mutate(
    species_code = str_sub(commcd, 1, 3),
    gear_code = str_sub(commcd, 4, -1)
  ) %>%
  
  # 2. Calculate grouping means at every level
  # Level A: Port + Species + Gear
  group_by(Region, species_code, gear_code) %>%
  mutate(across(all_of(target_cols), ~mean(.x, na.rm = TRUE), .names = "psg_mean_{.col}")) %>%
  ungroup() %>%
  
  # Level B: Species + Gear
  group_by(species_code, gear_code) %>%
  mutate(across(all_of(target_cols), ~mean(.x, na.rm = TRUE), .names = "sg_mean_{.col}")) %>%
  ungroup() %>%
  
  # Level C: Species only
  group_by(species_code) %>%
  mutate(across(all_of(target_cols), ~mean(.x, na.rm = TRUE), .names = "s_mean_{.col}")) %>%
  ungroup() %>%
  
  # Level D: Gear only
  group_by(gear_code) %>%
  mutate(across(all_of(target_cols), ~mean(.x, na.rm = TRUE), .names = "g_mean_{.col}")) %>%
  ungroup() %>%
  
  # 3. Apply the Waterfall Imputation
  mutate(across(
    .cols = all_of(target_cols),
    .names = "{.col}2",
    .fns = ~ case_when(
      !is.na(.x) ~ .x,
      
      # Level 2: Port + Species + Gear
      !is.nan(get(paste0("psg_mean_", str_remove(cur_column(), "2$")))) ~ 
        get(paste0("psg_mean_", str_remove(cur_column(), "2$"))),
      
      # Level 3: Species + Gear
      !is.nan(get(paste0("sg_mean_", str_remove(cur_column(), "2$")))) ~ 
        get(paste0("sg_mean_", str_remove(cur_column(), "2$"))),
      
      # Level 4: Species
      !is.nan(get(paste0("s_mean_", str_remove(cur_column(), "2$")))) ~ 
        get(paste0("s_mean_", str_remove(cur_column(), "2$"))),
      
      # Level 5: Gear
      !is.nan(get(paste0("g_mean_", str_remove(cur_column(), "2$")))) ~ 
        get(paste0("g_mean_", str_remove(cur_column(), "2$"))),
      
      # Level 6: Global Mean
      TRUE ~ mean(get(str_remove(cur_column(), "2$")), na.rm = TRUE)
    )
  )) %>%
  
  # 4. Cleanup helper columns
  select(-starts_with("psg_mean_"), -starts_with("sg_mean_"), 
         -starts_with("s_mean_"), -starts_with("g_mean_"))%>%
  glimpse()


# Check a sample of the new columns
df_updated2 %>% select(Region,commcd, ends_with("2")) %>% 
  head()

# View results
print(df_updated2)
# view(df_updated2)

# manually check value - example
test2<-df_updated2%>%
  filter(species_code=="CPS")%>%
  summarise(Vessel_output_test=mean(Vessel_output, na.rm=T))%>%
  glimpse()

# df_updated2%>%filter(iopac_commcd=="NAFX")%>%select(iopac_commcd,total_price,vessel_output_pm,vessel_income_pm,vessel_employment_pm,processor_output_pm,processor_income_pm, processor_employment_pm, tot_out_pm,tot_inc_pm,tot_emp_pm)


# check NAs -----------
df_updated2 %>%
  filter(is.na(Vessel_output2)) %>%
  select(commcd, species_code, gear_code) %>%
  unique()



# version with only filled in multipliers ---------------------------
df_updated1a<-df_updated1%>%
  select(Region:Sector,id_commcd:TotEmp2)%>%
  glimpse()


df_updated2a<-df_updated2%>%
  select(Region:Sector,id_commcd:TotEmp2)%>%
  glimpse()
          
          
          
          
# save ---------------
write_csv(df_updated1,"./results/multipliers_2023_ca2.csv")
write_csv(df_updated2,"./results/multipliers_2023_port2.csv")

write_csv(df_updated1a,"./results/multipliers_2023_ca3.csv")
write_csv(df_updated2a,"./results/multipliers_2023_port3.csv")

# Jennifer Selgrath
# NOAA CINMS
# Deep sea coral valuation

# GOAL: Estimate IO-PAC multiplier values for COMMCD and ports for missing values. Multipliers are 2020 values.  

#----------------------------------------------------------
library(tidyverse)
library(stringr)
#----------------------------------------------------------
remove(list=ls())
setwd("G:/My Drive/research/r_projects/dsc_valuation")

# state level multipliers
d1<- read_csv("./data/commsector_wCAmults.csv")%>% 
  select(-revenue,-year)%>%
  unique()%>%
  glimpse()

# port level multipliers
d2<- read_csv("./data/commsector_wportmults.csv")%>% 
  select(-revenue,-year)%>%
  unique()%>%
  glimpse()

# List of columns to process
target_cols <- c("Vessel_output", "Vessel_income", "Vessel_employment", 
                 "Processor_output", "Processor_income", "Processor_employment", 
                 "TotOut", "TotInc")

# Pre-calculate global mean
global_means <- colMeans(d1[, target_cols], na.rm = TRUE) # ca mult
global_means2 <- colMeans(d2[, target_cols], na.rm = TRUE) # port mult

# California multipliers --------------------------------

# Create the new columns and calculate means
df_updated1 <- d1 %>%
  # 1. Extract codes
  mutate(
    species_code = str_sub(COMMCD, 1, 3),
    gear_code = str_sub(COMMCD, 4, -1)
  ) %>%
  # 2. Calculate the different grouping means
  group_by(species_code, gear_code) %>%
  mutate(across(all_of(target_cols), ~mean(.x, na.rm = TRUE), .names = "sg_mean_{.col}")) %>%
  ungroup() %>%
  
  group_by(species_code) %>%
  mutate(across(all_of(target_cols), ~mean(.x, na.rm = TRUE), .names = "s_mean_{.col}")) %>%
  ungroup() %>%
  
  group_by(gear_code) %>%
  mutate(across(all_of(target_cols), ~mean(.x, na.rm = TRUE), .names = "g_mean_{.col}")) %>%
  ungroup() %>%
  
  # 3. Apply Imputation Waterfall (using direct references)
  mutate(across(
    .cols = all_of(target_cols),
    .names = "{.col}2",
    .fns = ~ case_when(
      !is.na(.x) ~ .x,
      
      # Level 2: Species + Gear
      !is.nan(get(paste0("sg_mean_", cur_column() %>% str_remove("2$")))) ~ 
        get(paste0("sg_mean_", cur_column() %>% str_remove("2$"))),
      
      # Level 3: Species
      !is.nan(get(paste0("s_mean_", cur_column() %>% str_remove("2$")))) ~ 
        get(paste0("s_mean_", cur_column() %>% str_remove("2$"))),
      
      # Level 4: Gear
      !is.nan(get(paste0("g_mean_", cur_column() %>% str_remove("2$")))) ~ 
        get(paste0("g_mean_", cur_column() %>% str_remove("2$"))),
      
      # Level 5: Global dataset mean
      # TRUE ~ mean(get(str_remove(cur_column(), "2$")), na.rm = TRUE)
      TRUE ~ global_means[str_remove(cur_column(), "2$")]
    )
  )) %>%
  # 4. Cleanup
  select(-starts_with("sg_mean_"), -starts_with("s_mean_"), 
         -starts_with("g_mean_"))%>%
  glimpse()

# Check a sample of the new columns
df_updated1 %>% select(COMMCD, ends_with("2")) %>% 
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


# -------------------------------
# port multipliers --

df_updated2 <- d2 %>%
  # 1. Extract codes
  mutate(
    species_code = str_sub(COMMCD, 1, 3),
    gear_code = str_sub(COMMCD, 4, -1)
  ) %>%
  
  # 2. Calculate grouping means at every level
  # Level A: Port + Species + Gear
  group_by(PortGroup_IOPAC, species_code, gear_code) %>%
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
df_updated2 %>% select(PortGroup_IOPAC,COMMCD, ends_with("2")) %>% 
  head()

# View results
print(df_updated2)
# view(df_updated2)

# manually check
test2<-df_updated2%>%
  filter(species_code=="CPS")%>%
  summarise(Vessel_output_test=mean(Vessel_output, na.rm=T))%>%
  glimpse()

# save ---------------
write_csv(df_updated1,"./results/multipliers_2020_ca.csv")
write_csv(df_updated2,"./results/multipliers_2020_port.csv")
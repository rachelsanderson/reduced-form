## This file reads in raw excel EIA capacity data
## and calculates % capacity additions by source for each group,
## saves result as a csv file so that I can add some cool vars in Python

library(tidyverse)
library(dplyr)

# Code that imports raw annual generation data by state ------------------------------------

df <- read_csv("Dropbox (Princeton)/Data for Tax Equity Project/eia_capacity_data/existcapacity_annual.csv")

colnames(df) <- gsub(" ", "_",tolower(colnames(df)))

df$year <- as.numeric(df$year)
df$nameplate_cap <- as.numeric(gsub(",", "", df$`nameplate_capacity_(megawatts)`))
df$summer_cap <- as.numeric(gsub(",", "", df$`summer_capacity_(megawatts)`))
df$num_facilities <- as.numeric(df$facilities)
df[is.na(df$num_facilities),c('num_facilities')] <- 0
df$num_generators <- as.numeric(df$generators)
df[is.na(df$num_generators),c('num_generators')]  <- 0
df$state <- df$state_code


df <- df %>% select(-c(`nameplate_capacity_(megawatts)`,
                 `summer_capacity_(megawatts)`,
                 state_code,
                 generators,
                 facilities))

# Calculate percent capacity (change, stock) by source ----------------------------------

# Convert "all sources" row into a column 

df <- df %>% filter(producer_type=="Total Electric Power Industry") %>%
  ungroup() %>%
  select(-c("producer_type"))

to_merge <- df %>% filter(fuel_source == "All Sources",) %>%
  mutate(tot_nameplate_cap = nameplate_cap,
         tot_summer_cap = summer_cap,
         tot_num_gen = num_generators,
         tot_num_facilities = num_facilities) %>%
  select(state, year, tot_nameplate_cap, tot_summer_cap,
         tot_num_gen, tot_num_facilities) %>%
  group_by(state) %>% 
  mutate(tot_cap_change = tot_nameplate_cap - lag(tot_nameplate_cap), 
         tot_cap_growth_rate = (tot_cap_change / tot_nameplate_cap) * 100) %>%
  arrange(state,year) 

new_df <- df %>% filter(fuel_source != "All Sources") %>%
  group_by(state,year) %>%
  right_join(to_merge, by=c('year', 'state')) %>%
  mutate(p_nameplate_cap = nameplate_cap/tot_nameplate_cap,
         p_summer_cap = summer_cap/tot_summer_cap) %>%
  group_by(fuel_source) %>%
  mutate(cap_change = nameplate_cap - lag(nameplate_cap),
         cap_growth_rate = cap_change/nameplate_cap) %>%
  arrange(state, fuel_source, year) %>% filter(year!=1990)

new_df$energy_source <- new_df$fuel_source
new_df <- new_df %>% ungroup() %>% select(-fuel_source)

outDir = "Dropbox (Princeton)/Tax Equity Code/Stata_Analysis/MakeStataData/InputData/"
write_csv(new_df, paste0(outDir,"annual_capacity_data.csv"))


# Compare to generator level data -----------------------------------------
# 
# df.alt <- read_csv(paste0(outDir,"annual_capacity_form860.csv"))
# df.alt <- df.alt[-c(1)]
# 
# df.alt %>% group_by(plant_state, technology) %>% 
#   mutate(tot_cap = cumsum(nameplate_cap)) %>% View()

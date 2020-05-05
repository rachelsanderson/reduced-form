## This file reads in raw excel EIA generation data
## and calculates % generation by source for each group,
## saves result as a csv file so that I can add some cool vars in Python

library(ggplot2)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(grid)
library(directlabels)
library(usmap)

# Code that imports raw annual generation data by state ------------------------------------

df <- readxl::read_xls(path="Dropbox (Princeton)/Data for Tax Equity Project/eia_summary_data/annual_generation_state.xls",
                        skip=1)
colnames(df) <- gsub(" ", "_",tolower(colnames(df)))
colnames(df)[ncol(df)] <- "mw_generation"


# Calculate percent generation by source ----------------------------------

# Convert "total" row into a column 

to_merge <- df %>% filter(energy_source == "Total") %>%
  group_by(year,state,type_of_producer) %>%
  summarise(total_gen = sum(mw_generation)) 

new_df <- df %>% filter(energy_source != "Total") %>%
  group_by(year, state, type_of_producer) %>%
  mutate(perc_gen = mw_generation/sum(mw_generation)) %>% 
  right_join(to_merge, by=c('year', 'state', 'type_of_producer')) 

new_df <- new_df %>% 
  filter(type_of_producer == "Total Electric Power Industry") %>%
  ungroup() %>% select(-type_of_producer)

# fix this to match cap data
new_df$energy_source <- gsub("Hydroelectric Conventional", "Hydroelectric", new_df$energy_source)

outDir = "Dropbox (Princeton)/Tax Equity Code/Stata_Analysis/MakeStataData/InputData/"

write_csv(new_df, paste0(outDir,"annual_generation_data.csv"))


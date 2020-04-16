library(tidyverse)
library(anytime)
library(stringr)

########## IMPORT CLEAN PROPOSED GEN LIST

utilities <- read.csv(file = "~/Dropbox (Princeton)/Data for Tax Equity Project/eia_data/eia_master_files/master_util_list.csv")

# Fix column names
colnames(utilities)[1] <- "utility_name"
colnames(utilities) <- lapply(colnames(utilties), function(x) gsub("X","",x))

## Fix variable types
proposed_gen$nameplate_cap <- as.numeric(proposed_gen$nameplate_cap)
proposed_gen$curr_complete_date <-as.Date(anydate(paste0(proposed_gen$curr_scheduled_year, "-",
                                                         proposed_gen$curr_sceheduled_month)), format="%Y-%m-%d")

## Drop after 2008
proposed_gen <- proposed_gen %>% filter(year>2008) %>% select(-X)

# 2.6k utilities, with 12.4k proposed projects 
proposed_gen <- proposed_gen %>% add_count(utility_name)

# let's merge with utilities dataset
# every proposed gen has a match, but not all utilities have proposed gen
merged <- proposed_gen %>% left_join(utilities, by='utility_name') 

### CODE TO DEDUPLICATE DATA
### Problem is that some generators appear in multiple years, so we would like
### to ID generators over time

## At a minimum, we should  expect proposed generators to appear again if 
## scheduled completion year is (t + 2)
merged <- merged %>% mutate(should_appear_again = ifelse(curr_scheduled_year > year + 1, 1, 0 ))
sum(merged$should_appear_again) # 6066/12399 = approx 1/2 generators fall into this category 

# focus on id'ing those? 
merged<- merged %>% group_by(utility_name,plant_name,plant_state,prime_mover,nameplate_cap) %>%
  mutate(new_id = group_indices()) 

merged <- merged %>% group_by(new_id) %>%
  mutate(num_obs = n())


goodCols <- c("utility_name", "plant_state", 'prime_mover', 'year','curr_scheduled_year','nameplate_cap')
temp <- merged %>%filter(num_obs > 1) %>% arrange(utility_name) %>% select(goodCols, new_id) 
# 9141 fit this category?

  merged %>% filter(should_appear_again ==1,
                                  (num_obs != 1 | year != 2018)) %>%
  arrange(new_id) %>% 
  select(utility_name, plant_name, new_id, year,curr_scheduled_year, should_appear_again, 
         status, generator_id, num_obs,plant_state) %>% View()

#check for cancellations and such... need to link with generator data to see what comes online 



# first make within year ID 
# identical technologies at same plant (i.e. multiple gen units in plant)
# within same year are assigned the same id
merged <- merged %>% group_by(utility_name,plant_name,
                              plant_state,
                              prime_mover,
                              nameplate_cap,
                              year,
                              curr_scheduled_year) %>%
  mutate(group_id = group_indices(),
         n_group = n()) 

## if n_group == 1 no problem; need to consider cases with n_group >1 
## this represents case with multiple generating units at same plant that appear identical
merged %>% filter(n_group > 1) %>% 
  select(utility_name,plant_name,plant_state,prime_mover,nameplate_cap,year,curr_scheduled_year) %>% View()


## make varaible that is like nunits... or something

# Compare with if we don't have year



# 2625 utilities have no proposed gen
no_matches <- anti_join(utilities, proposed_gen, by='utility_name')
nrow(utilities)-nrow(no_matches)

########## Define status labels of generators in dataset
status_labels <- c("Cancelled", "Permits Pending",
                   "Other", "Planned, not started",
                   "Permitted", "Completed, not operating",
                   "Under construction", "Almost complete")
#IP L = permits pending; OT P T TS U V


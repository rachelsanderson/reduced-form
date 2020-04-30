library(ggplot2)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(anytime)
library(scales)
library(lubridate)
library(zoo)
library(ggpubr)
library(usmap)
library(xtable)


# Set global directories --------------------------------------------------

dataDir = "~/Dropbox (Princeton)/Data for Tax Equity Project/eia_data/eia8602018/"
figDir = "~/Dropbox (Princeton)/Figures/GenFigs/"

# Set up data -------------------------------------------------------------

gen_data_merged = read.csv(paste0(dataDir,'merged_clean_address.csv'))
gen_data = read.csv(paste0(dataDir,'gen_2018.csv'))

# gen_data$nameplate_cap <- as.numeric(gen_data$Nameplate.Capacity..MW.)
# gen_data_merged$nameplate_cap <- as.numeric(gen_data_merged$nameplate_capacity_mw)

# Fix numeric variables
np=gsub(",","",gen_data_merged$nameplate_capacity_mw)
gen_data_merged$nameplate_cap = as.numeric(np)
gen_data_merged %>% select(nameplate_cap, nameplate_capacity_mw)

# Fix date variables
fix_dates <- function(df_year, df_month){
  dates<- paste(df_year,df_month, rep(01, length(df_year)), sep = "/")
  new_dates <- as.Date(dates)
  return(new_dates)
}

gen_data$Operating.Date <- fix_dates(gen_data$Operating.Year,gen_data$Operating.Month)
gen_data$Operating.Quarter <- as.yearqtr(gen_data$Operating.Date)

gen_data_merged$operating_date <- fix_dates(gen_data_merged$operating_year,gen_data_merged$operating_month)
gen_data_merged$operating_quarter <- as.yearqtr(gen_data_merged$operating_date)

## Format into quarters
format_quarters <- function(x) {
  x <- as.yearqtr(x)
  year <- as.integer(x)
  quart <- as.integer(format(x, "%q"))
  
  paste(c("Q1","Q2","Q3","Q4")[quart], 
        year)
}

calc_num_quarters <- function(dates) {
  as.yearqtr(max(dates))- as.yearqtr(min(dates))
}


# Redo the solar pictures -------------------------------------------------

omitList = c("IPP CHP", "Industrial Non-CHP", "Industrial CHP")

solar <- filter(gen_data_merged, technology == "Solar Photovoltaic",
                  operating_year >=2010, region != "Hawaii",
                region!= "Alaska",
                !(sector_name %in% omitList))

solar_tot <- solar %>% group_by(operating_quarter, sector_name, grant_program) %>%
  summarise(tot_cap = sum(nameplate_cap))

# total MW additions by year and sector
ggplot(solar_tot, aes(x=operating_quarter,y=tot_cap, fill = sector_name, 
                      )) +
  geom_bar(stat='identity',position='stack', alpha = 0.8) + 
  scale_x_yearqtr(format = "%YQ%q", 
                  breaks = seq(from = min(solar_tot$operating_quarter)+.75, 
                               to = max(solar_tot$operating_quarter), by = 1),
                  limits=c(min(solar_tot$operating_quarter), 
                           max=max(solar_tot$operating_quarter))) +
  theme_bw() + 
  xlab("\nStart Operation Date") +
  ylab("Total Capacity (MW)\n") + 
  geom_vline(xintercept=as.yearqtr("2016 Q4"), linetype='dotted') +
  theme(axis.text.x = element_text(angle=90),
        legend.position="right",
        legend.title=element_blank())

ggsave(paste0(figDir,"solar_additions.png"),width=10,height=7)

ggplot(solar_tot, aes(x=operating_quarter,y=tot_cap, fill = grant_program)) +
  geom_bar(stat='identity',position='stack', alpha = 0.8) + 
  scale_x_yearqtr(format = "%YQ%q", 
                  breaks = seq(from = min(solar_tot$operating_quarter)+.75, 
                               to = max(solar_tot$operating_quarter), by = 1),
                  limits=c(min(solar_tot$operating_quarter), 
                           max=max(solar_tot$operating_quarter))) +
  theme_bw() + 
  xlab("\nStart Operation Date") +
  ylab("Total Capacity (MW)\n") + 
  # scale_fill_discrete(label= c("No Loan Grant","1603 Grant")) + 
  geom_vline(xintercept=as.yearqtr("2016 Q4"), linetype='dotted') +
  theme(axis.text.x = element_text(angle=90),
        legend.position=c(0.2,0.8),
        legend.title=element_blank())

ggsave(paste0(figDir,"solar_loan_grants.png"),width=10,height=7)


## annual additions graph
solar %>% group_by(operating_year, sector_name, grant_program) %>%
  summarise(tot_cap = sum(nameplate_cap)) %>%
  ggplot(aes(x=operating_year,y=tot_cap, fill = grant_program)) +
  geom_bar(stat='identity',position='stack', alpha = 0.8) + 
  theme_bw() + 
  scale_x_continuous(breaks=seq(2008,2018,1)) +
  xlab("\nStart Operation Date") +
  ylab("Total Capacity (MW)\n") + 
  theme(axis.text.x = element_text(angle=90),
        legend.position=c(0.2,0.8),
        legend.title=element_blank())
ggsave(paste0(figDir, 'sollar_add_ann.png'),width=10,height=7)


# Regional Analysis -------------------------------------------------------

solar %>% group_by(operating_quarter, grant_program, region) %>%
  summarise(tot_cap = sum(nameplate_cap)) %>% 
  ggplot(aes(x=operating_quarter,y=tot_cap, fill=grant_program)) + 
  geom_bar(stat='identity',position='stack') +
  theme_bw() + 
  facet_wrap(~region, scales="free_y", nrow=2) + 
  xlab("\nStart Operation Date") +
  ylab("Capacity (MW)\n") + 
  geom_vline(xintercept=as.yearqtr("2016 Q4"), linetype='dotted') +
  theme(axis.text.x = element_text(angle=90),
        legend.position="bottom",
        legend.title=element_blank())

ggsave(paste0(figDir,"solar_additions_region.png"),width=14,height=8)


solar %>% group_by(operating_year, grant_program, region) %>%
  summarise(tot_cap = sum(nameplate_cap)) %>% 
  ggplot(aes(x=operating_year,y=tot_cap, fill=grant_program)) + 
  geom_bar(stat='identity',position='stack') +
  theme_bw() + 
  facet_wrap(~region, scales="free_y", nrow=2) + 
  xlab("\nStart Operation Date") +
  ylab("Capacity (MW)\n") + 
  geom_vline(xintercept=2016, linetype='dotted') +
  theme(axis.text.x = element_text(angle=90),
        legend.position="bottom",
        legend.title=element_blank())
ggsave(paste0(figDir,"solar_additions_region_ann.png"),width=14,height=8)


# by state ----------------------------------------------------------------

solar_reg <- solar %>%
  group_by(operating_quarter, regulated) %>% 
  summarise(tot_cap = sum(nameplate_cap)) 

ggplot(solar_reg, aes(x=operating_quarter,y=tot_cap, fill=regulated)) +
  geom_bar(stat='identity',position='stack') +
  scale_x_yearqtr(format = "%YQ%q", 
                  breaks = seq(from = min(solar$operating_quarter)+.75, 
                               to = max(solar$operating_quarter), by = 1),
                  limits=c(min(solar$operating_quarter), 
                           max=max(solar$operating_quarter))) +
  theme_bw() +
  facet_wrap(~regulated,nrow=2) + 
  xlab("\nStart Operation Date") +
  ylab("Total Capacity (MW)\n") + 
  geom_vline(xintercept=as.yearqtr("2016 Q4"), linetype='dotted') +
  theme(axis.text.x = element_text(angle=90),
        legend.position="bottom",
        legend.title=element_blank())

ggsave(paste0(figDir,"solar_reg.png"),width=8,height=11)

# look geographically, southern spots not as clumped in back half of year

solar %>% group_by(operating_quarter,region) %>% 
  summarise(d_cap = sum(nameplate_cap)) %>% 
  ungroup() %>%
  group_by(region) %>% 
  mutate(tot_cap = cumsum(d_cap),
         d_cap_p = d_cap/tot_cap) %>% 
  arrange(region, operating_quarter) %>%
ggplot(aes(x=operating_quarter,y=tot_cap, fill=region)) +
  geom_bar(stat='identity',position='stack') +
  scale_x_yearqtr(format = "%YQ%q", 
                  breaks = seq(from = min(solar$operating_quarter)+.75, 
                               to = max(solar$operating_quarter), by = 1),
                  limits=c(min(solar$operating_quarter), 
                           max=max(solar$operating_quarter))) +
  theme_bw() + 
  facet_wrap(~region, nrow = 2, scales="free") + 
  xlab("\nStart Operation Date") +
  ylab("Total solar operating capacity \n") + 
  geom_vline(xintercept=as.yearqtr("2016 Q4"), linetype='dotted') +
  theme(axis.text.x = element_text(angle=90),
        legend.position="none")

ggsave(paste0(figDir,"tot_cap_region.png"),width=12, height=7)



# What's going on in new england ------------------------------------------

solar_raw %>% filter(region == "ISO-NE") %>% 
  group_by(plant_state, operating_quarter) %>%
  summarise(tot_cap = sum(nameplate_capacity_mw)) %>%
  arrange(plant_state)%>%
  ggplot(aes(x=operating_quarter, y = tot_cap, fill=plant_state)) + 
  geom_bar(stat='identity')

solar_raw %>% filter(state=="MA", operating_year < 2015) %>%
  arrange(desc(nameplate_cap)) %>% 
  select(operating_quarter, nameplate_cap, 
         plant_state, 
         plant_name,
         utility_name) %>%
  View()
 


# Work in the utility data ------------------------------------------------

# is there a region variable? 
gen_data_merged %>% group_by(street_address, technology) %>% 
  summarise(tot_cap = sum(nameplate_cap),
            nUnits = n()) %>% 
  filter(technology == "Solar Photovoltaic") %>% arrange(desc(tot_cap)) %>%
  ungroup() %>% 
  mutate(share = tot_cap/sum(tot_cap)) %>% View()

gen_data_merged %>% filter(technology == "Solar Photovoltaic",
                           operating_year > 2008) %>%
  group_by(operating_quarter, street_address) %>% 
  summarise(tot_cap = sum(nameplate_cap),
            nUnits = n()) %>% 
  ungroup() %>% group_by(operating_quarter) %>%
  mutate(share = tot_cap/sum(tot_cap)) %>%  
  arrange(operating_quarter, desc(tot_cap)) %>% 
  filter(share >= 0.05) %>% 
  ggplot(aes(x=operating_quarter,y=tot_cap, fill=street_address)) +
  geom_bar(stat='identity',position='stack') +
  theme(legend.position = "none")


# Build dataset for regressions  -----------------------------------------------------

#quarter 4 FE

y1 <- solar %>% group_by(operating_quarter, plant_state) %>%
  summarise(d_cap = sum(nameplate_cap))

y2 <- solar %>% group_by(operating_quarter, region) %>%
  summarise(d_cap = sum(nameplate_cap))

y3 <- solar %>% group_by(operating_year, plant_state) %>%
  summarise(d_cap=sum(nameplate_cap))

y1_fe <- y1 %>% group_by(plant_state) %>%
  mutate(state_demean = d_cap-mean(d_cap)) %>%
  ungroup()%>% group_by(operating_quarter) %>%
  mutate(state_time_demean = state_demean-mean(d_cap))

y1_fe %>% ggplot(aes(x=operating_quarter, y = state_time_demean,color=plant_state)) + geom_line()

y2 <- solar %>% group_by(operating_quarter, region) %>%
  summarise(d_cap = sum(nameplate_cap))
y2_fe <- y2 %>% group_by(region) %>%
  mutate(state_demean = d_cap-mean(d_cap)) %>%
  ungroup()%>% group_by(region) %>%
  mutate(state_time_demean = state_demean-mean(d_cap))
y2_fe %>% ggplot(aes(x=operating_quarter, y = state_demean,color=region)) + geom_line()



# Look at top utilities by quarter ----------------------------------------

topUtilities <- solar %>% group_by(parent_utility) %>%
  summarise(cap = sum(nameplate_cap)) %>% arrange(desc(cap)) %>% 
  select(parent_utility) %>% slice(1:20)

solar %>% group_by(parent_utility, operating_quarter) %>%
  summarise(cap = sum(nameplate_cap)) %>%
  arrange(desc(cap)) %>%
  filter(parent_utility %in% topUtilities$parent_utility) %>%
  ggplot(aes(x=operating_quarter,y=cap,fill=parent_utility)) +
  geom_bar(stat='identity') +
  facet_wrap(~parent_utility) +
  theme(legend.position = "none")


solar %>% mutate(util = ifelse((parent_utility %in% topUtilities$parent_utility), "top 20", "other")) %>%
  ggplot(aes(x=operating_quarter,y=nameplate_cap,fill=util)) +
  geom_bar(stat='identity') +
  theme(legend.position = "bottom") +
  facet_wrap(~region, scales="free",nrow=2) +
  xlab("\nStart Operation Date") +
  ylab("Total Capacity (MW)\n") + 
  geom_vline(xintercept=as.yearqtr("2016 Q4"), linetype='dotted') +
  theme(axis.text.x = element_text(angle=90),
        legend.position="bottom",
        legend.title=element_blank())
ggsave(paste0(figDir,"top_builders.png"),width=16,height=9)

solar %>% group_by(region, parent_utility) %>% 
  summarise(cap = sum(nameplate_cap)) %>%
  arrange(region, desc(cap)) %>%
  slice(1:10) %>%
  View()


solar %>% group_by(ownership) %>%
  summarise(cap = sum(nameplate_cap)) %>% View()
  
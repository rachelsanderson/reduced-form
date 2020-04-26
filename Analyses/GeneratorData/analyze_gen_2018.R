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

gen_data_merged = read.csv(paste0(dataDir,'merged_new.csv'))
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

solar <- filter(gen_data_merged, energy_source_1=="SUN" & operating_year >=2006)
omitList = c("IPP CHP", "Industrial Non-CHP", "Industrial CHP")
solar_tot <- solar %>% filter(operating_year >2010, !(sector_name %in% omitList)) %>%
  group_by(operating_quarter, sector_name, grant_program) %>%
  summarise(tot_cap = sum(nameplate_cap))

# total MW additions by year and sector
ggplot(solar_tot, aes(x=operating_quarter,y=tot_cap, fill = sector_name, 
                      color=sector_name)) +
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
        legend.position="bottom",
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
        legend.position="bottom",
        legend.title=element_blank())

ggsave(paste0(figDir,"solar_loan_grants.png"),width=10,height=7)



solar_ann <- solar %>% filter(operating_year >2008, !(sector_name %in% omitList)) %>%
  group_by(operating_year, sector_name) %>%
  summarise(tot_cap = sum(nameplate_cap))

ggplot(solar_ann, aes(x=operating_year,y=tot_cap, fill = sector_name, color=sector_name)) +
  geom_bar(stat='identity',position='stack', alpha = 0.8) + 
  theme_bw() + 
  scale_x_continuous(breaks=seq(2008,2018,1)) +
  xlab("\nStart Operation Date") +
  ylab("Total Capacity (MW)\n") + 
  theme(axis.text.x = element_text(angle=90),
        legend.position="bottom",
        legend.title=element_blank())
ggsave(paste0(figDir, 'sollar_add_ann.png'),width=10,height=7)

ggplot(solar %>% filter(operating_year >=2010), aes(x=operating_date,
                         fill=I('blue'),
                         col=I("black"),
                        )) + 
  geom_histogram(bins=calc_num_quarters(solar$operating_date)*2,
                 boundary = 0.5, alpha=0.6) +
  geom_vline(xintercept = as.Date("2016-11-17"), 
             linetype='dotted') + 
  scale_x_date("Operating Date",
               breaks = date_breaks("6 months"),
               labels = format_quarters) +
  theme(axis.text.x = element_text(angle=90))


ggsave(paste0(figDir,"solar_nAdditions.png"),width=10,height=7)
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


# Make data for regressions  -----------------------------------------------------

# observation is capacity addition in state by quarter 
# y_st

solar_raw <- filter(gen_data_merged, energy_source_1=="SUN" & operating_year >=2006)
omitList = c("IPP CHP", "Industrial Non-CHP", "Industrial CHP")
solar <- solar_raw %>%
  filter(!(sector_name %in% omitList),operating_year >= 2010,
         loan_grant=="False") %>%
  group_by(operating_quarter, sector_name, region) %>%
  summarise(tot_cap = sum(nameplate_cap)) 
  
ggplot(solar, aes(x=operating_quarter,y=tot_cap, fill=region)) +
  geom_bar(stat='identity',position='stack') +
  scale_x_yearqtr(format = "%YQ%q", 
                  breaks = seq(from = min(solar$operating_quarter)+.75, 
                               to = max(solar$operating_quarter), by = 1),
                  limits=c(min(solar$operating_quarter), 
                           max=max(solar$operating_quarter))) +
  theme_bw() + 
  xlab("\nStart Operation Date") +
  ylab("Total Capacity (MW)\n") + 
  geom_vline(xintercept=as.yearqtr("2016 Q4"), linetype='dotted') +
  theme(axis.text.x = element_text(angle=90),
        legend.position="bottom",
        legend.title=element_blank())

ggsave(paste0(figDir,"solar_additions_region.png"),width=10,height=7)


# by state ----------------------------------------------------------------

solar_reg <- solar_raw %>%
  filter(!(sector_name %in% omitList),operating_year >= 2010) %>%
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


solar_region <- solar_raw %>%
  filter(!(sector_name %in% omitList),
         operating_year >= 2010,
         region != "Hawaii") %>%
  group_by(operating_quarter, region) %>% 
  summarise(d_cap = sum(nameplate_cap)) %>% 
  arrange(operating_quarter,region)  
  
ggplot(solar_region, aes(x=operating_quarter,y=d_cap, fill=region)) +
  geom_bar(stat='identity',position='stack') +
  scale_x_yearqtr(format = "%YQ%q", 
                  breaks = seq(from = min(solar$operating_quarter)+.75, 
                               to = max(solar$operating_quarter), by = 1),
                  limits=c(min(solar$operating_quarter), 
                           max=max(solar$operating_quarter))) +
  theme_bw() + 
  facet_wrap(~region, nrow = 2, scales="free") + 
  xlab("\nStart Operation Date") +
  ylab("Capacity Additions (MW)\n") + 
  geom_vline(xintercept=as.yearqtr("2016 Q4"), linetype='dotted') +
  theme(axis.text.x = element_text(angle=90),
        legend.position="none")

ggsave(paste0(figDir, "solar_regional.png"),width=12,height=7)


solar_cumsum_region <- solar_raw %>% filter(!(sector_name %in% omitList),
                                            operating_year >= 2010,
                                            region != "Hawaii") %>%
  group_by(operating_quarter,region) %>% 
  summarise(d_cap = sum(nameplate_cap)) %>% 
  ungroup() %>%
  group_by(region) %>% 
  mutate(tot_cap = cumsum(d_cap),
         d_cap_p = d_cap/tot_cap) %>% 
  arrange(region, operating_quarter) 

ggplot(solar_cumsum_region, aes(x=operating_quarter,y=tot_cap, fill=region)) +
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

ggsave(paste0(figDir,"dead_idea.png"),width=12, height=7)



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
 
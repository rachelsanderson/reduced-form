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

# Fix numeric variables
gen_data$nameplate_cap <- as.numeric(gen_data$Nameplate.Capacity..MW.)
gen_data_merged$nameplate_cap <- as.numeric(gen_data_merged$nameplate_capacity_mw)

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


# Redo the wind pictures -------------------------------------------------

wind <- filter(gen_data_merged, technology=="Onshore Wind Turbine")
omitList = c("IPP CHP", "Industrial Non-CHP", "Industrial CHP")
wind_tot <- wind %>% filter(!(sector_name %in% omitList), operating_year>2000) %>%
  group_by(operating_quarter, sector_name) %>%
  summarise(tot_cap = sum(nameplate_cap))

# total MW additions by year and sector
ggplot(wind_tot, aes(x=operating_quarter,y=tot_cap, fill = sector_name, color=sector_name)) +
  geom_bar(stat='identity',position='stack', alpha = 0.8) + 
  scale_x_yearqtr(format = "%YQ%q", 
                  breaks = seq(from = min(wind_tot$operating_quarter)+.75, 
                               to = max(wind_tot$operating_quarter), by = 1),
                  limits=c(min(wind_tot$operating_quarter), 
                           max=max(wind_tot$operating_quarter))) +
  theme_bw() + 
  xlab("\nStart Operation Date") +
  ylab("Capacity Additions (MW)\n") + 
  # geom_vline(xintercept=as.yearqtr("2016 Q4"), linetype='dotted') +
  theme(axis.text.x = element_text(angle=90),
        legend.position="bottom",
        legend.title=element_blank())

ggsave(paste0(figDir,"wind_additions.png"),width=10,height=7)

# by region ----------------------------------------------------------------

wind_reg <- wind %>%
  filter(!(sector_name %in% omitList), operating_year >= 2001) %>%
  group_by(operating_quarter, regulated) %>% 
  summarise(tot_cap = sum(nameplate_cap)) 


ggplot(wind_reg, aes(x=operating_quarter,y=tot_cap, fill=regulated)) +
  geom_bar(stat='identity',position='stack') +
  scale_x_yearqtr(format = "%YQ%q", 
                  breaks = seq(from = min(wind_reg$operating_quarter)+.75, 
                               to = max(wind_reg$operating_quarter), by = 1),
                  limits=c(min(wind_reg$operating_quarter), 
                           max=max(wind_reg$operating_quarter))) +
  theme_bw() +
  facet_wrap(~regulated,nrow=2) + 
  xlab("\nStart Operation Date") +
  ylab("Total Capacity (MW)\n") + 
  # geom_vline(xintercept=as.yearqtr("2016 Q4"), linetype='dotted') +
  theme(axis.text.x = element_text(angle=90),
        legend.position="bottom",
        legend.title=element_blank())

# look geographically, southern spots not as clumped in back half of year


wind_region <- wind %>%
  filter(!(sector_name %in% omitList),
         operating_year >= 2001,
         region!="Hawaii", 
         region!="Alaska")%>%
  group_by(operating_year, region) %>% 
  summarise(d_cap = sum(nameplate_cap)) %>% 
  arrange(operating_year,region)  

ggplot(wind_region, aes(x=operating_year,y=d_cap, fill=region)) +
  geom_bar(stat='identity',position='stack') +
  theme_bw() + 
  facet_wrap(~region, nrow = 2, scales="free") + 
  xlab("\nStart Operation Date") +
  ylab("Capacity Additions (MW)\n") + 
  ggtitle("Annual wind capacity additions (MW)") + 
  theme(axis.text.x = element_text(angle=90),
        legend.position="none")

ggsave(paste0(figDir, "wind_regional.png"),width=12,height=7)


solar_cumsum_region <- solar_raw %>% filter(!(sector_name %in% omitList),
                                            operating_year >= 2010,
                                            region != "Hawaii") %>%
  group_by(operating_quarter,region) %>% 
  summarise(d_cap = sum(nameplate_cap)) %>% 
  ungroup() %>%
  group_by(region) %>% 
  mutate(tot_cap = cumsum(d_cap),
         d_cap_p = d_cap/tot_cap) %>% 
  arrange(region, operating_quarter)  %>% View()

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

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

# Set up data -------------------------------------------------------------

gen_data = read.csv("~/Dropbox (Princeton)/Data for Tax Equity Project/eia_data/eia8602018/gen_2018.csv")
figDir = "~/Dropbox (Princeton)/Figures/GenFigs/"

# Fix numeric variables
gen_data$nameplate_cap <- as.numeric(gen_data$Nameplate.Capacity..MW.)

# Fix date variables
dates<- paste(gen_data$Operating.Year,gen_data$Operating.Month, rep(01, nrow(gen_data)), sep = "/")
gen_data$Operating.Date <- as.Date(dates)

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

gen_data$Operating.Quarter <- as.yearqtr(gen_data$Operating.Date)


# Redo the solar pictures -------------------------------------------------

solar <- filter(gen_data, Energy.Source.1=="SUN" & Operating.Year >=2006)
omitList = c("IPP CHP", "Industrial Non-CHP", "Industrial CHP")
solar_tot <- solar %>% filter(Operating.Year >2010, !(Sector.Name %in% omitList)) %>%
  group_by(Operating.Quarter, Sector.Name) %>%
  summarise(tot_cap = sum(nameplate_cap))

# total MW additions by year and sector
ggplot(solar_tot, aes(x=Operating.Quarter,y=tot_cap, fill = Sector.Name, color=Sector.Name)) +
  geom_bar(stat='identity',position='stack', alpha = 0.8) + 
  scale_x_yearqtr(format = "%YQ%q", 
                  breaks = seq(from = min(solar_tot$Operating.Quarter)+.75, 
                               to = max(solar_tot$Operating.Quarter), by = 1),
                  limits=c(min(solar_tot$Operating.Quarter), 
                           max=max(solar_tot$Operating.Quarter))) +
  theme_bw() + 
  xlab("\nStart Operation Date") +
  ylab("Total Capacity (MW)\n") + 
  geom_vline(xintercept=as.yearqtr("2016 Q4"), linetype='dotted') +
  theme(axis.text.x = element_text(angle=90),
        legend.position="bottom",
        legend.title=element_blank())

ggsave(paste0(figDir,"solar_additions.png"),width=10,height=7)

ggplot(solar %>% filter(Operating.Year >=2010), aes(x=Operating.Date,
                         fill=I('blue'),
                         col=I("black"),
                         alpha=0.8)) + 
  geom_histogram(bins=calc_num_quarters(solar$Operating.Date),
                 boundary = 0.5) +
  geom_vline(xintercept = as.Date("2016-11-17"), 
             linetype='dotted') + 
  scale_x_date("Operating Date",
               breaks = date_breaks("6 months"),
               labels = format_quarters) +
  theme(axis.text.x = element_text(angle=90))


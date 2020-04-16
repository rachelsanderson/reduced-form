library(ggplot2)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(purrr)
library(anytime)
library(scales)
library(lubridate)
library(zoo)

########## IMPORT CLEAN PROPOSED GEN LIST

proposed_gen <- read.csv(file = "~/Dropbox (Princeton)/Tax Equity Code/Clean Data/proposed_gen_master_list.csv")

########## Fix variable types
proposed_gen$nameplate_cap <- as.numeric(proposed_gen$nameplate_cap)
proposed_gen$curr_complete_date <-as.Date(anydate(paste0(proposed_gen$curr_scheduled_year, "-",
                                                  proposed_gen$curr_sceheduled_month)), format="%Y-%m-%d")

########## Add a 'period' variable to simplify the analysis
proposed_gen$period <- 1
proposed_gen$period[proposed_gen$year > 2008] <- 2
proposed_gen$period[proposed_gen$year > 2012] <- 3
proposed_gen$period <- factor(proposed_gen$period, labels=c('Pre-2009','2009-2012','2013-2018'))

########## Define status labels of generators in dataset
status_labels <- c("Cancelled", "Permits Pending",
                   "Other", "Planned, not started",
                   "Permitted", "Completed, not operating",
                   "Under construction", "Almost complete")

########## Need to figure out which are duplicates

## missing current schedule year only for < 2008, so drop them from dataset
proposed_gen %>% filter(is.na(proposed_gen$curr_scheduled_year)) %>% group_by(year) %>% tally()
proposed_gen %>% group_by(year) %>% tally()
proposed_gen<- proposed_gen %>% filter(year > 2008)


########## ID Renewables in the list
########## Note: There are different ways to classify "renewables"
##########       This first attempt uses the "energy source" variable 

renew_sources <- c("SUN","WND", "GEO", "WAT")

proposed_gen$pm <- factor(unlist(map(strsplit(as.character(proposed_gen$energy_source),'\''), 2,
                                     .default=NaN)))

proposed_gen <- proposed_gen %>% mutate(renew = (pm %in% renew_sources))


########## Look at overall number of proposed generators over time...
not_cancelled <- proposed_gen %>% filter(status != 'IP')

ggplot(not_cancelled, aes(x=status)) + 
  geom_bar() +
  facet_wrap(~year, scales='free_x')

## Increased number of not cancelled generators per year
ggplot(not_cancelled, aes(x=year)) + 
  geom_bar() +
  xlab('Year') + 
  ylab('') + 
  ggtitle('Number of proposed generators by year') +
  theme(plot.title=element_text(hjust=0.5))

ggsave('Dropbox (Princeton)/Figures/num_proposed_gen.png', width=9, height=6)

cap_by_yr <- not_cancelled %>% group_by(year, renew) %>% summarise(tot_cap = sum(nameplate_cap))

ggplot(cap_by_yr, aes(x=year, y=tot_cap, color=renew, fill=renew)) + 
  geom_line() +
  ggtitle("Proposed capacity additions")+
  ylab("Capacity (MW)\n")
ggsave('Dropbox (Princeton)/Figures/proposed_renew_cap.png', width=9, height=6)

ggplot(not_cancelled, aes(x=nameplate_cap, color=renew, fill=renew)) + 
  geom_histogram() +
  facet_wrap(~period, nrow= 3) + 
  xlab('Nameplate Capacity (MW)') + 
  ggtitle('Size of proposed capacity additions') + 
  theme(plot.title=element_text(hjust=0.5))
    # plot(legend.title = element_text("Renewable Source?"))

ggplot(not_cancelled, aes(x=nameplate_cap, color=renew, fill=renew)) + 
  geom_density(alpha=0.5) +
  facet_wrap(~period, nrow= 3) + 
  xlab('Nameplate Capacity (MW)') + 
  ggtitle('Size of proposed capacity additions') + 
  theme(plot.title=element_text(hjust=0.5),
        legend.position="bottom")
ggsave('Dropbox (Princeton)/Figures/proposed_renew_size.png')

num_util <- not_cancelled %>% group_by(year) %>% 
  summarise(nutil = length(unique(utility_name)))

## number of unique utility names
ggplot(num_util, aes(x=year, y=nutil)) + geom_line()


## NEED TO WEIGHT BY SIZE (look at capacity proposed by year)

## adding back in cancelled generators messes up this trend
ggplot(proposed_gen, aes(x=year)) + 
  geom_bar()

ggplot(proposed_gen, aes(x=status,fill=renew,color=renew)) +
  geom_bar() +
  facet_wrap(~period,nrow=3) + 
  scale_x_discrete(labels = status_labels) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) 

########## ID Renewables in the list
########## Note: There are different ways to classify "renewables"
##########       This second attempt uses the 'prime mover variable'

proposed_gen %>% count(prime_mover)

ggplot(proposed_gen, aes(x=status, fill=prime_mover, color=prime_mover)) + 
  geom_bar() +
  facet_wrap(~period, scales='free_x', nrow=3) 
  theme(legend.text = element_blank())

# drop observations with "" prime mover
# proposed_gen$pm <- factor(proposed_gen$prime_mover,
#                           levels=factor(proposed_gen$prime_mover),
#                           labels=pm_labels)


## Analyze scheduled construction dates by renewable vs not  
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
  
proposed_gen$scheduled_quarter <- as.yearqtr(proposed_gen$curr_complete_date)
  
ggplot(proposed_gen, aes(x=scheduled_quarter,col=renew,
                         fill=renew)) + 
    geom_bar(position='stack') +
    scale_x_continuous(name="\nScheduled Completion Date") +
    ylab("# Generators") +
  scale_fill_discrete(labels=c("Non-renewable","Renewable")) + 
  scale_color_discrete(labels=c("Non-renewable","Renewable")) + 
  theme(legend.position="bottom",
        legend.title= element_blank())

ggsave('Dropbox (Princeton)/Figures/competion_date.png')
  
  ggplot(proposed_gen, aes(x=curr_complete_date,
                           fill=I('blue'),
                           col=I("black"),
                           alpha=0.8)) + 
    geom_histogram(bins=calc_num_quarters(proposed_gen$curr_complete_date),
                   boundary = 0.5) +
    geom_vline(xintercept = as.Date("2016-11-17"), 
               linetype='dotted') + 
    scale_x_date("Year and quarter when things were counted",
                 # breaks = date_breaks("3 months"),
                 labels = format_quarters) +
    theme(axis.text.x = element_text(angle=90))
  
  hist(proposed_gen$curr_complete_date, breaks='quarters',
       # xlab = deparse(substitute(proposed_gen$curr_complete_date)),
       plot = TRUE, freq = FALSE,
       start.on.monday = TRUE, format)

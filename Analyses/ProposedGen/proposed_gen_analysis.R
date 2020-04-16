library(ggplot2)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(purrr)
library(anytime)
library(scales)
library(lubridate)
library(zoo)
library(ggpubr)

################################################
### Set local filenames here
################################################

proposed_gen_csv = "~/Dropbox (Princeton)/Tax Equity Code/Clean Data/proposed_gen_master_list_post08.csv"

########## IMPORT CLEAN PROPOSED GEN LIST

proposed_gen <- read.csv(file = proposed_gen_csv)

# Fix variable types
proposed_gen$nameplate_cap <- as.numeric(proposed_gen$nameplate_cap)
proposed_gen$curr_complete_date <-as.Date(anydate(paste0(proposed_gen$curr_scheduled_year, "-",
                                                  proposed_gen$curr_sceheduled_month)), format="%Y-%m-%d")

# Make unique ids for each generator, since they appear many times 

unique_id_vars = c("utility_name",
                   "plant_name",
                   "plant_state",
                   "generator_id",
                   "prime_mover",
                   "nameplate_cap")

proposed_gen <- proposed_gen %>% group_by(.dots=unique_id_vars) %>% 
  mutate(nObs = n(), 
         unique_id = group_indices()) %>% 
  arrange(desc(nObs),unique_id) 

# take only the first year that a proposed generator appears
unique_gen <- proposed_gen %>% group_by(unique_id) %>% filter(year == first(year))

# drop also those that are cancelled in first year observed
unique_gen <- unique_gen %>% filter(status != "IP") %>%
  mutate(time_to_complete = curr_scheduled_year - year)

# Let's make some summary statistics
ts1 <- unique_gen %>% group_by(primary_source,year) %>% 
  summarise(n_gen = n(),
            tot_cap = sum(nameplate_cap),
            avg_build_yrs = mean(time_to_complete),
            avg_cap = mean(nameplate_cap))

ggplot(ts1 %>% filter(primary_source %in% c('wind','solar')), 
       aes(y=avg_cap,x=year, color=primary_source)) +
  geom_line()
# remove weird primary sources
# steam has only 63 MW; "" has 1573; other has 4.7l, tires has 3.8k

weirdSources <- c("", "tires", "steam", "other")
unique_gen <- unique_gen %>% filter(!(primary_source %in% weirdSources)) %>%
    filter(curr_scheduled_year<=2025)

nGen <- ggplot(unique_gen, 
       aes(x=curr_scheduled_year,fill=primary_source, color=primary_source)) + 
  ggtitle("Proposed additions by primary source type") + 
  geom_bar(alpha=0.8) +
  theme(legend.position = "none",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5))

nCap <- ggplot(unique_gen, 
       aes(y = nameplate_cap, x=curr_scheduled_year,fill=primary_source, color=primary_source)) + 
  ggtitle("Proposed additions by primary source type") + 
  geom_bar(stat='identity') + 
  theme(legend.position = 'bottom', 
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5))

ggarrange(nGen, nCap,
          ncol = 1, nrow = 2) 

ggplot(unique_gen, 
       aes(y = time_to_complete, x=curr_scheduled_year,fill=primary_source, color=primary_source)) + 
  ggtitle("Proposed additions by primary source type") + 
  geom_bar(stat='identity') + 
  theme(legend.position = 'bottom', 
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5))

nCap

# more interesting is average capacity size


########## Define status labels of generators in dataset
status_labels <- c("Cancelled", "Permits Pending",
                   "Other", "Planned, not started",
                   "Permitted", "Completed, not operating",
                   "Under construction", "Almost complete")

## missing current schedule year only for < 2008, so drop them from dataset
proposed_gen %>% filter(is.na(proposed_gen$curr_scheduled_year)) %>% group_by(year) %>% tally()


########## ID Renewables in the list
########## Note: There are different ways to classify "renewables"
##########       This first attempt uses the "energy source" variable 


ggplot(not_cancelled, aes(x=status, color=pm,fill=pm)) + 
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

proposed_gen %>% count(primary_source)

# remove cancelled projects, weird primary sources
# steam has only 63 MW; "" has 1573; other has 4.7l, tires has 3.8k
weirdSources <- c("", "tires", "steam", "other")
proposed_gen %>% group_by(primary_source) %>% 
  summarise(sum = sum(nameplate_cap))

clean_df <- proposed_gen %>% 
  filter(status!='IP',
         !(primary_source %in% weirdSources))


ggplot(clean_df, 
       aes(x=year,fill=primary_source, color=primary_source)) + 
  ggtitle("Proposed additions by primary source type") + 
  geom_bar(alpha=0.8) +
  theme(legend.position = 'bottom', 
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5))



## NEED TO DO THE UNIQUE VARIABLE

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

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


# Housekeeping ------------------------------------------------------------

theme_set(
  theme_classic(base_size = 12)
)


# Set local filenames 
proposed_gen_csv = "~/Dropbox (Princeton)/Tax Equity Code/Clean Data/proposed_gen_master_list_post08.csv"
figDir = "~/Dropbox (Princeton)/Figures/ProposedGenFigs/"

# Import data

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

# remove weird energy sources

weirdSources <- c("", "tires", "steam", "other")
unique_gen <- unique_gen %>% filter(!(primary_source %in% weirdSources)) %>%
  filter(curr_scheduled_year<=2025)

# make separate df for wind_solar
wind_solar <- unique_gen %>% filter(primary_source %in% c("wind", "solar"))
wind_solar <- filter(wind_solar, region!="") # fix these, they belong to LA

# Function for annotating graphs ------------------------------------------

annotate_plot <- function(plt){
   plt <- plt + 
    # add the shaded parts
    
    # Loan grant eligible 
    geom_vline(xintercept=2011.5,linetype="dotted") +
    annotate("rect", xmin=2007.5, xmax=2011.5, ymin= -Inf, 
             ymax=Inf, alpha=0.1, 
             fill='blue') +
    # ITC round 1
    geom_vline(xintercept=2016.5,linetype="dotted") +
    annotate("rect", xmin=2011.5, xmax=2016.5, ymin= -Inf, 
             ymax=Inf, alpha=0.2, 
             fill='yellow') +
    # ITC round 2
    geom_vline(xintercept=2016.5,linetype="dotted") +
    annotate("rect", xmin=2016.5, xmax=Inf, ymin= -Inf, 
             ymax=Inf, alpha=0.2, 
             fill='red') +
    
    # add the text
    annotate('text', label='Loan grant + ITC', x=2009.5, y=Inf, 
             vjust = 1.5) +
    annotate('text', label='ITC Round 1', x=2014, y=Inf, 
             vjust = 1.5) +
    annotate('text', label='ITC Round 2', x=Inf, y=Inf, 
             vjust = 1.5, hjust=1.05) 
    return(plt)
}


# All projects by proposal/completion date  ------------------------------------------------------

ts1 <- unique_gen %>% group_by(primary_source,year) %>% 
  summarise(n_gen = n(),
            tot_cap = sum(nameplate_cap),
            avg_build_yrs = mean(time_to_complete),
            avg_cap = mean(nameplate_cap))

# total capacity by proposed year of EIA data 
plt1 <- ggplot(ts1,
       aes(x=year, y=tot_cap, fill=primary_source)) +
  geom_bar(stat='identity',position='stack') + 
  xlab("\nYear*") +
  ylab("Total Proposed Capacity (MW)\n") +
  ggtitle("Capacity of proposed generators by technology") +
  # labs(caption = "*Year indicates first year that generator appears in EIA data") +
  theme_bw() + 
  scale_x_continuous(breaks=seq(min(ts1$year),max(ts$year),1)) + 
  theme(legend.position="none",
        axis.text.x = element_text(angle=90),
        legend.title = element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.caption = element_text(hjust = 0, face = "italic")) 
annotate_plot(plt1)
ggsave(file = paste0(figDir,'proposed_gen_tot_cap.png'), width = 10, height=7)

# Number of projects by proposed year of EIA data 
annotate_plot(
  ggplot(ts1, aes(x=year, y=n_gen, fill=primary_source)) +
  geom_bar(stat='identity',position='stack') +
  xlab("\nYear*") +
  ylab("Number of proposed projects\n") +
  ggtitle("Number of proposed generators by technology") +
  labs(caption = "*Year indicates first year that generator appears in EIA data") +
  theme_bw() + 
  scale_x_continuous(breaks=seq(min(ts1$year),max(ts$year),1)) + 
  theme(legend.position="bottom",
        axis.text.x = element_text(angle=90),
        legend.title = element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.caption = element_text(hjust = 0, face = "italic")) 
)

ggsave(file = paste0(figDir,'proposed_gen_nGen.png'), width = 10, height=7)

# Same analysis but using scheduled year 
annotate_plot(
ggplot(unique_gen, 
       aes(x=curr_scheduled_year,fill=primary_source)) + 
  ggtitle("Number of proposed generators by technology") + 
  geom_bar(position='stack') +
  xlab("\nScheduled Completion Year*") +
  ylab("Number of projects\n") + 
  # labs(caption = "*Indicates initial scheduled completion date") +
  theme_bw() +
  scale_x_continuous(breaks=seq(min(unique_gen$curr_scheduled_year),max(unique_gen$curr_scheduled_year),1)) + 
  theme(legend.position="none",
        axis.text.x = element_text(angle=90),
        legend.title = element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.caption = element_text(hjust = 0, face = "italic")) 
)
ggsave(file=paste0(figDir,'complete_dates_nGen.png'), width = 10, height=7)

annotate_plot(
ggplot(unique_gen, 
       aes(x=curr_scheduled_year,y=nameplate_cap,fill=primary_source)) + 
  ggtitle("Total capacity of proposed generators by technology") + 
  geom_bar(stat='identity',position='stack') +
  xlab("\nScheduled Completion Year*") +
  ylab("Capacity (MW)\n") + 
  labs(caption = "*Indicates initial scheduled completion date") +
  theme_bw() +
  scale_x_continuous(breaks=seq(min(unique_gen$curr_scheduled_year),max(unique_gen$curr_scheduled_year),1)) + 
  theme(legend.position="bottom",
        axis.text.x = element_text(angle=90),
        legend.title = element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.caption = element_text(hjust = 0, face = "italic")) 
)
ggsave(file=paste0(figDir,'complete_dates_tot_cap.png'), width = 10, height=7)


# Wind and solar projects by date -----------------------------------------

annotate_plot(
ggplot(wind_solar, 
       aes(x=year,y=nameplate_cap,fill=primary_source)) + 
  ggtitle("Total proposed capacity by initial proposed year") + 
  geom_bar(stat='identity',position='stack') +
  xlab("\nYear*") +
  ylab("Capacity (MW)\n") + 
  scale_x_continuous(breaks=seq(2008,2018,1)) + 
  # labs(caption = "*Indicates initial scheduled completion date") +
  theme_bw() +
  theme(legend.position = c(0.1,0.8),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(angle=90)) 
)
ggsave(file=paste0(figDir,'year1_ws_tot_cap.png'),width = 10, height=7)

annotate_plot(
  ggplot(wind_solar, 
         aes(x=year,fill=primary_source)) + 
    ggtitle("Number of projects by initial proposed year") + 
    geom_bar(position='stack') +
    xlab("\nYear*") +
    ylab("Capacity (MW)\n") + 
    labs(caption = "*Indicates proposed year") +
    scale_x_continuous(breaks=seq(2008,2018,1)) + 
    theme_bw() +
    theme(legend.position = c(0.1,0.8),
          legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0, face = "italic"),
          axis.text.x = element_text(angle=90)) 
)
ggsave(file=paste0(figDir,'year1_ws_nUnits.png'),width = 10, height=7)

annotate_plot(
ggplot(wind_solar, 
       aes(x=curr_scheduled_year,y=nameplate_cap,fill=primary_source)) + 
  ggtitle("Total proposed capacity by completion year") + 
  geom_bar(stat='identity',position='stack') +
  xlab("\nScheduled Completion Year*") +
  ylab("Capacity (MW)\n") + 
  labs(caption = "*Indicates initial scheduled completion date") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic")) 
)
ggsave(file=paste0(figDir,'wind_solar_cap.png'), width = 10, height=7)

annotate_plot(
ggplot(wind_solar, 
       aes(x=curr_scheduled_year,fill=primary_source)) + 
  ggtitle("Number of proposed generators by technology") + 
  geom_bar(position='stack') +
  xlab("\nScheduled Completion Year*") +
  ylab("Number of projects\n") + 
  labs(caption = "*Indicates initial scheduled completion date") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic")) 
)
ggsave(file=paste0(figDir,'wind_solar_ngen.png'), width = 10, height=7)


ggplot(wind_solar %>% filter(primary_source=='solar'),aes(x=nameplate_cap,fill=period)) +
  geom_histogram() +
  ggtitle('Solar project size by proposal period') +
  facet_wrap(~period, nrow=3, scale='free_y') +
  theme_bw() + 
  xlab('Nameplate Capacity (MW)') +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5))
ggsave(file=paste0(figDir,'solar_hist.png'),width=7,height=10)

# Analyzing year-completion year  -----------------------------------------

ggplot(wind_solar, aes(x=curr_scheduled_year,fill=primary_source)) +
   geom_bar() + 
  facet_wrap(~period, scale='free', nrow=3) +
  scale_x_continuous(breaks=seq(2008,2025,1)) +
  xlab("\n Scheduled completion year") + 
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90),
        legend.title=element_blank())
ggsave(file=paste0(figDir,'year_to_year.png'), width=7, height=10)


# duplicate entry analysis ------------------------------------------------

ws <- proposed_gen %>% filter(primary_source %in% c('wind','solar'),
                              status != 'OT',
                              status != 'IP')
# do this for nonduplicate data
ggplot(ws, aes(x=alt_status, fill=primary_source)) + geom_bar() +
  facet_wrap(~year, scale='free_y') +
  theme(legend.position='bottom',
  axis.text.x = element_text(angle=90, hjust=1))

## look at percent that become 'under construction' when rules 
## change

# Where are the projects located? -----------------------------------------

make_summary_df <- function(df,group_var,source){
  #do year by year
  summ <- df %>% filter(primary_source == source) %>%
  group_by(.dots=group_var) %>%
    summarise(tot_cap = sum(nameplate_cap),
              nUnits = n(),
              avg_cap = mean(nameplate_cap)) %>% 
              # completion_time = mean(time_to_complete),
              # nUtils = n_distinct(utility_name)) %>%
    # percent step needs fixing ffor multiple group vars
    mutate(percent = round(tot_cap/sum(tot_cap),2)) %>% 
    arrange(desc(percent))
  return(summ)
}

make_table <- function(df, group, source){
  
  # name for saving latex output
  tabName <- paste(deparse(substitute(df)),group,source, sep = "_")
  
  # make table df
  t_df <- make_summary_df(df, group, source)
  colnames(t_df)[1] <- group
  
  # add a totals row
  total_row <- data.frame(group="Total",t(colSums(t_df[,-1])))
  colnames(total_row)[1] <- group
  total_row$avg_cap <- 0
  
  t_df <- t_df %>% bind_rows(total_row)
  
  # make the table with the xtable package
  alignVec <- rep('c',ncol(t_df)+1)
  alignVec[1] <- 'l' # left align first col
  out_tab <- xtable(t_df, booktabs=TRUE,auto=TRUE,align=alignVec,digits=2)
  caption(out_tab) <- paste("Location of proposed", source, "projects, 2008-2018")
  names(out_tab) <- c(group,
                      'Capacity (MW)',
                      '# Projects',
                      'Avg. Size (MW)',
                      '% Capacity')
  write(capture.output(print(out_tab,  
                             include.rownames = FALSE,
                             caption.placement = getOption("xtable.caption.placement", "top"))), 
        file=paste0(figDir,paste0(tabName,'.tex')))
  return(t_df)
}

region_tabs <- lapply(c('wind','solar'),FUN = function(x) make_table(wind_solar, 'region',x))
regulated_tabs <- lapply(c('wind','solar'),FUN = function(x) make_table(wind_solar,'regulated',x))


## Plot year by year differences in regulated vs. dergulated projects
## where year = initial completion date

# plot by regulated vs not for diff technologies 
make_plots <- function(df, technology){
  tt <- make_summary_df(df,c('regulated','curr_scheduled_year'),technology) %>%
    arrange(regulated, curr_scheduled_year) 
  
  year_sums <- tt %>% group_by(curr_scheduled_year) %>% 
    summarise(year_tot = sum(tot_cap),
              year_nUnits = sum(nUnits)) 
  
  tt <- tt %>% right_join(year_sums, by='curr_scheduled_year') %>%
    mutate(percent_units = nUnits/year_nUnits,
           percent_tot = tot_cap/year_tot)
  
  plot_projects(tt, technology)
  return(tt)
}

plot_projects <- function(df, technology){
  minYear = min(df$curr_scheduled_year)
  maxYear = max(df$curr_scheduled_year)
  
  #tot cap
  annotate_plot(
  ggplot(df, aes(x=curr_scheduled_year,y=tot_cap,color=regulated,fill=regulated)) +
    geom_bar(stat='identity', width=0.8, position = 'dodge') +
    ylab('Total Capacity (MW)\n') + 
    xlab('\n Scheduled completion year') + 
    scale_x_continuous(breaks=seq(minYear, maxYear, 1)) + 
    ggtitle(paste0('Total capacity of proposed projects ', technology))+
    theme_bw() + 
    theme(legend.position=c(0.8,0.8),
          axis.text.x = element_text(angle = 90),
          legend.title=element_blank())
  )
  ggsave(paste0(figDir, paste0('ann_tot_cap_',technology),'.png'),width=10,height=7)
  
  #num units
  annotate_plot(
  ggplot(df, aes(x=curr_scheduled_year,y=nUnits,color=regulated,fill=regulated)) +
    geom_bar(stat='identity', width=0.8, position = 'dodge') +
    ylab('Number of units\n') + 
    xlab('\n Scheduled completion year') + 
    scale_x_continuous(breaks=seq(minYear, maxYear, 1)) + 
    ggtitle(paste0('Total number of proposed projects ', technology))+
    theme_bw() + 
    theme(legend.position=c(0.8,0.8),
          axis.text.x = element_text(angle = 90),
          legend.title=element_blank())
  )
  ggsave(paste0(figDir, paste0('ann_nUnits_',technology),'.png'),width=10,height=7)
  
  #avg project size by year 
  annotate_plot(
  ggplot(df, aes(x=curr_scheduled_year,y=avg_cap,color=regulated,fill=regulated)) +
    geom_bar(stat='identity', width=0.8, position = 'dodge') +
    ylab('Average capacity (MW)\n') + 
    xlab('\n Scheduled completion year') + 
    scale_x_continuous(breaks=seq(minYear, maxYear, 1)) + 
    ggtitle(paste0('Average capacity of proposed projects ', technology))+
    theme_bw() +
    theme(legend.position='bottom',
          axis.text.x = element_text(angle = 90),
          legend.title=element_blank())
  )
  ggsave(paste0(figDir, paste0('ann_avg_cap_',technology),'.png'),width=10,height=7)
}



df <- make_plots(wind_solar,'solar')

make_plots(wind_solar,'wind')


## make in loop for wind and solar 

# Anythign below here idk -------------------------------------------------


## below here idk what it is1
t <- wind_solar %>% group_by(primary_source, plant_state) %>%
  summarise(n_gen = n(),
            tot_cap = sum(nameplate_cap)) 

t1<- t %>% filter(primary_source == 'solar') %>% ungroup() %>%
  mutate(state = plant_state) %>%
  select(state,tot_cap) %>%
  filter(state != "")

t2 <- t %>% filter(primary_source == 'solar') %>% ungroup() %>%
  mutate(state = plant_state) %>%
  select(state,n_gen) %>%
  filter(state != "")

plot_usmap(regions=c('states'), data=t1, values='tot_cap') 
plot_usmap(regions=c('states'), data=t2, values='n_gen') 
t3 <- t %>% filter(primary_source == 'wind') %>% ungroup() %>%
  mutate(state = plant_state) %>%
  select(state,tot_cap) %>%
  filter(state != "")

plot_usmap(regions=c('states'), data=t3, values='tot_cap') 
# add regions


# Where are projects located? 
## need year year plot

ggplot(unique_gen, 
       aes(y = time_to_complete, x=curr_scheduled_year,fill=primary_source, color=primary_source)) + 
  ggtitle("Proposed additions by primary source type") + 
  geom_bar(stat='identity') + 
  theme(legend.position = 'bottom', 
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5))

ggplot(unique_gen %>% filter(as.logical(renew)),
       aes(x=year, 
           color=primary_source,
           fill=primary_source)) + 
  geom_histogram()

ggplot(unique_gen %>% filter(as.logical(renew)),
       aes(x=curr_scheduled_year, y=nameplate_cap,
           color=primary_source,
           fill=primary_source)) + 
  geom_bar(stat='identity')

ggplot(unique_gen %>% filter(as.logical(renew)),
       aes(x= nameplate_cap,fill=primary_source)) + 
  geom_histogram()

nCap

ts <- unique_gen %>% filter(as.logical(renew) & primary_source != "bio") %>%
group_by(year, primary_source) %>%
  summarise(avg_cap = mean(nameplate_cap)) 
ggplot(ts, aes(x=year,y=avg_cap, 
               color=primary_source,
               fill=primary_source)) +
  geom_bar(stat='identity') +
  facet_wrap(~primary_source)

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

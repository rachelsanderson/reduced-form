library(ggplot2)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(ggpubr)

##############################################
## Import Annual Capacity Data by State
##############################################

## Read in annual state-level capacity data

raw_cap_data <- read.csv(file = "~/Dropbox (Princeton)/Data for Tax Equity Project/eia_capacity_data/existcapacity_annual.csv",
                         stringsAsFactors = FALSE, header=TRUE)
colnames(raw_cap_data) <- raw_cap_data[1,]
raw_cap_data <- raw_cap_data[-1,]

## Fix some of the variables 

raw_cap_data$year <- as.numeric(raw_cap_data$Year)
raw_cap_data$total_capacity <- as.numeric(gsub(",", "", raw_cap_data$`Nameplate Capacity (Megawatts)`))
raw_cap_data$summer_capacity <- as.numeric(gsub(",", "", raw_cap_data$`Summer Capacity (Megawatts)`))
raw_cap_data$facilities <- as.numeric(raw_cap_data$Facilities)
raw_cap_data$facilities[is.na(raw_cap_data$facilities)] <- 0
raw_cap_data$state <- raw_cap_data$`State Code`

total_raw_cap <- raw_cap_data %>% filter(`Producer Type` == "Total Electric Power Industry")

##############################################
## Plot total renewable capacity (stock) by year
##############################################

## Ignoring nuclear for now
renew_list <- c("Hydroelectric", "Other Biomass", "Wind", "Solar Thermal and Photovoltaic",
                "Geothermal", "Pumped Storage", "Wood and Wood Derived Fuels")

total_raw_cap <- total_raw_cap %>% mutate(renew = ifelse(`Fuel Source` %in% renew_list,1,0))
 
denom <- total_raw_cap %>% filter(`Fuel Source`=="All Sources") %>% group_by(year) %>% 
  summarise(all_sources_tot_cap = sum(total_capacity))

tot_renews <- total_raw_cap %>% filter(renew==1) %>% group_by(`Fuel Source`, year) %>%
  summarise(tot_cap = sum(total_capacity)) 

pRenew <- tot_renews %>% group_by(year) %>% 
  summarise(renew_tot_cap = sum(tot_cap)) %>%
  right_join(denom, by='year') %>%
  mutate(p_renew = (renew_tot_cap/all_sources_tot_cap) * 100)
  
ggplot(tot_renews, aes(x = year, y = tot_cap, 
                       group =`Fuel Source`,
                       color=`Fuel Source`, 
                       fill = `Fuel Source`)) +
  geom_area(position='stack',alpha=0.5) + 
  ggtitle("U.S. Renewable Energy Capacity (MW), 1990-2018") +
  scale_y_continuous('Total Cacity (MW)') +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.justification=c(0,1), 
        legend.position=c(0.05, 0.95),
        legend.background = element_blank(),
        legend.key = element_blank())

ggsave(file='~/Dropbox (Princeton)/Figures/total_renewable_capacity.png',width=10,height=7)

##############################################
## Plot percent renewable capacity by year
##############################################

tot_renews <- tot_renews %>% right_join(pRenew, by=c('year'))

ggplot(tot_renews, aes(x = year, y = tot_cap, 
                       group =`Fuel Source`,
                       color=`Fuel Source`, 
                       fill = `Fuel Source`)) +
  geom_area(position='stack',alpha=0.5) + 
  geom_line(aes(y=p_renew*10e3),color="black") +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Total Nameplate Capacity (MW)",
    
    # Add a second axis for % capacity
    sec.axis = sec_axis(~.*10e-7, name="% renewable capacity")
  )

ggsave(file='~/Dropbox (Princeton)/Figures/tot_renew_cap.png',width=10,height=7)

###########################################################################
## Compare percent of added capacity that is renewable by year vs all sources
## This looks really good so I'm going to do by technology
##########################################################################

pRenew <- pRenew %>% mutate(diff_year = year - lag(year),  # Difference in time (just in case there are gaps)
         diff_tot = all_sources_tot_cap - lag(all_sources_tot_cap), # Difference in route between years
         diff_renew = renew_tot_cap - lag(renew_tot_cap),
         p_tot_change = (diff_tot / diff_year)/all_sources_tot_cap * 100,
         p_renew_change = (diff_renew / diff_year)/renew_tot_cap * 100)


growth_rates <- pRenew %>% select(year, p_tot_change, p_renew_change) %>% 
  filter(!is.na(p_tot_change)) %>% 
  gather(key="series", value = "growth", c(p_tot_change, p_renew_change))

raw_changes <- pRenew %>% select(year, diff_tot, diff_renew) %>% 
  filter(!is.na(diff_tot)) %>% 
  gather(key="series", value = "raw_change", c(diff_tot, diff_renew))

pChange <- ggplot(growth_rates, aes(x=year, y = growth, color=series)) + geom_line() +
  theme(legend.position = "none") + xlab("Year") + ylab("% Capacity Growth") +
  geom_vline(xintercept=2012,linetype="dotted") +
  geom_vline(xintercept=2009,linetype="dotted") +
  geom_vline(xintercept=2006,linetype="dotted") +
  geom_vline(xintercept=2016,linetype="dotted") 

rawChange <- ggplot(raw_changes, aes(x=year, y = raw_change, color=series)) + 
  geom_line() +
  scale_color_discrete(labels=c("Renewables","All Sources")) + 
  xlab("Year") + ylab("Annual Capacity Change (MW)")+ 
  geom_vline(xintercept=2012,linetype="dotted") +
  geom_vline(xintercept=2006,linetype="dotted") +
  geom_vline(xintercept=2009,linetype="dotted") +
  geom_vline(xintercept=2016,linetype="dotted") +
  theme(legend.position = "bottom",
        legend.title = element_blank())

ggarrange(pChange, rawChange,
         labels = c("Growth Rates", "Raw Changes"),
         ncol = 1, nrow = 2) 

ggsave(file='~/Dropbox (Princeton)/Figures/relative_growth_renew_tot.png',width=10,height=7)

### Growth rate by technology

omit_list <- c("", "All Sources", "Other", "Other Gases")

growth_rates <- total_raw_cap %>% group_by(year, `Fuel Source`) %>%
  summarise(tot_cap = sum(total_capacity))  %>% 
  group_by(`Fuel Source`) %>% arrange(`Fuel Source`) %>% 
  mutate(diff_year = year -lag(year),
         diff_cap = tot_cap - lag(tot_cap),
         growth_rate = (diff_cap / diff_year)/tot_cap * 100) %>%
  filter(year >= 2004) %>% 
  filter(!(`Fuel Source` %in% omit_list))

growth_rates$renew <- ifelse(growth_rates$`Fuel Source` %in% renew_list, 1, 0)
growth_rates$renew <- factor(growth_rates$renew, levels = c(0,1), labels= c("Non-renewable fuel sources", "Renewable fuel sources"))

ggplot(growth_rates, aes(x=year,y=growth_rate,color=`Fuel Source`)) + 
  geom_line() +
  facet_wrap(~renew, nrow=2,scale="free_y") +
  geom_vline(xintercept=2012,linetype="dotted") +
  geom_vline(xintercept=2006,linetype="dotted") +
  geom_vline(xintercept=2009,linetype="dotted") +
  geom_vline(xintercept=2016,linetype="dotted") +
  ylab("% Annual Capacity Growth")+
  theme(legend.position="bottom",
        legend.title=element_blank()) 

ggsave(filename = '~/Dropbox (Princeton)/Figures/growth_rates.png',width=10,height=7)


ggplot(growth_rates, aes(x=year,y=diff_cap,color=`Fuel Source`)) + 
  geom_line() + 
  facet_wrap(~renew, nrow=2,scale="free_y") +
  geom_vline(xintercept=2012,linetype="dotted") +
  geom_vline(xintercept=2006,linetype="dotted") +
  geom_vline(xintercept=2009,linetype="dotted") +
  geom_vline(xintercept=2016,linetype="dotted") +
  ylab("Annual Capacity Additions (MW)")+
  theme(legend.position="bottom",
        legend.title=element_blank()) 

ggsave(filename = '~/Dropbox (Princeton)/Figures/yearly_changes.png',width=10,height=7)


############################################################################################
## Plot total renewable Capacity by year by technology after 2005 (ITC starts in 2006)
########################################################################################## 

after_2005 <- tot_renews %>% filter(year >= 2004)

ggplot(after_2005, aes(x = year, y = tot_cap, color=`Fuel Source`)) +
  geom_line() +
  facet_wrap(~`Fuel Source`,scale="free") + 
  annotate("rect", xmin=2006, xmax=2016, ymin= -Inf, 
           ymax=Inf, alpha=0.1, 
           fill='blue') +
  annotate("rect", xmin=2009, xmax=2012, ymin= -Inf, ymax=Inf, alpha=0.2, fill='blue') +
  guides(color=FALSE) + 
 ggtitle("U.S. Renewable Energy Capacity (MW), 2005-2018")  + 
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(file='~/Dropbox (Princeton)/Figures/after_2005_stock.png',width=10,height=7)


## Look at growth by year (discrete capacity additions)
after_2005 <- after_2005 %>% group_by(`Fuel Source`) %>% arrange(year) %>%
  mutate(Diff_year = year - lag(year),  # Difference in time (just in case there are gaps)
                  Diff_growth = tot_cap - lag(tot_cap), # Difference in route between years
                  Rate_percent = (Diff_growth / Diff_year)/tot_cap * 100) # g)

ggplot(after_2005, aes(x = year, y = Diff_growth, 
                       color =`Fuel Source`)) +
  geom_line() +  
  annotate("rect", xmin=2006, xmax=2016, ymin= -Inf, 
           ymax=Inf, alpha=0.1, 
           fill='blue') +
  annotate("rect", xmin=2009, xmax=2012, ymin= -Inf, 
           ymax=Inf, alpha=0.2, fill='blue') +
  guides(color=FALSE) + 
  facet_wrap(~`Fuel Source`,scale="free") + 
  ggtitle("Annual Capacity Additions (MW), 2005-2018")  + 
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(file='~/Dropbox (Princeton)/Figures/after_2005_additions.png',width=10,height=7)



############################################################################################
## Do same stuff by region -- make above into a function that I can feed in region
########################################################################################## 

tot_renews_by_state <- raw_renewables_cap %>% group_by(state, year) %>%
  summarise(tot_state_cap = sum(total_capacity))%>% filter(year>= 2005)

ggplot(tot_renews_by_state, aes(x=year, y=tot_state_cap,color=state)) + geom_line()

northwest = c("WA", "OR", "ID",'MT','WY','NV','UT')
caiso = c("CA")
southwest = c('AZ','NM','CO')
ercot = c("TX")
spp = c('OK', 'NE', 'SD','KS')
miso = c('ND','MN','IA','IL','MS','WI','LA','IN', 'MI', 'MO','AR')
southeast = c('FL','GA','AL','SC','NC','TN')
pjm = c('OH','KY','VA','WV','DE','PA','MD','NJ')
nyiso = c('NY')
iso_ne = c('VT','NH','MA','CT','RI','ME')
regionList <- list(northwest, caiso, southwest, ercot, spp,
                   miso, southeast,pjm,nyiso,iso_ne)

regions <-  data.frame(state=unique(raw_cap_data$state))
regions$region[is.element(regions$state,northwest)] <- "northwest"
regions$region[is.element(regions$state,caiso)] <- "caiso"
regions$region[is.element(regions$state,ercot)] <- "ercot"
regions$region[is.element(regions$state,spp)] <- "spp"
regions$region[is.element(regions$state,southwest)] <- "southwest"
regions$region[is.element(regions$state,miso)] <- "miso"
regions$region[is.element(regions$state,southeast)] <- "southeast"
regions$region[is.element(regions$state,pjm)] <- "pjm"
regions$region[is.element(regions$state,nyiso)] <- "nyiso"
regions$region[is.element(regions$state,iso_ne)] <- "iso_ne"
regions$region[is.element(regions$state,c('HI','AK'))] <- 'hawaii + alaska'
renew_cap_region <- tot_renews_by_state %>% full_join(regions, by='state') 

renew_cap_region <- na.omit(renew_cap_region) %>% group_by(region,year) %>%
  summarize(tot_region_cap=sum(tot_state_cap))

gg <- ggplot(renew_cap_region, 
       aes(x=year, y=tot_region_cap,fill=region,color=region)) + 
  geom_area(position='stack', alpha=0.7)

ggsave(gg,file='~/Dropbox (Princeton)/Figures/tot_renew_by_region.png',width=10,height=7)

### do it by technology
temp <- na.omit(raw_renewables_cap %>% full_join(regions, by='state')) %>%filter(year >=2005)
temp <- temp %>% group_by(region, year,`Fuel Source`) %>% summarise(tot_cap = sum(total_capacity))
ggplot(temp, aes(x=year, y=tot_cap,fill=region,color=region)) +
  geom_area(position='stack',alpha=0.7) +
  facet_wrap(~`Fuel Source`,scale='free') 
ggsave(file='~/Dropbox (Princeton)/Figures/tot_renew_by_region_tech.png',width=10,height=7)


##############################################
## Make regulated vs not regulated variable
##############################################

retailChoice <- c("CA","CT","DC","DE",
                  "IL","MA","MD","ME","MI",
                  "MT","NH","NJ","NY","OH",
                  "PA","RI","TX")

tot_renews_by_state$retailChoice <- ifelse((tot_renews_by_state$state %in% retailChoice), 1, 0)
by_reg <- tot_renews_by_state %>% group_by(retailChoice, year) %>% summarise(tot_renew_cap = sum(tot_state_cap))
ggplot(by_reg, aes(x=year, y=tot_renew_cap,fill=retailChoice,color=retailChoice)) + 
  geom_point()

#### need to standardize somehow



## TODO: Look at placed in service vs in construction requirements (i.e. use proposed generation data)


## TODO: Look at capacity additions by state, region, regulated vs not



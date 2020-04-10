library(ggplot2)
library(tidyverse)
library(dplyr)
library(gridExtra)

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
## Plot total renewable Capacity by year
##############################################

## Ignoring nuclear for now
renew_list <- c("Hydroelectric", "Other Biomass", "Wind", "Solar Thermal and Photovoltaic",
                "Geothermal", "Pumped Storage", "Wood and Wood Derived Fuels")

raw_renewables_cap <- total_raw_cap %>% filter(`Fuel Source` %in% renew_list) 
 
tot_renews <- raw_renewables_cap %>% group_by(`Fuel Source`, year) %>%
  summarise(tot_cap = sum(total_capacity)) 

ggplot(tot_renews, aes(x = year, y = tot_cap, 
                       group =`Fuel Source`,
                       color=`Fuel Source`, 
                       fill = `Fuel Source`)) +
  geom_area(position='stack',alpha=0.5) + 
  ggtitle("U.S. Renewable Energy Capacity (MW), 1990-2018") +
  scale_x_discrete('Year', seq(from=min(tot_renews$year),
                               to=max(tot_renews$year), 
                               by=5)) +
  scale_y_continuous('Total Cacity (MW)') +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.justification=c(0,1), 
        legend.position=c(0.05, 0.95),
        legend.background = element_blank(),
        legend.key = element_blank())

ggsave(file='~/Dropbox (Princeton)/Figures/total_renewable_capacity.png',width=10,height=7)

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


## TODO: Look at placed in service vs in construction requirements (i.e. use proposed generation data)


## TODO: Look at capacity additions by state, region, regulated vs not

## # # # #  state is too hard so do regions
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
temp <- na.omit(raw_renewables_cap %>% full_join(regions, by='state'))
temp <- temp %>% group_by(region, year,`Fuel Source`) %>% summarise(tot_cap = sum(total_capacity))
ggplot(temp, aes(x=year, y=tot_cap,fill=region,color=region)) +
  geom_area(position='stack',alpha=0.7) +
  facet_wrap(~`Fuel Source`,scale='free') 
ggsave(file='~/Dropbox (Princeton)/Figures/tot_renew_by_region_tech.png',width=10,height=7)

##############################################
## Identify Solar Capacity
##############################################
solar_cap_all_states <- raw_renewables_cap %>% filter(`Fuel Source` == "Solar Thermal and Photovoltaic") %>%
                        group_by(year) %>% summarise(tot_renew_cap = sum(total_capacity)) 
solar_cap_all_states$diff <- solar_cap_all_states$tot_renew_cap - lag(solar_cap_all_states$tot_renew_cap)

ggplot(solar_cap_all_states, aes(x=year)) + 
  geom_bar(stat="identity", aes(y = diff)) +
  geom_line(aes(y = tot_renew_cap))
  


ann_renew_cap_all_states <- raw_renewables_cap %>% group_by(year) %>% summarise(tot_renew_cap = sum(total_capacity)) 
ann_cap_all_states <- total_raw_cap %>% group_by(year) %>% summarise(tot_renew_cap = sum(total_capacity)) 

#############################################
## Truncate to 2004
##############################################

solar_trunc <- solar_cap_all_states %>% filter(year >= 2004)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(solar_trunc, aes(x=year)) + 
  geom_bar(stat="identity", aes(y = diff)) +
  geom_line(aes(y = tot_renew_cap)) +
  labs(x="Year", y ="Installed Capacity (MW)", title="U.S. Solar Capacity (2004-2018)") + 
  theme(plot.title=element_text(hjust=0.5)) +
  scale_x_continuous("Year", breaks = x_axis_breaks) +
  scale_fill_manual(values=cbbPalette) + 
  scale_colour_manual(values=cbbPalette)

View(solar_trunc)



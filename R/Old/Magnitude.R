# Magnitude
# Is congestion an issue?
# How often is the network congested?
# What states?
# What gens were most congested?
setwd("C:/Users/Matthew/Dropbox/Mispricing/Analysis")
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(padr)

### Get DATA

mpa <- fread("D:/Thesis/Data/mpa_final.csv") %>% 
    mutate(settlementdate = ymd_hms(settlementdate))

#add all sumarised cols of interest
#input: grouped df
#output: agregate summares
summ_all <- function(df){
    df %>% 
        summarise(quantity = sum(dispatchmwh),
                  ave_rev = ifelse(sum(dispatchmwh)>0,
                                   sum(rev_rrp_30)/sum(dispatchmwh),
                                   NA),
                  ave_rev_lmp = ifelse(sum(dispatchmwh)>0,
                                       sum(rev_lmp)/sum(dispatchmwh),
                                       NA),
                  ave_rev_lmp0 = ifelse(sum(dispatchmwh)>0,
                                        sum(rev_lmp0)/sum(dispatchmwh),
                                        NA),
                  total_rev_rrp = sum(rev_rrp_30),
                  total_rev_lmp = sum(rev_lmp),
                  total_rev_lmp0 = sum(rev_lmp0),
                  dif_ave =  ave_rev - ave_rev_lmp,
                  dif_ave_0 = ave_rev - ave_rev_lmp0,
                  dif_total = total_rev_rrp - total_rev_lmp,
                  dif_total_0 = total_rev_rrp - total_rev_lmp0)
}

summ_all_2 <- function(df){
    df %>% 
        summarise(quantity = ifelse(sum(dispatchmwh)>0,
                                    sum(dispatchmwh),
                                    NA),
                  
                  total_mispricing = sum(abs(rev_rrp_30 - rev_lmp)),
                  adj_total_mispricing = sum(abs(rev_rrp_30 - rev_lmp0)),
                  total_overcomp = sum(rev_rrp_30 - rev_lmp),
                  adj_total_overcomp = sum(rev_rrp_30 - rev_lmp0),
                  
                  ave_mispricing = total_mispricing / quantity,
                  adj_ave_mispricing = adj_total_mispricing / quantity,
                  ave_overcomp = total_overcomp / quantity,
                  adj_ave_overcomp = adj_total_overcomp / quantity
        )
}

### MAGNITUDE

# Congestion
congested <- mpa %>% select(settlementdate) %>% filter(year(settlementdate)<2019, year(settlementdate)>2009) %>%#full years 
    unique() %>% mutate(constrained = 1) %>% 
    pad(interval = "5 min", break_above = 2000000, start_val = ymd_hms("2010-01-01 00:05:00"), 
        end_val = ymd_hms("2019-01-01 00:00:00")) %>% replace(is.na(.), 0)#add 0 if not congested


#what proportion of the time is the network congested (by year) and filtered by daylight hour
congested %>% group_by(year = floor_date(settlementdate, "year")) %>% 
    summarise(perc = sum(constrained)/n(), hours = "all") %>% .[1:9,] %>% #all times
    rbind(congested %>% filter(settlementdate %>% 
                                        substr(12,13) %>% #extract hour
                                        as.numeric() %in% c(7:10,16:19)) %>% 
                   group_by(year = floor_date(settlementdate, "year")) %>% 
                   summarise(perc = sum(constrained)/n(),
                             hours = "7am - 10pm")) %>%#between 7am and 9:55pm
    ggplot(aes(x = year, y = perc, colour = hours))+
    geom_line(size = 2) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
    labs(title = "Percentage of Time NEM is Congested", y = "Percent", x = "Year") +
    theme(text = element_text(size = 12))

#what proportion of time is the network congested (by year)
congested %>% group_by(year = floor_date(settlementdate, "year")) %>% 
    summarise(perc = sum(constrained)/n(), hours = "all") %>% .[1:9,] %>% #all times 
    ggplot(aes(x = year, y = perc))+
    geom_line(size = 2) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
    labs(y = "Percent", x = "Year") +
    theme(text = element_text(size = 12))+
    ggsave("output/Charts/Perc_Congested_NEM.png", width = 6)



    
    
    congested %>% filter(settlementdate %>% 
                             substr(12,13) %>% #extract hour
                             as.numeric() %in% c(7:21)) %>% 
    group_by(year = floor_date(settlementdate, "year")) %>% 
    summarise(perc = sum(constrained)/n())
    

#graph by month, grouped by state
congested_state <- mpa %>% select(settlementdate, state) %>% filter(year(settlementdate)<2019, year(settlementdate)>2009) %>% 
    unique() %>% mutate(constrained = 1) %>% 
    pad(interval = "5 min", start_val = ymd_hms("2010-01-01 00:05:00"), 
        end_val = ymd_hms("2019-01-01 00:00:00"), break_above = 10000000, group = "state") %>% 
    replace(is.na(.), 0)#add 0 if not congested

congested_state %>% group_by(year = floor_date(settlementdate, "year"), state) %>% 
    summarise(perc = sum(constrained)/n()) %>% .[1:45,] %>%  
    ggplot(aes(x = year, y = perc, colour = state, group = state))+
    geom_line(size = 2)+
    facet_wrap(~state) +
    labs(y = "Percent", x = "Year")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.5)) +
    theme(legend.position = "none") +
    ggsave("Output/Charts/Congested_state.png", width = 10)

#most congested generators
congested_duid <-  mpa %>% filter(year(settlementdate) == 2018, (lmp < rrp30)) %>% 
    select(settlementdate, station, fuel_type) %>%
    unique() %>% 
    group_by(station) %>% summarise(count = n(), fuel_type = fuel_type[1]) %>% 
    arrange(-count) %>% mutate(perc = count/(12*24*365))
congested_duid %>% .[1:10,] %>% fwrite("Output/congestion by duid top 10 - assump no output included.csv")

congested_duid_2 %>% .[1:10,] %>% fwrite("Output/congestion by duid top 10.csv")#this is flawed use c_duid above instead as would need each period dispatchmwh to make statetment


#congestion by generator type
#at any point in time, what is the percentage of generators who are constrained off by fuel type
congested_fuel <- mpa %>%  filter(settlementdate < ymd_hms("2019-01-01 00:05:00 UTC"), 
                                  settlementdate > ymd_hms("2010-01-01 00:00:00 UTC")) %>% 
    filter(lmp < rrp, dispatchmwh > 0) %>% #constrained off and producing
    select(settlementdate, fuel_type) %>% 
    unique() %>% mutate(constrained = 1) %>% 
    pad(interval = "5 min", start_val = ymd_hms("2010-01-01 00:05:00"), 
        end_val = ymd_hms("2019-01-01 00:00:00"), break_above = 10000000, group = "fuel_type") %>% 
    replace(is.na(.), 0) %>% #add 0 if not congested
    group_by(year = floor_date(settlementdate, "year"), fuel_type) %>% 
    summarise(perc = sum(constrained)/n()) 


congested_fuel %>% filter(year(year) < 2019) %>% filter(!(fuel_type %in% c("Battery", "Liquid Fuel"))) %>% 
    ggplot(aes(x = year, y = perc, colour = fuel_type, group = fuel_type))+
    geom_line(size = 1.5)+
    facet_wrap(~fuel_type) +
    labs(y = "Percent", x = "Year")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.5)) +
    theme(legend.position = "none") +
    ggsave("Output/Charts/Congested_fuel.png", width = 10)

#distribution of congestion
congested_duid_2 <-  mpa %>% filter(year(settlementdate) == 2018, dispatchmwh > 0) %>% #removes all duids not dispatched
    select(settlementdate, station, fuel_type) %>%
    unique() %>% 
    group_by(station) %>% summarise(count = n(), fuel_type = fuel_type[1]) %>% 
    arrange(-count) %>% mutate(perc = count/(12*24*365))

uncongested_duid <- mpa %>% filter(year(settlementdate) == 2018, dispatchmwh > 0) %>% #remove old/new gens & non productive
    select(station, fuel_type) %>% unique() %>% #get unique data for station
    filter(!(station %in% congested_duid_2$station)) %>% #remove all congested 
    mutate(perc = 0) #empty! :O t/f don;t need to merge

mpa %>% filter(station == "Bodangora Wind Farm")

congested_duid_2 %>% 
    ggplot(aes(perc*100, fill = fuel_type))+
    geom_histogram() +
    labs(title = "Percentage of Time Each Generator is Mispriced (2018)", 
         y = "# Generators", x = "Percent", fill = "Fuel Type") +
    ggsave("Output/Charts/Distribution of Overcomp 2018.png", width = 10)


congested_duid_2 %>% .[1:10,] %>% select(-count) %>% fwrite("Output/congestion by duid top 10.csv")#this is flawed use c_duid above instead as would need each period dispatchmwh to make statetment
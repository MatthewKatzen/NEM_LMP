#All graphs for paper

# Load packages
library(scales)
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
Sys.setenv(TZ='UTC')

#### section 4.1
#########################

generator_details <- fread("D:/NEM_LMP/Data/generator_details_cleaned")
LPA <- fread("D:/NEM_LMP/Data/Raw/dispatch_local_price_24-01-2020.csv") %>% clean_names() %>% 
    mutate(settlementdate = ymd_hms(settlementdate)) %>% 
    filter(year(settlementdate) %in% c(2013:2019)) %>% 
    select(-locally_constrained) %>% left_join(generator_details, by = "duid")

#% time nem congested
LPA %>% select(settlementdate) %>% unique() %>% 
    group_by(year = year(settlementdate)) %>% 
    summarise(prop = n()/(12*24*365)) %>% 
    ggplot(aes(x = year, y = prop))+
    geom_line(size = 2) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
    labs(y = "Percent", x = "Year") +
    ggsave("D:/NEM_LMP/Output/Final/Perc_Congested_NEM.png", width = 10)

#% time state congested

files <- paste0("D:/NEM_LMP/Data/Cleaned/MC/", list.files("D:/NEM_LMP/Data/Cleaned/MC/"))[73:83]

dipatch_2019 <- files %>% map(~fread(.x) %>% 
                              select(settlementdate, duid, fuel_type, region, dispatchmwh)) %>% 
    rbindlist() #grouped dispatch totals

fwrite(dispatch_2019, "D:/NEM_LMP/Data/Cleaned/Misc/dipsatch_2019.csv")
dispatch_2019 <- fread("D:/NEM_LMP/Data/Cleaned/Misc/dipsatch_2019.csv") %>% mutate(settlementdate = ymd_hms(settlementdate))

LPA  %>% 
    select(settlementdate, region) %>% 
    unique() %>% 
    group_by(year = year(settlementdate),region) %>% 
    summarise(prop = n()/(12*24*365)) %>% 
    ggplot(aes(x = year, y = prop, colour = region))+
    facet_wrap(~region) +
    geom_line(size = 2) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.5)) +
    labs(y = "Percent", x = "Year") +
    theme(text = element_text(size = 12)) +
    ggsave("D:/NEM_LMP/Output/Final/Congested_State.png", width = 10)

#prop of year are duids lpa!=0 and mwh>0

LPA %>% inner_join(dispatch_2019 %>% filter(year(settlementdate)==2019), 
                   by = c("duid", "settlementdate")) %>% 
    group_by(duid, fuel_type.x) %>% 
    summarise(prop = n()/(12*24*365)) %>% 
    ggplot(aes(x = prop, fill = fuel_type.x))+
    geom_histogram() +
    scale_y_continuous(limits = c(0,75)) +
    scale_x_continuous(limits = c(-.01,0.35)) +
    labs(title = "Generator-level frequency of congestion in 2019") +
    ggsave("D:/NEM_LMP/Output/Final/Duid_Constrained_Frequency_2019.png", width = 8)

LPA %>% inner_join(dispatch_2019 %>% filter(year(settlementdate)==2019), 
                   by = c("duid", "settlementdate")) %>% 
    group_by(duid, fuel_type.x) %>% 
    summarise(prop = n()/(12*24*365)) %>% 
    arrange(-prop) %>% .[1:10,] #top 7/10 are wind



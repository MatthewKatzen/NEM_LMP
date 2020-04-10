#South Australian Trends

# Load packages
library(scales)
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
library(padr)
Sys.setenv(TZ='UTC')

full_vom <- fread("D:/NEM_LMP/Output/MC/full_mc_vom.csv")
generator_details <- fread("D:/NEM_LMP/Data/Raw/generator_details_cleaned.csv")

#output of SA semi-scheduled wind gen
full_vom %>% filter(region == "SA", fuel_type %in% c("Wind")) %>% 
    group_by(year) %>% summarise(Total=sum(Q)/1000, Constrained=sum(QC)/1000, Unconstrained = Total - Constrained) %>% 
    gather(var,value,-year) %>% 
    mutate(var = factor(var, levels = c("Total", "Unconstrained", "Constrained"))) %>% #reorder legend
    ggplot(aes(x = year, y = value, colour= var)) +
    geom_line(size = 2) +
    labs(x = "Year", y = "GWh") +
    theme(legend.title=element_blank())+
    ggsave("D:/NEM_LMP/Output/Final/SA_wind_semi.png", width = 10)


#wind scheduled only capacity SA
full_vom %>% group_by(duid, region, fuel_type) %>% 
    summarise(start = min(year)) %>% filter(region == "SA", fuel_type %in% c("Wind")) %>% 
    left_join(fread("D:/NEM_LMP/Data/Raw/generator_details.csv") %>% clean_names(), by = c("duid")) %>% 
    select(duid, start, region.x, fuel_type.x, capacity) %>% 
    group_by(start) %>% 
    summarise(capacity = sum(capacity)) %>% 
    add_row(start = 2015, capacity = 0) %>% #add 2015 entry of 0 capacity
    arrange(start) %>% 
    ggplot(aes(x = start, y = cumsum(capacity)))+
    geom_line(size = 2)+
    labs(x = "Year", y = "MW")+
    ggsave("D:/NEM_LMP/Output/Final/SA_wind_capacity.png", width = 10)

#how much semi v non-scheduled wind
fread("D:/NEM_LMP/Data/Raw/generator_details.csv") %>% clean_names() %>% 
    filter(region == "South Australia", fuel_type == "Wind") %>% 
    group_by(schedule_type) %>% summarise(sum = sum(capacity))

#total wind capacity
sa_solar_wind %>% group_by(duid, region, fuel_type) %>% 
    summarise(start = min(year(settlementdate))) %>% filter(fuel_type %in% c("Wind")) %>% 
    left_join(fread("D:/NEM_LMP/Data/Raw/generator_details.csv") %>% clean_names(), by = c("duid")) %>% 
    select(duid, start, region.x, fuel_type.x, capacity) %>% 
    group_by(start) %>% 
    summarise(capacity = sum(capacity)) %>% 
    add_row(start = 2015, capacity = 0) %>% #add 2015 entry of 0 capacity
    arrange(start) %>% 
    ggplot(aes(x = start, y = cumsum(capacity)))+
    geom_line(size = 2)+
    labs(x = "Year", y = "MW", title = "Wind Total Capacity") +
    ggsave("D:/NEM_LMP/Output/Final/SA_wind_total_capacity.png", width = 10)

#get vintages
files <- paste0("D:/NEM_LMP/Data/Raw/INITIALMW/" ,list.files("D:/NEM_LMP/Data/Raw/INITIALMW"))
gens <- generator_details %>% filter(region == "SA", fuel_type %in% c("Solar","Wind")) %>% .[["duid"]]

sa_solar_wind_full <- files %>% map(~fread(.x) %>% clean_names() %>%
                                   filter(duid %in% gens)) %>% 
    rbindlist() %>% 
    left_join(generator_details, by = "duid")

fwrite(sa_solar_wind_full, "D:/NEM_LMP/Data/Cleaned/Misc/sa_solar_wind_full.csv")

vintage <- sa_solar_wind_full %>% group_by(duid) %>% summarise(start = min(year(settlementdate)))

#AO
    
full_vom %>% filter(fuel_type == "Wind", region == "SA") %>% 
    group_by(duid, year) %>% summary_table_7() %>% 
    left_join(vintage, by = "duid") %>% 
    ggplot(aes(x = year, y = AO, group = duid, colour = start))+
    geom_line(size = 1.2)+
    scale_color_gradient(low = "blue", high = "red") +
    labs(x = "Year", y = "$/MWh", colour = "Comission Year")+
    ggsave("D:/NEM_LMP/Output/Final/SA_wind_AO.png", width = 10)

full_vom %>% filter(fuel_type == "Wind", region == "SA", year %in% c(2009,2014)) %>% 
    group_by(year) %>% summary_table_7()  -> temp2

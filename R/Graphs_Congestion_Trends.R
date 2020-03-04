#Congestion Trends

# Load packages

library(tidyverse, tidyr)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
Sys.setenv(TZ='UTC')

#load data
LPA <- fread("D:/NEM_LMP/Data/Raw/dispatch_local_price_24-01-2020.csv") %>% clean_names() %>% 
    mutate(settlementdate = ymd_hms(settlementdate)) %>% 
    filter(year(settlementdate) %in% c(2019)) %>% 
    select(-locally_constrained)

generator_details_raw <- fread("D:/NEM_LMP/Data/RAW/generator_details.csv") %>% 
    select(-c("ConnectionPtId", "Thermal Efficiency", "Auxiliaries", "Emission Intensity Sent-Out", 
              "Capacity")) %>% 
    rename(REGIONID = Region) %>% 
    mutate(Region = case_when(REGIONID == "Queensland" ~ "QLD",
                              REGIONID == "New South Wales" ~ "NSW",
                              REGIONID == "Victoria" ~ "VIC",
                              REGIONID == "South Australia" ~ "SA",
                              REGIONID == "Tasmania" ~ "TAS")) %>% 
    clean_names() %>% 
    select(-regionid) 

pumped_hydro_stations <- generator_details %>% filter(type == "Load" & fuel_type == "Hydro") %>% select(station) %>% 
    unique() %>% .[['station']]

generator_details <- generator_details %>% 
    mutate(fuel_type = case_when(((station %in% pumped_hydro_stations) & type == "Load") ~ 
                                     "Pumped Hydro (Pumping)", #deal with loads
                                 ((station %in% pumped_hydro_stations) & type == "Gen") ~ 
                                     "Pumped Hydro (Release)",
                                 (!(station %in% pumped_hydro_stations) & type == "Gen" & fuel_type == "Hydro") ~ 
                                     "Hydro",
                                 (fuel_type == "Battery" & type == "Load") ~ "Battery (Charge)",
                                 (fuel_type == "Battery" & type == "Gen") ~ "Battery (Discharge)",
                                 TRUE ~ fuel_type))

fwrite(generator_details, "D:/NEM_LMP/Data/generator_details_cleaned")

generator_details <- fread("D:/NEM_LMP/Data/generator_details_cleaned")

#what %time of network congested?
LPA %>% select(settlementdate) %>% unique() %>% 
    group_by(year = year(settlementdate)) %>% 
    summarise(prop = n()/(12*24*365)) %>% 
    ggplot(aes(x = year, y = prop))+
    geom_line(size = 2) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
    labs(title = "Percentage of Time NEM is Congested", y = "Percentage", x = "Year") +
    theme(text = element_text(size = 12)) +
    ggsave("D:/NEM_LMP/Output/Congestion_Trends/NemCongested.png")

#what %time of time state congested?
LPA_details <- LPA %>% 
    left_join(generator_details, by = "duid")

LPA_details %>% 
    select(settlementdate, region) %>% 
    unique() %>% 
    group_by(year = year(settlementdate),region) %>% 
    summarise(prop = n()/(12*24*365)) %>% 
    ggplot(aes(x = year, y = prop, colour = region))+
    facet_wrap(~region) +
    geom_line(size = 2) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.5)) +
    labs(title = "Percentage of Time State is Congested",y = "Percent", x = "Year") +
    theme(text = element_text(size = 12)) +
    ggsave("D:/NEM_LMP/Output/Congestion_Trends/StateCongested.png")
 
#% time each gen is congested(all time)
LPA_details %>% filter(year(settlementdate)==2018) %>% 
    group_by(duid, fuel_type) %>% 
    summarise(prop = n()/(12*24*365)) %>% 
    ggplot(aes(x = prop, fill = fuel_type))+
    geom_histogram() +
    scale_y_continuous(limits = c(0,75)) +
    scale_x_continuous(limits = c(-.01,0.35)) +
    labs(title = "Generator-level frequency of congestion in 2018") +
    ggsave("D:/NEM_LMP/Output/Congestion_Trends/DuidConstrained2018.png", width = 8)

LPA_details %>% filter(year(settlementdate)==2019) %>% 
    group_by(duid, fuel_type) %>% 
    summarise(prop = n()/(12*24*365)) %>% 
    ggplot(aes(x = prop, fill = fuel_type))+
    geom_histogram() +
    scale_y_continuous(limits = c(0,75)) +
    scale_x_continuous(limits = c(-.01,0.35)) +
    labs(title = "Generator-level frequency of congestion in 2019") +
    ggsave("D:/NEM_LMP/Output/Congestion_Trends/DuidConstrained2019.png", width = 8)


#why s it different to paper?
LPA_details %>% filter(year(settlementdate)==2019) %>% 
    group_by(duid, fuel_type) %>% 
    summarise(prop = n()/(12*24*365)) %>% arrange(-prop)

LPA_details %>% filter(duid == "MURRAY") %>% head()

#%time each state is congested (only when producing)
location <- "D:/NEM_LMP/Data/Cleaned/MC/"
files <- paste0(location, list.files(location))

dispatch_data <- files[73:84] %>% map(~fread(.x) %>% 
                                   select(settlementdate, duid, fuel_type, dispatchmwh)) %>% 
    rbindlist() %>% 
    mutate(settlementdate = ymd_hms(settlementdate))

LPA_Dispatch <- LPA %>% right_join(dispatch_data, by = c("settlementdate", "duid")) %>% 
    mutate(local_price_adjustment = ifelse(is.na(local_price_adjustment),
                                                 0,
                                                 local_price_adjustment))

LPA_Dispatch %>% group_by(duid, fuel_type.y) %>% 
    summarise(prop = sum(local_price_adjustment!=0)/n()) %>% 
    ggplot(aes(x = prop, fill = fuel_type.y))+
    geom_histogram() +
    ggtitle("Constrained when dispatched 2019") +#graph
    ggsave("D:/NEM_LMP/Output/Congestion_Trends/DuidConstrainedWhenDispatched2019.png", width = 8)


LPA_Dispatch %>% group_by(duid, fuel_type) %>% 
    summarise(prop = sum(local_price_adjustment!=0 & dispatchmwh>0.01)/(12*24*365)) %>% arrange(-prop) %>% 
    select(duid, fuel_type, prop)#table


LPA_Dispatch %>% group_by(duid, fuel_type) %>% 
    summarise(prop = sum(local_price_adjustment!=0 & dispatchmwh>0.01)/(12*24*365)) %>% 
    ggplot(aes(x = prop, fill = fuel_type))+
    geom_histogram() #thesis reproduction

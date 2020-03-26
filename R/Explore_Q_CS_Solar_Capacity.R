#Make graphs for section 4.5(QLD)

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

qld_data <- fread("D:/NEM_LMP/Data/Cleaned/Misc/qld_data.csv") %>% mutate(settlementdate = ymd_hms(settlementdate))

#Solar dispatch over time
qld_data %>% filter(fuel_type == "Solar") %>% 
    mutate(year = floor_date(settlementdate, "quarter")) %>% 
    group_by(year) %>% 
    summarise(Q = sum(dispatchmwh), QC = sum(dispatchmwh[local_price_adjustment!=0])) %>% 
    gather(var, value, -year) %>% 
    ggplot(aes(x = year, y = value/1000, colour = var))+
    geom_line(size = 2)+
    labs(x = "Quarter", y = "GWh", title = "QLD Solar")+
    ggsave("D:/NEM_LMP/Output/Q_CS/Solar_GWh.png", width = 10)

#Q_CS gen output over time
q_cs_duids <- c("BARCALDN", "LILYSF1", "BARRON-1", "BARRON-2", "CALL_B_1", "CALL_B_2", "CPP_3", "CPP_4", "DAYDSF1", "HAYMSF1", "CLARESF1", "CSPVPS1", "YABULU2", "GSTONE1", "GSTONE2", "GSTONE3", "GSTONE4", "GSTONE5", "GSTONE6", "HAUGHT11", "KAREEYA1", "KAREEYA2", "KAREEYA3", "KAREEYA4", "EMERASF1", "QLIS2M", "CLERMSF1", "MACKAYGT", "RUGBYR1", "MSTUART1", "MSTUART2", "MSTUART3", "KSP1", "RRSF1", "QROW1K", "QROW2K", "HAMISF1", "WHITSF1", "STAN-1", "STAN-2", "STAN-3", "STAN-4", "YABULU","SMCSF1", "MEWF1")


qld_data %>% filter(fuel_type == "Solar", duid %in% q_cs_duids) %>% 
    mutate(year = floor_date(settlementdate, "quarter")) %>% 
    group_by(year) %>% 
    summarise(Q = sum(dispatchmwh), QC = sum(dispatchmwh[local_price_adjustment!=0])) %>% 
    gather(var, value, -year) %>% 
    ggplot(aes(x = year, y = value/1000, colour = var))+
    geom_line(size = 2)+
    labs(x = "Quarter", y = "GWh", title = "Q_CS_* Solar")+
    ggsave("D:/NEM_LMP/Output/Q_CS/Solar_Q_CS_GWh.png", width = 10)



#When did Q_CS start binding?
constraints_yearly <- fread("D:/Thesis/Data/NEMSIGHT/constraints_yearly.csv") %>% 
    mutate(year = paste0(Interval, "-01-01") %>% ymd()) %>% 
    clean_names() %>% 
    select(year, id, state, num_events, av_mv) 
    
#thermal v nonthermal
constraints_yearly %>% 
    filter(state == "QLD", year >=2013,
           !(substr(id, 2,3) %in% c(">N", ":N", "^N", "+N", "_N"))) %>% #filter out interconnector only constraints
    mutate(constraint_type = case_when(substr(id, 2,2) == ">" ~ "Thermal",
                                       TRUE ~ "System Strength")) %>% #all other cases, just put as System strength
    group_by(year, constraint_type) %>% 
    summarise(sum = sum(num_events)) %>% 
    ggplot(aes(x = year, y = sum, colour = constraint_type))+
    geom_line(size = 2)+
    ggsave("D:/NEM_LMP/Output/Q_CS/QLD_Constraints_TS.png", width = 10)

#all different types fo constraints
constraints_yearly %>% 
    filter(state == "QLD", year >=2013,
           !(substr(id, 2,3) %in% c(">N", ":N", "^N", "+N", "_N"))) %>% #filter out interconnector only constraints
    mutate(constraint_type = substr(id, 2, 2)) %>% 
    group_by(year, constraint_type) %>% 
    summarise(sum = sum(num_events)) %>% 
    ggplot(aes(x = year, y = sum, colour = constraint_type))+
    geom_line(size = 2)

constraints_yearly %>% filter(state=="QLD") %>% .[["id"]] %>% substr(2,4) %>% table()

#Q_CS v others
constraints_yearly %>% 
    filter(state == "QLD", year(year) >=2013,
           !(substr(id, 2,3) %in% c(">N", ":N", "^N", "+N", "_N"))) %>% #filter out interconnector only constraints
    mutate(total_mv = num_events*av_mv,
           type = case_when((substr(id, 1,4) == "Q_CS") ~ "Q_CS",
                            (substr(id, 2,2) == "^") ~ "Thermal",
                             TRUE ~ "Other")) %>% 
    group_by(year, type) %>% 
    summarise(total_events = sum(num_events), total_mv = sum(total_mv), total_av = total_mv/total_events) %>% 
    ggplot(aes(x = year, y = total_events, colour = type))+
    geom_line(size = 2)

#why are MVs so bloody high?
constraints_yearly %>% 
    filter(state == "QLD", year(year) >=2013,
           !(substr(id, 2,3) %in% c(">N", ":N", "^N", "+N", "_N"))) %>% #filter out interconnector only constraints
    mutate(total_mv = num_events*av_mv,
           type = case_when((substr(id, 1,4) == "Q_CS") ~ "Q_CS",
                            (substr(id, 2,2) == "^") ~ "Thermal",
                            TRUE ~ "Other")) %>% 
    group_by(year, type) %>% 
    summarise(total_events = sum(num_events), total_mv = sum(total_mv), total_av = total_mv/total_events)

constraints_yearly %>% 
    filter(state == "QLD", year(year) >= 2019,
           !(substr(id, 2,3) %in% c(">N", ":N", "^N", "+N", "_N"))) %>% #filter out interconnector only constraints
    mutate(total_mv = num_events*av_mv,
           type = case_when((substr(id, 1,4) == "Q_CS") ~ "Q_CS",
                            (substr(id, 2,2) == "^") ~ "Thermal",
                            TRUE ~ "Other")) %>% arrange(total_mv) %>% .[1:10,]

#Solar DUID dispatch over time
qld_data %>% filter(fuel_type == "Solar") %>% 
    mutate(year = floor_date(settlementdate, "quarter")) %>% 
    group_by(duid, year) %>% 
    summarise(Q = sum(dispatchmwh), QC = sum(dispatchmwh[local_price_adjustment!=0]), QnonC = Q-QC) %>% 
    gather(var, value, -year, -duid) %>% 
    filter(var == "Q") %>% 
    ggplot(aes(x = year, y = value/1000, colour = duid))+
    geom_line(size = 2)+
    labs(x = "Quarter", title = "QLD Solar")


#When did each q_cs gen start producing?    
qld_data %>% filter(duid %in% q_cs_duids) %>% 
    group_by(duid, fuel_type) %>% 
    summarise(start = min(floor_date(settlementdate, "month"))) %>% 
    arrange(start) %>% as.data.frame() %>% 
    left_join(generator_details %>% select(duid, station))

#When did each q_cs station start producing?    
qld_data %>% filter(duid %in% q_cs_duids) %>% 
    left_join(generator_details %>% select(duid, station)) %>% 
    group_by(station, fuel_type) %>% 
    summarise(start = min(floor_date(settlementdate, "month"))) %>% 
    arrange(start) %>% as.data.frame() 

qld_data %>% filter(duid %in% q_cs_duids) %>% 
    group_by(duid, fuel_type) %>% 
    summarise(start = min(floor_date(settlementdate, "day"))) %>% 
    arrange(start) %>% as.data.frame() %>% 
    left_join(generator_details %>% select(duid, station))

#how many new plants per quarter?    
qld_data %>% filter(fuel_type == "Solar") %>% 
    mutate(year = floor_date(settlementdate, "quarter")) %>% 
    group_by(duid) %>% 
    summarise(start = min(year)) %>% 
    group_by(start) %>% 
    summarise(new_plants = n())

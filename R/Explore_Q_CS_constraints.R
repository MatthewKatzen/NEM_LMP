#Explore Q_CS

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


constraints_quarterly <- fread("D:/Thesis/Data/NEMSIGHT/constraints_quarterly.csv") %>% 
    mutate(quarter = paste0(20, substr(Interval,1,2), 
                            "-", as.numeric(substr(Interval,4,4))*3-2,
                            "-01") %>% ymd()) %>% 
    clean_names() %>% 
    select(quarter, id, state, num_events)

constraints_quarterly %>% filter(substr(id,1,4) == "Q_CS")
#Q_CS constraints only started in 2017q2

#get duids from NEMSIGHT
q_cs_duids <- c("BARCALDN", "LILYSF1", "BARRON-1", "BARRON-2", "CALL_B_1", "CALL_B_2", "CPP_3", "CPP_4", "DAYDSF1", "HAYMSF1", "CLARESF1", "CSPVPS1", "YABULU2", "GSTONE1", "GSTONE2", "GSTONE3", "GSTONE4", "GSTONE5", "GSTONE6", "HAUGHT11", "KAREEYA1", "KAREEYA2", "KAREEYA3", "KAREEYA4", "EMERASF1", "QLIS2M", "CLERMSF1", "MACKAYGT", "RUGBYR1", "MSTUART1", "MSTUART2", "MSTUART3", "KSP1", "RRSF1", "QROW1K", "QROW2K", "HAMISF1", "WHITSF1", "STAN-1", "STAN-2", "STAN-3", "STAN-4", "YABULU","SMCSF1", "MEWF1")

generator_details %>% filter(duid %in% q_cs_duids)

g


# QLD TO by hour of day

location <- "D:/NEM_LMP/Data/Cleaned/MC/"
files <- paste0(location, list.files(location))
qld_data <- files %>% map(~fread(.x) %>% filter(region == "QLD") %>% 
                                       select(settlementdate, duid, fuel_type, dispatchmwh, local_price_adjustment,
                                              rev_rrp_30, rev_lmp_censored, rev_lmp_mc)) %>% 
    rbindlist() %>% 
    mutate(settlementdate = ymd_hms(settlementdate)) %>% 
    mutate(TO = ifelse(local_price_adjustment!=0,
                       rev_rrp_30 - rev_lmp_censored,
                       0),
           TOmc = ifelse(local_price_adjustment!=0,
                         rev_rrp_30 - rev_lmp_mc,
                         0))

temp <- qld_data %>% 
    mutate(time = paste0("2020-01-01 ", format(settlementdate, "%H:%M:%S")) %>% ymd_hms(),
           year = as.character(year(settlementdate))) %>% 
    group_by(year, time, fuel_type) %>% 
    summarise(Q = sum(dispatchmwh),
              QC = sum(dispatchmwh[local_price_adjustment != 0]),
              prop = mean(local_price_adjustment != 0), 
              sum = sum(TO), summc = sum(TOmc)) %>% 
    ungroup()
#TO
temp %>% filter(year == 2019) %>% 
    ggplot(aes(x = time, y = sum/1000000, colour = fuel_type)) + 
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(title = "TO 2019") +
    ggsave("D:/NEM_LMP/Output/Q_CS/TO2019_by_hour.png", width = 7)

temp %>% filter(year == 2019) %>% 
    ggplot(aes(x = time, y = summc/1000000, colour = fuel_type)) + 
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(title = "TOmc 2019") +
    ggsave("D:/NEM_LMP/Output/Q_CS/TOmc_by_hour.png", width = 7)

temp %>% filter(year == 2018) %>% 
    ggplot(aes(x = time, y = sum/1000000, colour = fuel_type)) + 
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(title = "TO 2018") +
    ggsave("D:/NEM_LMP/Output/Q_CS/TO2018_by_hour.png", width = 7)

temp  %>% 
    ggplot(aes(x = time, y = sum, colour = year)) + 
    facet_wrap(~fuel_type)+
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(title = "TO FuelType x Year") +
    ggsave("D:/NEM_LMP/Output/Q_CS/TO FuelType x Year (by interval).png", width = 10, height = 7)

#Prop

temp %>% filter(year == 2019) %>% 
    ggplot(aes(x = time, y = prop, colour = fuel_type)) + 
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(title = "Prop 2019") 
    
temp %>% filter(year == 2018) %>% 
    ggplot(aes(x = time, y = prop, colour = fuel_type)) + 
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(title = "Prop 2018") 

temp %>%
    ggplot(aes(x = time, y = prop, colour = year)) + 
    facet_wrap(~fuel_type)+
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(title = "Prop RRP!= LMP FuelType x Year") +
    ggsave("D:/NEM_LMP/Output/Q_CS/Prop FuelType x Year (by interval).png", width = 10, height = 7)

#Q

temp %>% filter(year == 2019) %>% 
    ggplot(aes(x = time, y = Q, colour = fuel_type)) + 
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(title = "Q 2019") 

temp %>% filter(year == 2018) %>% 
    ggplot(aes(x = time, y = Q, colour = fuel_type)) + 
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(title = "Q 2018") 

temp %>% filter(fuel_type == "Solar") %>% ungroup() %>% 
    mutate(year = as.factor(year(time)),
        time = paste0("2013-01-01 ", format(time, "%H:%M:%S")) %>% ymd_hms()) %>%
    ggplot(aes(x = time, y = Q, colour = year)) + 
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(title = "Solar Q") 

temp %>% 
    ggplot(aes(x = time, y = Q, colour = year)) + 
    facet_wrap(~fuel_type)+
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(title = "Q FuelType x Year") +
    ggsave("D:/NEM_LMP/Output/Q_CS/Q FuelType x Year (by interval).png", width = 10, height = 7)

#QC 
temp %>% filter(fuel_type == "Solar") %>% 
    mutate(year = as.factor(year(time)),
           time = paste0("2013-01-01 ", format(time, "%H:%M:%S")) %>% ymd_hms()) %>%
    ggplot(aes(x = time, y = QC, colour = year)) + 
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(title = "Solar QC") 

temp  %>% 
    ggplot(aes(x = time, y = QC, colour = year)) + 
    facet_wrap(~fuel_type)+
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(title = "QC FuelType x Year") +
    ggsave("D:/NEM_LMP/Output/Q_CS/QC FuelType x Year (by interval).png", width = 10, height = 7)


temp  %>% filter(year %in% c(2018, 2019)) %>% 
    ggplot(aes(x = time, y = summc, colour = year)) + 
    facet_wrap(~fuel_type)+
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(title = "TOmc FuelType x Year") +
    ggsave("D:/NEM_LMP/Output/Q_CS/TOmc FuelType x Year (by interval).png", width = 10, height = 7)

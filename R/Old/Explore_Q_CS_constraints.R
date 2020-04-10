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
generator_details <- fread("D:/NEM_LMP/Data/generator_details_cleaned")

q_cs_duids <- c("BARCALDN", "LILYSF1", "BARRON-1", "BARRON-2", "CALL_B_1", "CALL_B_2", "CPP_3", "CPP_4", "DAYDSF1", "HAYMSF1", "CLARESF1", "CSPVPS1", "YABULU2", "GSTONE1", "GSTONE2", "GSTONE3", "GSTONE4", "GSTONE5", "GSTONE6", "HAUGHT11", "KAREEYA1", "KAREEYA2", "KAREEYA3", "KAREEYA4", "EMERASF1", "QLIS2M", "CLERMSF1", "MACKAYGT", "RUGBYR1", "MSTUART1", "MSTUART2", "MSTUART3", "KSP1", "RRSF1", "QROW1K", "QROW2K", "HAMISF1", "WHITSF1", "STAN-1", "STAN-2", "STAN-3", "STAN-4", "YABULU","SMCSF1", "MEWF1")

generator_details %>% filter(duid %in% q_cs_duids) %>% 
    select(fuel_type) %>% table()





# QLD TO by hour of day

location <- "D:/NEM_LMP/Data/Cleaned/MC/"
files <- paste0("D:/NEM_LMP/Data/Cleaned/MC/", list.files("D:/NEM_LMP/Data/Cleaned/MC/"))
qld_data <- paste0("D:/NEM_LMP/Data/Cleaned/MC/", list.files("D:/NEM_LMP/Data/Cleaned/MC/")) %>% 
    map(~fread(.x) %>% filter(region == "QLD") %>% 
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

fwrite(qld_data, "D:/NEM_LMP/Data/Cleaned/Misc/qld_data.csv")
qld_data <- fread("D:/NEM_LMP/Data/Cleaned/Misc/qld_data.csv") %>% mutate(settlementdate = ymd_hms(settlementdate))

qld_summary <- qld_data %>% 
    mutate(time = paste0("2020-01-01 ", format(settlementdate, "%H:%M:%S")) %>% ymd_hms(),
           year = settlementdate %>% year() %>% as.character()) %>% 
    group_by(year, time, fuel_type) %>% 
    summarise(Q = sum(dispatchmwh),
              QC = sum(dispatchmwh[local_price_adjustment != 0]),
              prop = mean(local_price_adjustment != 0), 
              sum = sum(TO), summc = sum(TOmc)) %>% 
    ungroup()

fwrite(qld_summary, "D:/NEM_LMP/Data/Cleaned/Misc/qld_summary.csv")
qld_summary <- fread("D:/NEM_LMP/Data/Cleaned/Misc/qld_summary.csv") %>% mutate(time = ymd_hms(time))


#TO
qld_summary  %>% filter(year %in% c(2018, 2019),
                        fuel_type %in% c("Black Coal", "Gas", "Solar")) %>% 
    ggplot(aes(x = time, y = sum/1000000, colour = as.character(year))) + 
    facet_wrap(~fuel_type, nrow = 3)+
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(x = "Interval", y = "Total Overcompensation ($m)", colour = "Year") +
    ggsave("D:/NEM_LMP/Output/Q_CS/TO FuelType x Year (by interval).png", width = 10, height = 7)

#TOmc

qld_summary  %>% filter(year %in% c(2018, 2019),
                        fuel_type %in% c("Black Coal", "Gas", "Solar")) %>% 
    ggplot(aes(x = time, y = summc/1000000, colour = as.character(year))) + 
    facet_wrap(~fuel_type, nrow = 3)+
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(x = "Interval", y = "Adjusted Total Overcompensation ($m)", colour = "Year") +
    ggsave("D:/NEM_LMP/Output/Q_CS/TOmc FuelType x Year (by interval).png", width = 10, height = 7)

#Prop

qld_summary  %>% filter(year %in% c(2018, 2019),
                        fuel_type %in% c("Black Coal", "Gas", "Solar")) %>% 
    ggplot(aes(x = time, y = prop, colour = as.character(year))) + 
    facet_wrap(~fuel_type, nrow = 3)+
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(x = "Interval", y = "Proportion of time Congested", colour = "Year") +
    ggsave("D:/NEM_LMP/Output/Q_CS/Prop FuelType x Year (by interval).png", width = 10, height = 7)


#Q

qld_summary  %>% filter(year %in% c(2018, 2019),
                        fuel_type %in% c("Black Coal", "Gas", "Solar")) %>% 
    ggplot(aes(x = time, y = Q, colour = year)) + 
    facet_wrap(~fuel_type, nrow = 3)+
    geom_line()+
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(x = "Interval", y = "Dispatch Quantity", colour = "Year") +
    ggsave("D:/NEM_LMP/Output/Q_CS/Q FuelType x Year (by interval).png", width = 10, height = 7)

#QC 
qld_summary  %>% filter(year %in% c(2018, 2019),
                        fuel_type %in% c("Black Coal", "Gas", "Solar")) %>%
    ggplot(aes(x = time, y = QC, colour = year)) +
    facet_wrap(~fuel_type, nrow = 3) +
    geom_line() +
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    labs(x = "Interval", y = "Constrained Dispatch Quantity", colour = "Year") +
    ggsave("D:/NEM_LMP/Output/Q_CS/QC FuelType x Year (by interval).png", width = 10, height = 7)


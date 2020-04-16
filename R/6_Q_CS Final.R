#Queensland Black coalk consildated

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

#generate dataset
qld_constraint_data <- paste0("D:/NEM_LMP/Data/Raw/RHS/",list.files("D:/NEM_LMP/Data/Raw/RHS")) %>% 
    map(~ fread(.x) %>% clean_names() %>% 
            filter(marginalvalue != 0) %>%
            group_by(settlementdate, constraintid) %>% 
                filter(intervention == max(intervention)) %>%  #keep intervention run only
                ungroup() %>% 
            select(settlementdate, constraintid, marginalvalue) %>% 
            filter(substr(constraintid,1,1) %in% c('Q')) %>%  #only QLD
            mutate(settlementdate = ymd_hms(settlementdate))) %>% 
    rbindlist()

fwrite(qld_constraint_data, "D:/NEM_LMP/Data/Cleaned/Misc/qld_constraint_data.csv")

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



#load data
qld_constraint_data <- fread("D:/NEM_LMP/Data/Cleaned/Misc/qld_constraint_data.csv") %>% #only from 2015
    mutate(settlementdate = ymd_hms(settlementdate))
qld_data <- fread("D:/NEM_LMP/Data/Cleaned/Misc/qld_data.csv") %>% 
    mutate(settlementdate = ymd_hms(settlementdate)) %>% 
    filter(year(settlementdate) >= 2015)
generator_details <- fread("D:/NEM_LMP/Data/Raw/generator_details_cleaned.csv")
generator_details_uncleaned <- fread("D:/NEM_LMP/Data/Raw/generator_details.csv") %>% clean_names()
subregions_data <- fread("D:/NEM_LMP/Data/Raw/Zonal mapping UPDATED.csv")
q_cs_duids <- c("BARCALDN", "LILYSF1", "BARRON-1", "BARRON-2", "CALL_B_1", "CALL_B_2", "CPP_3", "CPP_4", "DAYDSF1", "HAYMSF1", "CLARESF1", "CSPVPS1", "YABULU2", "GSTONE1", "GSTONE2", "GSTONE3", "GSTONE4", "GSTONE5", "GSTONE6", "HAUGHT11", "KAREEYA1", "KAREEYA2", "KAREEYA3", "KAREEYA4", "EMERASF1", "QLIS2M", "CLERMSF1", "MACKAYGT", "RUGBYR1", "MSTUART1", "MSTUART2", "MSTUART3", "KSP1", "RRSF1", "QROW1K", "QROW2K", "HAMISF1", "WHITSF1", "STAN-1", "STAN-2", "STAN-3", "STAN-4", "YABULU","SMCSF1", "MEWF1")

#where are q_cs?
subregions_data %>% filter(region %in% c("QLD", "QLD1"))
subregions_data %>% filter(duid %in% q_cs_duids)




#GRAPHS

#time of day Q_CS binds
qld_constraint_data %>% filter(substr(constraintid,1,4) == "Q_CS") %>% 
    mutate(time = paste0("2020-01-01 ", format(settlementdate, "%H:%M:%S")) %>% ymd_hms(),
                               date = floor_date(settlementdate, "day"),
           Year = as.factor(year(settlementdate))) %>% 
    group_by(time, Year) %>% tally() %>% group_by(Year) %>% 
    pad(start_val = ymd_hms("2020-01-01 00:00:00"), end_val = ymd_hms("2020-01-01 23:55:00")) %>% 
    mutate(n = ifelse(is.na(n), 0, n)) %>% 
    ggplot(aes(x = time, y = n, colour = Year)) +
    geom_line(size = 2)+
    scale_x_datetime(date_labels = "%H:%M") +
    labs(x = "Time", y = "Number of times constraint binds") +
    guides(colour = guide_legend(title = "year"))+
    ggsave("D:/NEM_LMP/Output/Final/Q_CS_time_of_day_year.png", width = 10)

#when did Q_CS start?
qld_constraint_data %>% filter(substr(constraintid,1,4) == "Q_CS") %>% 
    filter(settlementdate == min(settlementdate))

#monthly
qld_constraint_data %>% filter(substr(constraintid,1,4) == "Q_CS") %>% 
    group_by(floor_date(settlementdate, "month")) %>% summarise(count = n())
#yearly
qld_constraint_data %>% filter(substr(constraintid,1,4) == "Q_CS") %>% 
    group_by(floor_date(settlementdate, "year")) %>% summarise(count = n())



#TABLES

#When did each q_cs gen start producing?    
qld_data %>% filter(duid %in% q_cs_duids) %>% 
    group_by(duid, fuel_type) %>% 
    summarise(start = min(floor_date(settlementdate, "month"))) %>% 
    arrange(start) %>% as.data.frame() %>% 
    left_join(generator_details_uncleaned %>% select(duid, station, capacity))

#summary of each fuel type 
generator_details_uncleaned %>% filter(duid %in% q_cs_duids) %>% 
    select(duid, station, fuel_type, capacity) %>% 
    group_by(fuel_type) %>% summarise(count = n(), capacity = sum(capacity))


#TO of each constraint
qld_data %>% filter(year(settlementdate) == 2019, local_price_adjustment != 0) %>% 
    left_join(qld_constraint_data, by = "settlementdate") %>% 
    group_by(constraintid) %>% summarise(count = n_distinct(settlementdate), TO = sum(TO), TOmc = sum(TOmc)) %>% arrange(-TO)

#duplicates removed
qld_data %>% filter(year(settlementdate) == 2019, local_price_adjustment != 0) %>% 
    left_join(qld_constraint_data, by = "settlementdate") %>% 
    group_by(settlementdate) %>% filter(all(constraintid == first(constraintid))) %>% ungroup() %>% 
    group_by(constraintid) %>% summarise(count = n_distinct(settlementdate), TO = sum(TO), TOmc = sum(TOmc)) %>% arrange(-TO)

#how many rows removed?
qld_data %>% filter(year(settlementdate) == 2019, local_price_adjustment != 0) %>% 
    left_join(qld_constraint_data, by = "settlementdate") %>% 
    group_by(constraintid) %>% summarise(count = n_distinct(settlementdate), TO = sum(TO), TOmc = sum(TOmc)) %>% arrange(-TO) %>% 
    select(count) %>% colSums() #23931

qld_data %>% filter(year(settlementdate) == 2019, local_price_adjustment != 0) %>% 
    left_join(qld_constraint_data, by = "settlementdate") %>% 
    group_by(settlementdate) %>% filter(all(constraintid == first(constraintid))) %>% ungroup() %>% 
    group_by(constraintid) %>% summarise(count = n_distinct(settlementdate), TO = sum(TO), TOmc = sum(TOmc)) %>% arrange(-TO) %>% 
    select(count) %>% colSums() #11891

11891/23931

#what % of TO was by Q_CS?
qld_data %>% filter(year(settlementdate) == 2019, local_price_adjustment != 0) %>% 
    left_join(qld_constraint_data, by = "settlementdate") %>% 
    group_by(Q_CS = (substr(constraintid,1,4)=="Q_CS")) %>% 
    summarise(count = n_distinct(settlementdate), TO = sum(TO), TOmc = sum(TOmc)) %>% 
    mutate(percTO = TO/1522000000)

qld_data %>% filter(year(settlementdate) == 2019, local_price_adjustment != 0) %>% 
    left_join(qld_constraint_data, by = "settlementdate") %>% 
    group_by(settlementdate) %>% filter(all(constraintid == first(constraintid))) %>% ungroup() %>% 
    group_by(Q_CS = (substr(constraintid,1,4)=="Q_CS")) %>% 
    summarise(count = n_distinct(settlementdate), TO = sum(TO), TOmc = sum(TOmc)) %>% 
    mutate(percTO = TO/1522000000)

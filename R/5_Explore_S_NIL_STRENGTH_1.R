#https://aemo.com.au/-/media/files/electricity/nem/security_and_reliability/congestion-information/transfer-limit-advice-system-strength.pdf?la=en
#https://aemo.com.au/Market-Notices?marketNoticeQuery=60252&marketNoticeFacets=

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

#up to 11/12/2017 max 1200
#11/12/17 max 1295
#5/12/18 1295-1460
#16/9/19 1300 - 1750

#generate data
sa_constraint_data <- paste0("D:/NEM_LMP/Data/Raw/RHS/",list.files("D:/NEM_LMP/Data/Raw/RHS")) %>% 
    map(~ fread(.x) %>% clean_names() %>% 
            filter(marginalvalue != 0) %>%
            group_by(settlementdate, constraintid) %>% 
            filter(intervention == max(intervention)) %>%  #keep intervention run only
            ungroup() %>% 
            select(settlementdate, constraintid, marginalvalue, rhs) %>% 
            filter(substr(constraintid,1,1) %in% c('S')) %>% 
            mutate(settlementdate = ymd_hms(settlementdate))) %>% 
    rbindlist()

fwrite(sa_constraint_data, "D:/NEM_LMP/Data/Cleaned/Misc/sa_constraint_data.csv")

#load data 
generator_details <- fread("D:/NEM_LMP/Data/Raw/generator_details_cleaned.csv")
sa_constraint_data <- fread("D:/NEM_LMP/Data/Cleaned/Misc/sa_constraint_data.csv") %>% mutate(settlementdate = ymd_hms(settlementdate))


#When does SS start binding?
sa_constraint_data %>% filter(constraintid %in% c("S_WIND_1200_AUTO", "S_NIL_STRENGTH_1", "S_SA_WIND_1200")) %>% 
    group_by(constraintid) %>% 
    summarise(start = min(settlementdate),
              end = max(settlementdate))

    #S_WIND_1200_AUTO created 8/09/2017, S_SA_WIND_1200 created 7/08/2018
    #first onyl active in 2017q3, second only active 2017q3/4
    #S_NIL_STRENGTH_1 ffrom 18q1 for all non-synchronous

#what type of gens are in SS?
ss_duids <- c("NBHWF1", "BLUFF1", "BNGSF1", "BNGSF2", "SNOWNTH1", "SNOWSTH1", "CLEMGPWF", "HDWF1", "HDWF2", "HDWF3", "HALLWF1", "LGAPWF1", "LKBONNY2", 
    "LKBONNY3", "HALLWF2", "SNOWTWN1", "TBSF1", "WGWF1", "WATERLWF")

#any wind and solar not in constraint?
generator_details %>% filter(region == "SA", fuel_type %in% c("Wind","Solar"), !(duid %in% ss_duids))


#download 2013-2014

c(201301:201412) %>% map(~rhs.fun(.x))

#Thermal v non thermal constraints
sa_constraint_data %>% 
    filter(rhs > 0.001, #remove all zero limit constraints
           !(substr(constraintid, 2,3) %in% c(">S", ":S", "^S", "+S", "_S"))) %>% #filter out interconnector only constraints
    mutate(constraint_type = case_when(substr(constraintid, 2,2) == ">" ~ "Thermal",
                                       TRUE ~ "System Strength")) %>% #all other cases, just put as System strength
    group_by(constraint_type, year = year(settlementdate)) %>% summarise(sum = n()) %>% 
    ggplot(aes(x = year, y = sum, colour = constraint_type)) +
    geom_line(size = 2)+
    labs(x = "Year", y = "# 5 min intervals", colour = "Constraint Type") +
    ggsave("D:/NEM_LMP/Output/Final/SA_constraints_freq.png", width = 10)




#download all scada files

files <- paste0("D:/NEM_LMP/Data/Raw/SCADA/" ,list.files("D:/NEM_LMP/Data/Raw/SCADA"))
gens <- generator_details %>% filter(region == "SA", fuel_type %in% c("Solar","Wind")) %>% .[["duid"]]
    
sa_solar_wind_scada <- files %>% map(~fread(.x) %>% clean_names() %>%
                          filter(duid %in% gens)) %>% 
    rbindlist() %>% 
    left_join(generator_details, by = "duid")

fwrite(sa_solar_wind_scada, "D:/NEM_LMP/Data/Cleaned/Misc/sa_wind_solar_scada.csv")
sa_solar_wind_scada <- fread("D:/NEM_LMP/Data/Cleaned/Misc/sa_wind_solar_scada.csv")


temp2 <- sa_solar_wind_scada %>% group_by(settlementdate) %>% summarise(total_initialmw = sum(scadavalue)) %>% 
    mutate(settlementdate = ymd_hms(settlementdate))

#when did solar start?
sa_solar_wind_scada %>% group_by(duid, fuel_type) %>% 
    summarise(min = min(settlementdate)) %>% as.data.frame()

#how much did solar produce?
sa_solar_wind_scada %>% group_by(year(settlementdate), fuel_type) %>% summarise(sum = sum(scadavalue)/12)

#make cdf
sa_solar_wind_scada_aggregate <- sa_solar_wind_scada %>% 
    group_by(settlementdate) %>% summarise(total_initialmw = sum(scadavalue)) %>% 
    mutate(settlementdate = ymd_hms(settlementdate)) %>% #get 5 min totals
    filter(settlementdate %within% interval(ymd_hms("2017-01-01 00:05:00 UTC"), ymd_hms("2020-01-01 00:00:00 UTC"))) %>% 
    mutate(constraint_group = case_when(
        settlementdate %within% interval(ymd_hms("2017-08-07 00:05:00 UTC"), ymd_hms("2017-12-11 00:00:00 UTC")) ~ 
            "7/8/2017 - 10/12/2017",
        settlementdate %within% interval(ymd_hms("2017-12-11 00:05:00 UTC"), ymd_hms("2018-12-05 00:00:00 UTC")) ~ 
            "11/12/2017 - 4/12/2018",
        settlementdate %within% interval(ymd_hms("2018-12-05 00:05:00 UTC"),ymd_hms("2020-01-01 00:00:00 UTC")) ~ 
            "5/12/2018 - 31/12/2019",
        TRUE ~ "1/1/2017 - 6/8/2017"))

sa_solar_wind_scada_aggregate$constraint_group <- factor(sa_solar_wind_scada_aggregate$constraint_group, 
                                 levels = c("1/1/2017 - 6/8/2017", "7/8/2017 - 10/12/2017", "11/12/2017 - 4/12/2018", 
                                            "5/12/2018 - 31/12/2019"),
                                 labels = c("1/1/2017 - 6/8/2017", "7/8/2017 - 10/12/2017", "11/12/2017 - 4/12/2018",
                                            "5/12/2018 - 31/12/2019"))

#CDF
sa_solar_wind_scada_aggregate %>% 
    ggplot(aes(x = total_initialmw, group = constraint_group, colour = constraint_group)) + 
    stat_ecdf(size = 2) +
    geom_vline(xintercept = 1200, linetype = "dashed")+
    geom_vline(xintercept = 1295, linetype = "dashed")+
    guides(colour = guide_legend(title="Constraint Group")) +
    labs(x = "MW", y = "Cumulative Density") +
    ggsave("D:/NEM_LMP/Output/Final/SA_Wind_Solar_Scada_CDF.png", width = 10)

#PDF
sa_solar_wind_scada_aggregate %>% 
    ggplot(aes(x = total_initialmw))+
    geom_histogram() +
    facet_wrap(~constraint_group)

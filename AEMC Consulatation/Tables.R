### MONASH consultation for AEMC
### Author: Matthew Katzen
### This file creates all the summary tables 

# Load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)

### Load data 
Full_Data <- fread("D:/AEMC Consultation/Data/Cleaned/Full Data - CLEAN.csv") %>% 
    mutate(settlementdate = ymd_hms(settlementdate)) 


### Create Summary function
# input: grouped data frame
# output: summary table same as that found in paper
summary_table <- function(df){
    df %>% 
        summarise(quantity = sum(dispatchmwh),
                  total_mispricing = sum(abs(rev_rrp_30 - rev_lmp_censored))/1000000,
                  adj_total_mispricing = sum(abs(rev_rrp_30 - rev_lmp0))/1000000,
                  total_overcomp = sum(rev_rrp_30 - rev_lmp_censored)/1000000,
                  adj_total_overcomp = sum(rev_rrp_30 - rev_lmp0)/1000000,
                  
                  ave_mispricing = total_mispricing * 1000000 / quantity,
                  adj_ave_mispricing = adj_total_mispricing * 1000000/ quantity,
                  ave_overcomp = total_overcomp * 1000000 / quantity,
                  adj_ave_overcomp = adj_total_overcomp * 1000000 / quantity
        )
}

#############
# Yearly 
Yearly <- Full_Data %>% group_by(year = year(settlementdate)) %>% summary_table() %>% 
    inner_join(Dispatch_Table_files %>% 
                   map(~ fread(.x)) %>% 
                   rbindlist() %>% mutate(month = ymd(month)) %>% 
                   group_by(year = year(month)) %>% 
                   summarise(total_quantity = sum(total_quantity),
                             total_revenue_rrp30 = sum(total_revenue_rrp30)),
               by = "year") %>% 
    select(year, total_quantity, constrained_quantity = quantity, total_revenue_rrp30,
           total_mispricing, adj_total_mispricing, total_overcomp, adj_total_overcomp, 
           ave_mispricing, adj_ave_mispricing, ave_overcomp,adj_ave_overcomp)

write_xlsx(Yearly, 
           "Yearly.xlsx")

#############
# 2019

Fuel_Type_2019 <- Full_Data %>% filter(year(settlementdate) == 2019) %>% 
    group_by(fuel_type) %>% summary_table() %>% 
    inner_join(Dispatch_Table_files %>% 
                   map(~ fread(.x)) %>% 
                   rbindlist() %>% mutate(month = ymd(month)) %>% 
                   filter(year(month) == 2019) %>% 
                   group_by(fuel_type) %>% 
                   summarise(total_quantity = sum(total_quantity),
                             total_revenue_rrp30 = sum(total_revenue_rrp30)),
               by = "fuel_type") %>% 
    select(fuel_type, total_quantity, constrained_quantity = quantity, total_revenue_rrp30,
           total_mispricing, adj_total_mispricing, total_overcomp, adj_total_overcomp, 
           ave_mispricing, adj_ave_mispricing, ave_overcomp,adj_ave_overcomp)

Region_2019 <- Full_Data %>% filter(year(settlementdate) == 2019) %>% 
    group_by(region) %>% summary_table() %>% 
    inner_join(Dispatch_Table_files %>% 
                   map(~ fread(.x)) %>% 
                   rbindlist() %>% mutate(month = ymd(month)) %>% 
                   filter(year(month) == 2019) %>% 
                   group_by(region) %>% 
                   summarise(total_quantity = sum(total_quantity),
                             total_revenue_rrp30 = sum(total_revenue_rrp30)),
               by = "region") %>% 
    select(region, total_quantity, constrained_quantity = quantity, total_revenue_rrp30,
           total_mispricing, adj_total_mispricing, total_overcomp, adj_total_overcomp, 
           ave_mispricing, adj_ave_mispricing, ave_overcomp,adj_ave_overcomp)

Region_Fuel_Type_2019 <- Full_Data %>% filter(year(settlementdate) == 2019) %>% 
    group_by(region, fuel_type) %>% summary_table() %>% 
    inner_join(Dispatch_Table_files %>% 
                   map(~ fread(.x)) %>% 
                   rbindlist() %>% mutate(month = ymd(month)) %>% 
                   filter(year(month) == 2019) %>% 
                   group_by(region, fuel_type) %>% 
                   summarise(total_quantity = sum(total_quantity),
                             total_revenue_rrp30 = sum(total_revenue_rrp30)),
               by = c("region", "fuel_type")) %>% 
    select(region, fuel_type, total_quantity, constrained_quantity = quantity, total_revenue_rrp30,
           total_mispricing, adj_total_mispricing, total_overcomp, adj_total_overcomp, 
           ave_mispricing, adj_ave_mispricing, ave_overcomp,adj_ave_overcomp)

Subregion_2019 <- data.frame(NA)

Participant_2019 <- Full_Data %>% filter(year(settlementdate) == 2019) %>% 
    group_by(participant) %>% summary_table() %>% 
    inner_join(Dispatch_Table_files %>% 
                   map(~ fread(.x)) %>% 
                   rbindlist() %>% mutate(month = ymd(month)) %>% 
                   filter(year(month) == 2019) %>% 
                   group_by(participant) %>% 
                   summarise(total_quantity = sum(total_quantity),
                             total_revenue_rrp30 = sum(total_revenue_rrp30)),
               by = "participant") %>% 
    select(participant, total_quantity, constrained_quantity = quantity, total_revenue_rrp30,
           total_mispricing, adj_total_mispricing, total_overcomp, adj_total_overcomp, 
           ave_mispricing, adj_ave_mispricing, ave_overcomp,adj_ave_overcomp)

Station_2019 <- Full_Data %>% filter(year(settlementdate) == 2019) %>% 
    group_by(station, fuel_type, region) %>% summary_table() %>% 
    inner_join(Dispatch_Table_files %>% 
                   map(~ fread(.x)) %>% 
                   rbindlist() %>% mutate(month = ymd(month)) %>% 
                   filter(year(month) == 2019) %>% 
                   group_by(station, fuel_type) %>% 
                   summarise(total_quantity = sum(total_quantity),
                             total_revenue_rrp30 = sum(total_revenue_rrp30)),
               by = c("station", "fuel_type")) %>% 
    select(station, fuel_type, region, total_quantity, constrained_quantity = quantity, total_revenue_rrp30,
           total_mispricing, adj_total_mispricing, total_overcomp, adj_total_overcomp, 
           ave_mispricing, adj_ave_mispricing, ave_overcomp,adj_ave_overcomp)

write_xlsx(list("Fuel Type" = Fuel_Type_2019,
                "Region" = Region_2019,
                "Region & Fuel Type" = Region_Fuel_Type_2019,
                "Subregion" = Subregion_2019,
                "Participant" = Participant_2019,
                "Station" = Station_2019), 
           "2019.xlsx")

#############
# 2018

Fuel_Type_2018 <- Full_Data %>% filter(year(settlementdate) == 2018) %>% 
    group_by(fuel_type) %>% summary_table() %>% 
    inner_join(Dispatch_Table_files %>% 
                   map(~ fread(.x)) %>% 
                   rbindlist() %>% mutate(month = ymd(month)) %>% 
                   filter(year(month) == 2018) %>% 
                   group_by(fuel_type) %>% 
                   summarise(total_quantity = sum(total_quantity),
                             total_revenue_rrp30 = sum(total_revenue_rrp30)),
               by = "fuel_type") %>% 
    select(fuel_type, total_quantity, constrained_quantity = quantity, total_revenue_rrp30,
           total_mispricing, adj_total_mispricing, total_overcomp, adj_total_overcomp, 
           ave_mispricing, adj_ave_mispricing, ave_overcomp,adj_ave_overcomp)

Region_2018 <- Full_Data %>% filter(year(settlementdate) == 2018) %>% 
    group_by(region) %>% summary_table() %>% 
    inner_join(Dispatch_Table_files %>% 
                   map(~ fread(.x)) %>% 
                   rbindlist() %>% mutate(month = ymd(month)) %>% 
                   filter(year(month) == 2018) %>% 
                   group_by(region) %>% 
                   summarise(total_quantity = sum(total_quantity),
                             total_revenue_rrp30 = sum(total_revenue_rrp30)),
               by = "region") %>% 
    select(region, total_quantity, constrained_quantity = quantity, total_revenue_rrp30,
           total_mispricing, adj_total_mispricing, total_overcomp, adj_total_overcomp, 
           ave_mispricing, adj_ave_mispricing, ave_overcomp,adj_ave_overcomp)

Region_Fuel_Type_2018 <- Full_Data %>% filter(year(settlementdate) == 2018) %>% 
    group_by(region, fuel_type) %>% summary_table() %>% 
    inner_join(Dispatch_Table_files %>% 
                   map(~ fread(.x)) %>% 
                   rbindlist() %>% mutate(month = ymd(month)) %>% 
                   filter(year(month) == 2018) %>% 
                   group_by(region, fuel_type) %>% 
                   summarise(total_quantity = sum(total_quantity),
                             total_revenue_rrp30 = sum(total_revenue_rrp30)),
               by = c("region", "fuel_type")) %>% 
    select(region, fuel_type, total_quantity, constrained_quantity = quantity, total_revenue_rrp30,
           total_mispricing, adj_total_mispricing, total_overcomp, adj_total_overcomp, 
           ave_mispricing, adj_ave_mispricing, ave_overcomp,adj_ave_overcomp)

Participant_2018 <- Full_Data %>% filter(year(settlementdate) == 2018) %>% 
    group_by(participant) %>% summary_table() %>% 
    inner_join(Dispatch_Table_files %>% 
                   map(~ fread(.x)) %>% 
                   rbindlist() %>% mutate(month = ymd(month)) %>% 
                   filter(year(month) == 2018) %>% 
                   group_by(participant) %>% 
                   summarise(total_quantity = sum(total_quantity),
                             total_revenue_rrp30 = sum(total_revenue_rrp30)),
               by = "participant") %>% 
    select(participant, total_quantity, constrained_quantity = quantity, total_revenue_rrp30,
           total_mispricing, adj_total_mispricing, total_overcomp, adj_total_overcomp, 
           ave_mispricing, adj_ave_mispricing, ave_overcomp,adj_ave_overcomp)

Station_2018 <- Full_Data %>% filter(year(settlementdate) == 2018) %>% 
    group_by(station, fuel_type, region) %>% summary_table() %>% 
    inner_join(Dispatch_Table_files %>% 
                   map(~ fread(.x)) %>% 
                   rbindlist() %>% mutate(month = ymd(month)) %>% 
                   filter(year(month) == 2018) %>% 
                   group_by(station, fuel_type) %>% 
                   summarise(total_quantity = sum(total_quantity),
                             total_revenue_rrp30 = sum(total_revenue_rrp30)),
               by = c("station", "fuel_type")) %>% 
    select(station, fuel_type, region, total_quantity, constrained_quantity = quantity, total_revenue_rrp30,
           total_mispricing, adj_total_mispricing, total_overcomp, adj_total_overcomp, 
           ave_mispricing, adj_ave_mispricing, ave_overcomp,adj_ave_overcomp)

write_xlsx(list("Fuel Type" = Fuel_Type_2018,
                "Region" = Region_2018,
                "Region & Fuel Type" = Region_Fuel_Type_2018,
                "Participant" = Participant_2018,
                "Station" = Station_2018), 
           "2018.xlsx")

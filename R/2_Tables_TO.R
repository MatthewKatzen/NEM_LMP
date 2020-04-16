#MC_Tables
#
#Create tables from MC2 dataset

# Load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
Sys.setenv(TZ='UTC')

full_vom <- fread("D:/NEM_LMP/Output/MC/full_mc_vom.csv")

#2019

table_2019_fueltype <- full_vom %>% filter(year == 2019) %>% 
    group_by(fuel_type) %>% 
    summary_table_7() 

fwrite(table_2019_fueltype, "D:/NEM_LMP/Output/Final/2019_fuel_type_vom.csv")

table_2019_fueltype %>%  mutate_if(is.double,as.integer)

table_2019_fueltype %>%  mutate_if(is.double,as.integer) %>% 
    select(fuel_type, Q, QC, TM, TMmc, AM, AMmc, RevRRP, PercTM, PercTMmc) %>% xtable()

table_2019_fueltype %>%  mutate_if(is.double,as.integer) %>% 
    select(fuel_type, Q, QC, TO, TOmc, AO, AOmc, RevRRP, PercTO, PercTOmc) %>% xtable()

#MC_graphs
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

full_mc <- fread("D:/NEM_LMP/Output/MC/full_mc.csv")

#TO
full_mc %>% group_by(year, fuel_type) %>% summary_table_6() %>% 
    ggplot(aes(x = year, y = TO, colour = fuel_type)) +
        geom_line(size = 2) +
    ggsave("D:/NEM_LMP/Output/MC/TO_fueltype.png", width = 10)

full_mc %>% group_by(year, fuel_type) %>% summary_table_6() %>% 
    ggplot(aes(x = year, y = TObar, colour = fuel_type)) +
    geom_line(size = 2) +
    ggsave("D:/NEM_LMP/Output/MC/TObar_fueltype.png", width = 10)

full_mc %>% group_by(year, fuel_type) %>% summary_table_6() %>% 
    ggplot(aes(x = year, y = TOmc, colour = fuel_type)) +
    geom_line(size = 2) +
    ggsave("D:/NEM_LMP/Output/MC/TOmc_fueltype.png", width = 10)


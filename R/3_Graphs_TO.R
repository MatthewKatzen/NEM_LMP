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

full_vom <- fread("D:/NEM_LMP/Output/MC/full_mc_vom.csv")

#TO by fuel x state
full_vom %>% group_by(year, fuel_type, region) %>% summary_table_7() %>% 
    ggplot(aes(x = year, y = TO, colour = fuel_type)) +
    geom_line(size = 2) +
    facet_wrap(~region) +
    labs(x = "Year", y = "Total Overcompensation ($m)")+
    guides(colour = guide_legend(title = "Fuel Type"))+
    ggsave("D:/NEM_LMP/Output/Final/TO_fueltype_region.png", width = 12, height = 5)

#TOmc by fuel x state
full_vom %>% group_by(year, fuel_type, region) %>% summary_table_7() %>% 
    ggplot(aes(x = year, y = TOmc, colour = fuel_type)) +
    geom_line(size = 2) +
    facet_wrap(~region) +
    labs(x = "Year", y = "Adjusted Total Overcompensation ($m)")+
    guides(colour = guide_legend(title = "Fuel Type"))+
    ggsave("D:/NEM_LMP/Output/Final/TOmc_fueltype_region.png", width = 12, height = 5)

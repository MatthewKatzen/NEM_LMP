#Create TS graphs

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

#Total Mispricing by region
Full_Data %>% mutate(year = year(settlementdate)) %>% 
    filter(year < 2020) %>% 
    group_by(region, year) %>% summary_table() %>% 
    ggplot(aes(x = year, y = total_mispricing, colour = region)) +
    geom_line()
    
#TM by region and fuel_type
Full_Data %>% mutate(year = year(settlementdate)) %>% 
    filter(year < 2020) %>% 
    group_by(region, year, fuel_type) %>% summary_table() %>% 
    ggplot(aes(x = year, y = total_mispricing, colour = fuel_type)) +
    geom_line(size = 1.5) +
    facet_wrap(~ region) 
    


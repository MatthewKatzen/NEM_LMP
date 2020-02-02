### DISTRIBUTIONAL
# Difference in revenue by fuel source in 2018
# Who wins and loses in 2018?

setwd("C:/Users/Matthew/Dropbox/Mispricing/Analysis")
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(padr)

###2018 table
mpa %>% filter(year(settlementdate) == 2018) %>% 
    group_by(fuel_type) %>% summ_all_2() %>%
    fwrite("Output/tables for paper/mispricing fuel type 2018.csv")

mpa %>% filter(year(settlementdate) == 2018) %>% mutate(c_on = (rrp<lmp)) %>% 
    group_by(fuel_type, c_on) %>% summ_all() %>% 
    select(fuel_type, c_on, quantity, dif_ave, dif_ave_0, dif_total, dif_total_0) %>% 
fwrite("Output/fuel type 2018 stats c_on-off.csv")



### Winners and losers
#winners 0
mpa %>% filter(year(settlementdate) == 2018) %>% 
    group_by(station, fuel_type) %>% summ_all() %>% 
    select(station, fuel_type, quantity, dif_ave, dif_ave_0, dif_total, dif_total_0) %>% 
    arrange(-dif_total_0) %>% .[1:10,]

#losers 0
mpa %>% filter(year(settlementdate) == 2018) %>% 
    group_by(station, fuel_type) %>% summ_all() %>% 
    select(station, fuel_type, quantity, dif_ave, dif_ave_0, dif_total, dif_total_0) %>% 
    arrange(dif_total_0) %>% .[1:10,]

#winners
mpa %>% filter(year(settlementdate) == 2018) %>% 
    group_by(station, fuel_type) %>% summ_all() %>% 
    select(station, fuel_type, quantity, dif_ave, dif_ave_0, dif_total, dif_total_0) %>% 
    arrange(-dif_total) %>% .[1:10,]

#losers
mpa %>% filter(year(settlementdate) == 2018) %>% 
    group_by(station, fuel_type) %>% summ_all() %>% 
    select(station, fuel_type, quantity, dif_ave, dif_ave_0, dif_total, dif_total_0) %>% 
    arrange(dif_total) %>% .[1:10,]

#desnity of congestion perc
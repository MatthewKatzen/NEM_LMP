setwd("C:/Users/Matthew/Dropbox/Mispricing/Analysis")
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(padr)
library(janitor)

#load data
mpa_load <- fread("D:/Thesis/Data/LOAD/mpa_final_load.csv") %>% 
    mutate(settlementdate = ymd_hms(settlementdate))

summary(mpa_load)
mpa_load$fuel_type %>% table() #only battery and hydro :)

mpa_load %>% group_by(fuel_type) %>% summ_all_2() %>% view

mpa_load$dispatchmwh %>% summary()

#compare to mpa
mpa %>% filter(year(settlementdate) == 2018) %>% 
    group_by(fuel_type) %>% summ_all_2()

mpa %>% filter(station == "Hornsdale Power Reserve")

### 2018 table fixed
mpa_load %>% filter(year(settlementdate) == 2018) %>% 
    group_by(fuel_type) %>% summ_all_2() %>%
    fwrite("Output/tables for paper/mispricing load fuel type 2018.csv")

#remove pumped hydro and battery
hydro_stations <- mpa_load %>% filter(fuel_type == "Hydro") %>% select(duid, station) %>% unique()

table_2018 <- mpa %>% filter(year(settlementdate) == 2018,
                             !(station %in% hydro_stations$station),#remove pumped hydro
                             fuel_type != "Battery") %>% #remove battery
    group_by(fuel_type) %>% summ_all_2() 

table_2018 %>% select(-fuel_type) %>% colSums() #all


#get total quantity pumped hydro
fread("D:/Thesis/Data/NEMSIGHT/2018_gen.csv") %>% 
    filter(station %in% hydro_stations$station) %>% 
    summarise(sum = sum(dispatchgwh)) 

#now just subtract that from the nemssight figures 
#(not our aggregates bc some gens have mixed fuel types which stuffs it up)

16245 - 718.38



#revenue

fread("D:/Thesis/Data/NEMSIGHT/2018_rev.csv") %>% 
    filter(station %in% hydro_stations$station) %>% 
    summarise(sum = sum(rev_m))

1408 - 90 #nemsight fuel type - nemsight station
    






#hornsdale

mpa_load %>% filter(year(settlementdate) == 2018,
                    station == "Hornsdale Power Reserve") %>% summ_all()

mpa %>% filter(year(settlementdate) == 2018,
               station == "Hornsdale Power Reserve") %>% summ_all_2()

fread("D:/Thesis/Data/NEMSIGHT/2018_gen.csv") %>%
    filter(station == "Hornsdale Power Reserve")

fread("D:/Thesis/Data/NEMSIGHT/2018_rev.csv") %>% 
    filter(station == "Hornsdale Power Reserve") 

### hornsdale data form nemsight
hornsdale <- fread("D:/Thesis/Data/NEMSIGHT/2018_hornsdale.csv") %>% 
    group_by(duid) %>% summarise(gwh = sum(mw)/(12*1000))

#sa rrp
rrp_sa_x2 <- fread("D:/Thesis/Data/NEMSIGHT/2018_sa_rrp.csv") %>% 
    rbind(fread("D:/Thesis/Data/NEMSIGHT/2018_sa_rrp.csv")) %>% select(rrp)

#hornsdale
temp <- fread("D:/Thesis/Data/NEMSIGHT/2018_hornsdale.csv") %>% 
    mutate(mwh = mw/12) %>% cbind(rrp_sa_x2) %>% 
    cbind(group = rep(1:35040, each = 6)) %>% 
    group_by(group) %>% #add RRP30
    mutate(rrp30 = mean(rrp)) %>% 
    ungroup() %>% 
    mutate(rev_rrp30 = mwh*rrp30) %>% 
    group_by(duid) %>% 
    summarise(total_rev = sum(rev_rrp30))

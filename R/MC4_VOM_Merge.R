#MC4
#




# Load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
Sys.setenv(TZ='UTC')

gas_combined <- fread("D:/NEM_LMP/Data/Cleaned/vom/gas_combined.csv") %>% 
    mutate(date = ymd(date)) #date
renewable_vom <- fread("D:/NEM_LMP/Data/Cleaned/vom/renewable_vom.csv") %>% 
    mutate(quarter = ymd(quarter))#quarter
black_coal_combined <- fread("D:/NEM_LMP/Data/Cleaned/vom/black_coal_combined.csv") %>% 
    mutate(month = ymd(month))#month
brown_coal_combined <- fread("D:/NEM_LMP/Data/Cleaned/vom/brown_coal_combined.csv") %>% 
    mutate(quarter = ymd(quarter))#quarter
liquid_fuel_combined <- fread("D:/NEM_LMP/Data/Cleaned/vom/brown_coal_combined.csv") %>% 
    mutate(quarter = ymd(quarter))#quarter

for (year in 2013:2019){
    for (j in 1:12){
        location <- paste0("D:/NEM_LMP/Data/Cleaned/INITIALMW/",year,"/Step 4 - Mutated/")#location of step 4
        files <- paste0(location, list.files(location))
        
        data_temp <- files[j] %>% map(~fread(.x)) %>% rbindlist() %>% 
            mutate(settlementdate = ymd_hms(settlementdate)) %>% 
            mutate(month = floor_date(as.Date(settlementdate), "month")) %>% 
            mutate(date = as.Date(settlementdate)) %>% 
            mutate(quarter = ymd(floor_date(month, "quarter")))
        
        if (year==2019 & j==12){#2020/01/01 is stuffing this up, just move thsoe dates to 2019/12/31
            data_temp <- files[j] %>% map(~fread(.x)) %>% rbindlist() %>% 
                mutate(settlementdate = ymd_hms(settlementdate)) %>% 
                mutate(settlementdate = data.table::fifelse(as.Date(settlementdate) == as.Date("2020-01-01"),
                                                            settlementdate - days(1),
                                                            settlementdate)) %>% 
                mutate(month = floor_date(as.Date(settlementdate), "month")) %>% 
                mutate(date = as.Date(settlementdate)) %>% 
                mutate(quarter = ymd(floor_date(month, "quarter")))
        } else{
            data_temp <- files[j] %>% map(~fread(.x)) %>% rbindlist() %>% 
                mutate(settlementdate = ymd_hms(settlementdate)) %>% 
                mutate(month = floor_date(as.Date(settlementdate), "month")) %>% 
                mutate(date = as.Date(settlementdate)) %>% 
                mutate(quarter = ymd(floor_date(month, "quarter")))
        }
        
        data_mc <- data_temp %>% 
            left_join(black_coal_combined %>% select(duid, month, mc_vom) , 
                      by = c("duid", "month")) %>% 
            left_join(gas_combined %>% select(duid, date, mc_vom), 
                      by = c("duid", "date")) %>% 
            left_join(renewable_vom %>% select(duid, quarter, mc_vom), 
                      by = c("duid", "quarter")) %>% 
            left_join(brown_coal_combined %>% select(duid, quarter, mc_vom), 
                      by = c("duid", "quarter")) %>% 
            left_join(liquid_fuel_combined %>% select(duid, quarter, mc_vom), 
                      by = c("duid", "quarter")) %>% 
            mutate(mc_vom = pmax(mc_vom.x,mc_vom.y, mc_vom, mc_vom.x.x, mc_vom.y.y, na.rm = TRUE)) %>%
            select(-c(month, date, quarter, mc_vom.y, mc_vom.x, mc_vom.x.x, mc_vom.y.y)) %>% 
            mutate(rev_lmp_mc = pmax(lmp_censored, mc_vom)*dispatchmwh)
        
        fwrite(data_mc, paste0("D:/NEM_LMP/Data/Cleaned/MC/", year,"_", j,".csv"))
        rm(data_temp)
        rm(data_mc)
    }
}

# summary_table_5
# input: df (data frame) that is grouped at (duid, year) level for cleaning
# output: measures of interest (no averages yet, as they are done at the full data level)

summary_table_5 <- function(df){
    df %>% 
        summarise(Q = sum(dispatchmwh),
                  QC = sum(dispatchmwh[local_price_adjustment != 0]),#filter out all non constrained observations
                  
                  TM = sum(abs(rev_rrp_30[local_price_adjustment != 0] - 
                                   rev_lmp_censored[local_price_adjustment != 0]))/1000000,
                  TMbar = sum(abs(rev_rrp_30[local_price_adjustment != 0] - 
                                      rev_lmp0[local_price_adjustment != 0]))/1000000,
                  TMmc = sum(abs(rev_rrp_30[local_price_adjustment != 0] - 
                                     rev_lmp_mc[local_price_adjustment != 0]))/1000000,
                  
                  TO = sum(rev_rrp_30[local_price_adjustment != 0] - 
                               rev_lmp_censored[local_price_adjustment != 0])/1000000,
                  TObar = sum(rev_rrp_30[local_price_adjustment != 0] - 
                                  rev_lmp0[local_price_adjustment != 0])/1000000,
                  TOmc = sum(rev_rrp_30[local_price_adjustment != 0] - 
                                 rev_lmp_mc[local_price_adjustment != 0])/1000000,
                  RevRRP = sum(rev_rrp_30)/1000000
                  
        )
}

#calculate monthly tables


location <- "D:/NEM_LMP/Data/Cleaned/MC/"
files <- paste0(location, list.files(location))

for (file in files){
    file_name <- substr(file, 28, 34)
    data_temp <- file %>% fread() %>% 
        mutate(year = substr(file, 28, 31)) %>% 
        group_by(duid, year, station, participant, fuel_type, region) %>% 
        summary_table_5()
    fwrite(data_temp, paste0("D:/NEM_LMP/Output/MC/monthly/", file_name, ".csv"))
}

#merge all tables

summary_table_6 <- function(df){
    df %>% summarise(Q = sum(Q),
                     QC = sum(QC),
                     
                     TM = sum(TM),
                     TMbar = sum(TMbar),
                     TMmc = sum(TMmc),
                     
                     TO = sum(TO),
                     TObar = sum(TObar),
                     TOmc = sum(TOmc),
                     RevRRP = sum(RevRRP))
    
}

location <- "D:/NEM_LMP/Output/MC/monthly/"
files <- paste0(location, list.files(location))
full_vom <- files %>% map(~fread(.x)) %>% rbindlist() #%>% group_by(duid, year, station, fuel_type, region) %>% summary_table_6()

fwrite(full_vom, "D:/NEM_LMP/Output/MC/full_mc_vom.csv")


#MC2
#
#Merge all MC data with Clean2 (dispatch etc) and add LMPmc



# Load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
Sys.setenv(TZ='UTC')

monthly_mc <- fread("D:/NEM_LMP/Output/MC/gen_monthly_mc.csv")%>% 
    mutate(month = as.Date(month))
gas_mc <- fread("D:/NEM_LMP/Output/MC/gas_mc.csv") %>% select(duid, date, mc) %>% 
    mutate(date = as.Date(date))

for (year in 2017:2019){
    for (j in 1:12){
        location <- paste0("D:/NEM_LMP/Data/Cleaned/INITIALMW/",year,"/Step 4 - Mutated/")#location of step 4
        files <- paste0(location, list.files(location))
        rm(data_temp)
        data_temp <- files[j] %>% map(~fread(.x)) %>% rbindlist() %>% 
            mutate(settlementdate = ymd_hms(settlementdate)) %>% 
            mutate(month = floor_date(as.Date(settlementdate), "month")) %>% 
            mutate(date = as.Date(settlementdate))
        
        data_mc <- data_temp %>% 
            left_join(monthly_mc , 
                      by = c("duid", "month")) %>% 
            left_join(gas_mc %>% select(duid, date, mc), 
                      by = c("duid", "date")) %>% 
            mutate(mc = case_when(is.na(mc.x) & is.numeric(mc.y) ~ mc.y,
                                  is.na(mc.y) & is.numeric(mc.x) ~ mc.x,
                                  is.na(mc.x) & is.na(mc.y) ~ 0)) %>% 
            mutate(mc = ifelse(is.na(mc),
                               0,
                               mc)) %>% #zero didnt come through for some reason
            select(-c(month, date, mc.x, mc.y)) %>% 
            mutate(rev_lmp_mc = pmax(lmp_censored, mc)*dispatchmwh)
        
        fwrite(data_mc, paste0("D:/NEM_LMP/Data/Cleaned/MC/", year,"_", j,".csv"))
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

#calculate monthyl tables


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

location <- "D:/NEM_LMP/Output/MC/monthly/"
files <- paste0(location, list.files(location))
full_mc <- files %>% map(~fread(.x)) %>% rbindlist() 

fwrite(full_mc, "D:/NEM_LMP/Output/MC/full_mc.csv")


### MONASH consultation for AEMC
### Author: Matthew Katzen
### This file cleans and merges the datasets

# Load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)

### Load data 
data_location <- "D:/AEMC Consultation/Data/Raw"

LPA <- fread(paste0(data_location, "/dispatch_local_price_24-01-2020.csv")) %>% clean_names() %>% 
    mutate(settlementdate = ymd_hms(settlementdate)) 

Price <- fread(paste0(data_location, "/dispatchprice_24-01-2020.csv")) %>% clean_names() %>% 
    filter(intervention == 0) %>% 
    mutate(region = substr(regionid, 1, nchar(regionid)-1))%>% 
    mutate(settlementdate = ymd_hms(settlementdate))%>% 
    select(-regionid) %>% 
    group_by(interval = cut(settlementdate, breaks = "30 min"), region) %>% #add RRP30 
    mutate(rrp30 = mean(rrp)) %>% 
    as.data.frame() %>% 
    filter(intervention == 0)#remove all intervention prices, they dont actually change anything

Zones <- read_excel(paste0(data_location, "/Zonal mapping with DUID.xlsx")) %>% 
    .[!duplicated(.$duid),]#only keep one of each duid 



### Fix Price dataset

(duplicated(Price %>% filter(intervention == 0) %>% select(settlementdate, regionid))) %>% which() 
        #all duplicates ar caused by intervention. So does anything change with intervention pricing?

temp <- Price %>% group_by(settlementdate, regionid) %>% filter(n() > 1) %>% as.data.frame() 
        #keep only duplicates
    
temp2 <- temp %>% select(-intervention) %>% #remove intervention
    group_by(settlementdate, regionid) %>% filter(n() > 1) %>% as.data.frame() 

rm(temp, temp2)
        #temp and temp2 are the same t/f repeat run duplications ar just caused by intervention==1 => remove int==1. 



### Fix Zones Dataset
        #seems like some duid have multiple generator names, lets just remove duplicated duid for now and only keep one
        #row of each


#why is merged smaller than LPA?
(LPA$duid %>% unique())[(!((LPA$duid %>% unique()) %in% Zones$duid)) %>% which()]#missing duids in Zones
Zones$duid %>% sort()
        #b/c they have been decommissioned. Just use NEMSIGHT_Details dataset from NEMSIGHT



#Use NEMSIGHT data instead of AEMC

NEMSIGHT_Details <- fread("D:/AEMC Consultation/Data/NEMSIGHT_Details.csv") %>% 
    select(-c("ConnectionPtId", "Thermal Efficiency", "Auxiliaries", "Emission Intensity Sent-Out", 
              "Capacity")) %>% 
    rename(REGIONID = Region) %>% 
    mutate(Region = case_when(REGIONID == "Queensland" ~ "QLD",
                              REGIONID == "New South Wales" ~ "NSW",
                              REGIONID == "Victoria" ~ "VIC",
                              REGIONID == "South Australia" ~ "SA",
                              REGIONID == "Tasmania" ~ "TAS")) %>% 
    clean_names() %>% 
    select(-regionid) 


### Merge

clean_data_location <- "D:/AEMC Consultation/Data/Cleaned/Monthly"

Merged_1 <- LPA %>% inner_join(NEMSIGHT_Details, by = "duid") #add gen details

Merged_2 <- Merged_1 %>% inner_join(Price, by = c("settlementdate", "region")) #add price data

#Add dispatch quantity (seperate monthly files)
Dispatch_files <- paste0(data_location, "/output/", list.files(paste0(data_location, "/output"))) %>% 
    .[!str_detect(.,pattern="dispatchload_partial_2020-01.csv")] %>% #remove 2020 csv
    .[103:114] #only 2018


for (file_name in Dispatch_files){
    temp <- Merged_2 %>% inner_join(fread(file_name) %>% mutate(settlementdate = ymd_hms(settlementdate)) %>% 
                                        filter(intervention == 0),#remove int==1 
                            by = c("settlementdate", "duid"))
    fwrite(temp, paste0(clean_data_location, "/", substr(file_name, 38, nchar(file_name)-4), "-CLEAN.csv"))
}


# merge months together
Merged_3 <- paste0(clean_data_location, "/", list.files(clean_data_location)) %>% 
    map(~ fread(.x, stringsAsFactors = FALSE)) %>% 
    rbindlist() %>% 
    mutate(settlementdate = ymd_hms(settlementdate)) 

#add revenue calculations
Merged_4 <- Merged_3 %>% 
    mutate(dispatchmwh = totalcleared/12) %>% #convert MW to MWh 
    mutate(rev_rrp_30 = rrp30*dispatchmwh) %>% 
    mutate(lmp = rrp + local_price_adjustment) %>% 
    mutate(rev_lmp = lmp*dispatchmwh) %>% 
    mutate(rev_lmp0 = pmax(lmp, 0)*dispatchmwh) 


### 2018 TABLE

summ_all_2 <- function(df){
    df %>% 
        summarise(quantity = ifelse(sum(dispatchmwh)>0,
                                    sum(dispatchmwh),
                                    NA),
        
                  total_mispricing = sum(abs(rev_rrp_30 - rev_lmp)),
                  adj_total_mispricing = sum(abs(rev_rrp_30 - rev_lmp0)),
                  total_overcomp = sum(rev_rrp_30 - rev_lmp),
                  adj_total_overcomp = sum(rev_rrp_30 - rev_lmp0),
                  
                  ave_mispricing = total_mispricing / quantity,
                  adj_ave_mispricing = adj_total_mispricing / quantity,
                  ave_overcomp = total_overcomp / quantity,
                  adj_ave_overcomp = adj_total_overcomp / quantity
        )
}

table_2018 <- Merged_4 %>% filter(type == "Gen") %>% 
    group_by(fuel_type) %>% summ_all_2()


#

temp <- fread("D:/AEMC Consultation/Data/Raw/output/dispatchload_2018-01.csv") %>% 
    mutate(settlementdate = ymd_hms(settlementdate))

tail(temp %>% filter(intervention ==1))

temp %>% filter(settlementdate == ymd_hms("2018-01-12 20:35:00 UTC"), duid == "YWPS4")

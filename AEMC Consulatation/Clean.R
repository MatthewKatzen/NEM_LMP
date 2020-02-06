### MONASH consultation for AEMC
### Author: Matthew Katzen
# This file cleans and merges the datasets
# Justification for some decisions are found in Justification.R They are referenced in this document as (J#) 

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
    filter(intervention == 0) %>% #remove all intervention prices, they dont actually change anything (J1)
    mutate(region = substr(regionid, 1, nchar(regionid)-1))%>% 
    mutate(settlementdate = ymd_hms(settlementdate))%>% 
    select(-regionid) %>% 
    group_by(interval = cut(settlementdate, breaks = "30 min"), region) %>% #add RRP30 
    mutate(rrp30 = mean(rrp)) %>% 
    as.data.frame()

Zones <- read_excel(paste0(data_location, "/Zonal mapping with DUID.xlsx")) %>% 
    .[!duplicated(.$duid),]#only keep one of each duid 

NEMSIGHT_Details <- fread("D:/AEMC Consultation/Data/RAW/NEMSIGHT_Details.csv") %>% #using nemsight as it gives us feul_type
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



################
###Merge

clean_data_location <- "D:/AEMC Consultation/Data/Cleaned/Monthly"

Merged_1 <- LPA %>% inner_join(NEMSIGHT_Details, by = "duid") #add gen details

Merged_2 <- Merged_1 %>% inner_join(Price, by = c("settlementdate", "region")) #add price data

#Add dispatch quantity (seperate monthly files)

Dispatch_files <- paste0(data_location, "/output/", list.files(paste0(data_location, "/output"))) %>% 
    .[43:126] #only 2013 - 2019

for (file_name in Dispatch_files){
    Merged_2 %>% inner_join(fread(file_name) %>% mutate(settlementdate = ymd_hms(settlementdate)) %>% 
                                filter(intervention == 0),#remove int==1 
                            by = c("settlementdate", "duid")) %>% 
        fwrite(paste0(clean_data_location, "/", substr(file_name, 38, nchar(file_name)-4), "-CLEAN.csv"))
}


# merge months together
Merged_3 <- paste0(clean_data_location, "/", list.files(clean_data_location)) %>% 
    map(~ fread(.x, stringsAsFactors = FALSE)) %>% 
    rbindlist() %>% 
    mutate(settlementdate = ymd_hms(settlementdate)) 

#add censored and revenue calculations

Merged_4 <- Merged_3 %>% select(settlementdate, duid, local_price_adjustment, type, station, participant, fuel_type,
                                region, rrp, rrp30, totalcleared) %>% 
    mutate(dispatchmwh = totalcleared/12, #convert MW to MWh 
           lmp = rrp + local_price_adjustment, #lmp calculation
           lmp_censored = case_when((lmp < -1000) ~ (-1000), #censoring
                                    (lmp > 14200) ~ 14200,
                                    TRUE ~ lmp)) %>% 
    mutate(rev_lmp_censored = lmp_censored*dispatchmwh,
           rev_rrp_30 = rrp30*dispatchmwh, 
           rev_lmp0 = pmax(lmp_censored, 0)*dispatchmwh) %>%
    filter(type == "Gen") #only keep gens (remove loads)

fwrite(Merged_4, "D:/AEMC Consultation/Data/Cleaned/Full Data - CLEAN.csv")

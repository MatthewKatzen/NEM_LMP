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
data_location <- "D:/AEMC Consultation/Data"

LPA <- fread(paste0(data_location, "/dispatch_local_price_24-01-2020.csv")) %>% clean_names() %>% 
    mutate(settlementdate = ymd_hms(settlementdate))

Price <- fread(paste0(data_location, "/dispatchprice_24-01-2020.csv")) %>% clean_names() %>% 
    filter(intervention == 0) %>%
    mutate(region = substr(regionid, 1, nchar(regionid)-1))%>% 
    mutate(settlementdate = ymd_hms(settlementdate))

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
    clean_names()


### Merge

Merged_1 <- LPA %>% merge(NEMSIGHT_Details, by = "duid")

Merged_2 <- Merged_1 %>% merge(Price, by = c("settlementdate", "region"))

Dispatch_files <- paste0(data_location, "/output/", list.files(paste0(data_location, "/output"))) %>% 
    .[!str_detect(.,pattern="dispatchload_partial_2020-01.csv")] %>% #remove 2020 csv
    .[126]

for (file_name in Dispatch_files){
    Merged_2 %>% merge(fread(file_name), by = c("settlementdate", "duid"))
}

%>% #only first 2 files used for now
    map(~ fread(.x)) %>% 
    rbindlist() %>% 
    mutate(settlementdate = ymd_hms(settlementdate))
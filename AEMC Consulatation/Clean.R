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
### Mispricing

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

pumped_hydro_stations <- NEMSIGHT_Details %>% filter(type == "Load" & fuel_type == "Hydro") %>% select(station) %>% 
    unique() %>% .[['station']]

#add charging censored and revenue calculations
Merged_4 <- Merged_3 %>% select(settlementdate, duid, local_price_adjustment, type, station, participant, fuel_type,
                                region, rrp, rrp30, totalcleared) %>% 
    mutate(fuel_type = case_when(((station %in% pumped_hydro_stations) & type == "Load") ~ 
                                     "Pumped Hydro (Charge)", #deal with loads
                                 ((station %in% pumped_hydro_stations) & type == "Gen") ~ 
                                     "Pumped Hydro (Discharge)",
                                 (!(station %in% pumped_hydro_stations) & type == "Gen" & fuel_type == "Hydro") ~ 
                                     "Hydro (Discharge)",
                                 (fuel_type == "Battery" & type == "Load") ~ "Battery (Charge)",
                                 (fuel_type == "Battery" & type == "Gen") ~ "Battery (Discharge)",
                                 TRUE ~ fuel_type),
           totalcleared = case_when(type == "Load" ~ (-totalcleared),
                                    type == "Gen" ~ totalcleared)) %>% 
    mutate(dispatchmwh = totalcleared/12, #convert MW to MWh 
           lmp = rrp + local_price_adjustment, #lmp calculation
           lmp_censored = case_when((lmp < -1000) ~ (-1000), #censoring
                                    (lmp > 14200) ~ 14200,
                                    TRUE ~ lmp)) %>% 
    mutate(rev_lmp_censored = lmp_censored*dispatchmwh,
           rev_rrp_30 = rrp30*dispatchmwh, 
           rev_lmp0 = pmax(lmp_censored, 0)*dispatchmwh)

fwrite(Merged_4, "D:/AEMC Consultation/Data/Cleaned/Full Data - CLEAN.csv")

#############
# Total dispatch and rev

Dispatch_files <- paste0(data_location, "/output/", list.files(paste0(data_location, "/output"))) %>% 
    .[43:126] #only 2013 - 2019

clean_dispatch_location <- "D:/AEMC Consultation/Data/Cleaned/Dispatch Monthly"

for (file_name in Dispatch_files){
    fread(file_name) %>% filter(totalcleared > 0) %>% 
        mutate(settlementdate = ymd_hms(settlementdate)) %>% 
        inner_join(NEMSIGHT_Details, by = "duid") %>% 
        inner_join(Price, by = c("settlementdate", "region")) %>% 
        fwrite(paste0(clean_dispatch_location, "/", substr(file_name, 38, nchar(file_name)-4), " - Dispatch CLEAN.csv"))
}

pumped_hydro_stations <- NEMSIGHT_Details %>% filter(type == "Load" & fuel_type == "Hydro") %>% select(station) %>% 
    unique() %>% .[['station']]

Dispatch_Cleaned_files <- paste0(clean_dispatch_location, "/", list.files(clean_dispatch_location))

clean_dispatch_table_location <- "D:/AEMC Consultation/Data/Cleaned/Dispatch Monthly Tables"

# get summary for each month
for (file_name in Dispatch_Cleaned_files){
    file_name %>% 
        fread() %>% 
        mutate(settlementdate = ymd_hms(settlementdate)) %>% 
        select(settlementdate, duid, type, station, participant, fuel_type,
               region, rrp, rrp30, totalcleared) %>% 
        mutate(fuel_type = case_when(((station %in% pumped_hydro_stations) & type == "Load") ~ 
                                         "Pumped Hydro (Charge)", #deal with loads
                                     ((station %in% pumped_hydro_stations) & type == "Gen") ~ 
                                         "Pumped Hydro (Discharge)",
                                     (!(station %in% pumped_hydro_stations) & type == "Gen" & fuel_type == "Hydro") ~ 
                                         "Hydro (Discharge)",
                                     (fuel_type == "Battery" & type == "Load") ~ "Battery (Charge)",
                                     (fuel_type == "Battery" & type == "Gen") ~ "Battery (Discharge)",
                                     TRUE ~ fuel_type),
               totalcleared = case_when(type == "Load" ~ (-totalcleared),
                                        type == "Gen" ~ totalcleared)) %>% 
        mutate(dispatchmwh = totalcleared/12,  #convert MW to MWh 
               month = ymd(paste0(substr(file_name, 65,71),"-01"))) %>% #add month variable
        group_by(month, duid, station, participant, type, fuel_type, region) %>% 
        summarise(total_quantity = sum(dispatchmwh),
                  total_revenue_rrp30 = sum(dispatchmwh * rrp30)) %>% 
        fwrite(paste0(clean_dispatch_table_location, "/", 
                      substr(file_name, 52, nchar(file_name)-21), 
                      " - Dispatch Table.csv"))
}


Dispatch_Table_files <- paste0(clean_dispatch_table_location, "/", list.files(clean_dispatch_table_location))

Yearly_Tables <- Dispatch_Table_files %>% 
    map(~ fread(.x)) %>% 
    rbindlist() %>% mutate(month = ymd(month)) %>% 
    group_by()



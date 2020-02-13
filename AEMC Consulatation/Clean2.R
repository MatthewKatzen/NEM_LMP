# Load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
Sys.setenv(TZ='UTC')

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

Subregions <- fread(paste0(data_location, "/Zonal mapping UPDATED.csv")) %>% 
    .[!duplicated(.$duid),] %>% 
    rename(subregion = zone)#only keep one of each duid 

NEMSIGHT_Details <- fread("D:/AEMC Consultation/Data/RAW/NEMSIGHT_Details.csv") %>% 
    #using nemsight as it gives us feul_type
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


###########
#  2019

Clean_dipatch_files_location <- "D:/AEMC Consultation/Data/Raw/Nemweb/"
Dispatch_files <- paste0(Clean_dipatch_files_location, list.files(paste0(Clean_dipatch_files_location)))
Step_1_Location <- "D:/AEMC Consultation/Data/Cleaned/INITIALMW/2019/Step 1 - LPA/"

LPA <- fread(paste0(data_location, "/dispatch_local_price_24-01-2020.csv")) %>% clean_names() %>% 
    mutate(settlementdate = ymd_hms(settlementdate)) %>% 
    select(-locally_constrained)

for(file_name in Dispatch_files[13:24]){
    file_name %>% fread() %>% filter(initialmw > 0) %>% 
        mutate(settlementdate = ymd_hms(settlementdate)) %>% 
        group_by(settlementdate, duid) %>% 
        filter(intervention == max(intervention),
               #if intervention, keep it and remove int==0
               initialmw > 0) %>% 
        ungroup() %>% 
        left_join(LPA, by = c("settlementdate", "duid")) %>% 
        mutate(local_price_adjustment = if_else(is.na(local_price_adjustment), 0, local_price_adjustment)) %>% 
        fwrite(paste0(Step_1_Location, substr(file_name, 38, 43), " - STEP 1.csv"))
}

Step_1_files <- paste0(Step_1_Location, list.files(paste0(Step_1_Location)))
Step_2_Location <- "D:/AEMC Consultation/Data/Cleaned/INITIALMW/2019/Step 2 - Details/"

NEMSIGHT_Details <- fread("D:/AEMC Consultation/Data/RAW/NEMSIGHT_Details.csv") %>% 
    #using nemsight as it gives us feul_type
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

for(file_name in Step_1_files){
    file_name %>% fread() %>% 
        mutate(settlementdate = ymd_hms(settlementdate)) %>% 
        inner_join(NEMSIGHT_Details, by = "duid") %>% 
        fwrite(paste0(Step_2_Location, 
                      substr(file_name, nchar(file_name) - 18, 
                                              nchar(file_name) - 13), 
                      " - STEP 2.csv"))
}

Step_2_files <- paste0(Step_2_Location, list.files(paste0(Step_2_Location)))
Step_3_Location <- "D:/AEMC Consultation/Data/Cleaned/INITIALMW/2019/Step 3 - Price/"

Price <- fread(paste0(data_location, "/dispatchprice_24-01-2020.csv")) %>% clean_names() %>% 
    filter(intervention == 0) %>% #remove all intervention prices, they dont actually change anything (J1)
    mutate(region = substr(regionid, 1, nchar(regionid)-1))%>% 
    mutate(settlementdate = ymd_hms(settlementdate))%>% 
    select(-regionid) %>% 
    group_by(interval = cut(settlementdate, breaks = "30 min"), region) %>% #add RRP30 
    mutate(rrp30 = mean(rrp)) %>% 
    as.data.frame()

for(file_name in Step_2_files){
    file_name %>% fread() %>% 
        mutate(settlementdate = ymd_hms(settlementdate)) %>% 
        inner_join(Price, by = c("settlementdate", "region")) %>% 
        fwrite(paste0(Step_3_Location, 
                      substr(file_name, nchar(file_name) - 18, 
                             nchar(file_name) - 13), 
                      " - STEP 3.csv"))
}


pumped_hydro_stations <- NEMSIGHT_Details %>% filter(type == "Load" & fuel_type == "Hydro") %>% select(station) %>% 
    unique() %>% .[['station']]

Step_3_files <- paste0(Step_3_Location, list.files(paste0(Step_3_Location)))
Step_4_Location <- "D:/AEMC Consultation/Data/Cleaned/INITIALMW/2019/Step 4 - Mutated/"

for(file_name in Step_3_files){
    file_name %>% fread() %>% 
        select(settlementdate, duid, local_price_adjustment, type, station, participant, fuel_type,
               region, rrp, rrp30, initialmw) %>% 
        mutate(settlementdate = ymd_hms(settlementdate)) %>% 
        mutate(fuel_type = case_when(((station %in% pumped_hydro_stations) & type == "Load") ~ 
                                         "Pumped Hydro (Charge)", #deal with loads
                                     ((station %in% pumped_hydro_stations) & type == "Gen") ~ 
                                         "Pumped Hydro (Release)",
                                     (!(station %in% pumped_hydro_stations) & type == "Gen" & fuel_type == "Hydro") ~ 
                                         "Hydro",
                                     (fuel_type == "Battery" & type == "Load") ~ "Battery (Charge)",
                                     (fuel_type == "Battery" & type == "Gen") ~ "Battery (Discharge)",
                                     TRUE ~ fuel_type),
               initialmw = case_when(type == "Load" ~ (-initialmw),
                                        type == "Gen" ~ initialmw)) %>% 
        mutate(dispatchmwh = initialmw/12, #convert MW to MWh 
               lmp = rrp + local_price_adjustment, #lmp calculation
               lmp_censored = case_when((lmp < -1000) ~ (-1000), #censoring
                                        ((lmp > 14700) & 
                                             settlementdate %within% interval(ymd_hms("2019-07-01 00:05:00 UTC"), 
                                                                              ymd_hms("2020-07-01 00:00:00 UTC"))) ~ 
                                            14700,
                                        ((lmp > 14500) & 
                                             settlementdate %within% interval(ymd_hms("2018-07-01 00:05:00 UTC"), 
                                                                              ymd_hms("2019-07-01 00:00:00 UTC"))) ~ 
                                            14500,
                                        TRUE ~ lmp)) %>% 
        mutate(rev_lmp_censored = lmp_censored*dispatchmwh,
               rev_rrp_30 = rrp30*dispatchmwh, 
               rev_lmp0 = pmax(lmp_censored, 0)*dispatchmwh) %>% 
        fwrite(paste0(Step_4_Location, 
                       substr(file_name, nchar(file_name) - 18, 
                              nchar(file_name) - 13), 
                       " - STEP 4.csv"))
}


Step_4_files <- paste0(Step_4_Location, list.files(paste0(Step_4_Location)))


###########
#  2018

Clean_dipatch_files_location <- "D:/AEMC Consultation/Data/Raw/Nemweb/"
Dispatch_files <- paste0(Clean_dipatch_files_location, list.files(paste0(Clean_dipatch_files_location)))
Step_1_Location <- "D:/AEMC Consultation/Data/Cleaned/INITIALMW/2018/Step 1 - LPA/"

LPA <- fread(paste0(data_location, "/dispatch_local_price_24-01-2020.csv")) %>% clean_names() %>% 
    mutate(settlementdate = ymd_hms(settlementdate)) %>% 
    select(-locally_constrained)

for(file_name in Dispatch_files[1:12]){
    file_name %>% fread() %>% filter(initialmw > 0) %>% 
        mutate(settlementdate = ymd_hms(settlementdate)) %>% 
        group_by(settlementdate, duid) %>% 
        filter(intervention == max(intervention),
               #if intervention, keep it and remove int==0
               initialmw > 0) %>% 
        ungroup() %>% 
        left_join(LPA, by = c("settlementdate", "duid")) %>% 
        mutate(local_price_adjustment = if_else(is.na(local_price_adjustment), 0, local_price_adjustment)) %>% 
        fwrite(paste0(Step_1_Location, substr(file_name, 38, 43), " - STEP 1.csv"))
}

Step_1_files <- paste0(Step_1_Location, list.files(paste0(Step_1_Location)))
Step_2_Location <- "D:/AEMC Consultation/Data/Cleaned/INITIALMW/2018/Step 2 - Details/"

NEMSIGHT_Details <- fread("D:/AEMC Consultation/Data/RAW/NEMSIGHT_Details.csv") %>% 
    #using nemsight as it gives us feul_type
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

for(file_name in Step_1_files){
    file_name %>% fread() %>% 
        mutate(settlementdate = ymd_hms(settlementdate)) %>% 
        inner_join(NEMSIGHT_Details, by = "duid") %>% 
        fwrite(paste0(Step_2_Location, 
                      substr(file_name, nchar(file_name) - 18, 
                             nchar(file_name) - 13), 
                      " - STEP 2.csv"))
}

Step_2_files <- paste0(Step_2_Location, list.files(paste0(Step_2_Location)))
Step_3_Location <- "D:/AEMC Consultation/Data/Cleaned/INITIALMW/2018/Step 3 - Price/"

Price <- fread(paste0(data_location, "/dispatchprice_24-01-2020.csv")) %>% clean_names() %>% 
    filter(intervention == 0) %>% #remove all intervention prices, they dont actually change anything (J1)
    mutate(region = substr(regionid, 1, nchar(regionid)-1))%>% 
    mutate(settlementdate = ymd_hms(settlementdate))%>% 
    select(-regionid) %>% 
    group_by(interval = cut(settlementdate, breaks = "30 min"), region) %>% #add RRP30 
    mutate(rrp30 = mean(rrp)) %>% 
    as.data.frame()

for(file_name in Step_2_files){
    file_name %>% fread() %>% 
        mutate(settlementdate = ymd_hms(settlementdate)) %>% 
        inner_join(Price, by = c("settlementdate", "region")) %>% 
        fwrite(paste0(Step_3_Location, 
                      substr(file_name, nchar(file_name) - 18, 
                             nchar(file_name) - 13), 
                      " - STEP 3.csv"))
}


pumped_hydro_stations <- NEMSIGHT_Details %>% filter(type == "Load" & fuel_type == "Hydro") %>% select(station) %>% 
    unique() %>% .[['station']]

Step_3_files <- paste0(Step_3_Location, list.files(paste0(Step_3_Location)))
Step_4_Location <- "D:/AEMC Consultation/Data/Cleaned/INITIALMW/2018/Step 4 - Mutated/"

for(file_name in Step_3_files){
    file_name %>% fread() %>% 
        select(settlementdate, duid, local_price_adjustment, type, station, participant, fuel_type,
               region, rrp, rrp30, initialmw) %>% 
        mutate(settlementdate = ymd_hms(settlementdate)) %>% 
        mutate(fuel_type = case_when(((station %in% pumped_hydro_stations) & type == "Load") ~ 
                                         "Pumped Hydro (Charge)", #deal with loads
                                     ((station %in% pumped_hydro_stations) & type == "Gen") ~ 
                                         "Pumped Hydro (Release)",
                                     (!(station %in% pumped_hydro_stations) & type == "Gen" & fuel_type == "Hydro") ~ 
                                         "Hydro",
                                     (fuel_type == "Battery" & type == "Load") ~ "Battery (Charge)",
                                     (fuel_type == "Battery" & type == "Gen") ~ "Battery (Discharge)",
                                     TRUE ~ fuel_type),
               initialmw = case_when(type == "Load" ~ (-initialmw),
                                     type == "Gen" ~ initialmw)) %>% 
        mutate(dispatchmwh = initialmw/12, #convert MW to MWh 
               lmp = rrp + local_price_adjustment, #lmp calculation
               lmp_censored = case_when((lmp < -1000) ~ (-1000), #censoring
                                        ((lmp > 14700) & 
                                             settlementdate %within% interval(ymd_hms("2019-07-01 00:05:00 UTC"), 
                                                                              ymd_hms("2020-07-01 00:00:00 UTC"))) ~ 
                                            14700,
                                        ((lmp > 14500) & 
                                             settlementdate %within% interval(ymd_hms("2018-07-01 00:05:00 UTC"), 
                                                                              ymd_hms("2019-07-01 00:00:00 UTC"))) ~ 
                                            14500,
                                        ((lmp > 14200) & 
                                             settlementdate %within% interval(ymd_hms("2017-07-01 00:05:00 UTC"), 
                                                                              ymd_hms("2018-07-01 00:00:00 UTC"))) ~ 
                                            14200,
                                        ((lmp > 14000) & 
                                             settlementdate %within% interval(ymd_hms("2016-07-01 00:05:00 UTC"), 
                                                                              ymd_hms("2017-07-01 00:00:00 UTC"))) ~ 
                                            14000,
                                        ((lmp > 13800) & 
                                             settlementdate %within% interval(ymd_hms("2015-07-01 00:05:00 UTC"), 
                                                                              ymd_hms("2016-07-01 00:00:00 UTC"))) ~ 
                                            13800,
                                        ((lmp > 13500) & 
                                             settlementdate %within% interval(ymd_hms("2014-07-01 00:05:00 UTC"), 
                                                                              ymd_hms("2015-07-01 00:00:00 UTC"))) ~ 
                                            13500,
                                        ((lmp > 13100) & 
                                             settlementdate %within% interval(ymd_hms("2012-07-01 00:05:00 UTC"), 
                                                                              ymd_hms("2014-07-01 00:00:00 UTC"))) ~ 
                                            13100,
                                        TRUE ~ lmp)) %>% 
        mutate(rev_lmp_censored = lmp_censored*dispatchmwh,
               rev_rrp_30 = rrp30*dispatchmwh, 
               rev_lmp0 = pmax(lmp_censored, 0)*dispatchmwh) %>% 
        fwrite(paste0(Step_4_Location, 
                      substr(file_name, nchar(file_name) - 18, 
                             nchar(file_name) - 13), 
                      " - STEP 4.csv"))
}


Step_4_files <- paste0(Step_4_Location, list.files(paste0(Step_4_Location)))
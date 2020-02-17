#AEMC Consultation
#Author: Matthew Katzen (MONASH)

#Clean1.R loads all data and outputs LMPs for analysis
#This is done in 4 seperate steps to make the process a bit more manageable for my PC. Perhaps it could be merged into a single run on a PC with more ram, or with smarter merging.

# Load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
Sys.setenv(TZ='UTC')

### Load datasets
#################

data_location <- "D:/AEMC Consultation/Data/Raw"

Clean_dipatch_files_location <- "D:/AEMC Consultation/Data/Raw/INITIALMW/"
All_Dispatch_files <- paste0(Clean_dipatch_files_location, list.files(Clean_dipatch_files_location))

LPA <- fread(paste0(data_location, "/dispatch_local_price_24-01-2020.csv")) %>% clean_names() %>% 
    mutate(settlementdate = ymd_hms(settlementdate)) %>% 
    select(-locally_constrained)

generator_details <- fread("D:/AEMC Consultation/Data/RAW/generator_details.csv") %>% #data which contains fuel type for each duid
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

Price <- fread(paste0(data_location, "/dispatchprice_24-01-2020.csv")) %>% clean_names() %>% 
    filter(intervention == 0) %>% #remove all intervention prices, they dont actually change anything. Just an indicator
                                    #for intervention dispatch quantity (which is definitely different)
    mutate(region = substr(regionid, 1, nchar(regionid)-1))%>% 
    mutate(settlementdate = ymd_hms(settlementdate))%>% 
    select(-regionid) %>% 
    group_by(interval = cut(settlementdate, breaks = "30 min"), region) %>% #add RRP30 
    mutate(rrp30 = mean(rrp)) %>% 
    as.data.frame()

pumped_hydro_stations <- generator_details %>% filter(type == "Load" & fuel_type == "Hydro") %>% select(station) %>% 
    unique() %>% .[['station']]

### Loop through years
#########################


years <- 2013:2019
for (year in years){
    
    #step 1: start with dispatch, remove non-intervention runs, merge with LPA
    Step_1_Location <- paste0("D:/AEMC Consultation/Data/Cleaned/INITIALMW/",year,"/Step 1 - LPA/")
    Dispatch_files <- All_Dispatch_files[grepl(year, All_Dispatch_files)]
    
    for(file_name in Dispatch_files){
        file_name %>% fread() %>% filter(initialmw > 0) %>% 
            mutate(settlementdate = ymd_hms(settlementdate)) %>% 
            group_by(settlementdate, duid) %>% 
            filter(intervention == max(intervention),
                   #if intervention, keep it and remove int==0
                   initialmw > 0) %>% 
            ungroup() %>% 
            left_join(LPA, by = c("settlementdate", "duid")) %>% 
            mutate(local_price_adjustment = if_else(is.na(local_price_adjustment), 0, local_price_adjustment)) %>% 
            fwrite(paste0(Step_1_Location, 
                          substr(file_name, nchar(file_name) - 10, 
                                 nchar(file_name) - 4), 
                          " - STEP 1.csv"))
    }
    
    #step 2: merge with generator_details to get duid characteristics 
    Step_1_files <- paste0(Step_1_Location, list.files(paste0(Step_1_Location)))
    Step_2_Location <- paste0("D:/AEMC Consultation/Data/Cleaned/INITIALMW/",year,"/Step 2 - Details/")
    
    for(file_name in Step_1_files){
        file_name %>% fread() %>% 
            mutate(settlementdate = ymd_hms(settlementdate)) %>% 
            inner_join(generator_details, by = "duid") %>% 
            fwrite(paste0(Step_2_Location, 
                          substr(file_name, nchar(file_name) - 19, 
                                 nchar(file_name) - 13), 
                          " - STEP 2.csv"))
    }
    
    #step3: merge with price data (rrp)
    Step_2_files <- paste0(Step_2_Location, list.files(paste0(Step_2_Location)))
    Step_3_Location <- paste0("D:/AEMC Consultation/Data/Cleaned/INITIALMW/",year,"/Step 3 - Price/")
    
    for(file_name in Step_2_files){
        file_name %>% fread() %>% 
            mutate(settlementdate = ymd_hms(settlementdate)) %>% 
            inner_join(Price, by = c("settlementdate", "region")) %>% 
            fwrite(paste0(Step_3_Location, 
                          substr(file_name, nchar(file_name) - 19, 
                                 nchar(file_name) - 13), 
                          " - STEP 3.csv"))
    }
    
    #step 4: final cleaning and censoring of lmps
    Step_3_files <- paste0(Step_3_Location, list.files(paste0(Step_3_Location)))
    Step_4_Location <- paste0("D:/AEMC Consultation/Data/Cleaned/INITIALMW/",year,"/Step 4 - Mutated/")
    
    for(file_name in Step_3_files){
        file_name %>% fread() %>% 
            select(settlementdate, duid, local_price_adjustment, type, station, participant, fuel_type,
                   region, rrp, rrp30, initialmw) %>% 
            mutate(settlementdate = ymd_hms(settlementdate)) %>% 
            mutate(fuel_type = case_when(((station %in% pumped_hydro_stations) & type == "Load") ~ 
                                             "Pumped Hydro (Pumping)", #deal with loads
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
                   lmp_censored = case_when((lmp < -1000) ~ (-1000), #censoring at price cap and floor
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
                                                 settlementdate %within% interval(ymd_hms("2013-07-01 00:05:00 UTC"), 
                                                                                  ymd_hms("2014-07-01 00:00:00 UTC"))) ~ 
                                                13100,
                                            ((lmp > 12900) & 
                                                 settlementdate %within% interval(ymd_hms("2012-07-01 00:05:00 UTC"), 
                                                                                  ymd_hms("2013-07-01 00:00:00 UTC"))) ~ 
                                                12900,
                                            TRUE ~ lmp)) %>% 
            mutate(rev_lmp_censored = lmp_censored*dispatchmwh,
                   rev_rrp_30 = rrp30*dispatchmwh, 
                   rev_lmp0 = pmax(lmp_censored, 0)*dispatchmwh) %>% 
            fwrite(paste0(Step_4_Location, 
                          substr(file_name, nchar(file_name) - 19, 
                                 nchar(file_name) - 13), 
                          " - STEP 4.csv"))
    }
}

# Load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)

summary_table_2 <- function(df){
    df %>% 
        summarise(Q = sum(dispatchmwh),
                  QC = sum(dispatchmwh[local_price_adjustment != 0]),
                  
                  TM = sum(abs(rev_rrp_30[local_price_adjustment != 0] - 
                                                 rev_lmp_censored[local_price_adjustment != 0]))/1000000,
                  TMbar = sum(abs(rev_rrp_30[local_price_adjustment != 0] - 
                                                     rev_lmp0[local_price_adjustment != 0]))/1000000,
                  TO = sum(rev_rrp_30[local_price_adjustment != 0] - 
                                           rev_lmp_censored[local_price_adjustment != 0])/1000000,
                  TObar = sum(rev_rrp_30[local_price_adjustment != 0] - 
                                               rev_lmp0[local_price_adjustment != 0])/1000000,
                  
                  AM = TM * 1000000 / abs(QC),
                  AMbar = TMbar * 1000000/ abs(QC),
                  AO = TO * 1000000 / abs(QC),
                  AObar = TObar * 1000000 / abs(QC),
                  
                  RevRRP = sum(rev_rrp_30)/1000000,
                  
                  PercTM = TM/RevRRP*100,
                  PercTMbar = TMbar/RevRRP*100,
                  PercTO = TO/RevRRP*100,
                  PercTObar = TO/RevRRP*100
                  
        )
}

data_location <- "D:/AEMC Consultation/Data/Raw"

Subregions <- fread(paste0(data_location, "/Zonal mapping UPDATED.csv")) %>% 
    .[!duplicated(.$duid),] %>% 
    rename(subregion = zone)#only keep one of each duid 

#########
# 2019

Step_4_Location <- "D:/AEMC Consultation/Data/Cleaned/INITIALMW/2019/Step 4 - Mutated/"
Step_4_files <- paste0(Step_4_Location, list.files(paste0(Step_4_Location)))

data_2019 <- Step_4_files %>% map(~fread(.x)) %>% rbindlist()

table_2019_fuel_type <- data_2019 %>% 
    group_by(FuelType = fuel_type) %>% summary_table_2()

table_2019_region <- data_2019 %>% 
    group_by(Region = region) %>% summary_table_2()

table_2019_region_fuel_type <- data_2019 %>% 
    group_by(Region = region, FuelType = fuel_type) %>% summary_table_2()

table_2019_station <- data_2019 %>% 
    left_join(Subregions %>% select(-region), by = "duid") %>% 
    group_by(Station = station, Participant = participant, FuelType = fuel_type, 
             Region = region, Subregion = subregion) %>% 
    summary_table_2()

table_2019_subregion <- data_2019 %>% 
    inner_join(Subregions %>% select(-region), by = "duid")  %>% 
    group_by(subregion) %>% summary_table_2()

write_xlsx(list("Fuel Type" = table_2019_fuel_type,
                "Region" = table_2019_region,
                "Region & Fuel Type" = table_2019_region_fuel_type,
                "Subregion" = table_2019_subregion,
                "Station" = table_2019_station), 
           "2019.xlsx")

years <- 2013:2018
for (year in years){
    Step_4_Location <- paste0("D:/AEMC Consultation/Data/Cleaned/INITIALMW/",year,"/Step 4 - Mutated/")
    Step_4_files <- paste0(Step_4_Location, list.files(paste0(Step_4_Location)))
    
    data_temp <- Step_4_files %>% map(~fread(.x)) %>% rbindlist()
    
    temp_fuel_type <- data_temp %>% 
        group_by(FuelType = fuel_type) %>% summary_table_2()
    
    temp_region <- data_temp %>% 
        group_by(Region = region) %>% summary_table_2()
    
    temp_region_fuel_type <- data_temp %>% 
        group_by(Region = region, FuelType = fuel_type) %>% summary_table_2()
    
    temp_station <- data_temp %>% 
        group_by(Station = station, Participant = participant, FuelType = fuel_type, Region = region) %>% 
        summary_table_2()
    
    write_xlsx(list("Fuel Type" = temp_fuel_type,
                    "Region" = temp_region,
                    "Region & Fuel Type" = temp_region_fuel_type,
                    "Station" = temp_station), 
               paste0(year,".xlsx"))
}

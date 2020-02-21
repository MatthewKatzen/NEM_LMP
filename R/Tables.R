#AEMC Consultation
#Author: Matthew Katzen (MONASH)

# Tables.R creates the relevant worksheets requested.
# Load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
Sys.setenv(TZ='UTC')

### Load data
data_full <- fread("D:/AEMC Consultation/Output/NonMCfull.csv")

# summary_table_4
#input: df (data frame) that is grouped at level of interest (e.g. fuel_type, region) and has already been run through        summary_table_3 at yearly level
#output: all measures of interest 
summary_table_4 <- function(df){
    df %>% 
        summarise(Q = sum(Q),
                  QC = sum(QC),
                  
                  TM = sum(TM),
                  TMbar = sum(TMbar),
                  TO = sum(TO),
                  TObar = sum(TObar),
                  
                  AM = TM*1000000/abs(QC),
                  AMbar = TMbar*1000000/abs(QC),
                  AO = TO*1000000/abs(QC),
                  AObar = TObar*1000000/abs(QC),
                  
                  RevRRP = sum(RevRRP),
                  
                  PercTM = TM/RevRRP*100,
                  PercTMbar = TMbar/RevRRP*100,
                  PercTO = TO/RevRRP*100,
                  PercTObar = TObar/RevRRP*100
        )
}

#e.g.
#data_full %>% filter(year == 2013) %>% group_by(region) %>% 
#    summary_table_4()


data_location <- "D:/AEMC Consultation/Data/Raw"

Subregions <- fread("D:/AEMC Consultation/Data/Raw/Zonal mapping UPDATED.csv") %>% 
    .[!duplicated(.$duid),] %>% #only keep one of each duid 
    rename(subregion = zone)

### Create Table worksheets
##############


write_xlsx(list("Fuel Type" = data_full %>% filter(year == 2019) %>% group_by(fuel_type) %>% summary_table_4(),
                "Region" = data_full %>% filter(year == 2019) %>% group_by(region) %>% summary_table_4(),
                "Region & Fuel Type" = data_full %>% filter(year == 2019) %>% 
                    group_by(region, fuel_type) %>% summary_table_4(),
                "Subregion" = data_full %>% filter(year == 2019) %>% 
                    left_join(Subregions %>% select(-region), by = "duid") %>% 
                    group_by(subregion) %>% summary_table_4(),
                "Station" = data_full %>% filter(year == 2019) %>% 
                    left_join(Subregions %>% select(-region), by = "duid") %>% 
                    group_by(station, participant, fuel_type, 
                             region, subregion) %>% summary_table_4()), 
           "2019.xlsx")

for (i in c(2013:2018)){
    write_xlsx(list("Fuel Type" = data_full %>% filter(year == i) %>% group_by(fuel_type) %>% summary_table_4(),
                    "Region" = data_full %>% filter(year == i) %>% group_by(region) %>% summary_table_4(),
                    "Region & Fuel Type" = data_full %>% filter(year == i) %>% 
                        group_by(region, fuel_type) %>% summary_table_4(),
                    "Station" = data_full %>% filter(year == i) %>% 
                        group_by(station, participant, fuel_type, 
                                 region) %>% summary_table_4()), 
               paste0(i, ".xlsx"))    
}


#MC_Tables
#
#Create tables from MC2 dataset

# Load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
Sys.setenv(TZ='UTC')

summary_table_7 <- function(df){
    df %>% 
        summarise(Q = sum(Q),
                  QC = sum(QC),
                  
                  TM = sum(TM),
                  TMbar = sum(TMbar),
                  TMmc = sum(TMmc),
                  
                  TO = sum(TO),
                  TObar = sum(TObar),
                  TOmc = sum(TOmc),
                  
                  AM = TM*1000000/abs(QC),
                  AMbar = TMbar*1000000/abs(QC),
                  AMmc = TMmc*1000000/abs(QC),
                  
                  AO = TO*1000000/abs(QC),
                  AObar = TObar*1000000/abs(QC),
                  AOmc = TOmc*1000000/abs(QC),
                  
                  RevRRP = sum(RevRRP),
                  
                  PercTM = TM/RevRRP*100,
                  PercTMbar = TMbar/RevRRP*100,
                  PercTMmcr = TMmc/RevRRP*100,
                  
                  PercTO = TO/RevRRP*100,
                  PercTObar = TObar/RevRRP*100,
                  PercTOmc = TOmc/RevRRP*100
        )
}

full_vom <- fread("D:/NEM_LMP/Output/MC/full_mc_vom.csv")

full_mc %>%  
    group_by(year, fuel_type) %>% 
    summary_table_7() %>% 
    fwrite("D:/NEM_LMP/Output/MC/year_fuel_type_vom.csv")

full_mc %>%  
    group_by(year, region) %>% 
    summary_table_7() %>% 
    fwrite("D:/NEM_LMP/Output/MC/year_region_vom.csv")



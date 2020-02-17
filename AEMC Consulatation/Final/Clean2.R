#AEMC Consultation
#Author: Matthew Katzen (MONASH)

#Clean2.R uses cleaned files from Clean1.R (step 4) to aggregate measures at yearly level

# Load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
Sys.setenv(TZ='UTC')

# summary_table_3
# input: df (data frame) that is grouped at (duid, year) level for cleaning
# output: measures of interest (no averages yet, as they are done at the full data level)
 
summary_table_3 <- function(df){
    df %>% 
        summarise(Q = sum(dispatchmwh),
                  QC = sum(dispatchmwh[local_price_adjustment != 0]),#filter out all non constrained observations
                  
                  TM = sum(abs(rev_rrp_30[local_price_adjustment != 0] - 
                                   rev_lmp_censored[local_price_adjustment != 0]))/1000000,
                  TMbar = sum(abs(rev_rrp_30[local_price_adjustment != 0] - 
                                      rev_lmp0[local_price_adjustment != 0]))/1000000,
                  TO = sum(rev_rrp_30[local_price_adjustment != 0] - 
                               rev_lmp_censored[local_price_adjustment != 0])/1000000,
                  TObar = sum(rev_rrp_30[local_price_adjustment != 0] - 
                                  rev_lmp0[local_price_adjustment != 0])/1000000,
                  RevRRP = sum(rev_rrp_30)/1000000
                  
        )
}

output_location <- "D:/AEMC Consultation/Output/"

for (year in c(2013:2019)){
    location <- paste0("D:/AEMC Consultation/Data/Cleaned/INITIALMW/",year,"/Step 4 - Mutated/")#location of step 4
    files <- paste0(location, list.files(location))
    data_temp <- files %>% map(~fread(.x)) %>% rbindlist() %>% 
        mutate(year = year) %>% 
        group_by(duid, year, station, participant, fuel_type, region) %>% 
        summary_table_3()
    fwrite(data_temp, paste0(output_location, year, ".csv"))
}

yearly_files <- paste0(output_location,list.files(output_location))

data_full <- yearly_files %>% map(~fread(.x)) %>% rbindlist()

fwrite(data_full, paste0(output_location, "full.csv"))

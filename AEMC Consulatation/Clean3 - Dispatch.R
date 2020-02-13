# Get all dispatch data from nemweb
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
Sys.setenv(TZ='UTC')


dispatch_fun <- function(yearmonth){
    external_data_location <- "D:/AEMC Consultation/Data/Raw/Nemweb/Dispatchload" #for big data
    year <- substr(yearmonth, 1, 4)
    month <- substr(yearmonth, 5, 6)
    url <- 0
    csv_name <- paste0(external_data_location, "/PUBLIC_DVD_DISPATCHLOAD_", yearmonth, "010000.CSV")
    if(!file.exists(csv_name)){
        url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_", 
                      year, "_", month, 
                      "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCHLOAD_",yearmonth,
                      "010000.zip")
        temp <- tempfile()
        download.file(url, temp, mode="wb", method = "curl")
        unzip(temp, paste0("PUBLIC_DVD_DISPATCHLOAD_", yearmonth, "010000.CSV"),
              exdir = external_data_location)
    }
    dispatch <- fread(csv_name, sep=",", skip=1, stringsAsFactors = FALSE)
    dispatch <- dispatch %>%
        clean_names() %>% 
        select(duid, settlementdate, intervention, initialmw) %>%
        mutate(settlementdate = ymd_hms(settlementdate))
    if(url != 0){
        unlink(temp) #delete zip
    }    
}
for(i in c("01","02","03","04","05","06","07","08","09","10","11","12")){
    dispatch_fun(paste0("2019",i))
}

# Merge

Raw_Dispatch_Location <- "D:/Thesis/Data/Dispatch/"
Raw_Dispatch_Files <- paste0(Raw_Dispatch_Location, list.files(paste0(Raw_Dispatch_Location)))

Clean_dipatch_files_location <- "D:/AEMC Consultation/Data/Raw/Nemweb/"

for(file_name in Raw_Dispatch_Files[43:102]){
    fread(file_name) %>% select(SETTLEMENTDATE,DUID,INTERVENTION,INITIALMW) %>% clean_names() %>% 
    fwrite(paste0(Clean_dipatch_files_location, substr(file_name, 49, 54),".csv"))
}



#Functions

### EQS
### 

eqs.fun <- function(effective.ym) {
    external.data.location <- "D:/NEM_LMP/Data/Raw/EQS"  #for big data
    e.year <- substr(effective.ym, 1, 4)
    e.month <- substr(effective.ym, 5, 6)
    csv.name <- paste0(external.data.location, "/PUBLIC_DVD_GENERICCONSTRAINTRHS_", effective.ym, 
                       "010000.CSV")
    url <- 0 #initialise
    if(!file.exists(csv.name)){
        url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", e.year,"/MMSDM_",
                      e.year, "_", e.month, 
                      "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_GENERICCONSTRAINTRHS_", 
                      effective.ym, "010000.zip")
        temp <- tempfile()
        download.file(url, temp, mode="wb", method = "curl")
        unzip(temp, paste0("PUBLIC_DVD_GENERICCONSTRAINTRHS_", effective.ym, "010000.CSV"), 
              exdir = external.data.location)
    }
    #Clean EQS
    eqs <- read.csv(csv.name, sep=",", skip=1, stringsAsFactors = FALSE) %>% #load csv
        select(GENCONID, EFFECTIVEDATE, SCOPE, SPD_ID, SPD_TYPE, FACTOR) %>% #keep cols we are interested in
        filter(SCOPE == "DS") %>% #only care about dispatch constraints 
        #distinct() %>% #remove duplicate rows
        filter(SPD_TYPE %in% c("I,T")) %>%  #only get scale value, interconnector, and generator/load data
        mutate(EFFECTIVEDATE = ymd_hms(EFFECTIVEDATE)) %>% 
        mutate(DUID = str_replace(SPD_ID, "\\..*","")) %>% #remove SPD_ID left of `.`
        select(-SPD_ID)
    if(url != 0){ #checks if previous if statement was run
        unlink(temp) #delete zip
    }
    return(eqs)
}

### RHS
### 

#gets rhs and values for one month period
rhs.fun <- function(yearmonth){
    external.data.location <- "D:/NEM_LMP/Data/Raw/RHS" 
    year <- substr(yearmonth, 1, 4)
    month <- substr(yearmonth, 5, 6)
    url <- 0 #initialise
    #check if already downloaded
    csv.name <- paste0(external.data.location,"/PUBLIC_DVD_DISPATCHCONSTRAINT_", yearmonth, 
                       "010000.CSV")
    if(!file.exists(csv.name)){
        url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_",
                      year, "_",month, 
                      "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCHCONSTRAINT_",
                      yearmonth, "010000.zip")
        temp <- tempfile()
        download.file(url, temp, mode="wb", method = "curl") #download zip
        unzip(temp, paste0("PUBLIC_DVD_DISPATCHCONSTRAINT_", yearmonth, "010000.CSV"), 
              exdir = external.data.location) #unzip[ zipped file and save csv to external storage
    }
    #Clean RHS and MV
    rhs <- read.csv(csv.name, sep=",", skip=1, stringsAsFactors = FALSE)
    rhs <- rhs %>% filter(MARGINALVALUE != 0) %>% #remove unconstrained
        filter(substr(CONSTRAINTID,1,1) %in% c('Q','N','V','S','T','I')) %>%  #no fcas and other weird types
        mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE))
    if(url != 0){
        unlink(temp) #delete zip
    }    
    return(rhs)#save locally
}

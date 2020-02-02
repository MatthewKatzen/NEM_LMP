#Loads

duid_details_load <- fread("D:/Thesis/Data/NEMSIGHT/duid_details.csv") %>% filter(Type == "Load") %>% 
    select(-c("ConnectionPtId", "Thermal Efficiency", "Auxiliaries", "Emission Intensity Sent-Out", 
              "Capacity")) %>% 
    rename(REGIONID = Region) %>% 
    mutate(REGIONID = case_when(REGIONID == "Queensland" ~ "QLD1",
                                REGIONID == "New South Wales" ~ "NSW1",
                                REGIONID == "Victoria" ~ "VIC1",
                                REGIONID == "South Australia" ~ "SA1",
                                REGIONID == "Tasmania" ~ "TAS1"))

fwrite(duid_details_load,"D:/Thesis/Data/LOAD/duid_details_load_cleaned.csv")

mpa_nodisp_load <- fread("D:/Thesis/Data/MPA/mpa_cleaned.csv", stringsAsFactors = FALSE, drop = 1) %>% #mpa
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)) %>% 
    filter(SETTLEMENTDATE %within% interval(ymd_hms("2009-07-01 00:05:00"), 
                                            ymd_hms("2019-07-01 00:00:00"))) %>% 
    inner_join(fread("D:/Thesis/Data/LOAD/duid_details_load_cleaned.csv"),#duid details
               by = "DUID") %>%  
    inner_join(fread("D:/Thesis/Data/RRP/rrpfull_unique.csv", stringsAsFactors = FALSE) %>% #rrp
                   mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)), 
               by = c("SETTLEMENTDATE", "REGIONID"))

fwrite(mpa_nodisp_load, "D:/Thesis/Data/LOAD/mpa_nodisp_load.csv")

#loop through years to add dispatch
for (i in year){
    temp <- fread("D:/Thesis/Data/LOAD/mpa_nodisp_load.csv") %>% 
        inner_join(fread(paste0("D:/Thesis/Data/DISPATCH/yearly/dispatch_initial_", i, ".csv"),
                         stringsAsFactors = FALSE),
                   by = c("DUID", "SETTLEMENTDATE")) #dispatch
    
    fwrite(temp, paste0("D:/Thesis/Data/LOAD/yearly/mpa_initial_LOAD", i, ".csv"))
}

#merge each year
data_location <- "D:/Thesis/Data/LOAD/yearly"
files <- paste0(data_location, "/", list.files(data_location))

mpa_complete_load <- files %>% map(~ fread(.x, stringsAsFactors = FALSE)) %>% 
    rbindlist() %>% mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)) %>% 
    filter(INITIALMW != 0)

fwrite(mpa_complete_load, "D:/Thesis/Data/LOAD/mpa_complete_load.csv")

#add revenue
mpa_exp_load <- fread("D:/Thesis/Data/LOAD/mpa_complete_load.csv") %>% 
    mutate(DISPATCHMWH = INITIALMW/12) %>% #convert MW to MWh
    mutate(Rev_RRP_30 = RRP30*DISPATCHMWH) %>% 
    mutate(Rev_LMP = LMP*DISPATCHMWH) %>% 
    mutate(Rev_LMP0 = pmax(LMP, 0)*DISPATCHMWH) %>% 
    mutate(Rev_DIF = Rev_LMP - Rev_RRP_30) %>%   #how much you benefit from change to LMP system
    mutate(Rev_DIF_0 = pmax(Rev_LMP, Rev_LMP0) - Rev_RRP_30) %>%  #assume no neg LMP (no neg bids)
    mutate(STATE = case_when(REGIONID == "QLD1" ~ "QLD",
                             REGIONID == "NSW1" ~ "NSW",
                             REGIONID == "VIC1" ~ "VIC",
                             REGIONID == "SA1" ~ "SA",
                             REGIONID == "TAS1" ~ "TAS")) %>% 
    select(-REGIONID) %>% 
    clean_names()#clean up colnames



fwrite(mpa_exp_load, "D:/Thesis/Data/LOAD/mpa_final_load.csv")

mpa_exp_load %>% mutate(settlementdate = ymd_hms(settlementdate)) %>% head()



#load data
mpa_load <- fread("D:/Thesis/Data/LOAD/mpa_final_load.csv") %>% 
    mutate(settlementdate = ymd_hms(settlementdate))



#playing around
mpa %>% filter(initialmw< (0)) %>% select(dispatchmwh) %>% summary()
mpa %>% filter(initialmw!=(0)) %>% .[,'initialmw'] %>% as.numeric() %>% hist()

mpa %>% filter(initialmw<(0)) %>% .[,'initialmw'] %>% cut(breaks=c(-26:0)) %>% table() 

mpa_exp_load %>% filter(initialmw<0)



mpa_complete_load %>% filter(Station == "Tumut 3") %>% tail()

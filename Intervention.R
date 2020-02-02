# YEARS
year <- as.character(c(2009:2019))
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
yearmonths <- NULL
for (i in year){
    if (i == "2009"){
        temp <- paste0(i, months[7:12])
    } else if (i == "2019"){
        temp <- paste0(i, months[1:6])
    } else{
        temp <- paste0(i, months)
    }
    yearmonths <- c(yearmonths,temp)
}

#intervention pricing
rrp_int <- yearmonths %>% map(~ rrp_fun(.x, int = 1)) %>% 
    rbindlist() %>% 
    clean_names

fwrite(rrpfull_int, "D:/Thesis/Data/RRP/rrpfull_int.csv")

#read data
rrp_int <- fread("D:/Thesis/Data/RRP/rrpfull_int.csv") %>% 
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)) %>% 
    clean_names() %>% 
    mutate(rrp_int = rrp) %>% select(settlementdate, regionid, rrp_int)

rrpfull <- fread("D:/Thesis/Data/RRP/rrpfull.csv") %>% 
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)) %>% 
    clean_names() %>% 
    mutate(rrp_int = rrp) %>% select(settlementdate, regionid, rrp)


rrp_compare <- inner_join(rrp_int, rrpfull, by = c("settlementdate", "regionid"))

rrp_compare <- rrp_compare %>% mutate(dif = rrp_int - rrp) %>% 
    mutate(state = case_when(regionid == "QLD1" ~ "QLD",
                             regionid == "NSW1" ~ "NSW",
                             regionid == "VIC1" ~ "VIC",
                             regionid == "SA1" ~ "SA",
                             regionid == "TAS1" ~ "TAS")) %>% 
    select(-regionid)

mpa_int <- inner_join(mpa %>% select(settlementdate, state, lmp, rrp30, initialmw, dispatchmwh), 
                      rrp_compare
                      by = c("settlementdate", "state")) 

head(mpa_int)

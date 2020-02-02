
temp <- mpa %>% select(duid, station, fuel_type) %>% unique()
temp %>% select(station, fuel_type) %>% unique() %>% select(station) %>% duplicated() %>% which()
temp %>% filter(station == "Swanbank")
table_2018_duid %>% filter(duid == "SWAN_E")

#table by duid
table_all_duid <- mpa %>% mutate(year = year(settlementdate)) %>% 
    group_by(year, duid, fuel_type, station) %>% summ_all_2()
    
table_all_station <- mpa %>% mutate(year = year(settlementdate)) %>% 
    group_by(year, station, fuel_type) %>% summ_all_2()

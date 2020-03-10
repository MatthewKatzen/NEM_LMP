q_cs_dispatch <- qld_data %>% filter(duid %in% q_cs_duids)%>% 
    group_by(settlementdate) %>% summarise(dispatchmw = sum(dispatchmwh*12)) %>% 
    mutate(year = year(settlementdate) %>% as.character()) 
    
q_cs_dispatch %>% filter(settlementdate %within% interval(ymd_hms("2019-07-01 00:00:00 UTC"), 
                                                          ymd_hms("2019-12-31 00:00:00 UTC")),
                         year %in% c(2018,2019)) %>% 
    ggplot(aes(x = dispatchmw, group = year, colour = year)) + stat_ecdf()

#no kinks, where is the 1100 limit??
(q_cs_dispatch$dispatchmw < 1200) %>% sum()

#Nothing interesting going on, prbably just leave it
#https://aemo.com.au/-/media/files/electricity/nem/security_and_reliability/congestion-information/transfer-limit-advice-system-strength.pdf?la=en
#https://aemo.com.au/Market-Notices?marketNoticeQuery=60252&marketNoticeFacets=

#up to 11/12/2017 max 1200
#11/12/17 max 1295
#5/12/18 1295-1460
#16/9/19 1300 - 1750
generator_details <- fread("D:/NEM_LMP/Data/generator_details_cleaned")

#When does SS start binding?

constraints_quarterly <- fread("D:/Thesis/Data/NEMSIGHT/constraints_quarterly.csv") %>% clean_names()
constraints_quarterly %>% filter(substr(id,1,2)=="S_") %>% arrange(-num_events)
constraints_quarterly %>% filter(id %in% c("S_WIND_1200_AUTO", "S_NIL_STRENGTH_1", "S_SA_WIND_1200"))

    #S_WIND_1200_AUTO created 8/09/2017, S_SA_WIND_1200 created 7/08/2018
    #first onyl active in 2017q3, second only active 2017q3/4
    #S_NIL_STRENGTH_1 ffrom 18q1 for all non-synchronous

### CDF data
wind <- fread("D:/Thesis/Data/NEMSIGHT/wind_output.csv", header = TRUE) %>% 
    mutate(settlementdate = dmy_hm(settlementdate)) 
location <- "D:/NEM_LMP/Data/Cleaned/MC/"
files <- paste0(location, list.files(location))

dispatch_data <- files %>% map(~fread(.x) %>% 
                                   select(settlementdate, fuel_type, region, dispatchmwh) %>% 
                                   group_by(settlementdate, fuel_type, region) %>% 
                                   summarise(total_disp = sum(dispatchmwh))) %>% 
    rbindlist() #grouped dispatch totals

wind_and_solar <- wind %>% left_join(dispatch_data %>% filter(fuel_type == "Solar", region == "SA") %>% 
                                         mutate(initialmw = total_disp*12,
                                                settlementdate = ymd_hms(settlementdate)),
                                     by = "settlementdate") %>% #add total Wind (NEMSIGHT) to Solar (NEM)
    mutate(initialmw.y = ifelse(is.na(initialmw.y),
                                0,
                                initialmw.y)) %>% 
    mutate(total_initialmw = initialmw.x + initialmw.y)

temp <- wind_and_solar %>% 
    filter(settlementdate %within% interval(ymd_hms("2017-01-01 00:05:00 UTC"), ymd_hms("2020-01-01 00:00:00 UTC"))) %>% 
    mutate(constraint_group = case_when(
        settlementdate %within% interval(ymd_hms("2017-08-07 00:05:00 UTC"), ymd_hms("2017-12-11 00:00:00 UTC")) ~ 
            "7/8/2017 - 10/12/2017",
        settlementdate %within% interval(ymd_hms("2017-12-11 00:05:00 UTC"), ymd_hms("2018-12-05 00:00:00 UTC")) ~ 
            "11/12/2017 - 4/12/2018",
        settlementdate %within% interval(ymd_hms("2018-12-05 00:05:00 UTC"),ymd_hms("2020-01-01 00:00:00 UTC")) ~ 
            "5/12/2018 - 31/12/2019",
        TRUE ~ "1/1/2017 - 6/8/2017"))
temp$constraint_group <- factor(temp$constraint_group, 
                                levels = c("1/1/2017 - 6/8/2017", "7/8/2017 - 10/12/2017", "11/12/2017 - 4/12/2018", 
                                           "5/12/2018 - 31/12/2019"),
                                labels = c("1/1/2017 - 6/8/2017", "7/8/2017 - 10/12/2017", "11/12/2017 - 4/12/2018",
                                           "5/12/2018 - 31/12/2019"))

#CDF graph
temp %>% 
    ggplot(aes(x = total_initialmw, group = constraint_group, colour = constraint_group)) + stat_ecdf() +
    geom_vline(xintercept = 1200, linetype = "dashed")+
    geom_vline(xintercept = 1295, linetype = "dashed")+
    guides(colour = guide_legend(title="Constraint Group")) +
    ylab("CDF") +
    xlab("MW") +
    ggsave("D:/NEM_LMP/Output/CDF/ALL SA Wind and Solar3.png", width = 7)

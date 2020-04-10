#Old cdfs before new about Scada data to get non-scheduled wind


### CDF data

#generate data

location <- "D:/NEM_LMP/Data/Cleaned/MC/"
files <- paste0(location, list.files(location))

sa_solar <- files %>% map(~fread(.x) %>% 
                              select(settlementdate, fuel_type, region, dispatchmwh) %>%
                              filter(fuel_type == "Solar", region == "SA") %>% 
                              group_by(settlementdate, fuel_type) %>% 
                              summarise(total_disp = sum(dispatchmwh))) %>% 
    rbindlist() #grouped dispatch totals

fwrite(sa_solar, "D:/NEM_LMP/Data/Cleaned/Misc/SA_Solar.csv")

sa_wind_solar <- files %>% map(~fread(.x) %>% 
                                   select(settlementdate, duid, fuel_type, region, dispatchmwh) %>%
                                   filter(fuel_type %in% c("Wind","Solar"), region == "SA")) %>% 
    rbindlist() %>% 
    mutate(settlementdate = ymd_hms(settlementdate))


fwrite(sa_wind_solar, "D:/NEM_LMP/Data/Cleaned/Misc/sa_wind_solar.csv")


#load data
SA_Wind <- fread("D:/NEM_LMP/Data/Raw/NEMSIGHT/wind_output.csv", header = TRUE) %>% #bc we only have synchronous duid data
    mutate(settlementdate = dmy_hm(settlementdate)) 
sa_solar <- fread("D:/NEM_LMP/Data/Cleaned/Misc/SA_Solar.csv", header = TRUE) %>% 
    mutate(settlementdate = ymd_hms(settlementdate)) 

#

wind_and_solar <- SA_Wind %>% left_join(SA_Solar  %>% 
                                            mutate(initialmw = total_disp*12,
                                                   settlementdate = ymd_hms(settlementdate)),
                                        by = "settlementdate") %>% #add total Wind (NEMSIGHT) to Solar (NEM)
    mutate(initialmw.y = ifelse(is.na(initialmw.y),
                                0,
                                initialmw.y)) %>% 
    mutate(total_initialmw = initialmw.x + initialmw.y)


#CDF graph

wind_and_solar_2 <- wind_and_solar %>% 
    filter(settlementdate %within% interval(ymd_hms("2017-01-01 00:05:00 UTC"), ymd_hms("2020-01-01 00:00:00 UTC"))) %>% 
    mutate(constraint_group = case_when(
        settlementdate %within% interval(ymd_hms("2017-08-07 00:05:00 UTC"), ymd_hms("2017-12-11 00:00:00 UTC")) ~ 
            "7/8/2017 - 10/12/2017",
        settlementdate %within% interval(ymd_hms("2017-12-11 00:05:00 UTC"), ymd_hms("2018-12-05 00:00:00 UTC")) ~ 
            "11/12/2017 - 4/12/2018",
        settlementdate %within% interval(ymd_hms("2018-12-05 00:05:00 UTC"),ymd_hms("2020-01-01 00:00:00 UTC")) ~ 
            "5/12/2018 - 31/12/2019",
        TRUE ~ "1/1/2017 - 6/8/2017"))

wind_and_solar_2$constraint_group <- factor(wind_and_solar_2$constraint_group, 
                                            levels = c("1/1/2017 - 6/8/2017", "7/8/2017 - 10/12/2017", "11/12/2017 - 4/12/2018", 
                                                       "5/12/2018 - 31/12/2019"),
                                            labels = c("1/1/2017 - 6/8/2017", "7/8/2017 - 10/12/2017", "11/12/2017 - 4/12/2018",
                                                       "5/12/2018 - 31/12/2019"))

wind_and_solar_2 %>% 
    ggplot(aes(x = total_initialmw, group = constraint_group, colour = constraint_group)) + stat_ecdf() +
    geom_vline(xintercept = 1200, linetype = "dashed")+
    geom_vline(xintercept = 1295, linetype = "dashed")+
    guides(colour = guide_legend(title="Constraint Group")) +
    ylab("CDF") +
    xlab("MW") 

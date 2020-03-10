#CDF of wind

location <- "D:/NEM_LMP/Data/Cleaned/MC/"
files <- paste0(location, list.files(location))

dispatch_data <- files %>% map(~fread(.x) %>% 
                           select(settlementdate, fuel_type, region, dispatchmwh) %>% 
                           group_by(settlementdate, fuel_type, region) %>% 
                           summarise(total_disp = sum(dispatchmwh))) %>% 
    rbindlist() #grouped dispatch totals

fwrite(dispatch_data, "D:/NEM_LMP/Data/Cleaned/dispatch_fuel_region_settlementdate.csv")
dispatch_data <- fread("D:/NEM_LMP/Data/Cleaned/dispatch_fuel_region_settlementdate.csv") %>% 
    mutate(settlementdate = ymd_hms(settlementdate))


#quarterly

dispatch_data %>% filter(fuel_type %in% c("Wind", "Solar"), region == "SA") %>% 
    group_by(settlementdate) %>% #combine solar and wind at settlementdate
    summarise(total_disp = sum(total_disp)) %>% 
    mutate(year = settlementdate %>% year() %>% as.factor(),
           quarter = settlementdate %>% ymd_hms() %>% as.Date() %>% quarter()) %>% 
    ggplot(aes(x = total_disp*12, group=year, colour = year)) + stat_ecdf() +
    facet_wrap(~quarter) +
    ylab("CDF") +
    xlab("MW") +
    ggtitle("SA Wind and Solar Quarterly") +
    ggsave("Output/CDF/SA Wind and Solar Quarterly.png", width = 7)

dispatch_data %>% filter(fuel_type %in% c("Wind"), region == "SA") %>% 
    mutate(year = settlementdate %>% year() %>% as.factor(),
           quarter = settlementdate %>% ymd_hms() %>% as.Date() %>% quarter()) %>% 
    ggplot(aes(x = total_disp*12, group=year, colour = year)) + stat_ecdf() +
    facet_wrap(~quarter) +
    ylab("CDF") +
    xlab("MW") +
    ggtitle("SA Wind Quarterly") +
    ggsave("Output/CDF/SA Wind Quarterly.png", width = 7)

dispatch_data %>% filter(fuel_type %in% c("Solar"), region == "SA") %>% 
    mutate(year = settlementdate %>% year() %>% as.factor(),
           quarter = settlementdate %>% ymd_hms() %>% as.Date() %>% quarter()) %>% 
    ggplot(aes(x = total_disp*12, group=year, colour = year)) + stat_ecdf() +
    facet_wrap(~quarter) +
    ylab("CDF") +
    xlab("MW") +
    ggtitle("SA Solar Quarterly") +
    ggsave("Output/CDF/SA Solar Quarterly.png", width = 7)

#yearly
dispatch_data %>% filter(fuel_type %in% c("Wind", "Solar"), region == "SA") %>% 
    group_by(settlementdate) %>% #combine solar and wind
    summarise(total_disp = sum(total_disp)) %>% 
    mutate(year = settlementdate %>% year() %>% as.factor()) %>% 
    ggplot(aes(x = total_disp*12, group=year, colour = year)) + stat_ecdf() +
    ylab("CDF") +
    xlab("MW") +
    ggtitle("SA Wind and Solar Yearly") +
    ggsave("Output/CDF/SA Wind and Solar Yearly.png", width = 7)

dispatch_data %>% filter(fuel_type %in% c("Wind"), region == "SA") %>% 
    mutate(year = settlementdate %>% year() %>% as.factor()) %>% 
    ggplot(aes(x = total_disp*12, group=year, colour = year)) + stat_ecdf() +
    ylab("CDF") +
    xlab("MW") +
    ggtitle("SA Wind Yearly") +
    ggsave("Output/CDF/SA Wind Yearly.png", width = 7)

dispatch_data %>% filter(fuel_type %in% c("Solar"), region == "SA") %>% 
    mutate(year = settlementdate %>% year() %>% as.factor()) %>% 
    ggplot(aes(x = total_disp*12, group=year, colour = year)) + stat_ecdf() +
    ylab("CDF") +
    xlab("MW") +
    ggtitle("SA Solar Yearly") +
    ggsave("Output/CDF/SA Solar Yearly.png", width = 7)

#NEMSIGHT data (incl non-scheduled gens)
#Nemweb dispatch
duids <- generator_details %>% filter(region == "SA", fuel_type %in% c("Wind", "Solar")) %>% .[["duid"]]


wind <- fread("D:/Thesis/Data/NEMSIGHT/wind_output.csv", header = TRUE) %>% 
    mutate(settlementdate = dmy_hm(settlementdate)) 

wind %>% 
    mutate(year = settlementdate %>% year() %>% as.factor()) %>% 
    filter(settlementdate != "2020-01-01 00:00:00") %>% 
    ggplot(aes(x = initialmw, group=year, colour = year)) + stat_ecdf() +
    ylab("CDF") +
    xlab("MW") +
    ggtitle("ALL SA Wind Yearly") +
    ggsave("D:/NEM_LMP/Output/CDF/ALL SA Wind Yearly.png", width = 7)

wind_and_solar <- wind %>% left_join(dispatch_data %>% filter(fuel_type == "Solar", region == "SA") %>% 
                                mutate(initialmw = total_disp*12,
                                       settlementdate = ymd_hms(settlementdate)),
                            by = "settlementdate") %>% #add total Wind (NEMSIGHT) to Solar (NEM)
    mutate(initialmw.y = ifelse(is.na(initialmw.y),
                                0,
                                initialmw.y)) %>% 
    mutate(total_initialmw = initialmw.x + initialmw.y)

wind_and_solar %>%
    mutate(year = settlementdate %>% year() %>% as.factor()) %>% 
    filter(settlementdate != "2020-01-01 00:00:00") %>% 
    ggplot(aes(x = total_initialmw, group=year, colour = year)) + stat_ecdf() +
    geom_vline(xintercept = 1200)+
    geom_vline(xintercept = 1295)+
    geom_vline(xintercept = 1460)+
    geom_vline(xintercept = 1750)+
    ylab("CDF") +
    xlab("MW") +
    ggtitle("ALL SA Wind and Solar Yearly")+
    ggsave("Output/CDF/ALL SA Wind and Solar Yearly.png", width = 7)



# LMP of a wind farm

year <- c(2013:2019)
location <- paste0("D:/NEM_LMP/Data/Cleaned/INITIALMW/",year,"/Step 4 - Mutated/")#location of step 4
files <- location %>% map(~paste0(.x, list.files(.x))) %>% unlist()

sa_wind_data <- files %>% map(~fread(.x) %>% filter(fuel_type == "Wind", region == "SA")) %>% rbindlist() %>% 
    mutate(settlementdate = ymd_hms(settlementdate))
sa_wind_data <- sa_wind_data %>% mutate(settlementdate = ymd_hms(settlementdate))

fwrite(sa_wind_data, "D:/NEM_LMP/Data/Cleaned/SA_wind_data.csv")

sa_wind_data %>% filter(duid == "BLUFF1") %>% 
    mutate(year = (settlementdate %>% year() %>% as.factor())) %>% 
    filter(settlementdate != "2020-01-01 00:00:00") %>% 
    ggplot(aes(x = lmp_censored, group=year, colour = year)) + stat_ecdf() +
    scale_x_continuous(limits = c(-1100, 500)) +
    ggtitle("LMP_Censored CDFs BLUFF1") +
    ggsave("D:/NEM_LMP/Output/CDF/LMP_BLUFF1.png", width = 7)

sa_wind_data %>% 
    mutate(year = (settlementdate %>% year() %>% as.factor())) %>% 
    filter(settlementdate != "2020-01-01 00:00:00") %>% 
    ggplot(aes(x = lmp_censored, group=year, colour = year)) + stat_ecdf() +
    scale_x_continuous(limits = c(-1100, 500)) +
    ggtitle("LMP_Censored CDFs all SA Wind") +
    ggsave("D:/NEM_LMP/Output/CDF/LMP_SA_WIND.png", width = 7)

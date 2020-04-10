#Explore_Q_CS_top20

# Load packages
library(scales)
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
Sys.setenv(TZ='UTC')

qld_data <- fread("D:/NEM_LMP/Data/Cleaned/Misc/qld_data.csv") %>% mutate(settlementdate = ymd_hms(settlementdate))

coal_daily <- qld_data %>% filter(fuel_type == "Black Coal") %>% 
    group_by(day = floor_date(settlementdate, "day")) %>% 
    summarise(TOsum = sum(TO), TOmcsum = sum(TOmc))
    

top_10 <- coal_daily %>% filter(year(day) == 2019) %>%  arrange(-TOsum) %>% .[1:10,] %>% .[["day"]]#highest
bottom_10 <- coal_daily %>% filter(year(day) == 2019) %>%  arrange(TOsum) %>% .[1:10,] %>% .[["day"]] #lowest

high_to <- qld_data %>% 
    mutate(time = paste0("2020-01-01 ", format(settlementdate, "%H:%M:%S")) %>% ymd_hms(),
           day = floor_date(settlementdate, "day")) %>% 
    filter(fuel_type %in% c("Black Coal", "Gas", "Solar"),
           day %in% top_10) %>% 
    group_by(settlementdate, time, day, fuel_type) %>% 
    summarise(Q = sum(dispatchmwh),
              QC = sum(dispatchmwh[local_price_adjustment != 0]),
              prop = mean(local_price_adjustment != 0),
              TOsum = sum(TO), TOmcsum = sum(TOmc)) %>% 
    ungroup()

high_CS_to <- qld_data %>% 
    mutate(time = paste0("2020-01-01 ", format(settlementdate, "%H:%M:%S")) %>% ymd_hms(),
           day = floor_date(settlementdate, "day")) %>% 
    filter(fuel_type %in% c("Black Coal", "Gas", "Solar"),
           day %in% top_10,
           duid %in% q_cs_duids) %>% 
    group_by(settlementdate, time, day, fuel_type) %>% 
    summarise(Q = sum(dispatchmwh),
              QC = sum(dispatchmwh[local_price_adjustment != 0]),
              prop = mean(local_price_adjustment != 0),
              TOsum = sum(TO), TOmcsum = sum(TOmc)) %>% 
    ungroup()

low_to <- qld_data %>% 
    mutate(time = paste0("2020-01-01 ", format(settlementdate, "%H:%M:%S")) %>% ymd_hms(),
           day = floor_date(settlementdate, "day")) %>% 
    filter(fuel_type %in% c("Black Coal", "Gas", "Solar"),
           day %in% bottom_10) %>% 
    group_by(settlementdate, time, day, fuel_type) %>% 
    summarise(Q = sum(dispatchmwh),
              QC = sum(dispatchmwh[local_price_adjustment != 0]),
              prop = mean(local_price_adjustment != 0), 
              TOsum = sum(TO), TOmcsum = sum(TOmc)) %>% 
    ungroup()

low_CS_to <- qld_data %>% 
    mutate(time = paste0("2020-01-01 ", format(settlementdate, "%H:%M:%S")) %>% ymd_hms(),
           day = floor_date(settlementdate, "day")) %>% 
    filter(fuel_type %in% c("Black Coal", "Gas", "Solar"),
           day %in% bottom_10,
           duid %in% q_cs_duids) %>% 
    group_by(settlementdate, time, day, fuel_type) %>% 
    summarise(Q = sum(dispatchmwh),
              QC = sum(dispatchmwh[local_price_adjustment != 0]),
              prop = mean(local_price_adjustment != 0), 
              TOsum = sum(TO), TOmcsum = sum(TOmc)) %>% 
    ungroup()


###
#high to v low to solar output
high_to %>% filter(fuel_type == "Solar") %>% 
    ggplot(aes(x = time, y = Q, group = day)) +
    geom_line()

low_to %>% filter(fuel_type == "Solar") %>% 
    ggplot(aes(x = time, y = Q, group = day)) +
    geom_line()
#Q is essentially the same

high_to %>% filter(fuel_type == "Solar") %>% 
    ggplot(aes(x = time, y = QC, group = day)) +
    geom_line()

low_to %>% filter(fuel_type == "Solar") %>% 
    ggplot(aes(x = time, y = QC, group = day)) +
    geom_line()

#QC is zero on lowest day

#all stats for top and bottom day
high_to %>% gather(var, value, -c(1:4)) %>% 
    filter(day == top_10[1]) %>% 
    ggplot(aes(x = time, y = value, colour = fuel_type)) +
    geom_line()+
    facet_wrap(~var, scales = "free") +
    labs(title = "Top")

low_to %>% gather(var, value, -c(1:4)) %>% 
        filter(day == bottom_10[1]) %>% 
        ggplot(aes(x = time, y = value, colour = fuel_type)) +
        geom_line()+
        facet_wrap(~var, scales = "free") +
        labs(title = "Bottom")
    


#solar output constraining coal
high_to %>% mutate(NonQ = Q-QC) %>% 
    gather(var, value, -c(1:4)) %>% 
    filter(fuel_type %in% c("Solar", "Black Coal"), 
           var %in% c("Q", "QC","NonQ")) %>% 
    ggplot(aes(x = time, y = value, colour = fuel_type)) +
    geom_line()+
    facet_grid(var~day, scales = "free")


low_to %>% mutate(NonQ = Q-QC) %>% 
    gather(var, value, -c(1:4)) %>% 
    filter(fuel_type %in% c("Solar", "Black Coal"), 
           var %in% c("Q", "QC","NonQ")) %>% 
    ggplot(aes(x = time, y = value, colour = fuel_type)) +
    geom_line()+
    facet_grid(var~day, scales = "free")

#solar output TO coal
high_to %>% 
    gather(var, value, -c(1:4)) %>% 
    filter(fuel_type %in% c("Solar", "Black Coal"), 
           var %in% c("Q","QC", "TOsum")) %>% 
    ggplot(aes(x = time, y = value, colour = fuel_type)) +
    geom_line()+
    facet_grid(var~day, scales = "free") + 
    labs(title = "QLD Top 10")+
    ggsave("D:/NEM_LMP/Output/Q_CS/top_10.png", width = 20, height = 10)

high_to_cs %>% mutate(NonQ = Q-QC) %>% 
    gather(var, value, -c(1:4)) %>% 
    filter(fuel_type %in% c("Solar", "Black Coal"), 
           var %in% c("Q","QC", "TOsum")) %>% 
    ggplot(aes(x = time, y = value, colour = fuel_type)) +
    geom_line()+
    facet_grid(var~day, scales = "free")+ 
    labs(title = "CS Top 10")+
    ggsave("D:/NEM_LMP/Output/Q_CS/CS_top_10.png", width = 20, height = 10)

low_to  %>% 
    gather(var, value, -c(1:4)) %>% 
    filter(fuel_type %in% c("Solar", "Black Coal"), 
           var %in% c("Q","QC", "TOsum")) %>% 
    ggplot(aes(x = time, y = value, colour = fuel_type)) +
    geom_line()+
    labs(title = "QLD Bottom 10")+
    facet_grid(var~day, scales = "free")+
    ggsave("D:/NEM_LMP/Output/Q_CS/Bottom_10.png", width = 20, height = 10)

low_CS_to  %>% 
    gather(var, value, -c(1:4)) %>% 
    filter(fuel_type %in% c("Solar", "Black Coal"), 
           var %in% c("Q","QC", "TOsum")) %>% 
    ggplot(aes(x = time, y = value, colour = fuel_type)) +
    geom_line()+
    labs(title = "CS Bottom 10")+
    facet_grid(var~day, scales = "free")+
    ggsave("D:/NEM_LMP/Output/Q_CS/CS_bottom_10.png", width = 20, height = 10)

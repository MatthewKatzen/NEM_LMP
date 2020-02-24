#VOM

# Load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
Sys.setenv(TZ='UTC')

gdp_data <- fread("D:/Thesis/Data/External/gdp_data.csv") %>% clean_names() %>% mutate(date = paste0("01/", date)) %>% 
    mutate(change = (gdp/lag(gdp))) %>% 
    mutate(base = NA)

for (i in 29:1){
    if (i==29){
        gdp_data$base <- 1
    }else{
        gdp_data$base[i] <- gdp_data$base[i+1]/gdp_data$change[i+1]
    }
}



vom_price <- fread("D:/Thesis/Data/External/vom.csv")

vom_data <- vom_price %>% 
    mutate(station = case_when(station %in% c("Callide B", "Callide C") ~ "Callide",
                               station == "Swanbank E GT" ~ "Swanbank",
                               station == "Barcaldine Power Station" ~ "Barcaldine",
                               station == "Beryl solar farm" ~ "Beryl Solar Farm",
                               station == "Bungala one solar farm"~ "Bungala One Solar Farm",
                               station == "Braemar" ~ "Braemar 1",
                               station == "Braemar 2 Power Station" ~ "Braemar 2",
                               station == "Catagunya / Liapootah / Wayatinah" ~ 
                                   "Liapootah Wayatinah Catagunya Aggregate",
                               station == "Laverton North" ~ "Laverton",
                               station == "Lemonthyme / Wilmot" ~ "Lemonthyme",
                               station == "Loy Yang A Power Station" ~ "Loy Yang A",
                               (substr(station, nchar(station) - 1, nchar(station)) %in% c("GT", "Gt")) &
                                   (station != "Hunter Valley GT") ~ 
                                   substr(station, 1, nchar(station) - 3), #gas suffix
                               station == "Condamine A" ~ "Condamine",
                               station == "Hume Dam NSW" ~ "Hume Hydro",
                               station == "Hume Dam VIC" ~ "Hume",
                               station %in% c("Murray 1", "Murray 2") ~ "Murray",
                               station == "Oakey 1 Solar Farm" ~ "Oakey 1 Solar Farm",
                               station == "Oakey Power Station" ~ "Oakey",
                               station == "Port Stanvac 1" ~ "Pt Stanvac",
                               station == "Swanbank E" ~ "Swanbank",
                               station %in% c("Yabulu PS", "Yabulu Steam Turbin") ~ "Yabulu",
                               station %in% c("Torrens Island A", "Torrens Island B") ~ "Torrens Island",
                               station == "Ballarat Energy Storage System" ~ "Ballarat Battery Energy Storage System",
                               station == "Hornsdale Power Reserve Unit 1" ~ "Hornsdale Power Reserve",
                               station == "Tamar Valley Combined Cycle" ~ "Tamar Valley",
                               station == "Yarwun Cogen" ~ "Yarwun",
                               TRUE ~ station)) %>% 
    right_join(generator_details, by = "station") %>% 
    filter(station %in% (full_mc$station %>% unique())) %>% #remove stations which aren't dispatched in data
    mutate(vom = ifelse(is.na(vom) & fuel_type=="Wind",
                        2.71,
                        vom),
           vom = ifelse(is.na(vom) & fuel_type=="Solar",
                        0,
                        vom)) %>% 
    group_by(fuel_type) %>% 
    mutate(vom = ifelse(is.na(vom),#remaining plants are just decommissioned and t/f not in esoo. Just fill with mean of fuel type
                        mean(vom, na.rm=TRUE),
                        vom))
vom_price_2 <- vom_price %>% .[rep(1:nrow(.), each=nrow(gdp_data)),] %>% 
    mutate(month = gdp_data[rep(1:nrow(gdp_data),
                                       nrow(vom_price)),
                                   "date"],
           base = gdp_data[rep(1:nrow(gdp_data),
                                nrow(vom_price)),
                            "base"], 
           adjusted_vom = vom * base) 

vom_price_3 <- vom_price_2 %>% 
    .[rep((1:nrow(vom_price_2)), each = 3),] %>% #convert quarterly to monthly
    mutate(month = dmy(month) + months(rep(c(0:2), times = nrow(vom_price_2))))

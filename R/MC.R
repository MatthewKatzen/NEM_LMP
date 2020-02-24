#MC analysis
#
#Get All MC for each duid from 2013-2019



# Load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
Sys.setenv(TZ='UTC')


generator_details <- fread("D:/NEM_LMP/Data/RAW/generator_details.csv") %>% clean_names() %>% 
    select(duid, station, fuel_type, thermal_efficiency, region) %>% 
    mutate(region = case_when(region == "Queensland" ~ "QLD",
                              region == "New South Wales" ~ "NSW",
                              region == "Victoria" ~ "VIC",
                              region == "South Australia" ~ "SA",
                              region == "Tasmania" ~ "TAS"))

#### Black Coal
#### 

#https://www.indexmundi.com/commodities/?commodity=coal-australian&months=120&currency=aud
#ton * (1/1000) (to per KG) * (1/29307600) (to per J) * (1000000000) (to per GJ) * (1/3.6) (to MWh)
coal_index <- fread("D:/Thesis/Data/External/black_coal_index.csv") %>% clean_names() %>% 
    select(month, coal_price = price) %>% 
    mutate(month = dmy(month)) %>% filter(month >= ymd("2013/01/01")) %>% 
    mutate(change = (coal_price/lag(coal_price))) %>% 
    mutate(base = NA)

for (i in 84:1){
    if (i==84){
        coal_index$base <- 1
    }else{
        coal_index$base[i] <- coal_index$base[i+1]/coal_index$change[i+1]
    }
}


black_coal_gens <- generator_details %>% filter(fuel_type == "Black Coal",
                                                !(duid %in% c("CALL_A_2", "ERGT01")))#not in dataset

black_coal_price <- fread("D:/Thesis/Data/External/black_coal_price.csv") %>% 
    mutate(station = ifelse(station %in% c("Callide B", "Callide C"),
                           "Callide",
                           station)) %>% 
    unique() %>% 
    right_join(black_coal_gens, by = "station") %>% 
    mutate(black_coal_price = ifelse(is.na(black_coal_price), 
                                     mean(black_coal_price, na.rm=TRUE), 
                                     black_coal_price)) #if missing from esoo (i.e. decommissioned) then make it mean of others


black_coal_mc <- black_coal_price %>% .[rep(1:nrow(.), each=nrow(coal_index)),] %>% 
    mutate(month = coal_index[rep(1:nrow(coal_index),
                                  53),#nrow(black_coal_price), not working in code for some reason
                              "month"],
           base = coal_index[rep(1:nrow(coal_index),
                                 53),#nrow(black_coal_price)
                             "base"]) %>% 
    mutate(black_coal_price_adjusted = black_coal_price * base) %>% 
    mutate(mc =  black_coal_price_adjusted * 3.6 * (1/thermal_efficiency))



#### Gas
#

#fread("D:/Thesis/Data/External/gas_price.csv") %>% clean_names() %>% rename(vic_price = vic_gas_price) %>% 
#    data.frame() %>% is.na() %>% sum()

gas_data <- fread("D:/Thesis/Data/External/gas_price.csv") %>% 
    clean_names() %>% rename(vic_price = vic_gas_price) %>% 
    mutate(day = dmy(day)) %>% 
    data.frame() %>% fill(colnames(.))#some missing days, just fill down

gas_gens <- generator_details %>% filter(fuel_type == "Gas")

gas_mc <- gas_gens %>% .[rep(1:nrow(.), each=nrow(gas_data)),] %>% 
    mutate(date = gas_data[rep(1:nrow(gas_data),
                                 nrow(gas_gens)),
                             "day"],
           vic_price = gas_data[rep(1:nrow(gas_data),
                                      nrow(gas_gens)),
                                  "vic_price"],
           adl_price = gas_data[rep(1:nrow(gas_data),
                                    nrow(gas_gens)),
                                "adl_price"],
           bri_price = gas_data[rep(1:nrow(gas_data),
                                    nrow(gas_gens)),
                                "bri_price"],
           syd_price = gas_data[rep(1:nrow(gas_data),
                                    nrow(gas_gens)),
                                "syd_price"],
           mc = case_when(region == "QLD" ~ bri_price/thermal_efficiency * 3.6,
                          region == "NSW" ~ syd_price/thermal_efficiency * 3.6,
                          region == "VIC" ~ vic_price/thermal_efficiency * 3.6,
                          region == "SA" ~ adl_price/thermal_efficiency * 3.6,
                          region == "TAS" ~ vic_price/thermal_efficiency * 3.6)) 

# Brown Coal
# ESOO 2019/20 = $0.64 /GJ 
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

brown_coal_data <- gdp_data %>% select(date, base) %>% 
    mutate(brown_coal_price = 0.64 * base) %>% 
    mutate(date = dmy(date))

brown_coal_gens <- generator_details %>% filter(fuel_type == "Brown Coal")

brown_coal_mc <- brown_coal_gens %>% .[rep(1:nrow(.), each=nrow(brown_coal_data)),] %>% 
    mutate(month = brown_coal_data[rep(1:nrow(brown_coal_data),
                                 nrow(brown_coal_gens)),
                             "date"],
           brown_coal_price = brown_coal_data[rep(1:nrow(brown_coal_data),
                                      nrow(brown_coal_gens)),
                                  "brown_coal_price"]) %>% 
    mutate(mc =  brown_coal_price * 3.6 * (1/thermal_efficiency)) 

brown_coal_mc_2 <- brown_coal_mc %>% .[rep((1:nrow(brown_coal_mc)), each = 3),] %>% #convert quarterly to monthly
    mutate(month = month + months(rep(c(0:2), times = nrow(brown_coal_mc))))

# Liquid Fuel
#ESOO 19/20 = $37.71 /GJ 

liquid_fuel_data <- gdp_data %>% select(date, base) %>% 
    mutate(liquid_fuel_price = 37.71 * base) %>% 
    mutate(date = dmy(date))

liquid_fuel_gens <- generator_details %>% filter(fuel_type == "Liquid Fuel") %>% 
    filter(thermal_efficiency != 0) #all te=0 are not in dispatch dataset

liquid_fuel_mc <- liquid_fuel_gens %>% .[rep(1:nrow(.), each=nrow(liquid_fuel_data)),] %>% 
    mutate(month = liquid_fuel_data[rep(1:nrow(liquid_fuel_data),
                                       nrow(liquid_fuel_gens)),
                                   "date"],
           liquid_fuel_price = liquid_fuel_data[rep(1:nrow(liquid_fuel_data),
                                                  nrow(liquid_fuel_gens)),
                                              "liquid_fuel_price"]) %>% 
    mutate(mc =  liquid_fuel_price * 3.6 * (1/thermal_efficiency))

liquid_fuel_mc_2 <- liquid_fuel_mc %>% .[rep((1:nrow(liquid_fuel_mc)), each = 3),] %>% 
    mutate(month = month + months(rep(c(0:2), times = nrow(liquid_fuel_mc))))

#merge

monthly_mc <- list(black_coal_mc %>% select(duid, month, mc), 
                   liquid_fuel_mc_2 %>% select(duid, month, mc), 
                   brown_coal_mc_2%>% select(duid, month, mc)) %>% rbindlist()

fwrite(monthly_mc, "D:/NEM_LMP/Output/MC/gen_monthly_mc.csv")
fwrite(gas_mc, "D:/NEM_LMP/Output/MC/gas_mc.csv")


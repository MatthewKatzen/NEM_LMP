#VOM

#skip MC2

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
                              region == "Tasmania" ~ "TAS")) %>% 
    filter(!(duid %in% c("GERMCRK", "MBAHNTH", "CALL_A_4"))) #non-schedueld

#duid BARKIPS1 is too new and doesn't have a te rating yet, give it mean of others
generator_details %>% filter(fuel_type == "Gas", thermal_efficiency > 0) %>% summarise(mean = mean(thermal_efficiency))
generator_details[22, "thermal_efficiency"] <- 0.3114615

full_mc <- fread("D:/NEM_LMP/Output/MC/full_mc.csv")
full_nonmc <- fread("D:/NEM_LMP/Output/NonMC/full.csv")

gas_mc <- fread("D:/NEM_LMP/Output/MC/gas_mc.csv")#date
black_coal_mc <- fread("D:/NEM_LMP/Output/MC/black_coal_mc.csv")#month
liquid_fuel_mc <- fread("D:/NEM_LMP/Output/MC/liquid_fuel_mc.csv")
brown_coal_mc <- fread("D:/NEM_LMP/Output/MC/brown_coal_mc.csv")

#VOM
gdp_data <- fread("D:/Thesis/Data/External/oecd_gdp.csv") %>% clean_names() %>% as.data.frame()

for (i in 28:1){
    if (i %in% c(27,28)){
        gdp_data$base <- 1 #assume no change in q4 2019
    }else{
        gdp_data$base[i] <- gdp_data$base[i+1]/(1+gdp_data$change[i]/100)
    }
}



vom_price <- fread("D:/Thesis/Data/External/vom.csv")

vom_data <- vom_price %>% 
    filter(!(station %in% c("Callide C", "Murray 2","Yabulu Steam Turbin", "Torrens Island B"))) %>% 
    mutate(station = case_when(station == "Callide B" ~ "Callide",
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
                               station == "Murray 1" ~ "Murray",
                               station == "Oakey 1 Solar Farm" ~ "Oakey 1 Solar Farm",
                               station == "Oakey Power Station" ~ "Oakey",
                               station == "Port Stanvac 1" ~ "Pt Stanvac",
                               station == "Swanbank E" ~ "Swanbank",
                               station == "Yabulu PS" ~ "Yabulu",
                               station == "Torrens Island A" ~ "Torrens Island",
                               station == "Ballarat Energy Storage System" ~ "Ballarat Battery Energy Storage System",
                               station == "Hornsdale Power Reserve Unit 1" ~ "Hornsdale Power Reserve",
                               station == "Tamar Valley Combined Cycle" ~ "Tamar Valley",
                               station == "Yarwun Cogen" ~ "Yarwun",
                               TRUE ~ station)) %>% 
    right_join(generator_details, by = "station") %>% 
    filter(duid %in% unique(full_mc$duid)) %>% #remove duids not in full_mc
    mutate(vom = ifelse(is.na(vom) & fuel_type=="Wind", #add in any missing wind at 2.71
                        2.71,
                        vom),
           vom = ifelse(is.na(vom) & fuel_type=="Solar", #add in missing solar at 0
                        0,
                        vom)) %>% 
    group_by(fuel_type) %>% 
    mutate(vom = ifelse(is.na(vom),#remaining plants are just decommissioned and t/f not in esoo. Just fill with mean of fuel type
                        mean(vom, na.rm=TRUE),
                        vom)) %>% ungroup()


vom_data_2 <- vom_data %>% .[rep(1:nrow(.), each=nrow(gdp_data)),] %>% #deflate vom by gdp
    mutate(quarter = gdp_data[rep(1:nrow(gdp_data),
                                       nrow(vom_data)),
                                   "quarter"] %>% dmy(),
           base = gdp_data[rep(1:nrow(gdp_data),
                                nrow(vom_data)),
                            "base"], 
           adjusted_vom = vom * base) 

#### merge mc and vom df
gas_mc <- fread("D:/NEM_LMP/Output/MC/gas_mc.csv")#date
black_coal_mc <- fread("D:/NEM_LMP/Output/MC/black_coal_mc.csv")#month
liquid_fuel_mc <- fread("D:/NEM_LMP/Output/MC/liquid_fuel_mc.csv") #quarter
brown_coal_mc <- fread("D:/NEM_LMP/Output/MC/brown_coal_mc.csv") #quarter


black_coal_combined <- black_coal_mc %>% mutate(quarter = floor_date(ymd(month), "quarter")) %>% 
    left_join(vom_data_2, by = c("duid", "quarter")) %>% 
    mutate(mc_vom = mc+adjusted_vom) %>% 
    filter(duid %in% unique(full_nonmc$duid))#remove all duids not in dispatch data

gas_combined <- gas_mc %>% mutate(quarter = floor_date(ymd(date), "quarter")) %>% 
    left_join(vom_data_2, by = c("duid", "quarter")) %>% 
    mutate(mc_vom = mc+adjusted_vom)%>% 
    filter(duid %in% unique(full_nonmc$duid))#remove all duids not in dispatch data

liquid_fuel_combined <- liquid_fuel_mc %>% mutate(quarter = dmy(quarter)) %>% 
    left_join(vom_data_2, by = c("duid", "quarter")) %>% 
    mutate(mc_vom = mc+adjusted_vom)%>% 
    filter(duid %in% unique(full_nonmc$duid))#remove all duids not in dispatch data

brown_coal_combined <- brown_coal_mc %>% mutate(quarter = dmy(quarter)) %>% 
    left_join(vom_data_2, by = c("duid", "quarter")) %>% 
    mutate(mc_vom = mc+adjusted_vom)%>% 
    filter(duid %in% unique(full_nonmc$duid))#remove all duids not in dispatch data

renewable_vom <-  vom_data_2 %>% filter(!(duid %in% c(black_coal_combined$duid, 
                                                      gas_combined$duid, 
                                                      liquid_fuel_combined$duid,
                                                      brown_coal_combined$duid))) %>% 
    rename(mc_vom = adjusted_vom)%>% 
    filter(duid %in% unique(full_nonmc$duid))#remove all duids not in dispatch data


fwrite(gas_combined, "D:/NEM_LMP/Data/Cleaned/vom/gas_combined.csv")
fwrite(renewable_vom, "D:/NEM_LMP/Data/Cleaned/vom/renewable_vom.csv")
fwrite(black_coal_combined, "D:/NEM_LMP/Data/Cleaned/vom/black_coal_combined.csv")
fwrite(brown_coal_combined, "D:/NEM_LMP/Data/Cleaned/vom/brown_coal_combined.csv")
fwrite(liquid_fuel_combined, "D:/NEM_LMP/Data/Cleaned/vom/brown_coal_combined.csv")

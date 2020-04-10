#which constraints are causing TO? We've assumed its CS but may not be

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

#load data
constraint_data <- fread("D:/NEM_LMP/Data/Cleaned/Misc/constraint_data.csv") %>% 
    mutate(settlementdate = ymd_hms(settlementdate))
qld_data <- fread("D:/NEM_LMP/Data/Cleaned/Misc/qld_data.csv") %>% mutate(settlementdate = ymd_hms(settlementdate))
eqs_data <- fread("D:/NEM_LMP/Data/Cleaned/Misc/eqs_data.csv")


#get data
qld_constraint_data <- fread("D:/NEM_LMP/Data/Cleaned/Misc/qld_constraint_data.csv") %>% #only from 2015
    mutate(settlementdate = ymd_hms(settlementdate))

#explore
constraint_data %>% group_by(constraintid) %>% 
    summarise(count = n(), totalmv = sum(marginalvalue), avmv = totalmv/count) %>% arrange(totalmv)

#merge
merged_data <- qld_data %>% filter(year(settlementdate) == 2019, local_price_adjustment != 0) %>% 
    left_join(constraint_data, by = "settlementdate")
(merged_data %>% is.na() %>% rowSums() > 0) %>% which() %>% head() #why do some have no constraint? interstate
merged_data <- merged_data[((merged_data %>% is.na() %>% rowSums()) == 0) %>% which(),] #remove them for now


#### TABLES
## TO by constraint

temp %>% group_by(constraintid) %>% summarise(count = n(), TO = sum(TO), TOmc = sum(TOmc)) %>% arrange(-TO)

#only uniquely binding constraints (no times when two constrints bind)
temp2 <- temp %>% group_by(settlementdate) %>% filter(all(constraintid == first(constraintid)))

temp2 %>% group_by(constraintid) %>% summarise(count = n(), TO = sum(TO), TOmc = sum(TOmc)) %>% arrange(-TO)



## Constraint by Fuel
temp %>% group_by(fuel_type, constraintid) %>% summarise(count = n(), TO=sum(TO)) %>% arrange(-TO)
temp2 %>% group_by(fuel_type, constraintid) %>% summarise(count = n(), TO=sum(TO)) %>% arrange(-TO)


#### GRAPHS
#### 
qld_constraint_data %>% mutate(time = paste0("2020-01-01 ", format(settlementdate, "%H:%M:%S")) %>% ymd_hms(),
                           date = floor_date(settlementdate, "day")) %>% 
    filter(constraintid == "Q_CS_1100") %>% 
    group_by(time) %>% tally() %>% 
    ggplot(aes(x = time, y = n)) +
    geom_line()





####EQS
####

yearmonths <- qld_constraint_data$genconid_effectivedate %>% 
    map(~paste0(substr(.x,1,4), substr(.x,6,7))) %>% unlist() %>% unique()

eqs_data <- yearmonths %>% map(~eqs.fun(.x)) %>% rbindlist() %>% clean_names() %>% rename(constraintid = genconid)
fwrite(eqs_data,"D:/NEM_LMP/Data/Cleaned/Misc/eqs_data.csv")

##### Merge
##### 

constraint_data <- fread("D:/NEM_LMP/Data/Cleaned/Misc/constraint_data.csv") %>% 
    #select(settlementdate, constraintid, rhs, marginalvalue) %>% 
    filter(!(constraintid %in% c("T_V_NIL_BL1", "VT_000"))) %>% #old and onloy interconnector
    mutate(settlementdate = ymd_hms(settlementdate))
qld_data <- fread("D:/NEM_LMP/Data/Cleaned/Misc/qld_data.csv") %>% mutate(settlementdate = ymd_hms(settlementdate))
eqs_data <- fread("D:/NEM_LMP/Data/Cleaned/Misc/eqs_data.csv")

temp <- left_join(constraint_data, eqs_data, "constraintid")

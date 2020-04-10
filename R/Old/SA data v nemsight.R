#Thermal v non thermal constraints
sa_constraint_data %>% 
    filter(#constraintid %in% temp1, #perhaps filter out all constraints not in nemsight list
           !(substr(constraintid, 2,3) %in% c(">S", ":S", "^S", "+S", "_S"))) %>% #filter out interconnector only constraints
    mutate(constraint_type = case_when(substr(constraintid, 2,2) == ">" ~ "Thermal",
                                       TRUE ~ "System Strength")) %>% #all other cases, just put as System strength
    group_by(constraint_type, year = year(settlementdate)) %>% summarise(sum = n()) %>% 
    ggplot(aes(x = year, y = sum, colour = constraint_type)) +
    geom_line(size = 1.5)+
    labs(x = "Year", y = "Number of 5 min intervals", colour = "Constraint Type", title = "SA constraints - using Raw data") +
    lims(y = c(0,30000))+
    ggsave("D:/NEM_LMP/Output/SA/SA constraints rawdata.png")

sa_constraint_data %>% 
    filter(rhs > 0.001, #remove all zero limit constraints
        !(substr(constraintid, 2,3) %in% c(">S", ":S", "^S", "+S", "_S"))) %>% #filter out interconnector only constraints
    mutate(constraint_type = case_when(substr(constraintid, 2,2) == ">" ~ "Thermal",
                                       TRUE ~ "System Strength")) %>% #all other cases, just put as System strength
    group_by(constraint_type, year = year(settlementdate)) %>% summarise(sum = n()) %>% 
    ggplot(aes(x = year, y = sum, colour = constraint_type)) +
    geom_line(size = 1.5)+
    labs(x = "Year", y = "Number of 5 min intervals", colour = "Constraint Type", title = "SA constraints - using Raw data - Zeros removed") +
    lims(y = c(0,30000))+
    ggsave("D:/NEM_LMP/Output/SA/SA constraints rawdata - Zeros removed.png")

#why is it different to thesis?

nemsight_constraints <- fread("D:/NEM_LMP/Data/Raw/Nemsight/constraints_yearly.csv") %>% clean_names() %>% 
    rename(constraintid = id) %>% filter(state == "SA")

nemsight_constraints %>% 
    filter(!(substr(constraintid, 2,3) %in% c(">S", ":S", "^S", "+S", "_S"))) %>% #filter out interconnector only constraints
    mutate(constraint_type = case_when(substr(constraintid, 2,2) == ">" ~ "Thermal",
                                       TRUE ~ "System Strength")) %>% #all other cases, just put as System strength
    group_by(constraint_type, year = interval) %>% summarise(sum = sum(num_events)) %>% 
    ggplot(aes(x = year, y = sum, colour = constraint_type)) +
    geom_line(size = 1.5)+
    labs(x = "Year", y = "Number of 5 min intervals", colour = "Constraint Type", title = "SA constraints - using NEMSIGHT aggregates") +
    lims(y = c(0,30000))+
    ggsave("D:/NEM_LMP/Output/SA/SA constraints NEMSIGHT.png")
    

nemsight_constraints %>% 
    filter(interval == 2015, !(substr(constraintid, 2,3) %in% c(">S", ":S", "^S", "+S", "_S"))) %>% #filter out interconnector only constraints
    mutate(constraint_type = case_when(substr(constraintid, 2,2) == ">" ~ "Thermal",
                                       TRUE ~ "System Strength")) %>% arrange(-num_events)

sa_constraint_data %>% 
    filter(year(settlementdate) == 2015, !(substr(constraintid, 2,3) %in% c(">S", ":S", "^S", "+S", "_S"))) %>% #filter out interconnector only constraints
    mutate(constraint_type = case_when(substr(constraintid, 2,2) == ">" ~ "Thermal",
                                       TRUE ~ "System Strength")) %>% 
    group_by(constraintid) %>% summarise(sum = n()) %>% arrange(-sum)


temp1 <- nemsight_constraints %>% 
    filter(!(substr(constraintid, 2,3) %in% c(">S", ":S", "^S", "+S", "_S"))) %>% 
    .[['constraintid']] %>% unique()

temp2 <- sa_constraint_data %>% 
    filter(!(substr(constraintid, 2,3) %in% c(">S", ":S", "^S", "+S", "_S"))) %>% 
    .[['constraintid']] %>% unique()

#which data are missing from nemsight aggregate?
temp3 <- temp2[(!(temp2 %in% temp1)) %>% which()]
sa_constraint_data %>% filter(constraintid %in% temp3) %>% group_by(constraintid) %>% summarise(count = n(), mean(rhs)) %>% arrange(-count) %>% data.frame()
sa_constraint_data %>% group_by(constraintid) %>% summarise(count = n(), mean(rhs)) %>% arrange(-count) %>% data.frame()

sa_constraint_data %>% filter(constraintid == "S_LBMY_2B-LB3")

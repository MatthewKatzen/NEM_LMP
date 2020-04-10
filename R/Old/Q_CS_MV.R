#duplication of AEMC MArginal value table

qld_constraint_data %>% filter(floor_date(settlementdate, "month")=="2019-09-01 00:00:00 UTC") %>% 
    group_by(constraintid) %>% 
    summarise(sum = sum(marginalvalue)) %>% 
    arrange(sum)

qld_constraint_data %>% filter(floor_date(settlementdate, "month")=="2019-09-01 00:00:00 UTC") %>% 
    mutate(marginalvalue = ifelse(marginalvalue<(-15000),
                                  -15000,
                                  marginalvalue)) %>% 
    group_by(constraintid) %>% 
    summarise(sum = sum(marginalvalue)) %>% 
    arrange(sum) 

#MCC

c(201301:201412) %>% map(~rhs.mmc.fun(.x))

qld_mcc_data <- paste0("D:/NEM_LMP/Data/Raw/MCC_RHS/",list.files("D:/NEM_LMP/Data/Raw/MCC_RHS")) %>% 
    map(~ fread(.x) %>% clean_names() %>% 
            filter(substr(constraintid,1,1) %in% c('Q')) %>%  #only QLD
            mutate(settlementdate = ymd_hms(run_datetime))) %>% 
    rbindlist() %>% 
    select(settlementdate, constraintid, marginalvalue) %>% 
    unique() #for some reason some rows are duplicated


qld_mcc_data %>% mutate(marginalvalue = ifelse(marginalvalue<(-15000),
                                               -15000,
                                               marginalvalue)) %>% 
    group_by(constraintid, month = floor_date(settlementdate, "month"))%>% 
    summarise(sum = sum(marginalvalue)) %>% 
    arrange(sum) %>% 
    mutate(Q_CS = (substr(constraintid,1,4)=="Q_CS")) %>% 
    ggplot(aes(x = month, y = -sum, group = constraintid, colour = Q_CS)) +
    geom_line(size = 2) +
    labs(title = "QLD MV") +
    ggsave("D:/NEM_LMP/Output/Q_CS/CS_MV.png", width = 10)

qld_mcc_data %>% mutate(marginalvalue = ifelse(marginalvalue<(-15000),
                                               -15000,
                                               marginalvalue)) %>% 
    group_by(constraintid, year = floor_date(settlementdate, "year"))%>% 
    summarise(MV = sum(marginalvalue)) %>% 
    arrange(MV) %>% 
    filter(year=="2019-01-01 00:00:00")

#Q_CS all grouped together
qld_mcc_data %>% mutate(marginalvalue = ifelse(marginalvalue<(-15000),
                                               -15000,
                                               marginalvalue),
                        constraintid = ifelse((substr(constraintid,1,4)=="Q_CS"),
                                              "Q_CS",
                                              constraintid)) %>% 
    group_by(constraintid, month = floor_date(settlementdate, "quarter"))%>% 
    summarise(sum = sum(marginalvalue)) %>% 
    arrange(sum) %>% 
    mutate(Q_CS = (substr(constraintid,1,4)=="Q_CS")) %>% 
    ggplot(aes(x = month, y = -sum, group = constraintid, colour = Q_CS)) +
    geom_line(size = 2) +
    labs(title = "QLD MV") 

#Q_CS on its own
qld_mcc_data %>% filter(substr(constraintid,1,4)=="Q_CS") %>% 
    mutate(marginalvalue = ifelse(marginalvalue<(-15000),
                                               -15000,
                                               marginalvalue)) %>% 
    group_by(constraintid, month = floor_date(settlementdate, "mont"))%>% 
    summarise(sum = sum(marginalvalue)) %>% 
    arrange(sum) %>% 
    ggplot(aes(x = month, y = -sum, group = constraintid)) +
    geom_line(size = 2) +
    labs(title = "QLD MV") 

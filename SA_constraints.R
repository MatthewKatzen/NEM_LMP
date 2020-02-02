#graph all SA constraints occurances

setwd("C:/Users/Matthew/Dropbox/Mispricing/Analysis")
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(padr)
library(janitor)

#

constraints_yearly <- fread("D:/Thesis/Data/NEMSIGHT/constraints_yearly.csv") %>% 
    clean_names() %>% 
    mutate(id = case_when(id == "S_NIL_STRENGTH_1" ~ "S_NIL_STRENGTH_1 or S_WIND_1200_AUTO",
                          id == "S_WIND_1200_AUTO" ~ "S_NIL_STRENGTH_1 or S_WIND_1200_AUTO",
                          id == id ~ id))

top <- 
    constraints_yearly %>% filter(state == "SA") %>% 
    group_by(id) %>% 
    summarise(sum = sum(num_events)) %>% 
    top_n(10) %>% as.list() %>% .["id"] %>% .[[1]]

sa_strength <- 
    constraints_yearly %>% filter(id %in% top) %>% 
    mutate(interval = (paste0(interval, "-01-01")) %>% ymd()) %>% 
    group_by(id) %>% 
    pad(start_val = ymd("2013-01-01"), end_val = ymd("2018-01-01")) %>% replace(is.na(.), 0)#add missing rows of 0
    
sa_strength %>% 
    ggplot(aes(x = interval, y = num_events, colour = id)) +
    geom_line(aes(linetype = (id != "S_NIL_STRENGTH_1 or S_WIND_1200_AUTO")), size = 1.5) +
    xlab("Year") +
    ylab("Number of 5 min intervals") +
    labs(colour = "Constraint ID") +
    guides(linetype=FALSE) 


ggsave("SA_Constraints.png", width = 10, height = 5)

#seperate SA wind constraint

constraints_yearly_2 <- fread("D:/Thesis/Data/NEMSIGHT/constraints_yearly.csv") %>% 
    clean_names() 

top<- 
    constraints_yearly_2 %>% filter(state == "SA") %>% 
    group_by(id) %>% 
    summarise(sum = sum(num_events)) %>% 
    top_n(10) %>% as.list() %>% .["id"] %>% .[[1]]

sa_strength_2 <- 
    constraints_yearly_2 %>% filter(id %in% top) %>% 
    mutate(interval = (paste0(interval, "-01-01")) %>% ymd()) %>% 
    group_by(id) %>% 
    pad(start_val = ymd("2013-01-01"), end_val = ymd("2018-01-01")) %>% replace(is.na(.), 0)#add missing rows of 0

sa_strength_2 %>% 
    ggplot(aes(x = interval, y = num_events, colour = id)) +
    geom_line(aes(linetype = !(id %in% c("S_NIL_STRENGTH_1","S_WIND_1200_AUTO"))), size = 1.5) +
    xlab("Year") +
    ylab("Number of 5 min intervals") +
    labs(colour = "Constraint ID") +
    guides(linetype=FALSE) 


ggsave("SA_Constraints_2.png", width = 10, height = 5)
    

#merge all constraint types together
constraints_yearly <- fread("D:/Thesis/Data/NEMSIGHT/constraints_yearly.csv") %>% 
    clean_names()

constraints_yearly %>% filter(state == "SA") %>% select(interval, id, num_events) %>% 
    filter(!(substr(id, 2,3) %in% c(">V", ":V", "^V", "+V", "_V"))) %>% #filter out interconnector only constraints
    mutate(constraint_type = case_when(substr(id, 2,2) == ">" ~ "Thermal",
                                       TRUE ~ "System Strength")) %>% #all other cases, just put as System strength
    group_by(constraint_type, interval) %>% summarise(sum = sum(num_events)) %>% 
    ggplot(aes(x = interval, y = sum, colour = constraint_type)) +
    geom_line(size = 1.5)+
    xlab("Year") +
    ylab("Number of 5 min intervals") +
    labs(colour = "Constraint Type")

ggsave("SA_Constraints_combined.png", width = 10, height = 5)


#MV
constraints_yearly %>% filter(state == "SA") %>% select(interval, id, num_events, av_mv) %>% 
    filter(!(substr(id, 2,3) %in% c(">V", ":V", "^V", "+V", "_V"))) %>% #filter out interconnector only constraints
    mutate(constraint_type = case_when(substr(id, 2,2) == ">" ~ "Thermal",
                                       TRUE ~ "System Strength")) %>% #all other cases, just put as System strength
    mutate(total_mv = av_mv*num_events) %>% 
    group_by(constraint_type, interval) %>% summarise(total_av_mv = -sum(total_mv)/sum(num_events)) %>% 
    ggplot(aes(x = interval, y = total_av_mv, colour = constraint_type)) +
    geom_line(size = 1.5)+
    xlab("Year") +
    ylab("Number of 5 min intervals") +
    labs(colour = "Constraint Type")

ggsave("SA_Constraints_mv.png", width = 10, height = 5)

#Neg quanitity check
gen_types <- mpa %>% filter(year(settlementdate) == 2018) %>% 
    group_by(fuel_type) %>% summ_all_2() %>% select(fuel_type)

original <- mpa %>% filter(year(settlementdate) == 2018) %>% 
    group_by(fuel_type) %>% summ_all_2() %>% select(-fuel_type) %>% as.matrix

no_neg <- mpa %>% filter(year(settlementdate) == 2018, dispatchmwh > 0) %>% 
    group_by(fuel_type) %>% summ_all_2() %>% select(-fuel_type) %>% as.matrix()

dif <- data.frame(original - no_neg) %>% mutate(fuel_type = gen_types$fuel_type)

#only gas, hydro, and wind have neg values. ALso it's negligible when compared to original mispricign figures


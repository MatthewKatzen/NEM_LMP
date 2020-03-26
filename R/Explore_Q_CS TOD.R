
constraint_data <- fread("D:/NEM_LMP/Data/Cleaned/Misc/constraint_data.csv") %>% 
    #select(settlementdate, constraintid, rhs, marginalvalue) %>% 
    filter(!(constraintid %in% c("T_V_NIL_BL1", "VT_000"))) %>% #old and onloy interconnector
    mutate(settlementdate = ymd_hms(settlementdate))


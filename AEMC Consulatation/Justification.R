### Justification
# Author: Matthew Katezn
# 
# This file shows a bit of the thought process in why I made some decisions in the Clean file.

### (J1) Fix Price dataset

    (duplicated(Price %>% filter(intervention == 0) %>% select(settlementdate, regionid))) %>% which() 
    #all duplicates ar caused by intervention. So does anything change with intervention pricing?
    
    temp <- Price %>% group_by(settlementdate, regionid) %>% filter(n() > 1) %>% as.data.frame() 
    #keep only duplicates
    
    temp2 <- temp %>% select(-intervention) %>% #remove intervention
        group_by(settlementdate, regionid) %>% filter(n() > 1) %>% as.data.frame() 
    
    #temp and temp2 are the same t/f repeat run duplications ar just caused by intervention==1 => remove int==1. 

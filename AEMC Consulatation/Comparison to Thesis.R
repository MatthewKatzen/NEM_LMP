# Compare AEMO data to thesis data

#Thesis
mpa <- fread("D:/Thesis/Data/mpa_final.csv") %>% 
    mutate(settlementdate = ymd_hms(settlementdate))

mpa %>% filter(year(settlementdate) == "2018") %>% select(settlementdate, duid) %>% duplicated() %>% which()
    #no duplicates    
    
Merged_4 %>% filter(year(settlementdate) == "2018") %>% select(settlementdate, duid) %>% duplicated() %>% which()
    #no duplicates
    
Thesis_2018 <- mpa %>% filter(year(settlementdate) == "2018") %>% select(settlementdate, duid, lmp, dispatchmwh)
AEMO_2018 <- Merged_4 %>% filter(year(settlementdate) == "2018") %>% select(settlementdate, duid, lmp, dispatchmwh)


#are all AEMO_2018 in Thesis_2018?
temp1 <- do.call(paste0, (Thesis_2018 %>% select(settlementdate, duid)))
temp2 <- do.call(paste0, (AEMO_2018 %>% select(settlementdate, duid)))

(!(temp2 %in% temp1)) %>% which() %>% length() #all aemo are in tehsis :)
(!(temp1 %in% temp2)) %>% which() %>% length() #4471 missing from aemo, these are al the mssing rows


### Investigate Specific events

Thesis_2018[((!(temp1 %in% temp2)) %>% which()),] %>% head()

#2018-01-13 12:00:00 SNOWTWN1
AEMO_2018 %>% filter(settlementdate %within% interval(ymd_hms("2018-01-13 11:30:00 UTC"), 
                                                      ymd_hms("2018-01-13 12:30:00 UTC")),
                     duid == "SNOWTWN1")
Thesis_2018 %>% filter(settlementdate %within% interval(ymd_hms("2018-01-13 11:30:00 UTC"), 
                                                      ymd_hms("2018-01-13 12:30:00 UTC")),
                     duid == "SNOWTWN1")
    #seems like Thesis has extra binding events that fill in the gaps perhaps? 


#2018-01-13 12:50:00   BLUFF1
AEMO_2018 %>% filter(settlementdate %within% interval(ymd_hms("2018-01-13 12:30:00 UTC"), 
                                                      ymd_hms("2018-01-13 13:30:00 UTC")),
                     duid == "BLUFF1")
Thesis_2018 %>% filter(settlementdate %within% interval(ymd_hms("2018-01-13 12:30:00 UTC"), 
                                                      ymd_hms("2018-01-13 13:30:00 UTC")),
                     duid == "BLUFF1")
    #however this extra thesis event is on Nemsight

#Are they all wind famrs?
NEMSIGHT_Details %>% filter(duid %in% 
                                (Thesis_2018[((!(temp1 %in% temp2)) %>% which()),"duid"] %>% unique()))
    #no there are others gen types

#2018-12-29 08:25:00 JBUTTERS
AEMO_2018 %>% filter(settlementdate %within% interval(ymd_hms("2018-12-29 08:00:00 UTC"), 
                                                      ymd_hms("2018-12-29 08:55:00 UTC")),
                     duid == "JBUTTERS")

Thesis_2018 %>% filter(settlementdate %within% interval(ymd_hms("2018-12-29 08:00:00 UTC"), 
                                                      ymd_hms("2018-12-29 08:55:00 UTC")),
                     duid == "JBUTTERS")

#Do these extra events really explain huge shift in 2018 table??


log(Thesis_2018$dispatchmwh) %>% qplot + ylim(c(0,300000))
log(AEMO_2018$dispatchmwh) %>% qplot + ylim(c(0,300000))

### Histogram of LMPS
log(Thesis_2018$lmp) %>% qplot + ylim(c(0,300000))
log(AEMO_2018$lmp) %>% qplot + ylim(c(0,300000))


### MERGE 2 DATSETS
temp <- (AEMO_2018 %>% rename(aemo_lmp = lmp, aemo_dispatchmwh = dispatchmwh)) %>% 
    inner_join(Thesis_2018 %>% rename(thesis_lmp = lmp, thesis_dispatchmwh = dispatchmwh), 
                c('settlementdate','duid')) #%>% 
    # mutate(dif_lmp = thesis_lmp - aemo_lmp,
    #        dif_q = thesis_dispatchmwh - aemo_dispatchmwh,
    #        rev_dif = dif_lmp*aemo_dispatchmwh)


temp <- temp %>% mutate(aemo_lmp_censored = ifelse(aemo_lmp < -1000, -1000, aemo_lmp),
                        aemo_lmp_censored = ifelse(((aemo_lmp > 14200) & (aemo_lmp < 14500)), 14200, aemo_lmp_censored),
                        aemo_lmp_censored = ifelse(aemo_lmp > 14500, 14500, aemo_lmp_censored),
                        dif_lmp = thesis_lmp - aemo_lmp,
                        dif_lmp_censored = thesis_lmp - aemo_lmp_censored)

temp %>% filter(abs(dif_lmp_censored) > 10000) %>% head()
temp %>% filter(abs(dif_lmp_censored) > 10) %>% dim()

temp2 <- temp %>% filter(abs(dif_lmp_censored) > 10)



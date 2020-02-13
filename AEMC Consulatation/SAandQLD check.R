temp1 <- c("NBHWF1", "BLUFF1", "BNGSF1", "BNGSF2", "SNOWNTH1", "SNOWSTH1", "CLEMGPWF", "HDWF1", "HDWF2", "HDWF3", "HALLWF1", "LGAPWF1", "LKBONNY2", "LKBONNY3", "HALLWF2", "SNOWTWN1", "TBSF1", "WGWF1", "WATERLWF")

temp1.1 <- NEMSIGHT_Details %>% filter(duid %in% temp) %>% select(station) %>% .[["station"]]

temp2 <- c("BARCALDN", "LILYSF1", "BARRON-1", "BARRON-2", "CALL_B_1", "CALL_B_2", "CPP_3", "CPP_4", "DAYDSF1", "HAYMSF1", "CLARESF1", "CSPVPS1", "YABULU2", "GSTONE1", "GSTONE2", "GSTONE3", "GSTONE4", "GSTONE5", "GSTONE6", "HAUGHT11", "KAREEYA1", "KAREEYA2", "KAREEYA3", "KAREEYA4", "EMERASF1", "CLERMSF1", "MACKAYGT", "RUGBYR1", "MSTUART1", "MSTUART2", "MSTUART3", "KSP1", "RRSF1", "QROW1K", "QROW2K", "HAMISF1", "WHITSF1", "STAN-1", "STAN-2", "STAN-3", "STAN-4", "YABULU", "SMCSF1", "MEWF1")

temp2.1 <- NEMSIGHT_Details %>% filter(duid %in% temp2) %>% select(station, fuel_type) %>% .[["station"]]


Step_4_Location <- "D:/AEMC Consultation/Data/Cleaned/INITIALMW/2019/Step 4 - Mutated/"
Step_4_files <- paste0(Step_4_Location, list.files(paste0(Step_4_Location)))

data_2019 <- Step_4_files %>% map(~fread(.x)) %>% rbindlist() %>% group_by(station) %>% summary_table_2()

data_2019 %>% filter(station %in% temp2.1) %>% summarise(sum(TObar))

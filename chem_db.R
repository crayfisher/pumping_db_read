#office sever
ex_fn <- "//SLPA-FS1/Shared/On-Lake/Operations Planning/Pond Operations/Databases/Chemistry Database.xlsm"

#site server---------------
#ex_fn <-"//SLPA-FS3/Shared/On-Lake/Operations Planning/Pond Operations/Databases/Pumping Database.xlsm"
ex_tab <- "Pond Brine"

chem_db_raw <- read_excel(ex_fn,ex_tab)

chem_db <- chem_db_raw %>% 
  rename(location = `Sample Location`) %>% 
  mutate(date = floor_date(`Date & Time`,unit = "days"),
         date =  force_tz(date, "Australia/Perth")) %>% 
  dplyr::select(date,location,K) %>% 
  filter(location %in% c("SUMP1",
                         "SUMP2",
                         "SUMP3",
                         "SUMP4",
                         "SUMP5")) %>% 
  mutate(K = K/1000) %>% 
  mutate(location = str_replace(location,"SUMP", "Sump 0")) %>% 
  filter(K != 0) %>% 
  distinct(date, location,.keep_all = T)

chem_db %>% ggplot(aes(date,K,col = location))+
  geom_point()+
  facet_wrap(~location,
             ncol = 1)


ref_list_sumps %>% distinct(location, sump_gr)

pumping_db_proc_sumps_K <- pumping_db_proc %>% 
  filter(gr1 == "trenches") %>% 
  mutate(location = str_replace(location, "03B|03C","03")) %>% 
  group_by(location, date) %>% 
  summarise(Qm3 = sum(Qm3)) #%>% 
  #filter(Qm3 >0)

pumping_db_proc_sumps_K_test <- pumping_db_proc %>% 
  filter(gr1 == "trenches") %>% 
  summarise(Q = sum(Qm3))

div_dates <- parse_date_time(c("1/07/2021", "30/06/2022"), "dmy", tz ="Australia/Perth")

pumping_db_proc_sumps_K_test_2022 <-  pumping_db_proc %>% 
  filter(Qm3 >0) %>% 
  filter(gr1 == "trenches") %>% 
  #filter(date > today() - 365) %>% 
  filter(date >= parse_date_time(c("30/06/2022"), "dmy")) %>% 
  summarise(QLs = sum(Qm3/152 /3600/24*1000),
            QGL = sum(Qm3/1000/1000))
pumping_db_proc_sumps_K_test_2022

pumping_db_proc_sumps_K_test_2021 <-  pumping_db_proc %>% 
  ungroup() %>% 
  #filter(Qm3 >0) %>% 
  filter(gr1 == "trenches") %>% 
  #filter(date > today() - 365) %>% 
  filter(between (date,div_dates[1],div_dates[2])) %>% 
  summarise(QLs = sum(Qm3)/365 /3600/24*1000,
            QGL = sum(Qm3)/1000/1000)
pumping_db_proc_sumps_K_test_2021
  


pumping_db_proc_sumps_K1 <- pumping_db_proc_sumps_K %>% 

  #filter(Qm3 >0) %>% 
  left_join(chem_db) %>% 
  ungroup() %>% 
  group_by(location) %>% 
  fill(K, .direction = "downup") %>% 
  mutate(M = Qm3*K) %>% 
  mutate(time_interval = case_when(date < div_dates[1] ~ "1. 2019 to 1 July 2021",
                                   between (date,div_dates[1],div_dates[2]) ~ "2. 1 July 2021 to 30 June 2022",
                                   date > div_dates[2] ~ "3. 30 June 2022 to now" )) 
pumping_db_proc_sumps_K1_long <- pumping_db_proc_sumps_K1 %>% 
  filter(Qm3 >0) %>% 
  pivot_longer(-c(location,date,time_interval))
  
  
  
chart <- pumping_db_proc_sumps_K1_long %>% 
  ggplot(aes(date,value, col = name))+
  geom_point()+
  
  facet_grid(name ~ location,
             scales = "free_y")
chart

pumping_db_proc_sumps_K1_summary <- pumping_db_proc_sumps_K1 %>% 
  ungroup() %>% 
  group_by(time_interval) %>% 
  summarise(Q_GL = sum(Qm3)/1000/1000,
            M_kt = sum(M)/1000/1000)
pumping_db_proc_sumps_K1_summary
  
write_csv(pumping_db_proc_sumps_K1_summary,"brine_reconciliation.csv")

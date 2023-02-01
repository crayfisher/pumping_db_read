library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(plotly)


#office sever
ex_fn <- "//SLPA-FS1/Shared/On-Lake/Operations Planning/Pond Operations/Databases/Pumping Database.xlsm"

#site server---------------
#ex_fn <-"//SLPA-FS3/Shared/On-Lake/Operations Planning/Pond Operations/Databases/Pumping Database.xlsm"
ex_tab <- "SourceVolume"

pumping_db <- read_excel(ex_fn,ex_tab)


#tod <- today()

unit <- 2 # 1 = L/s or 2 = 1000 m3/d
unit_multiplier <- case_when(unit == 1 ~ 1/3600/24*1000,
                             unit == 2 ~ 1/1000)
unit_multiplier2 <- case_when(unit == 1 ~ 1*3600*24/1000/1000,
                              unit == 2 ~ 1*1000/3600/24*1000)
 
#read in bore list and grouping -----------
ref_list <- read_csv("ref_db_read.csv")
sel <- ref_list %>% 
  distinct(location) %>% 
  unlist()

#read in grouping for bores -------------
ref_list_sumps <- read_csv("ref_sump_db_read.csv") %>% 
  mutate(date = parse_date_time(date,
                                "dmy",
                                tz = "Australia/Perth")) %>% 
  pivot_longer(-date,
               names_to = "location",
               values_to = "sump_gr" )

pumping_db_proc <- pumping_db %>% 
  rename(date = `Source Volume (m3)`) %>% 
  mutate(date = force_tz(date, "Australia/Perth")) %>% 
  pivot_longer(-date,
               names_to = "location",
               values_to = "Qm3") %>% 

  filter(location %in% sel) %>% 
  left_join(ref_list)   %>%                     
  left_join(ref_list_sumps)    %>% 
  mutate(gr2 = ifelse(!is.na(sump_gr),sump_gr,gr2)) %>% 
  dplyr::select(-sump_gr) %>% 
  filter(gr2 != "transfer") %>% 
  mutate(date2= floor_date(date,unit = "week",week_start = 6))


#correction assuming sump 1, 2, old 3-------------
pumping_db_proc_corr_bores <- pumping_db_proc %>% 
  filter( gr1 == "bores") %>% 
  group_by(date,date2,gr1) %>% 
  summarise(Qm3_bores = sum(Qm3)) %>% 
  ungroup() %>% 
  select(date,Qm3_bores)

pumping_db_proc_corr_trenchesE <- pumping_db_proc %>% 
  filter( gr2 == "tr S (S2)") %>% 
  group_by(date,date2,gr2) %>% 
  summarise(Qm3_trenches = sum(Qm3)) %>% 
  #bind_cols(pumping_db_proc_corr_bores) %>% 
  left_join(pumping_db_proc_corr_bores) %>% 
  mutate(Qm3 = Qm3_trenches - Qm3_bores) %>% 
  mutate(gr2 = "tr S (S2)",
         location = "Sump 2'",
         gr1 = "trenches",
         abstraction_type = "trenches",
         aquifer = "LBS",
         shallow_gr = NULL)
tail(pumping_db_proc_corr_trenchesE)

pumping_db_proc_corr2 <- pumping_db_proc %>% 
  filter(gr2 != "tr S (S2)") %>% 
  bind_rows(pumping_db_proc_corr_trenchesE) %>% 
  filter(  Qm3 >0)
#write_excel_csv(pumping_db_proc_corr2,"pumping_db_proc_corr2.csv")


#additional grouping-------------
pumping_db_proc_sumps3 <- pumping_db_proc_corr2 %>% 
  group_by(date,gr1,gr2,abstraction_type, aquifer) %>%
  summarise(Qkm3 = sum(Qm3)/1000) %>% 
  
  filter(  Qkm3 >0) %>% 
  mutate(gr4 = ifelse(gr2 %in% c("tr S (S2)","tr E (S3-S5)"),
                      "tr S-E S 2-3-5",
                      gr2),
         gr5 = ifelse(gr1 == "bores",
                      "bores",gr4),
         gr6 = ifelse(gr1 == "bores",
                      "bores",gr2))

#filterd for plotting------
pumping_db_proc_sumps3_filt <- pumping_db_proc_sumps3 %>% 
  filter( date > today() - time_scale)


#dfs for trends----------
gr_total <- pumping_db_proc_sumps3_filt %>% 
  group_by(date) %>% 
  summarise(Qkm3= sum(Qkm3)) %>% 
  mutate(gr2 = "")

gr_network <- pumping_db_proc_sumps3_filt %>% 
  group_by(date,gr6) %>% 
  summarise(Qkm3= sum(Qkm3)) %>% 
  mutate(gr2 = "")

gr_bores_tr <- pumping_db_proc_sumps3_filt %>% 
  group_by(date,gr1) %>% 
  summarise(Qkm3= sum(Qkm3)) %>% 
  mutate(gr2 = "")

#bores----------
bores <- pumping_db_proc_corr2 %>% 
    distinct(location, gr1, gr2, abstraction_type,aquifer,shallow_gr)
write_csv(bores,"bores.csv")

#df for plotting bores---------
bores_df <- filter(pumping_db_proc_corr2,gr1 == "bores") %>% 
  mutate( Q = Qm3/1000) %>% 
  filter(date > today() - time_scale,
         Q >0)
write_excel_csv(bores_df,"bores_df.csv") 
save(bores_df,file ="bores_df.rdata")
bores_df_gr <- bores_df %>% 
  group_by(gr2,date     ) %>% 
  summarise(Q = sum(Q)) %>% 
  mutate(pad = "")

gr_bores_deep_sh <- bores_df %>% 
  group_by(date,gr2) %>% 
  summarise(Qm3= sum(Qm3)) %>% 
  mutate(pad = "")



bores_deep <- bores_df %>%
  filter(gr2 == "bores deep" )

bores_shallow_high_Q <- bores_df %>% 
  filter(shallow_gr == "high Q" ) 

bores_shallow_low_Q <- bores_df %>% 
  filter(shallow_gr == "low Q")  




library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(plotly)
#library(ggrepel)

#office sever
#ex_fn <- "//SLPA-FS1/Shared/On-Lake/Operations Planning/Pond Operations/Databases/Pumping Database.xlsm"

#site server
ex_fn <-"//SLPA-FS3/Shared/On-Lake/Operations Planning/Pond Operations/Databases/Pumping Database.xlsm"
ex_tab <- "SourceVolume"

pumping_db <- read_excel(ex_fn,ex_tab)

tod <- today()
data_length <- 30
unit <- 2 # 1 = L/s or 2 = 1000 m3/d
unit_multiplier <- case_when(unit == 1 ~ 1/3600/24*1000,
                             unit == 2 ~ 1/1000)
unit_multiplier2 <- case_when(unit == 1 ~ 1*3600*24/1000/1000,
                              unit == 2 ~ 1*1000/3600/24*1000)
  
  
  
sel <- "Sump 01|Sump 02|Sump 04|Sump 03|Sump 05|Pad|T2-"


force_tz(parse_date_time("1/1/2010","dmy",tz = "Pacific/Auckland"), "UTC")

pumping_db_proc <- pumping_db %>% 
  
  rename(date = `Source Volume (m3)`) %>% 
  mutate(date = force_tz(date, "Australia/Perth")) %>% 
  pivot_longer(-date,
               names_to = "location",
               values_to = "Qm3") %>% 
  mutate(location = str_replace(location, "Pad 29", "Pad29s")) %>% 
  filter(str_detect(location,
                    sel)) %>% 
  mutate(gr1 = case_when(str_detect(location,"Sump") ~ "trenches",
                         str_detect(location,"Pad") ~ "bores",
                         str_detect(location,"T2-") ~ "bores"),
         gr2 = case_when(str_detect(location,"Sump 02|Sump 01|Sump 03|Sump 05") ~ "tr E",
                         str_detect(location,"Sump 04") ~ "tr N",
                         str_detect(location,"s|S") & gr1 ==  "bores" ~ "bores shallow",
                         str_detect(location,"T2-") & gr1 ==  "bores" ~ "bores shallow",
                         str_detect(location,"s|S",negate = T) & gr1 ==  "bores" ~ "bores deep")
  ) %>% 
  mutate(date2= floor_date(date,unit = "week",week_start = 6))


bore_list <- pumping_db_proc %>% 
  distinct(location,   gr1,      gr2)


pumping_db_proc_corr_bores <- pumping_db_proc %>% 
  filter( gr1 == "bores") %>% 
  group_by(date,date2,gr1) %>% 
  summarise(Qm3_bores = sum(Qm3)) %>% 
  ungroup() %>% 
  select(Qm3_bores)

pumping_db_proc_corr_trenchesE <- pumping_db_proc %>% 
  filter( gr2 == "tr E") %>% 
  group_by(date,date2,gr1) %>% 
  summarise(Qm3_trenches = sum(Qm3)) %>% 
  bind_cols(pumping_db_proc_corr_bores) %>% 
  mutate(Qm3 = Qm3_trenches - Qm3_bores) %>% 
  select(- c(Qm3_trenches, Qm3_bores)) %>% 
  mutate(gr2 = "tr E",
         location = "Sump 2'")

pumping_db_proc_corr <- pumping_db_proc %>% 
  filter(gr2 != "tr E") %>% 
  bind_rows(pumping_db_proc_corr_trenchesE)

pumping_db_proc_corr %>% 
  write_excel_csv("data_corrected.csv")

pumping_db_proc_corr_sel <- pumping_db_proc_corr %>% 
  filter(date  > today() - data_length              ,
         date  < today()) %>% 
  mutate(Q = Qm3*unit_multiplier)

labs1 <- c("Deep Bores",
           "Shallow Bores",
           "East Trenches (bores subtracted)",
           "North Trenches")
names(labs1) <- c("bores deep",
                  "bores shallow",
                  "tr E",
                  "tr N")



pumping_db_proc_corr_sel_gr <- pumping_db_proc_corr_sel %>% 
  group_by(gr2,date) %>% 
  summarise(Q= sum(Q)) %>% 
  mutate(location = gr2)


pumping_db_proc_corr_sel_gr2 <- pumping_db_proc_corr_sel_gr %>% 
  group_by(date) %>% 
  summarise(Q = sum(Q)) %>% 
  mutate(gr2 = "total")%>% 
  mutate(location = gr2)

cap_fn <- "D:/Pawel/R/pumping_db_read/capacity.csv"


cap_df <- read_csv(cap_fn) %>% 
  pivot_longer(-date_week,
               names_to = "gr2",
               values_to = "Q") %>% 
  rename(date = date_week) %>% 
  mutate(gr2 = case_when(gr2 == "paleo" ~ "bores deep",
                         gr2 == "inter" ~ "bores shallow"),
         Q = Q*3600*24/1000/1000,
         date = parse_date_time(date,"dmy"),
         location = case_when(gr2 == "paleo" ~ "bores deep",
                              gr2 == "inter" ~ "bores shallow"))
cap_df_trunc <- cap_df %>% 
  filter(date  > today() - data_length,
         date  < today())

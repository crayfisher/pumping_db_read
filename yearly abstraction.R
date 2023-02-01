time_scale <- 9999
library(lubridate)
tod <- today()
source("read_data3.R")


fd <- str_glue("plots/yearly/{tod}/")

dir.create(fd,showWarnings = F, recursive = T)

date_lim <- parse_date_time(c("1/11/2021","31/10/2022"),"dmy")

pumping_db_proc_corr2_yr <- pumping_db_proc %>% 
  filter(gr2 != "trenches") %>% 
  bind_rows(pumping_db_proc_corr_trenches) 

write_excel_csv(pumping_db_proc_corr2_yr,"pumping_db_proc_corr2_yr.csv")

pumping_db_proc_sumps_for_yr <- pumping_db_proc_corr2_yr %>% 
  group_by(date,date2,gr1,gr2,abstraction_type, aquifer, shallow_gr) %>%
  summarise(Qkm3 = sum(Qm3)/1000) %>% 
    mutate(gr4 = ifelse(gr2 %in% c("tr S (S2)","tr E (S3-S5)"),
                      "tr S-E S 2-3-5",
                      gr2),
         gr5 = ifelse(gr1 == "bores",
                      "bores",gr4),
         gr6 = ifelse(gr1 == "bores",
                      "bores",gr1))
pumping_db_proc_sumps_for_yr_high_low <- pumping_db_proc_sumps_for_yr %>% 
  mutate(gr7 = str_glue("{gr4} {shallow_gr}",
                        .na = "")) %>% 
  mutate(gr7 = factor(gr7, levels = rev(c("bores shallow low Q",
                                      "bores shallow high Q",
                                      "bores deep ",
                                      "trenches ")))) %>% 
  mutate(QLs = Qkm3*1000/24/3600*1000)




chart <- pumping_db_proc_sumps_for_yr_high_low %>% 
  ggplot(aes(date,QLs,fill = gr7))+
  geom_bar(stat = "identity") +
  ylim(c(0,500))
  
 # geom_line()
chart


pumping_db_proc_sumps_for_yr_total <- pumping_db_proc_sumps_for_yr %>% 
  filter(between(date,date_lim[1],date_lim[2])) %>% 
  group_by(aquifer) %>% 
  summarise(Qkm3 = sum(Qkm3))
pumping_db_proc_sumps_for_yr_total
  
pumping_db_proc_sumps_for_yr_total2 <- pumping_db_proc_sumps_for_yr_high_low %>% 
  filter(between(date,date_lim[1],date_lim[2])) %>% 
  group_by(gr7) %>% 
  summarise(Qkm3 = mean(Qkm3)) %>% 
  mutate(QLs = Qkm3*1000/24/3600*1000)

write_csv(pumping_db_proc_sumps_for_yr_total2,"pumping_db_proc_sumps_for_yr_total2.csv")

source("read_data.R")
library(pals)

fd <- str_glue("plots/annual/")


dir.create(fd,showWarnings = F, recursive = T)

#additional grouping
pumping_db_proc_corr0_bores <- pumping_db_proc_corr %>% 
  filter(gr1 == "bores") %>% 
  select(date,                location,   Qm3, gr1,gr2 )

pumping_db_proc_corr0_trenches <- pumping_db_proc_corr %>% 
  filter(gr1 == "trenches") %>% 
  group_by(date) %>% 
  summarise( Qm3 = sum( Qm3)) %>% 
  mutate(gr1 = "trenches",
         gr2 = "trenches",
         location = "trenches") %>% 
  mutate(Qm3  = ifelse(Qm3 <0,0,Qm3))
pumping_db_proc_corr0_trenches


pumping_db_proc_corr0 <- bind_rows(pumping_db_proc_corr0_bores,pumping_db_proc_corr0_trenches)

#test <- pumping_db_proc_corr0 %>% distinct(location)

dat_filt <- pumping_db_proc_corr0 %>% 
  filter( date >= parse_date_time("30/6/2020","dmy"),
          date <= parse_date_time("30/06/2021","dmy")#,
          #Qm3 >0
          ) %>% 
  mutate(abstraction_type= ifelse(gr1 == "trenches",
                                      "trenches",
                                      gr2),
         aquifer = ifelse(abstraction_type == "bores deep",
                      "paleo",
                      "LBS"))
write_excel_csv(dat_filt,str_glue("{fd}dat_filt.csv"))

dat_filt_wide <- dat_filt %>% 
  dplyr::select(date, location, Qm3) %>% 
  pivot_wider(
              values_from = Qm3,
              names_from =  location   ) %>% 
  select(date,trenches,everything())

write_excel_csv(dat_filt_wide,str_glue("{fd}dat_filt_wide.csv"))

ref <- dat_filt %>% 
  distinct(location, abstraction_type, aquifer)
write_csv(ref,str_glue("{fd}ref.csv" )  )

pumping_db_aquifer <- dat_filt %>% 
  group_by(abstraction_type,date,aquifer) %>%
  summarise(Qkm3 = sum(Qm3)/1000) %>% 
  ungroup() %>% 
  filter( Qkm3 >0) 
  
  
year_total <-   dat_filt %>% 
 
  group_by(abstraction_type) %>% 
  summarise(QGL = sum(Qm3)/1000/1000,
            min_date = min(date),
            mx_date = max(date)) 
year_total 

write_excel_csv(year_total,str_glue("{fd}year_total.csv"))
  
library(scales)
#total
ch_title <- "total"
chart <- ggplot(pumping_db_aquifer,aes(date,Qkm3,fill = abstraction_type))+
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  ggtitle(ch_title)+
  facet_wrap(~aquifer,ncol = 1)

chart
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height =5)

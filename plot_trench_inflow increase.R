#long chart

labs1 <- c("Deep Bores",
           "Shallow Bores",
           "East Trenches (bores subtracted)",
           "North Trenches")
names(labs1) <- c("bores deep",
                  "bores shallow",
                  "tr E",
                  "tr N")

#pre construction
pumping_db_proc_corr_sel_long_A <- pumping_db_proc_corr %>% 
  filter(date  >= parse_date_time("1/6/21","dmy"),
         date  < parse_date_time("15/7/21","dmy")) %>% 
  mutate(Q = Qm3/1000) 


pumping_db_proc_corr_sel_long_gr1_A <- pumping_db_proc_corr_sel_long_A%>% 
  group_by(gr2,date) %>% 
  summarise(Q= sum(Q)) %>% 
  mutate(location = gr2) 

pumping_db_proc_corr_sel_long_gr2_A <- pumping_db_proc_corr_sel_long_gr1_A %>% 
  group_by(date) %>% 
  summarise(Q= sum(Q)) %>% 
  mutate(gr2 = "total")

#trench construction
pumping_db_proc_corr_sel_long_B <- pumping_db_proc_corr %>% 
  filter(date  >= parse_date_time("15/7/21","dmy"),
         date  < parse_date_time("26/08/21","dmy")) %>% 
  mutate(Q = Qm3/1000) 


pumping_db_proc_corr_sel_long_gr1_B <- pumping_db_proc_corr_sel_long_B %>% 
  group_by(gr2,date) %>% 
  summarise(Q= sum(Q)) %>% 
  mutate(location = gr2) 

pumping_db_proc_corr_sel_long_gr2_B <- pumping_db_proc_corr_sel_long_gr1_B %>% 
  group_by(date) %>% 
  summarise(Q= sum(Q)) %>% 
  mutate(gr2 = "total")













chart <-   ggplot(pumping_db_proc_corr_sel_long_gr1_A,
                  aes(date,Q, fill = gr2))+
  geom_bar(data = pumping_db_proc_corr_sel_long_gr1_A,
           stat = "identity")+
  geom_bar(data = pumping_db_proc_corr_sel_long_gr1_B,
           stat = "identity")+
  facet_wrap(~gr2,
             ncol =1,
             labeller = labeller(gr2 =  labs1))+
  stat_smooth(data = pumping_db_proc_corr_sel_long_gr1_A,
              method = "lm",
              se = F)+
  stat_smooth(data = pumping_db_proc_corr_sel_long_gr1_B,
              method = "lm",
              se = F)+
  ylab("Brine Extraction 1000 m3")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  scale_x_datetime(breaks = "1 months",
                   date_labels = "%b-%y")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))#
#scale_y_continuous(breaks = seq(0,150,50),minor_breaks = seq(0,150,10))
chart  
ggsave("brine_extraction_trends.png")

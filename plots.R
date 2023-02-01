source("read_data.R")

#main plot
chart <- pumping_db_proc_corr_sel %>% 
  ggplot(aes(date,Q, fill = location))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr2,
             ncol =1,
             labeller = labeller(gr2 =  labs1))+
  stat_smooth(data = pumping_db_proc_corr_sel_gr,
              method = "lm",
              se = F)+
  ylab("Brine Extraction 1000 m3")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))
#+
  #geom_step(data = cap_df_trunc,aes(date,Q))
  #+
  #theme(legend.position = "bottom")

chart  
ggsave(str_glue("pumping_k_m3_{tod}.png" ))
#library(plotly)
ch_pl <- ggplotly(chart)
ch_pl
htmlwidgets::saveWidget(ch_pl, str_glue("pumping_k_m3_{tod}.html"))



#total plot
chart <- pumping_db_proc_corr_sel_gr %>% 
 
  ggplot(aes(date,Q, fill = gr2))+
  geom_bar(stat = "identity")+

  stat_smooth(data  =pumping_db_proc_corr_sel_gr2,
              method = "lm",
              se = F)+
  ylab("Brine Extraction 1000 m3")+
  ggtitle("Total Brine Extraction")+
  scale_y_continuous(breaks = pretty_breaks(8),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))
chart  
ggsave(str_glue("pumping_total_k_m3_{tod}.png" ),width = 7, height = 3)


pumping_db_proc_corr_sel_bores_d <-  pumping_db_proc_corr_sel %>% 
  filter(gr2 == "bores deep",
         Q != 0) %>% 
  mutate(Q= Q/3600/24*1000*1000)
chart <- pumping_db_proc_corr_sel_bores_d %>% 
  ggplot(aes(date,Q, fill = location))+
  geom_bar(stat = "identity")+
  facet_wrap(~location,
             ncol =2)+
  ylab("Brine Extraction L/s")
chart    
ggplotly(chart)
ggsave(str_glue("pumping_deep {tod}.png" ))

pumping_db_proc_corr_sel_bores_s <-  pumping_db_proc_corr_sel %>% 
  filter(gr2 == "bores shallow",
         Q != 0)%>% 
  mutate(Q= Q/3600/24*1000*1000)
chart <- pumping_db_proc_corr_sel_bores_s %>% 
  ggplot(aes(date,Q, fill = location))+
  geom_bar(stat = "identity")+
  facet_wrap(~location,
             ncol =2,scales = "fixed")+
  ylab("Brine E/xtraction L/s")
chart  

ggsave(str_glue("pumping_shallow {tod}.png" ))


pumping_db_proc_sumps <- pumping_db_proc %>% 
  filter(str_detect(location,"Sump"),
         date > today() - 30,
         Qm3 >0) %>% 
  mutate(gr3 = case_when( str_detect(location,"03|05") ~ "sump 3-5",
                          str_detect(location,"02") ~ "sump 2",
                          str_detect(location,"4") ~ "sump 4"))
  

chart <- ggplot(pumping_db_proc_sumps,aes(date,Qm3,fill = location))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr3,
             ncol =1,scales = "fixed")


chart
ggsave(str_glue("pumping_sumps1 {tod}.png" ))

chart <- ggplot(pumping_db_proc_sumps,aes(date,Qm3,fill = location))+
  geom_bar(stat = "identity")


chart
ggsave(str_glue("pumping_sumps2 {tod}.png" ),width = 10, height =5)


pumping_db_proc_sumps3 <- pumping_db_proc_corr2 %>% 
  group_by(location2,date,gr3,gr1) %>%
  summarise(Qkm3 = sum(Qm3)/1000) %>% 
  
  filter(date > today() - 30,
         Qkm3 >0) %>% 
  mutate(gr4 = ifelse(gr3 %in% c("Sump 2''","Sump 3-5"),
                      "Sump 2-3-5",
                      gr3),
         gr5 = ifelse(gr1 == "bores",
                      "bores",gr4),
         location3 = ifelse(gr1 == "bores",
                            "bores",location2))

chart <- ggplot(pumping_db_proc_sumps3,aes(date,Qkm3,fill = location2))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr3,
             ncol =1,scales = "fixed")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))

chart
ggsave(str_glue("pumping_sumps3 {tod}.png" ),width = 10, height =5)

chart <- ggplot(pumping_db_proc_sumps3,aes(date,Qkm3,fill = location2))+
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))

chart
ggsave(str_glue("pumping_sumps4 {tod}.png" ),width = 10, height =4)


chart <- ggplot(pumping_db_proc_sumps3,aes(date,Qkm3,fill = location2))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr1,
             ncol =1,scales = "fixed")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))

chart
ggsave(str_glue("pumping_sumps5 {tod}.png" ),width = 10, height =5)


chart <- ggplot(pumping_db_proc_sumps3,aes(date,Qkm3,fill = location2))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr4,
             ncol =1,scales = "fixed")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))

chart
ggsave(str_glue("pumping_sumps6 {tod}.png" ),width = 10, height =5)


chart <- ggplot(filter(pumping_db_proc_sumps3,location2 != "Sump 04"),aes(date,Qkm3,fill = location3))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr5,
             ncol =1,scales = "fixed")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))

chart
 ggsave(str_glue("pumping_sumps7 {tod}.png" ),width = 10, height =5)


#long chart

pumping_db_proc_corr_sel_long <- pumping_db_proc_corr %>% 
  filter(date  >= parse_date_time("1/4/20","dmy"),
         date  < today()) %>% 
  mutate(Q = Qm3/1000) %>% 
  filter(gr1 == "bores")
labs1 <- c("Deep Bores",
           "Shallow Bores",
           "East Trenches (bores subtracted)",
           "North Trenches")
names(labs1) <- c("bores deep",
                  "bores shallow",
                  "tr E",
                  "tr N")


pumping_db_proc_corr_sel_long_gr1 <- pumping_db_proc_corr_sel_long %>% 
  group_by(gr2,date) %>% 
  summarise(Q= sum(Q)) %>% 
  mutate(location = gr2) %>% 
  filter(date >= parse_date_time("1/4/2020","dmy"))

pumping_db_proc_corr_sel_long_gr2 <- pumping_db_proc_corr_sel_long_gr1 %>% 
  group_by(date) %>% 
  summarise(Q= sum(Q)) %>% 
  mutate(gr2 = "total")


cap_df_trunc_long <- cap_df %>% 
  filter(date  >= parse_date_time("1/4/20","dmy"),
         date  < parse_date_time("1/6/22","dmy")
         )
list_bore <- pumping_db_proc_corr %>% 
  filter(gr1 == "bores",
         Qm3 > 0) %>% 
  group_by(location,gr2) %>% 
  summarise(date = first(date),
            gr2 = first(gr2)) %>% 
  filter(date >= parse_date_time("1/4/2020","dmy"))

chart <- pumping_db_proc_corr_sel_long %>% 
  ggplot(aes(date,Q, fill = location))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr2,
             ncol =1,
             labeller = labeller(gr2 =  labs1))+
  # stat_smooth(data = pumping_db_proc_corr_sel_long_gr1,
  #             method = "lm",
  #             se = F)+
  ylab("Brine Extraction 1000 m3")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  geom_step(data = cap_df_trunc_long,aes(date,Q))+
  scale_x_datetime(breaks = "2 months",
                   date_labels = "%b-%y")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))#
#scale_y_continuous(breaks = seq(0,150,50),minor_breaks = seq(0,150,10))
chart  

#library(plotly)
ch_pl <- ggplotly(chart, dynamicTicks = T)
ch_pl
htmlwidgets::saveWidget(ch_pl, str_glue("pumping_long_k_m3_{tod}.html"))


chart2 <- chart + geom_text_repel(data = fist_bore,
                                  aes(x=date,y = 25,
                                      label = location),
                                  angle=45,
                                  size=2,
                                  col = "grey",
                                  max.overlaps = Inf)+
  geom_vline(data = fist_bore,aes(xintercept = date),size =0.5,col = "grey")
chart2
ggsave(str_glue("pumping_long__k_m3_{tod}.png" ),width = 10,height =6)


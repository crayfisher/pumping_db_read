source("read_data.R")
library(pals)

fd <- str_glue("plots/{tod}/")
time_scale <- 900

dir.create(fd,showWarnings = F, recursive = T)

#additional grouping
pumping_db_proc_sumps3_yr <- pumping_db_proc_corr %>% 
  ungroup() %>% 
  group_by(date,date2              ,gr2,gr1) %>%
  summarise(Qkm3 = sum(Qm3)/1000) %>% 
  ungroup() %>% 
  filter(date > today() - time_scale,
         Qkm3 >0) %>% 
  mutate(gr2 = factor(gr2,levels = c("bores deep",
                                     "bores shallow",
                                     
                                     "tr S1-S2",
                                     "tr S3-S5",
                                     "tr N S4"))) %>% 
  mutate(date3  = floor_date(date,"week",week_start = wday(today()-1) ))
         
test<- tail(distinct(pumping_db_proc_sumps3_yr,date,date3),n=10)
test

library(scales)
#total
ch_title <- "total"
chart <- ggplot(pumping_db_proc_sumps3_yr,aes(date,Qkm3,fill = gr1))+
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  ggtitle(ch_title)

chart

ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height =5)


#network compasison
ch_title <- "network"
chart <- ggplot(pumping_db_proc_sumps3_yr,aes(date,Qkm3,fill = gr2))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr1,
             ncol =1,scales = "fixed")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  ggtitle(ch_title)

chart
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height =5)




#network compasison week
ch_title <- "network"
chart <- ggplot(pumping_db_proc_sumps3_yr ,aes(date3,Qkm3,fill = gr2))+
  geom_bar(stat = "summary")+
  facet_wrap(~gr1,
             ncol =1,scales = "fixed")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  scale_fill_brewer(palette = "Set1")+
  ggtitle(ch_title)+
  ylab ("Brine extraction 1000 m3/d")

chart
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d_week.png" ),width = 10, height =5)



#bores vs trenches
ch_title <- "bores vs trenches"

chart <- ggplot(pumping_db_proc_sumps3_yr,aes(date,Qkm3,fill = gr1))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr1,
             ncol =1,scales = "fixed")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  ggtitle(ch_title)

chart
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height =5)




time_scale2 <- 500
bores_df <- filter(pumping_db_proc_corr2,gr1 == "bores") %>% 
  mutate(pad = str_replace(location,"s$",""),
        Q = Qm3/1000) %>% 
  filter(date > today() - time_scale2,
         Q >0)
bores_df

bores_df_gr <- bores_df %>% 
  group_by(gr2,date     ) %>% 
  summarise(Q = sum(Q)) %>% 
  mutate(pad = "")




ch_title <- "bores deep"
chart <-  filter(bores_df,gr2 == "bores deep" )%>% 
  ggplot(aes(date,Q, fill = pad))+
  geom_bar(stat = "identity")+
    facet_wrap(~location,
               ncol =1,
               labeller = labeller(gr2 =  labs1),
               scales = "fixed")+
  
  scale_fill_manual(values=as.vector(cols25(25)))+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"),
                     limits = c(0,3))+
  ggtitle(ch_title)+
  stat_smooth(method = "lm",
              se = F)+
  ylab("Brine Extraction 1000 m3")

chart
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height = 10)


bores_shallow_high_Q <- filter(bores_df,gr2 == "bores shallow",
                               location %in% c("Pad 14s",
                                               "Pad 28s",
                                               "Pad29s"))
bores_list <- bores_df %>% distinct(location, gr1,   gr2 )


ch_title <- "bores shallow high Q"
chart <-  bores_shallow_high_Q %>% 
  ggplot(aes(date,Q, fill = pad))+
  geom_bar(stat = "identity")+
  facet_wrap(~location,
             ncol =1,
             labeller = labeller(gr2 =  labs1),
             scales = "fixed")+
  
  scale_fill_manual(values=as.vector(cols25(25)))+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"),
                     limits = c(0,3))+
  ggtitle(ch_title)+
  stat_smooth(method = "lm",
              se = F)+
  ylab("Brine Extraction 1000 m3")

chart
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height = 5)



#bore shallow low Q
bores_shallow_low_Q <- filter(bores_df,gr2 == "bores shallow",
                               (location %in% c("Pad 18s",
                                               "Pad 17s",
                                               "Pad 21s",
                                               "Pad 23s",
                                               "Pad 16s")))



ch_title <- "bores shallow low Q"
chart <-  bores_shallow_low_Q %>% 
  ggplot(aes(date,Q, fill = pad))+
  geom_bar(stat = "identity")+
  facet_wrap(~location,
             ncol =1,
             labeller = labeller(gr2 =  labs1),
             scales = "fixed")+
  
  scale_fill_manual(values=as.vector(cols25(25)))+
  scale_y_continuous(breaks = seq(0,0.4,0.2),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"),
                     limits = c(0,0.4))+
  ggtitle(ch_title)+
  stat_smooth(method = "lm",
              se = F)+
  ylab("Brine Extraction 1000 m3")+
  scale_x_datetime(date_breaks = "2 months",
                   date_labels = "%b %y")

chart
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height = 5)





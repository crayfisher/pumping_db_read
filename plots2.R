source("read_data.R")
library(pals)

fd <- str_glue("plots/{tod}/")
time_scale <- 30

dir.create(fd,showWarnings = F, recursive = T)

#additional grouping
pumping_db_proc_sumps3 <- pumping_db_proc_corr2 %>% 
  group_by(location2,date,gr3,gr1,gr2) %>%
  summarise(Qkm3 = sum(Qm3)/1000) %>% 
  
  filter(date > today() - time_scale,
         Qkm3 >0) %>% 
  mutate(gr4 = ifelse(gr3 %in% c("Sump 2''","Sump 3-5"),
                      "Sump 2-3-5",
                      gr3),
         gr5 = ifelse(gr1 == "bores",
                      "bores",gr4),
         gr6 = ifelse(gr1 == "bores",
                      "bores",gr3),
         location3 = ifelse(gr1 == "bores",
                            "bores",location2))

bores <- pumping_db_proc_corr2 %>% 
  mutate(abstraction_type = ifelse(gr1 == "trenches",
                                   "trenches",
                                   gr2),
         aquifer = ifelse(abstraction_type == "bores deep",
                          "paleo",
                          "LBS")) %>% 
  distinct(location, gr1, gr2, abstraction_type,aquifer)
write_csv(bores,"bores.csv")

library(scales)
#total
ch_title <- "total"
chart <- ggplot(pumping_db_proc_sumps3,aes(date,Qkm3,fill = location2))+
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  ggtitle(ch_title)

chart
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height =5)


#network compasison
ch_title <- "network"
chart <- ggplot(pumping_db_proc_sumps3,aes(date,Qkm3,fill = location2))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr6,
             ncol =1,scales = "fixed")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  ggtitle(ch_title)

chart
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height =5)



#bores vs trenches
ch_title <- "bores vs trenches"

chart <- ggplot(pumping_db_proc_sumps3,aes(date,Qkm3,fill = location2))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr1,
             ncol =1,scales = "fixed")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  ggtitle(ch_title)

chart
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height =5)


#Sump 3&5 vs Sump 2
ch_title <- "Sump 3&5 vs Sump 2"

chart <- ggplot(filter(pumping_db_proc_sumps3,location2 != "Sump 04"),aes(date,Qkm3,fill = location3))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr5,
             ncol =1,scales = "fixed")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  ggtitle(ch_title)

chart
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height =5)

time_scale2 <- time_scale
bores_df <- filter(pumping_db_proc_corr2,gr1 == "bores") %>% 
  mutate(pad = str_replace(location,"s$|S[1-9]$",""),
        Q = Qm3/1000) %>% 
  filter(date > today() - time_scale2,
         Q >0)
bores_df

bores_df_gr <- bores_df %>% 
  group_by(gr2,date     ) %>% 
  summarise(Q = sum(Q)) %>% 
  mutate(pad = "")


#bores deep/shallo
ch_title <- "bores deep-shallow"
chart <-  bores_df %>% 
  ggplot(aes(date,Q, fill = pad))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr2,
             ncol =1,
             labeller = labeller(gr2 =  labs1))+
  
  scale_fill_manual(values=as.vector(polychrome()))+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  ggtitle(ch_title)+
  stat_smooth(data = bores_df_gr,
              method = "lm",
              se = F)+
  ylab("Brine Extraction 1000 m3")

chart
#ggplotly(chart)
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height =5)

ch_title <- "bores deep"
chart <-  filter(bores_df,gr2 == "bores deep" )%>% 
  ggplot(aes(date,Q, fill = pad))+
  geom_bar(stat = "identity")+
    facet_wrap(~location,
               ncol =1,
               labeller = labeller(gr2 =  labs1),
               scales = "fixed")+
  
  #scale_fill_manual(values=as.vector(polychrome()))+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"),
                     limits = c(0,3))+
  ggtitle(ch_title)+
  stat_smooth(method = "lm",
              se = F)+
  ylab("Brine Extraction 1000 m3")

chart
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 8, height =10)


bores_shallow_high_Q <- bores_df %>% filter(str_detect(location,
                                                       str_glue("Pad 14s|Pad 28s|Pad29s|{new_bores_shallow_high_Q_txt}")) ) 
  # filter(bores_df,gr2 == "bores shallow",
  #                              location %in% c("Pad 14s",
  #                                              "Pad 28s",
  #                                              "Pad29s")) 

                             
bores_list <- bores_df %>% distinct(location, gr1,   gr2 )


ch_title <- "bores shallow high Q"
chart <-  bores_shallow_high_Q %>% 
  ggplot(aes(date,Q, fill = pad))+
  geom_bar(stat = "identity")+
  facet_wrap(~location,
             ncol =1,
             labeller = labeller(gr2 =  labs1),
             scales = "fixed")+
  
 # scale_fill_manual(values=as.vector(polychrome()))+
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
bores_shallow_low_Q <- bores_df %>% filter(gr2           == "bores shallow",
                                           str_detect(location,
                                                      str_glue("Pad 14s|Pad 28s|Pad29s|{new_bores_shallow_high_Q_txt}"),
                                                      negate = T))  
  # filter(bores_df,gr2 == "bores shallow",
  #                              !(location %in% c("Pad 14s",
  #                                              "Pad 28s",
  #                                              "Pad29s")))



ch_title <- "bores shallow low Q"
chart <-  bores_shallow_low_Q %>% 
  ggplot(aes(date,Q, fill = pad))+
  geom_bar(stat = "identity")+
  facet_wrap(~location,
             ncol =2,
             labeller = labeller(gr2 =  labs1),
             scales = "fixed")+
  
  #scale_fill_manual(values=as.vector(polychrome()))+
  scale_y_continuous(breaks = seq(0,0.4,0.2),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"),
                     limits = c(0,0.4))+
  ggtitle(ch_title)+
  stat_smooth(method = "lm",
              se = F)+
  ylab("Brine Extraction 1000 m3")

chart
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 7, height = 10)


#bore "bores 08 - vbs 14s, 29s"
bores_shallow_8_14s_29s <- filter(bores_df,location %in% c("Pad 14s",
                                                "Pad29s",
                                               "Pad 08"))



ch_title <- "bores 08 - vbs 14s, 29s"
chart <-  bores_shallow_8_14s_29s %>% 
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


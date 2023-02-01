time_scale <- 250
library(lubridate)
tod <- today()
source("read_data2.R")
library(pals)
library(scales)

fd <- str_glue("plots/{tod}/")

dir.create(fd,showWarnings = F, recursive = T)

#pad 30
pd30_bores <- c( "PD021S1",
                 "PD017S1",
                 "PD018S1",
                 "GT039S1")



bores_sel <- bores_df %>% 
  filter(uid     %in% pd30_bores)
  
  
  
chart <-  bores_sel %>% 
  ggplot(aes(date,Q, fill = pad))+
  geom_bar(stat = "identity")+
  # scale_fill_manual(values = cols)+
  scale_y_continuous(breaks = pretty_breaks(10),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  scale_x_datetime(date_breaks = "1 months",
                   date_labels = "%b %y")+
  ggtitle(ch_title)+
  # stat_smooth(data = bores_df_gr,
  #             method = "lm",
  #             se = F)+
  ylab("Brine Extraction 1000 m3")#+
 # theme_dark()

chart


#NT47 bores

nt47_bores <- "^T2|PD004S|PD016S|PD024S|NT053S|CT011S|GT039|PD017S|PD018S|PD021S"



bores_sel <- bores_df %>% 
  filter(str_detect(uid, nt47_bores))



chart <-  bores_sel %>% 
  ggplot(aes(date,Q, fill = pad))+
  geom_bar(stat = "identity")+
  # scale_fill_manual(values = cols)+
  scale_y_continuous(breaks = pretty_breaks(10),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  scale_x_datetime(date_breaks = "1 months",
                   date_labels = "%b %y")+
  ggtitle(ch_title)+
  # stat_smooth(data = bores_df_gr,
  #             method = "lm",
  #             se = F)+
  ylab("Brine Extraction 1000 m3")#+
# theme_dark()

chart
ggplotly()




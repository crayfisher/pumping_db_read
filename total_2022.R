time_scale <- 365
library(lubridate)
tod <- today()
source("read_data2.R")
library(pals)

fd <- str_glue("plots/{tod}/")

dir.create(fd,showWarnings = F, recursive = T)

date1 <- parse_date_time("1/11/2021","dmy")
n_days <-time_length(interval( date1,today()-1),"days")

total_2022 <- pumping_db_proc_sumps3_filt %>% 
  filter(date >= date1) %>% 
  group_by(aquifer) %>% 
  summarise(Q_GL= sum( Qkm3)/1000,
            Qkm3 = sum(Qkm3)) %>% 
  mutate(Q_Ls = Qkm3*1000 /n_days/3600/24*1000,
         days = n_days,
         Q_GL_A = Q_GL * 365/n_days)

total_2022

write_csv(total_2022, "total_2022_20220419.csv")

library(scales)
#total ---------
ch_title <- "total"
chart <- ggplot(pumping_db_proc_sumps3_filt,aes(date,Qkm3,fill = gr2))+
  geom_bar(stat = "identity")+
  scale_x_datetime(breaks = pretty_breaks(10) )+
  scale_y_continuous(breaks = pretty_breaks(5),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  ggtitle(ch_title)+
  stat_smooth(data = gr_total,
              method = "lm",
              se = F)

chart
fn_png <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" )
fn_html <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.html" )
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height = 5)
ch_pl <- ggplotly(chart, dynamicTicks = T)
ch_pl
htmlwidgets::saveWidget(ch_pl,file.path(normalizePath(dirname(fn_html)),basename(fn_html))) 



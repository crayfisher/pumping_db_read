time_scale <- 600
library(lubridate)
tod <- today()
source("read_data2.R")
library(pals)

fd <- str_glue("plots/long_term/{tod}/")

dir.create(fd,showWarnings = F, recursive = T)

pumping_db_proc_sumps3_filt <- pumping_db_proc_sumps3 %>%
  filter(date < today() -1,
         date > today()- time_scale,
         gr1 == "bores") %>%
  mutate(Qkm3 = Qkm3,
         date3  = floor_date(date,"week",week_start = wday(today()-1-1) ),
         date4 = floor_date(date,"month",week_start = wday(today()-1-1) ))

load("D:/Pawel/R/schedule/df_cum.rdata")
df_cum2 <- df_cum %>% 
  mutate(gr2 = "bores",
         Q_cumulative = Q_cumulative*3600*24/1000/1000)

library(scales)
#total ---------
ch_title <- "total_long_term"
chart <- ggplot(pumping_db_proc_sumps3_filt,aes(date,Qkm3,fill = gr2))+
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  ggtitle(ch_title)+
  geom_step(data = df_cum2,aes(`Estimated ENDING DATE`,Q_cumulative))+
  geom_vline(xintercept = now(),col = "black")


chart
fn_png <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d_cumulative.png" )
fn_html <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.html" )
ggsave(fn_png ,width = 10, height =5)
ch_pl <- ggplotly(chart, dynamicTicks = T)
ch_pl
htmlwidgets::saveWidget(ch_pl,file.path(normalizePath(dirname(fn_html)),basename(fn_html))) 
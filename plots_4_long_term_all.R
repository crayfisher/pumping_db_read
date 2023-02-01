time_scale <- 900
library(lubridate)
tod <- today()
source("read_data2.R")
library(pals)

lims1 <- parse_date_time(c("1/6/2020","1/1/2022"),"dmy")


unit_1 <- "3 months"
fd <- str_glue("plots/long_term/{tod}/")

dir.create(fd,showWarnings = F, recursive = T)

pumping_db_proc_sumps3_filt <- pumping_db_proc_sumps3 %>%
  filter(date < today() -1) %>%
  filter(date < today() -1)
  mutate(Qkm3 = Qkm3/7,
    date3  = floor_date(date,"week",week_start = wday(today()-1-1) ),
    date4 = floor_date(date,"month",week_start = wday(today()-1-1) ))
  

library(scales)
#total ---------
ch_title <- "total_week_long_term"
chart <- ggplot(pumping_db_proc_sumps3_filt,aes(date3,Qkm3,fill = gr2))+
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  scale_x_datetime(date_breaks =  unit_1,
                   date_labels = "%b %y",
                   name = "Date",
                   limits = lims1)+
  ggtitle(ch_title)

chart
fn_png <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" )
fn_html <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.html" )
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height =5)
ch_pl <- ggplotly(chart, dynamicTicks = T)
ch_pl
htmlwidgets::saveWidget(ch_pl,file.path(normalizePath(dirname(fn_html)),basename(fn_html))) 

#network compasison----------
ch_title <- "network_week_long_term"
chart <- ggplot(pumping_db_proc_sumps3_filt,aes(date3,Qkm3,fill = gr2))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr6,
             ncol =1,scales = "fixed")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  scale_x_datetime(date_breaks =  unit_1,
                   date_labels = "%b %y",
                   name = "Date",
                   limits = lims1)+
  ggtitle(ch_title)

chart
fn_png <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" )
fn_html <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.html" )
ggsave(fn_png,width = 10, height =5)
ch_pl <- ggplotly(chart, dynamicTicks = T)
ch_pl
htmlwidgets::saveWidget(ch_pl,file.path(normalizePath(dirname(fn_html)),basename(fn_html))) 



str_replace("a a","\ ","b")
#bores vs trenches------------
ch_title <- "bores vs trenches_week_long_term"

chart <- ggplot(pumping_db_proc_sumps3_filt,aes(date3,Qkm3,fill = gr2))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr1,
             ncol =1,scales = "fixed")+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  scale_x_datetime(date_breaks =  unit_1,
                   date_labels = "%b %y",
                   name = "Date",
                   limits = lims1)+
  ggtitle(ch_title)

chart
fn_png <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" )
fn_html <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.html" )
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height =5)
ch_pl <- ggplotly(chart, dynamicTicks = T)
ch_pl
htmlwidgets::saveWidget(ch_pl,file.path(normalizePath(dirname(fn_html)),basename(fn_html))) 


bores_df2 <- bores_df %>%
  filter(date < today() -1) %>%
  mutate(Q = Q/7,
         date3  = floor_date(date,"week",week_start = wday(today()-1-1) )) %>%
  group_by(date3,pad,location,gr2,shallow_gr) %>%
  summarise(Q= sum(Q))
# bores_df2 <- bores_df %>%
#   filter(date < today() -1) %>%
#   mutate(Q = Q/30,
#          date3  = floor_date(date,"month",week_start = wday(today()-1-1) )) %>%
#   group_by(date3,pad,location,gr2,shallow_gr) %>%
#   summarise(Q= sum(Q))


bores_deep <- bores_df2 %>%
    filter(gr2 == "bores deep" )

bores_shallow_high_Q <- bores_df2 %>% 
  filter(shallow_gr == "high Q" ) 

bores_shallow_low_Q <- bores_df2 %>% 
  filter(shallow_gr == "low Q")  




#bores deep/shallow------------
ch_title <- "bores deep-shallow_week_long_term"
#colls----
#generated with https://medialab.github.io/iwanthue/ for 100 cols
cols <- c("#97474d",
          "#3ab54b",
          "#724bc9",
          "#74d05a",
          "#956bed",
          "#98c037",
          "#3a5cd7",
          "#4c9b23",
          "#c36bea",
          "#bec043",
          "#6675f1",
          "#d3b532",
          "#3c4dbb",
          "#e0a936",
          "#3770df",
          "#de9326",
          "#7048b1",
          "#769627",
          "#9a3eb5",
          "#38bf79",
          "#c4319f",
          "#21d0ab",
          "#e73c83",
          "#457e27",
          "#d873e2",
          "#75b15f",
          "#ea63c7",
          "#3b7e3d",
          "#a163c8",
          "#a0a133",
          "#5653b1",
          "#e37e21",
          "#458ee8",
          "#e8582e",
          "#55d4c7",
          "#c13422",
          "#44bfcb",
          "#d43152",
          "#3eb496",
          "#b92873",
          "#84cc91",
          "#7c46a0",
          "#697c23",
          "#ba84ea",
          "#4b600e",
          "#8389ee",
          "#ae8827",
          "#736ec7",
          "#81761a",
          "#dc7bd4",
          "#3c9063",
          "#ba4691",
          "#6baf85",
          "#9a4b99",
          "#aab56e",
          "#6b4fa0",
          "#d5af61",
          "#2d62a8",
          "#bd5326",
          "#54bce6",
          "#e6655c",
          "#2d9693",
          "#ae3a45",
          "#5199cf",
          "#e98551",
          "#5756a0",
          "#d99553",
          "#5c7cb9",
          "#ad6226",
          "#9599dd",
          "#616117",
          "#d28bdb",
          "#306a3c",
          "#e672b2",
          "#1a6447",
          "#e36489",
          "#2f7b63",
          "#9b3a66",
          "#4d662b",
          "#aa84d4",
          "#815a16",
          "#c7a9ed",
          "#6f5d1d",
          "#d995d0",
          "#818c4b",
          "#7c4c8a",
          "#5e622c",
          "#7d67a5",
          "#9f7f45",
          "#505b8f",
          "#9d4529",
          "#b36d9e",
          "#dca87c",
          "#894b67",
          "#896338",
          "#e694b8",
          "#895029",
          "#e98e84",
          "#c56b79",
          "#b96f54")
#-------
chart <-  bores_df2 %>% 
  ggplot(aes(date3,Q, fill = pad))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr2,
             ncol =1)+
  
  scale_fill_manual(values = cols)+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  scale_x_datetime(date_breaks =  "2 months",
                   date_labels = "%b %y",
                   name = "Date",
                   limits = parse_date_time(c("1/4/2020","1/11/2021"),"dmy"))+
  scale_x_datetime(date_breaks =  unit_1,
                   date_labels = "%b %y",
                   name = "Date",
                   limits = lims1)+
  ggtitle(ch_title)+
  ylab("Brine Extraction 1000 m3")+
  theme_dark()

chart
fn_png <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" )
fn_html <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.html" )
ggplotly(chart)
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height =5)
ch_pl <- ggplotly(chart, dynamicTicks = T)
ch_pl
htmlwidgets::saveWidget(ch_pl,file.path(normalizePath(dirname(fn_html)),basename(fn_html))) 

# bores deep --------
ch_title <- "bores deep"
ch_title <- "Long Term Aquifer Pumping"


chart <-  bores_deep %>% 
  ggplot(aes(date3,Q, fill = pad))+
  geom_bar(stat = "identity",
           position = "stack")+
  facet_wrap(~location,
             ncol = 1,
             scales = "fixed")+
  #scale_fill_brewer(palette = "Dark2")+
  scale_fill_manual(values = cols)+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s")
                     )+
  scale_x_datetime(date_breaks =  unit_1,
                   date_labels = "%b %y",
                   name = "Date",
                   limits = lims1)+
  ggtitle(ch_title)+
  ylab("Brine Extraction 1000 m3")

chart

fn_png <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" )
fn_html <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.html" )
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height =8)
ch_pl <- ggplotly(chart, dynamicTicks = T)

ch_pl
htmlwidgets::saveWidget(ch_pl,file.path(normalizePath(dirname(fn_html)),basename(fn_html))) 


# bores shallow_high_Q --------

ch_title <- "bores shallow high Q"
chart <-  filter(bores_shallow_high_Q#,pad %in% c("Pad 28","Pad 14","Pad 29") 
                 )%>% 
  ggplot(aes(date3,Q, fill = pad))+
  geom_bar(stat = "identity")+
  facet_wrap(~location,
             ncol =1,
             scales = "fixed")+
  
  # scale_fill_manual(values=as.vector(polychrome()))+
  scale_y_continuous(breaks = pretty_breaks(),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"),
                     limits = c(0,3))+
  scale_x_datetime(date_breaks =  unit_1,
                   date_labels = "%b %y",
                   name = "Date",
                   limits = lims1)+
  ggtitle(ch_title)+
  ylab("Brine Extraction 1000 m3")

chart
fn_png <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" )
fn_html <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.html" )
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 8, height =10)
ch_pl <- ggplotly(chart, dynamicTicks = T)
ch_pl
htmlwidgets::saveWidget(ch_pl,file.path(normalizePath(dirname(fn_html)),basename(fn_html))) 


#bore shallow low Q----------

ch_title <- "bores shallow low Q"
chart <-  filter(bores_shallow_low_Q
                 #,pad %in% c("Pad 18","Pad 17","Pad 21", "Pad 23") 
                 ) %>% 
  ggplot(aes(date3,Q, fill = pad))+
  geom_bar(stat = "identity")+
  facet_wrap(~location,
             ncol =2,
             scales = "fixed")+
  scale_y_continuous(breaks = seq(0,0.4,0.2),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"),
                     limits = c(0,0.4))+
  scale_x_datetime(date_breaks =  unit_1,
                   date_labels = "%b %y",
                   name = "Date",
                   limits = lims1)+
  ggtitle(ch_title)+
  ylab("Brine Extraction 1000 m3")

chart
fn_png <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" )
fn_html <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.html" )
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 8, height =10)
ch_pl <- ggplotly(chart, dynamicTicks = T)
ch_pl
htmlwidgets::saveWidget(ch_pl,file.path(normalizePath(dirname(fn_html)),basename(fn_html))) 






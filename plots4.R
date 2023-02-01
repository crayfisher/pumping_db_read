time_scale <- 180
library(lubridate)
tod <- today()
source("read_data3.R")
library(pals)

fd <- str_glue("plots/{tod}/")

dir.create(fd,showWarnings = F, recursive = T)


library(scales)
#total ---------
ch_title <- "total"
chart <- ggplot(pumping_db_proc_sumps3_filt,aes(date,Qkm3,fill = gr2))+
  geom_bar(stat = "identity")+
  scale_x_datetime(breaks = pretty_breaks(5),
                   date_labels  = "%b %y")+
  scale_y_continuous(breaks = pretty_breaks(5),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  ggtitle(ch_title)#+
 # stat_smooth(data = gr_total,
  #            method = "lm",
   #           se = F)

chart
fn_png <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" )
fn_html <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.html" )
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height = 5)
ch_pl <- ggplotly(chart, dynamicTicks = T)
ch_pl
htmlwidgets::saveWidget(ch_pl,file.path(normalizePath(dirname(fn_html)),basename(fn_html))) 

#network compasison----------
ch_title <- "network"
chart <- ggplot(pumping_db_proc_sumps3_filt,aes(date,Qkm3,fill = gr2))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr6,
             ncol =1,scales = "free_y")+
  scale_x_datetime(breaks = pretty_breaks(5),
                   date_labels  = "%b %y" )+
  scale_y_continuous(breaks = pretty_breaks(5),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  ggtitle(ch_title)#+
 # stat_smooth(data = gr_network,
  #            method = "lm",
  #            se = F)

chart
fn_png <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" )
fn_html <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.html" )
ggsave(fn_png,width = 10, height =5)
ch_pl <- ggplotly(chart, dynamicTicks = T)
ch_pl
htmlwidgets::saveWidget(ch_pl,file.path(normalizePath(dirname(fn_html)),basename(fn_html))) 



str_replace("a a","\ ","b")
#bores vs trenches------------
ch_title <- "bores vs trenches"

chart <- ggplot(pumping_db_proc_sumps3_filt,aes(date,Qkm3,fill = gr2))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr1,
             ncol =1,scales = "free_y")+
  scale_x_datetime(breaks = pretty_breaks(5),
                   date_labels  = "%b %y" )+
  scale_y_continuous(breaks = pretty_breaks(5),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  ggtitle(ch_title)+
  stat_smooth(data = gr_bores_tr,
              method = "lm",
              se = F)

chart
fn_png <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" )
fn_html <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.html" )
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 10, height =5)
ch_pl <- ggplotly(chart, dynamicTicks = T)
ch_pl

htmlwidgets::saveWidget(ch_pl,file.path(normalizePath(dirname(fn_html)),basename(fn_html))) 



#bores deep/shallow------------
ch_title <- "bores deep-shallow"
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
chart <-  bores_df %>% 
  ggplot(aes(date,Q, fill = pad))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr2,
             ncol =1,scales = "free_y")+
  
  scale_fill_manual(values = cols)+
  scale_x_datetime(breaks = pretty_breaks(5),
                   date_labels  = "%b %y" )+
  scale_y_continuous(breaks = pretty_breaks(5),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  ggtitle(ch_title)+
  stat_smooth(data = bores_df_gr,
              method = "lm",
              se = F)+
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

chart <-  filter(bores_df,gr2 == "bores deep" )%>% 
  ggplot(aes(date,Q, fill = pad))+
  geom_bar(stat = "identity")+
    facet_wrap(~location,
               ncol = 2,
               scales = "fixed")+
  scale_y_continuous(breaks = pretty_breaks(5),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"),
                     limits = c(0,3))+
  ggtitle(ch_title)+
  stat_smooth(method = "lm",
              se = F)+
  ylab("Brine Extraction 1000 m3")

chart
fn_png <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" )
fn_html <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.html" )
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 8, height =10)
ch_pl <- ggplotly(chart, dynamicTicks = T)

ch_pl
htmlwidgets::saveWidget(ch_pl,file.path(normalizePath(dirname(fn_html)),basename(fn_html))) 


# bores shallow_high_Q --------

ch_title <- "bores shallow high Q"
chart <-  bores_shallow_high_Q %>% 
  ggplot(aes(date,Q, fill = pad))+
  geom_bar(stat = "identity")+
  facet_wrap(~location,
             ncol =2,
             scales = "fixed")+
  
 # scale_fill_manual(values=as.vector(polychrome()))+
  scale_x_datetime(breaks = pretty_breaks(5),
                   date_labels  = "%b %y" )+
  scale_y_continuous(breaks = pretty_breaks(5),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"),
                     limits = c(0,3))+
  ggtitle(ch_title)+
  stat_smooth(method = "lm",
              se = F)+
  ylab("Brine Extraction 1000 m3")

chart
fn_png <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" )
fn_html <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.html" )
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 8, height =10)
ch_pl <- ggplotly(chart, dynamicTicks = T)

htmlwidgets::saveWidget(ch_pl,file.path(normalizePath(dirname(fn_html)),basename(fn_html))) 


#bore shallow low Q----------

ch_title <- "bores shallow low Q"
chart <-  bores_shallow_low_Q %>% 
  ggplot(aes(date,Q, fill = pad))+
  geom_bar(stat = "identity")+
  facet_wrap(~location,
             ncol =2,
             scales = "fixed")+
  scale_y_continuous(breaks = seq(0,0.4,0.2),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"),
                     limits = c(0,0.4))+
  ggtitle(ch_title)+
  stat_smooth(method = "lm",
              se = F)+
  ylab("Brine Extraction 1000 m3")

chart
fn_png <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" )
fn_html <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.html" )
ggsave(str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" ),width = 8, height =10)
ch_pl <- ggplotly(chart, dynamicTicks = T)
ch_pl
htmlwidgets::saveWidget(ch_pl,file.path(normalizePath(dirname(fn_html)),basename(fn_html))) 




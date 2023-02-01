




# bores deep one pad 18--------

bore_sel_pat <- c("Pad 18", "Pad 17", "Pad 21")
bore_sel_pat1 <- paste(bore_sel_pat,collapse=" ")
source("read_monitoring.R")
bore_sel_pat
ch_title <- str_glue("bores deep_{bore_sel_pat1}")

bores_plotQ <- bores_df2 %>% 
  filter(pad %in% bore_sel_pat) %>% 
  rename(padID = pad,
         date = date3,
         value = Q) %>% 
  mutate(data_type = "Q",
         data_type2 = "Q") %>% 
  ungroup() %>% 
  dplyr::select(padID,  value, date, data_type, data_type2) %>% 
  mutate(value = value/3600/24*1000*1000)
  



bores_plot_all <-  bind_rows(bores_plot_WL, bores_plotQ)

distinct(bores_plot_all,padID)

chart <-  bores_plot_all %>% 
  ggplot(aes(date,
             value, 
             col = data_type2 ,
             fill = data_type2 ))+
 
  #facet_wrap(~ data_type,
  #           ncol = 1,
  #           scales = "free_y")+
  facet_grid(data_type ~ padID,
             scales = "free_y") +
  geom_point(data = bores_plot_all %>% filter(data_type == "WL"),
             size = 0.5)+

  geom_bar(data = bores_plot_all %>% filter(data_type == "Q"),
           stat = "identity",
           position = "stack")
chart
  
 
fn_png <-   str_glue("{fd}Q_WL_{ch_title}_{tod}_{time_scale}d.png" )
fn_html <-   str_glue("{fd}Q_WL_{ch_title}_{tod}_{time_scale}d.html" )
ggsave(fn_png,width = 10, height =5)
ch_pl <- ggplotly(chart, dynamicTicks = T)
ch_pl
htmlwidgets::saveWidget(ch_pl,file.path(normalizePath(dirname(fn_html)),basename(fn_html))) 








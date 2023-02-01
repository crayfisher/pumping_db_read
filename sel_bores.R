time_scale <- 180
library(lubridate)
tod <- today()

source("read_rain.R")

df_rain_sel <- df_rain %>% 
  mutate(uid = "rain", hydro_id = "rain", type = "rain") %>% 
  select(date, uid, hydro_id, value, type) %>% 
  filter(date > today() - days(time_scale)) %>% 
  mutate(ID3 = uid)



source("read_data3.R")
source("read_hydro_db.R")
library(pals)
#selected bores
fd <- str_glue("plots/selected/{tod}/")

dir.create(fd,showWarnings = F, recursive = T)

ch_title <- "WL UR QPD17-18-21S rain"



bores_df_sel <- bores_df %>% 
  mutate(value = Qm3/3600/24*1000,
         type = "Q") %>% 
  filter(str_detect(uid,"PD018S1|PD017S1|PD021S1|PT001S1|PT012S1|PD004S1|PD022S1|PD016S1|^T20")) %>% 
  select(date, uid, hydro_id, value, type) %>% 

  left_join(bores, by = c("uid" = "Uid")) %>% 
  mutate(EUR = as.numeric(ur_sel[1,1]),
         NUR = as.numeric(ur_sel[1,2]),
         dist_x = Easting - EUR,
         dist_y = Northing - NUR,
         dist = round((dist_x^2 + dist_y^2)^0.5/1000,1)) %>% 
  select(-c(Easting,Northing,EUR,NUR)) %>% 
  mutate(ID3 = as.factor(str_glue("{uid}_{hydro_id}_{dist}km "))) %>% 
  filter(dist <=3.5)

bores_df_sel
bores_df_sel %>% distinct(uid,dist) %>% arrange(dist)
ref_df

chart <-  bores_df_sel %>% 
  ggplot(aes(date,value, fill = uid))+
  geom_bar(stat = "identity")+
  facet_wrap(~uid,
             ncol =1,scales = "free_y")+
  
  #
  scale_x_datetime(breaks = pretty_breaks(10) )+
  scale_y_continuous(breaks = pretty_breaks(5),
                     sec.axis = sec_axis( trans= ~.*unit_multiplier2, name="L/s"))+
  ggtitle(ch_title)+
  ylab("Brine Extraction 1000 m3")+
  theme_dark()

chart

#trenches#####
trenches_df_sel <- pumping_db_proc_corr_trenches %>% 
  filter(date > today() - days(time_scale)) %>% 
  mutate(value = Qm3/3600/24*1000,
         type = "trenches",
         uid = location,
         hydro_id = location) %>% 
  filter(value >0) %>% 
  ungroup() %>% 
  dplyr::select(date, uid, hydro_id, value, type) %>% 
  mutate(ID3 = uid)
  



#bore_sel_pat_uid <- "GT101M1|NT047|PD030M|CT015M1|GT053M1|TT007M1|ST013M1|S3014M1|NL018M1|NT053M|CT053|KT030M1|NL018|S6009M1|PD016M"

#bore_sel_pat_hydro_uid <- "PD016M2"
bore_sel_pat_hydro_uid <- "xxxx"
bore_sel_pat_hydro_hid <- "Ur|LV_A|LYAGZ007"

df_hydro_sel <- df_comb_hydro %>% 
  # filter(ID %in% bore_sel_pat) #%>% 
  mutate(test = str_detect(Uid,bore_sel_pat_hydro_uid)|str_detect(ID,bore_sel_pat_hydro_hid)) %>% 
  filter(date > today() - days(time_scale)) %>% 
  filter(test == T) %>% 
  mutate(ID3 = as.factor(str_glue("{Uid}_{ID}"))) %>% 
  mutate(type = "WL",
         value = -value) %>% 
  rename(hydro_id = ID,
         uid = Uid) %>% 
  select(date, uid, hydro_id, value, type, ID3)

df_comb_pump_hydro <- bind_rows(df_hydro_sel,bores_df_sel,df_rain_sel,trenches_df_sel) %>% 
  mutate(type = factor(type,levels = c("Q","trenches","rain","WL")))
write_excel_csv(df_comb_pump_hydro,"df_comb_pump_hydro.csv")
chart <- df_hydro_sel %>% 
  ggplot(aes(date,value, col = hydro_id  ))+
  geom_point()+
  facet_wrap(~type,
             ncol = 1)
  
chart

chart <- ggplot(data = NULL, aes(date,value, fill = ID3, col = ID3 ))+
  geom_point(data = df_hydro_sel, size =0.5)+
  geom_bar(data = bores_df_sel, stat = "identity")+
  geom_bar(data = df_rain_sel, stat = "identity")+
  geom_bar(data = trenches_df_sel,stat = "identity")+

  facet_wrap(~type,
             ncol = 1,
             scales = "free_y")+
  scale_x_datetime(date_breaks = "1 months",
                   date_labels = "%b %y")
chart
ggplotly(chart)

chart1_WL <- ggplot(data = NULL, aes(date,value, fill = ID3, col = ID3 ))+
  geom_point(data = df_hydro_sel, size =0.5)+
  #geom_bar(data = bores_df_sel, stat = "identity")+
  #geom_bar(data = df_rain_sel, stat = "identity")+
 # geom_bar(data = trenches_df_sel,stat = "identity")+
  
  # facet_wrap(~type,
  #            ncol = 1,
  #            scales = "free_y")+
  scale_x_datetime(date_breaks = "1 months",
                   date_labels = "%b %y")+
  
  theme(legend.position = "bottom")
chart1_WL

fn_png <-   str_glue("{fd}WL_{ch_title}_{tod}_{time_scale}d.png" )


ggsave(str_glue(fn_png,width = 10, height =5))

labels_Q <- c("shallow bores pumping L/s","rain mm","trench pumping L/s")
names(labels_Q) <- c("Q","rain","trenches")
chart1_other <- ggplot(data = NULL, aes(date,value, fill = ID3 ))+
  #geom_point(data = df_hydro_sel, size =0.5)+
  geom_bar(data = bores_df_sel, stat = "identity",position = "stack" )+
  geom_bar(data = df_rain_sel, stat = "identity", fill = "blue")+
   geom_bar(data = trenches_df_sel,stat = "identity", fill = "black")+

   facet_wrap(~type,
              ncol = 1,
              scales = "free_y",
              labeller = labeller(type  =labels_Q))+
 # scale_fill_brewer(palette  = "Accent")+
  scale_color_brewer(palette  = "Accent")+
  scale_x_datetime(date_breaks = "1 months",
                   date_labels = "%b %y")+
  theme(legend.position = "top")
chart1_other
library(cowplot)
chart_all <- plot_grid(chart1_other, chart1_WL, 
                       align = "h",
                       axis =  "lr",
          ncol =1,
          labels = c('other data', 'WL'), label_size = 12)

chart_all

fn_png <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.png" )
fn_html <-   str_glue("{fd}pumping_{ch_title}_{tod}_{time_scale}d.html" )
ggplotly(chart)
ggsave(fn_png,width = 10, height =7,dpi = 1000)
ch_pl <- ggplotly(chart, dynamicTicks = T)
ch_pl
htmlwidgets::saveWidget(ch_pl,file.path(normalizePath(dirname(fn_html)),basename(fn_html))) 

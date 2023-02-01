library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(plotly)
#install.packages("plotly")

#devtools::install_github("slowkow/ggrepel")
library(ggrepel)
ex_fn <- "//SLPA-FS1/Shared/On-Lake/Operations Planning/Pond Operations/Databases/Pumping Database.xlsm"
#ex_fn <- "W:/Technical/01 Projects/05 Lake Way/18 Groundwater Modelling/_NEW FILING SYSTEM/RES_2021/model setup/pump database test/Pumping Database.xlsm"
ex_tab <- "SourceVolume"

pumping_db <- read_excel(ex_fn,ex_tab)

tod <- today()
sel <- "Sump 01|Sump 02|Sump 04|Sump 03|Pad|T2-"


force_tz(parse_date_time("1/1/2010","dmy",tz = "Pacific/Auckland"), "UTC")

pumping_db_proc <- pumping_db %>% 
  
  rename(date = `Source Volume (m3)`) %>% 
  mutate(date = force_tz(date, "Australia/Perth")) %>% 
  pivot_longer(-date,
               names_to = "location",
               values_to = "Qm3") %>% 
  mutate(location = str_replace(location, "Pad 29", "Pad29s")) %>% 
  filter(str_detect(location,
                    sel)) %>% 
  mutate(gr1 = case_when(str_detect(location,"Sump") ~ "trenches",
                         str_detect(location,"Pad") ~ "bores",
                         str_detect(location,"T2-") ~ "bores"),
         gr2 = case_when(str_detect(location,"Sump 02|Sump 01|Sump 03") ~ "tr E",
                         str_detect(location,"Sump 04") ~ "tr N",
                         str_detect(location,"s|S") & gr1 ==  "bores" ~ "bores shallow",
                         str_detect(location,"T2-") & gr1 ==  "bores" ~ "bores shallow",
                         str_detect(location,"s|S",negate = T) & gr1 ==  "bores" ~ "bores deep")
  ) %>% 
  mutate(date2= floor_date(date,unit = "week",week_start = 6))


bore_list <- pumping_db_proc %>% 
  distinct(location,   gr1,      gr2)


pumping_db_proc_corr_bores <- pumping_db_proc %>% 
  filter( gr1 == "bores") %>% 
  group_by(date,date2,gr1) %>% 
  summarise(Qm3_bores = sum(Qm3)) %>% 
  ungroup() %>% 
  select(Qm3_bores)

pumping_db_proc_corr_trenchesE <- pumping_db_proc %>% 
  filter( gr2 == "tr E") %>% 
  group_by(date,date2,gr1) %>% 
  summarise(Qm3_trenches = sum(Qm3)) %>% 
  bind_cols(pumping_db_proc_corr_bores) %>% 
  mutate(Qm3 = Qm3_trenches - Qm3_bores) %>% 
  select(- c(Qm3_trenches, Qm3_bores)) %>% 
  mutate(gr2 = "tr E",
         location = "Sump 2'")

pumping_db_proc_corr <- pumping_db_proc %>% 
  filter(gr2 != "tr E") %>% 
  bind_rows(pumping_db_proc_corr_trenchesE)


pumping_db_proc_corr_sel <- pumping_db_proc_corr %>% 
  filter(date  > today() -30              ,
         date  < today()) %>% 
  mutate(Q = Qm3/1000)
labs1 <- c("Deep Bores",
           "Shallow Bores",
           "East Trenches (bores subtracted)",
           "North Trenches")
names(labs1) <- c("bores deep",
                  "bores shallow",
                  "tr E",
                  "tr N")

chart <- pumping_db_proc_corr_sel %>% 
  ggplot(aes(date,Q, fill = location))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr2,
             ncol =1,
             labeller = labeller(gr2 =  labs1))+
  stat_smooth(data = filter(pumping_db_proc_corr_sel,gr2 == "tr E"),
              method = "lm",
              se = F)+
  ylab("Brine Extraction 1000 m3")+
  scale_y_continuous(breaks = pretty_breaks())
#scale_y_continuous(breaks = seq(0,150,50),minor_breaks = seq(0,150,10))
chart  
ggsave(str_glue("pumping_k_m3_{tod}.png" ))
#library(plotly)
ch_pl <- ggplotly(chart)
ch_pl
htmlwidgets::saveWidget(ch_pl, str_glue("pumping_k_m3_{tod}.html"))

pumping_db_proc_corr_sel_gr <- pumping_db_proc_corr_sel %>% 
  group_by(gr2,date) %>% 
  summarise(Q= sum(Q))


pumping_db_proc_corr_sel_gr2 <- pumping_db_proc_corr_sel_gr %>% 
  group_by(date) %>% 
  summarise(Q = sum(Q)) %>% 
  mutate(gr2 = "total")


chart <- pumping_db_proc_corr_sel_gr %>% 
  ggplot(aes(date,Q, fill = gr2))+
  geom_bar(stat = "identity")+
  # facet_wrap(~gr2,
  #            ncol =1,
  #            labeller = labeller(gr2 =  labs1))+
  geom_blank(data = pumping_db_proc_corr_sel_gr2)+
  stat_smooth(data  =pumping_db_proc_corr_sel_gr2,
              method = "lm",
              se = F)+
  ylab("Brine Extraction 1000 m3")+
  ggtitle("Total Brine Extraction")#+
# scale_y_continuous(breaks = seq(0,150,50),minor_breaks = seq(0,150,10))
chart  
ggsave(str_glue("pumping_total_k_m3_{tod}.png" ),width = 7, height = 3)




#############
#long chart

pumping_db_proc_corr_sel_long <- pumping_db_proc_corr %>% 
  filter(date  >= parse_date_time("1/1/21","dmy"),
         date  < today()) %>% 
  mutate(Q = Qm3/1000)
labs1 <- c("Deep Bores",
           "Shallow Bores",
           "East Trenches (bores subtracted)",
           "North Trenches")
names(labs1) <- c("bores deep",
                  "bores shallow",
                  "tr E",
                  "tr N")

fist_bore <- pumping_db_proc_corr %>% 
  filter(gr1 == "bores",
         Qm3 > 0) %>% 
  group_by(location,gr2) %>% 
  summarise(date = first(date),
            gr2 = first(gr2)) %>% 
  filter(date >= parse_date_time("1/1/2021","dmy"))


chart <- pumping_db_proc_corr_sel_long %>% 
  ggplot(aes(date,Q, fill = location))+
  geom_bar(stat = "identity")+
  facet_wrap(~gr2,
             ncol =1,
             labeller = labeller(gr2 =  labs1))+
  stat_smooth(data = filter(pumping_db_proc_corr_sel,gr2 == "tr E"),
              method = "lm",
              se = F)+
  ylab("Brine Extraction 1000 m3")+
  scale_y_continuous(breaks = pretty_breaks())
#scale_y_continuous(breaks = seq(0,150,50),minor_breaks = seq(0,150,10))
chart  

#library(plotly)
ch_pl <- ggplotly(chart, dynamicTicks = T)
ch_pl
htmlwidgets::saveWidget(ch_pl, str_glue("pumping_long_k_m3_{tod}.html"))


chart2 <- chart + geom_text_repel(data = fist_bore,
                                  aes(x=date,y = 150,
                                      label = location),
                                  angle=45,
                                  size=2,
                                  col = "grey",
                                  max.overlaps = Inf)+
  geom_vline(data = fist_bore,aes(xintercept = date),size =0.5,col = "grey")
chart2
ggsave(str_glue("pumping_long__k_m3_{tod}.png" ))

pumping_db_proc_corr_sel_long_gr1 <- pumping_db_proc_corr_sel_long %>% 
  group_by(gr2,date) %>% 
  summarise(Q= sum(Q))

pumping_db_proc_corr_sel_long_gr2 <- pumping_db_proc_corr_sel_long_gr1 %>% 
  group_by(date) %>% 
  summarise(Q= sum(Q)) %>% 
  mutate(gr2 = "total")


chart <- pumping_db_proc_corr_sel_long_gr1 %>% 
  ggplot(aes(date,Q, fill = gr2))+
  geom_bar(stat = "identity")+
  # facet_wrap(~gr2,
  #            ncol =1,
  #            labeller = labeller(gr2 =  labs1))+
  geom_blank(data = pumping_db_proc_corr_sel_long_gr2)+
  stat_smooth(data  =pumping_db_proc_corr_sel_long_gr2,
              method = "lm",
              se = F)+
  ylab("Brine Extraction 1000 m3")+
  ggtitle("Total Brine Extraction")#+
# scale_y_continuous(breaks = seq(0,150,50),minor_breaks = seq(0,150,10))
chart  
ggsave(str_glue("pumping_long_total_k_m3_{tod}.png" ),width = 7, height = 3)

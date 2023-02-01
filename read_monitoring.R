library(dbplyr)
library(tidyverse)
library(odbc)
library(lubridate)

library(plotly)
library(scales)

db_txt <-"W:/Technical/01 Projects/05 Lake Way/07 Hydrology/DATABASE/SO4_LY_HydroDatabase.accdb"
con = dbConnect(odbc::odbc(),
                .connection_string =  paste0("Driver=Microsoft Access Driver (*.mdb, *.accdb);DBQ=",
                                             db_txt))

test_db <- tbl(con,"001_Bore Trench ID")
monitoring_db <- tbl(con,"002_LoggerData")

monitoring_db_man <- tbl(con,"002_Manual Water Levels")


ref  <- tbl(con,"000_SITE_ID_REFERENCE")
ref_df <-  ref %>%
  collect() %>% 
  dplyr:: select(-Comment,-ID)

bores <- test_db %>% 
  dplyr::select(Uid, `Point ID`, `Pad ID`, Easting,Northing,`Aquifer Type`,`Use Type`) %>% 
  collect()


bores_pad <- bores %>% 
  dplyr::select(Uid,`Point ID`, `Pad ID`,`Aquifer Type`,`Use Type`)

dist <- 200*1000

df <- monitoring_db %>% 
  dplyr::select(`Bore ID`,`Calculated WL (mbgl)`,`Date / Time`) %>% 
  collect() %>% 
  left_join(ref_df,by = c("Bore ID" = "Alternate ID")) %>% 
  dplyr::select(-"Bore ID") %>% 
  rename(date  = `Date / Time`,
         ID = `Main ID`,
         value = `Calculated WL (mbgl)`) %>% 
  mutate(type = "logger")

df_man <- monitoring_db_man %>% 
  dplyr::select(`Date / Time`,`Bore ID`,`Water Level (mbgl)`) %>% 
  collect() %>% 
  left_join(ref_df,by = c("Bore ID" = "Alternate ID")) %>% 
  dplyr::select(-"Bore ID") %>% 
  rename(date  = `Date / Time`,
         ID = `Main ID`,
         value = `Water Level (mbgl)`)%>% 
  mutate(type = "manual")



df_comb <- bind_rows(df,df_man) %>% 
  left_join(bores_pad,by = c("ID" = "Point ID")) %>% 
  rename(padID = `Pad ID`,
         aquifer = `Aquifer Type`)

#use this  to select bores by pattern
#bore_sel_pat <- "LYPZB|LYPBB|LYSP|3 / 4|5 / 7|3/4|5/7"


bores_plot_WL <- df_comb %>% 
  filter(padID %in% bore_sel_pat,
                 aquifer     == "Paleo Sand") %>% 
  dplyr::select(padID,value,date,`Use Type`) %>% 
  rename(data_type2 = `Use Type`) %>% 
  mutate(data_type = "WL",
         value = - value)
view(bores_plot_WL)
distinct(bores_plot_WL,padID)

chart <- bores_plot_WL %>%
  ggplot(aes(date,value,col = padID))+
  #facet_wrap(~ID)+ #this will create "small multiple" charts by bore
  geom_point ()+
 # scale_y_reverse(breaks = pretty_breaks())+
  scale_x_datetime(date_breaks = "6 month",
                   date_labels = "%b %y")
chart


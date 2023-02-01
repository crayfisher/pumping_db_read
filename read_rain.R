
library(jsonlite)
library(lubridate)

library(tidyverse)
j_txt <- "https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?start=19070101&finish=20300101&station=13012&format=json&comment=R&username=prakowski@gmail.com"

j_data <- read_json (j_txt,simplifyVector = T)

df_rain <- j_data$data %>% 
  as_tibble() %>%
  unnest() %>% 
  mutate(date = parse_date_time(date,"ymd"))

df_rain

# df_plot <- df %>% 
#   filter(date > parse_date_time("1/1/2017","dmy")) %>% 
#   mutate(date_month = floor_date(date,"month")) %>% 
#   group_by(date_month) %>% 
#   summarise(value = sum(value))
# 
# chart <- df_plot %>% ggplot(aes(date_month,value))+
#   geom_bar(stat = "identity")
# chart
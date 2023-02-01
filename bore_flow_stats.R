flow_stats_bores <-  filter(pumping_db_proc_corr2,gr1 == "bores") %>% 
  mutate( QLs = Qm3/24/3600*1000) %>% 
  filter(date > today() - 90,
         QLs >0) %>% 
  group_by(location) %>% 
  summarise(Qmax = max(QLs),
            Q90 = quantile(QLs,0.9),
            Q75 = quantile(QLs,0.75),
            Qmean = mean(QLs))
flow_stats_bores
write_csv(flow_stats_bores,"flow_stats_bores.csv")

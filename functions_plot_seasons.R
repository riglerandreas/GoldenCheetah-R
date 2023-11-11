


plot_km_seasons <- function(df_gc, seasons, days_plot =365, km_goal = 9000){
  km_slope <- km_goal/365
  
  plot_df_gc <- df_gc %>%  mutate(year = lubridate::year(date),
                                  year = as.factor(year)) %>% 
    group_by(year) %>% arrange(date) %>% 
    mutate(km = cumsum(km),
           day = lubridate::yday(date)) %>%
    ungroup()
  if(max(plot_df_gc$date) != Sys.Date()){
    
    data_today <- plot_df_gc %>% filter(date == max(plot_df_gc$date)) %>% 
      mutate(date = Sys.Date())
    
    plot_df_gc <- plot_df_gc %>% bind_rows(data_today)
  }
  
  
  km_today = plot_df_gc %>% filter(date == Sys.Date()) %>% select(km) %>% pull()
  km_today = round(km_today)
  
  km_missing = round(km_goal -km_today)
  
  date_today <- Sys.Date()
  week_today <- isoweek(date_today)
  weeks_year <- 52
  weeks_left <- weeks_year - week_today
  year_today <- year(date_today)
  km_per_week <- round(km_missing / (weeks_left-1))
  
  plot_df_gc <- plot_df_gc %>% filter(day <= days_plot & year %in% seasons) 
  
  plot_df_gc %>%
    ggplot(aes(x=day, y = km,color = year)) + 
    geom_line() +
    geom_line(data= plot_df_gc %>% filter(year == year_today), size = 1) +
    #geom_point() +
    geom_abline(slope = km_slope, size = 1, linetype = 2, alpha = 1/2)+
    geom_vline(xintercept = lubridate::yday(Sys.Date()), linetype = 2)+
    labs(y = "km",
         title = str_c(km_today, " km until today, ",' Target: ', km_goal, ' km, that means: ' ,km_per_week, " km/week"),
         subtitle = str_c(weeks_left, " weeks left"))+
    theme_light()
  
  
}



plot_hm_seasons <- function(df_gc, seasons, days_plot =365, km_goal = 9000){
  km_slope <- km_goal/365
  
  plot_df_gc <- df_gc %>%  mutate(year = lubridate::year(date),
                                  year = as.factor(year)) %>% 
    group_by(year) %>% arrange(date) %>% 
    mutate(hoehengewinn = cumsum(hoehengewinn),
           day = lubridate::yday(date)) %>% ungroup()
  
  if(max(plot_df_gc$date) != Sys.Date()){
    
    data_today <- plot_df_gc %>% filter(date == max(plot_df_gc$date)) %>% 
      mutate(date = Sys.Date())
    
    plot_df_gc <- plot_df_gc %>% bind_rows(data_today)
  }
  
  
  hm_today = plot_df_gc %>% filter(date == Sys.Date()) %>% select(hoehengewinn) %>% pull()
  hm_today = round(hm_today)
  
  #km_missing = round(km_goal -km_today)
  
  date_today <- Sys.Date()
  week_today <- isoweek(date_today)
  weeks_year <- 52
  weeks_left <- weeks_year - week_today
  year_today <- year(date_today)
  #km_per_week <- round(km_missing / (weeks_left-1))
  
  plot_df_gc <- plot_df_gc %>% filter(day <= days_plot & year %in% seasons) 
  
  plot_df_gc %>%
    ggplot(aes(x=day, y = hoehengewinn/1000,color = year)) + 
    geom_line() +
    #geom_line(data= plot_df_gc %>% filter(year == year_today), size = 1) +
    #geom_point() +
    #geom_abline(slope = km_slope, size = 1, linetype = 2, alpha = 1/2)+
    geom_vline(xintercept = lubridate::yday(Sys.Date()), linetype = 2)+
    labs(y = "1000 m",
         title = str_c(hm_today, " m elevation gain until today, "))+
    theme_light()
  
  
}




#================================================================

plot_hm_km <- function(df_gc, seasons){
  #plot km over hm for each seson
  
  
  plot_df_gc <- df_gc %>%  mutate(year = lubridate::year(date),
                                  year = as.factor(year),) %>% 
    group_by(year) %>% arrange(date) %>% 
    mutate(hoehengewinn = cumsum(hoehengewinn),
           day = lubridate::yday(date)) %>% ungroup()
  
  if(max(plot_df_gc$date) != Sys.Date()){
    
    data_today <- plot_df_gc %>% filter(date == max(plot_df_gc$date)) %>% 
      mutate(date = Sys.Date())
    
    plot_df_gc <- plot_df_gc %>% bind_rows(data_today)
  }
  
  day_today = yday(Sys.Date())
  
  plot_df_gc1 <- plot_df_gc %>% filter(day <= day_today) %>%
    group_by(year) %>% 
    summarise(hoehengewinn = sum(hoehengewinn,na.rm = TRUE),
              km = sum(km,na.rm = TRUE))
  
  plot_df_gc2 <- plot_df_gc %>% filter(day <= day_today) %>%
    group_by(year) %>% arrange(day) %>%
    mutate(hoehengewinn = cumsum(hoehengewinn),
           km = cumsum(km))
  
  
  p_2<-plot_df_gc2 %>%
    ggplot(aes(x=km, y = hoehengewinn/1000,color = year)) + 
    geom_line() +
    
    labs(y = "1000 m",
         title = str_c( " m elevation gain until today, "))+
    theme_light()
  
  plot(p_2)
  #return(plot_df_gc2)
  
}

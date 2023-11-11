
read_gc_data <- function(path_csv){
  data_gc <-  read_csv( path_csv) %>% 
    mutate(date = parse_character(Datum, locale = locale(encoding = "latin1")),                                                      date = as.Date(date,format = "%d.%B %Y")) %>% 
    select( date, everything(), -Datum, -dauer) %>% 
    rename("hr" = "hf_mean",
           "duration" = "zeit_in_bewegung") %>%
    filter(duration >0)
  
  data_gc <- data_gc %>% mutate(hr_watt = case_when((w_mean >0) & (hr >0) ~ hr/w_mean)) %>% 
    arrange((date))
  
  return(data_gc)
  
}





generate_season_summary <- function(lactat_data, data_gc, duration_weeks){
  
  today_date = tibble(date = today())
  
  lactat_calculated <- lactat_data  %>% bind_rows(today_date) %>% 
    mutate(date_start = date - weeks(duration_weeks)) 
  
  activites_aggregated_list <- list()
  for (i in seq_along(lactat_calculated$date)) {
    date_start = lactat_calculated$date_start[i]
    date_end = lactat_calculated$date[i]
    
    w_2mmol <- lactat_calculated$w_2mmol[i]
    w_5mmol <- lactat_calculated$w_5mmol[i]
    
    dates = tibble(date = seq(ymd(date_start), ymd(date_end), by = "1 day"))
    
    activities_data <- dates %>% left_join(data_gc)  
    
    activites_aggregated_list[[i]] <- activities_data %>% 
      mutate(long = case_when(duration >= 3 ~ 1, TRUE ~ 0),
             high_TSS = case_when(TSS > 100 ~1, TRUE ~ 0)) %>%
      
      summarise(n = n(),
                n_long = sum(long, na.rm = TRUE),
                n_high_TSS = sum(high_TSS, na.rm = TRUE),
                TSS = sum(TSS, na.rm = TRUE),
                duration = sum(duration, na.rm = TRUE),
                watt = mean(w_mean, na.rm = TRUE)) %>%
      mutate(date_test = date_end,
             w_2mmol = w_2mmol,
             w_5mmol = w_5mmol)
    
  }
  
  result= bind_rows(activites_aggregated_list) %>% 
    distinct(date_test, .keep_all = TRUE) %>%
    mutate(year = lubridate::year(date_test) %>% as.factor())
  return(result)
}



plot_w_duration <- function(plot_data, duration_weeks){
  
  plt_duration <- plot_data %>% 
    ggplot(aes(y=w_2mmol))+
    geom_jitter(aes(x=duration, color = year))+
    #geom_vline(xintercept = activities_today$TSS)+
    geom_line(aes(x=duration, group = year, color = year))+
    labs(title = str_c("duration last ", duration_weeks, " weeks"))+
    theme_light()
  
  plt_tss <- plot_data %>% 
    ggplot(aes(y=w_2mmol))+
    geom_jitter(aes(x=TSS, color = year))+
    #geom_vline(xintercept = activities_today$TSS)+
    geom_line(aes(x=TSS, group = year, color = year))+
    geom_smooth(aes(x=TSS))+
    labs(title = str_c("TSS last ", duration_weeks, " weeks"))+
    theme_light()
  
  p_result = plt_duration / plt_tss
  return(p_result)
  
}

calc_and_plot <-function(lactat_data, data_gc, duration_weeks){
  
  plot_data <- generate_season_summary(lactat_data, data_gc,duration_weeks)
  plot_w_duration(plot_data,duration_weeks)
}



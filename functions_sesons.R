generate_season_summary <- function(lactat_data, data_gc, duration_weeks){
  # aggregates the aktivity data per season
  #
  # lactat_data ... tibble with date and watt_2mmol, watt_5mmol values for each season
  # data_gc ... activity data from gc 
  # duration_weeks ... how many weeks are considered per season?
  #
  # the date of today is also included as a season
  
  
  today_date = tibble(date = today())
  
  lactat_calculated <- lactat_data  %>% bind_rows(today_date) %>% 
    mutate(date_start = date - weeks(duration_weeks)) 
  
  activites_aggregated_list <- list()
  for (i in seq_along(lactat_calculated$date)) {
    date_start = lactat_calculated$date_start[i]
    date_end = lactat_calculated$date[i]
    
    watt_2mmol <- lactat_calculated$watt_2mmol[i]
    watt_5mmol <- lactat_calculated$watt_5mmol[i]
    
    dates = tibble(date = seq(ymd(date_start), ymd(date_end), by = "1 day"))
    
    activities_data <- dates %>% left_join(data_gc, by = "date")  
    activities_data <- activities_data %>% filter(duration >0)
    
    aggregation_1 <- activities_data %>% 
                            mutate(long = case_when(duration >= 3 ~ 1, TRUE ~ 0),
                                   high_TSS = case_when(TSS > 100 ~1, TRUE ~ 0)) %>%
                            
                            summarise(n = n(),
                                      n_long = sum(long, na.rm = TRUE),
                                      n_high_TSS = sum(high_TSS, na.rm = TRUE),
                                      TSS = sum(TSS, na.rm = TRUE),
                                      duration = sum(duration, na.rm = TRUE)) %>%
                            mutate(date_test = date_end,
                                   watt_2mmol = !!watt_2mmol,
                                   watt_5mmol = !!watt_5mmol)
    
    aggregation_watt <- activities_data %>% filter(w_mean>0) %>%
                              summarise(n_bike = n(),
                                        watt = mean(w_mean, na.rm = TRUE))
    
    activites_aggregated_list[[i]]  <- aggregation_1 %>% bind_cols(aggregation_watt)
  }
  
  result= bind_rows(activites_aggregated_list) %>% 
    distinct(date_test, .keep_all = TRUE) %>%
    mutate(year = lubridate::year(date_test) %>% as.factor())
  return(result)
}



#=================Correlation================================

correlation_2mmol <- function(data_season, col_selected){
  
  data_season <- data_season %>% filter(watt_2mmol >0) %>% 
    select(col_selected) %>% cor() %>% as_tibble() %>% 
    mutate(variables = col_selected) 
  data_season <- data_season %>% select(variables, watt_2mmol) %>% 
    filter(variables != "watt_2mmol") %>% 
    pivot_wider(names_from = variables, 
                values_from = watt_2mmol)
  
  return(data_season)
}

correlation_5mmol <- function(data_season, col_selected){
  
  data_season <- data_season %>% filter(watt_5mmol >0) %>% 
    select(col_selected) %>% cor() %>% as_tibble() %>% 
    mutate(variables = col_selected) 
  data_season <- data_season %>% select(variables, watt_5mmol) %>% 
    filter(variables != "watt_5mmol") %>% 
    pivot_wider(names_from = variables, 
                values_from = watt_5mmol)
  
  return(data_season)
}
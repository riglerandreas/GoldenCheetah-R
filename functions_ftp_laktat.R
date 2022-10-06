
#=========================== Laktat Curve =============================================

add_missing_dates <- function(df, colname){
  #creates a tibble with all dates between the first test and today
  #with the last known value of the colname for each day
  
  dates <- tibble(date = seq(min(df$date), Sys.Date(), by = "day"))
  
  result <- dates  %>% left_join(df, by = "date") %>% select(date, colname) %>% zoo::na.locf()
  
  return(result)
}



#==================================================================================================



ftp_calculation <- function(data_raw, date_selected){
  date_selected <- as.Date(date_selected)
  data_raw <- data_raw %>% filter(datum == date_selected) #year == jahr & month == monat)
  
  #fehlmessungen filter: (laktat muss steigen)
  data_raw <- data_raw %>% mutate(rising_laktat = laktat-lag(laktat, 
                                                             default =0)) %>% 
    filter(rising_laktat >0) %>% select(-rising_laktat)
  
  data_measured <- data_raw
  
  model <- lm(laktat~(watt*watt),data=data_raw)#as.list(test) %>% 
  #model_laktat <- nls(laktat~a*exp(b*watt),data = data_raw,start = list(a=0.01,b=0.01))
  model_laktat <- nls(watt~a*log(b*laktat),data = data_raw,start = list(a=10,b=1))
  
  data_raw %>% data_grid(watt = seq_range(watt,30)) %>%
    add_predictions(model,"laktat") %>%
    ggplot(aes(x=watt,y=laktat))+geom_point()+
    geom_point(data=data_raw,aes(color="original"))
  
  laktat_prediction <- data_raw %>% data_grid(laktat = seq_range(0.1:12.1,220)) %>%
    add_predictions(model_laktat,"watt") 
  
  w_5m <- laktat_prediction %>% filter(laktat >4.9 & 
                                         laktat < 5.1) %>% 
    summarise(w_5m = mean(watt)) %>% round(digits=1)
  w_2m <- laktat_prediction %>% filter(laktat >1.9 & 
                                         laktat < 2.1) %>% 
    summarise(w_2m = mean(watt)) %>% round(digits=1)
  
  laktat_result <- data_measured %>% mutate(gemessen = TRUE) %>%
    select(laktat,watt,gemessen) %>%
    bind_rows(laktat_prediction) %>%
    mutate(datum = date_selected)
  
  result <-  list(data = laktat_result,#laktat_prediction,
                  w_5m = w_5m,
                  w_2m = w_2m)
  
  return(result)
}



#==================================================================================================

###---------------------------calculation of 2mmol Watt
### with nonlineare regression model

watt_2mmol <- function(d){
  
  result <-tibble()
  #fehlmessungen filter: (laktat muss steigen)
  d <- d %>% arrange(watt) %>% mutate(ungueltig = laktat-lag(laktat, 
                                                             default =0)) %>% 
    filter(ungueltig >0) %>% select(-ungueltig)
  
  model_laktat <- nls(watt~a*log(b*laktat),data = d,start = list(a=10,b=1))
  
  
  
  laktat_prediction <- d %>% data_grid(laktat = seq_range(laktat,200)) %>%
    add_predictions(model_laktat,"watt") 
  date <- d %>% summarise(mean(datum)) %>% pull()
  watt <- laktat_prediction %>% filter(laktat <= 2) %>% summarise(watt = max(watt)) %>% pull()
  
  result <- tibble("date" = date,
                   "watt_2mmol" = watt)
  return(result)
}


#==================================================================================================

watt_xmmol <- function(d,x){
  
  result <-tibble()
  #fehlmessungen filter: (laktat muss steigen)
  d <- d %>% arrange(watt) %>% mutate(ungueltig = laktat-lag(laktat, 
                                                             default =0)) %>% 
    filter(ungueltig >0) %>% select(-ungueltig)
  
  model_laktat <- nls(watt~a*log(b*laktat),data = d,start = list(a=10,b=1))
  
  
  
  laktat_prediction <- d %>% data_grid(laktat = seq_range(laktat,200)) %>%
    add_predictions(model_laktat,"watt") 
  date <- d %>% summarise(mean(datum)) %>% pull()
  watt <- laktat_prediction %>% filter(laktat <= x) %>% summarise(watt = max(watt)) %>% pull()
  
  result <- watt
  return(result)
}
#==================================================================================================

select_ftp_mmol <- function(df){
  # to choose the most actual value w_5mmol or ftp
  # returns ftp_mmol
  
  df  <- df %>% distinct() %>% mutate(w_5mmol = case_when(is.na(w_5mmol) ~ftp, TRUE ~ w_5mmol))

  
  df <- df %>% arrange(date) %>%
    mutate(ftp_diff = as.numeric(ftp !=lag(ftp, default = 2)),
           w_5_diff = as.numeric(w_5mmol !=lag(w_5mmol, default = 2)),
           ftp_test = cumsum(ftp_diff),
           w_5_test = cumsum(w_5_diff)) 
  
    df_days <- df %>% group_by(ftp_test) %>% arrange(date) %>%
                        mutate(ftp_days = row_number()) %>% 
                            ungroup()
    
    df_days <- df_days %>% group_by(w_5_test) %>% arrange(date) %>%
                            mutate(w_5_days = row_number()) %>% 
                              ungroup()
    
    result <- df_days %>%  mutate(ftp_mmol = case_when(ftp_days < w_5_days ~ ftp, TRUE ~ w_5mmol),
                                  ftp_mmol = case_when(is.na(ftp_mmol) ~ ftp, TRUE ~ ftp_mmol))
  
  
  result <-  result %>% ungroup() %>% select(-ftp_diff, -ftp_test, -w_5_diff, -w_5_test)
    
  return(result)
}
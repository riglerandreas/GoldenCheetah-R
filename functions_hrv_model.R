

#=================================Prepare data for ml =======================================
data_to_minute <- function(d){
  d <- d %>% filter(str_detect(device,"P2MIn") &
                      hr > 50 &
                      hr <200 &
                      watt > 0) %>%
    rename("minute" = "sec_bin") %>%
    
    group_by(workout_code,date,device,file_name, hour, minute) %>% 
    summarise(watt = as.integer(mean(watt)),
              hr = as.integer(mean(hr))) %>%
    mutate(minutes_10 = minute %/% 10,
           workout_lower = str_to_lower(workout_code),
           cat = case_when(str_detect(workout_lower,"ga1") ~ "GA1",
                           str_detect(workout_lower,"sst") ~ "SST",
                           str_detect(workout_lower,"vo2max") ~ "Vo2max",
                           str_detect(workout_lower,"laktattest") ~ "Laktattest",
                           TRUE ~ "other")) %>% 
    select(-workout_lower)
  return(d)
}


watt_before <- function(x,n = 3){
  result = rollmean(x,n,na.pad = TRUE, align = "right", fill = 0)
  return(result)
}


data_features <- function(d){
  d <- d %>%   group_by(file_name) %>%
    arrange(minute) %>%
    mutate( hr_3 = lag(hr,3),
            hr_5 = lag(hr,5),
            hr_10 = lag(hr,10)) %>%
    mutate(watt_mean= cummean(watt),
           watt_3 = lag(watt,3,default = 0),
           watt_5 = lag(watt,5,default = 0),
           watt_10 = lag(watt,10,default = 0),
           watt_m2 = watt_before(watt,2),
           watt_m3 = watt_before(watt,3),
           watt_m4 = watt_before(watt,4),
           watt_m5 = watt_before(watt,5),
           watt_m10 = watt_before(watt,10),
           watt_m15 = watt_before(watt,15),
           watt_m20 = watt_before(watt,20),
           watt_m25 = watt_before(watt,25),
           watt_m30 = watt_before(watt,30),
           watt_m40 = watt_before(watt,40),
           watt_m50 = watt_before(watt,50),
           watt_m60 = watt_before(watt,60),
    ) %>% 
    ungroup()
  return(d)
  
}
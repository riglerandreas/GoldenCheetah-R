
#================================ GC Summary functions =================================

prepare_gc_summary <- function(df){

    df <- df %>% mutate(date = parse_character(Datum, locale = locale(encoding = "latin1")),
                        date = as.Date(date,format = "%d.%B %Y"))
    
    df <- df %>% select( date, everything(), -Datum, -dauer) %>% 
                                                                rename("hr" = "hf_mean",
                                                                       "duration" = "zeit_in_bewegung") 
    
    df <- df %>% filter(duration >0)
    df <- df %>% mutate(hr_watt = case_when((w_mean >0) & (hr >0) ~ hr/w_mean))
    
    return(df)
}
#=============================== GC Intervall activity data =============================  
#------------------------- Get Intervall data:------------------------------------------

get_intervals <- function(data_ride){
  library(zoo)
  
  intervals_gc_list <- data_ride$RIDE$INTERVALS
  
  intervals_n <- intervals_gc_list %>% length()
  
  intervals_list <- c()
  for (i in c(1:intervals_n)) {
    intervals_list[[i]] <- tibble(
      interval_name = intervals_gc_list[[i]]$NAME,
      interval_start = intervals_gc_list[[i]]$START,
      #interval_ptest = intervals_gc_list[[i]]$PTEST
    ) %>% 
      mutate(interval_start = abs(interval_start),
             interval_name = case_when(is.null(interval_name) ~" ",TRUE ~interval_name))
    
  }
  
  
  interval <- intervals_list %>% bind_rows() %>% arrange(interval_start)
  
  # get the ride data
  ridedata_list <- data_ride[[1]]$SAMPLES
  ridedata <- bind_rows(ridedata_list)
  
  
  
  #join with intervals
  ridedata_joined <- ridedata %>% left_join(interval, 
                                            by = c("SECS" = "interval_start")) %>%
    arrange(SECS)     
  
  #fill NA
  ridedata_interval <- ridedata_joined %>% 
                            #filter so that the first value is not Null
                      filter(SECS >= !!intervals_list[[1]]$interval_start ) %>%
                            mutate(interval_name = na.locf(interval_name),
                                  #interval_ptest = na.locf(interval_ptest)
                                  )
  
  #aggregate
  interval_aggregation <- ridedata_interval %>% group_by(interval_name) %>% #interval_ptest
    
    summarise(interval_watt = mean(WATTS, na.rm = TRUE),
              interval_hr = mean(HR, na.rm = TRUE),
              interval_hr_max = max(HR, na.rm = TRUE),
              interval_hr_min = min(HR, na.rm = TRUE),
              interval_sec = n())
  
  
  interval_aggregation <- interval_aggregation %>% 
    mutate(#file_name = files_selected$file_name[i],
      workout_code = as.character(data_ride$RIDE$TAGS$`Workout Code`),
      device = data_ride$RIDE$TAGS$Rad,
      start_time = data_ride$RIDE$STARTTIME,
      date = as.Date(start_time)) %>% select(date, workout_code, everything())
  
  
  
  
  return(interval_aggregation)
}


#-----------Movement time:

calculate_movement_time <- function(ridedata){
  result <- ridedata %>%  mutate(time_diff = SECS - lag(SECS, default = 0),
                                 km_diff = KM - lag(KM, default = 0),
                                 time_diff_movement = if_else(KPH ==0 & km_diff == 0 , 
                                                              time_diff, time_diff-1)) %>%
    summarise(movement_time = (max(SECS) - sum(time_diff_movement )))  #seconds_to_period()
  return(result$movement_time)
}




#------------elevation gain:--------------------------------------------------------------------

calculate_elevation_gain <- function(ridedata){
  if (sum(colnames(ridedata) %>% str_detect("ALT")) == 0){
    return(as.numeric(NA))
  }else{
    result <- ridedata %>% 
      arrange(SECS) %>% mutate(uphill = ALT-lag(ALT),
                               uphill = as.integer(uphill),
                               uphill = if_else(uphill >0, 
                                                if_else(uphill < 1,uphill,0L), #more than 1m/sec is too much (at the start when no altitude is available)
                                                0L)) %>%
      summarise(uphill = sum(uphill,na.rm = TRUE))
    return(result$uphill)
  }
  
}



#------------mean cadence:--------------------------------------------------------------------

calculate_cadence <- function(ridedata){
  if (sum(colnames(ridedata) %>% str_detect("CAD")) == 0){
    return(as.numeric(NA))
  }else{
    result <- ridedata %>% summarise(cadence = mean(CAD), na.rm = TRUE)
    
    return(result$cadence)
  }
  
}



#===================TIME in ZONE==================================================

#------------2mmol--------------------------------------------
calculate_time_2mmol_w <- function(d,w_2mmol, w_min = 50){
  #w_min ... time below this watt doesn't count
  if (sum(names(d) =="WATTS") == 0| is.na(w_2mmol)) {
    return(as.numeric(NA))
  }else{
    d<-d %>% select(WATTS) %>% 
      filter(WATTS <= w_2mmol &
               WATTS > w_min) %>%
      summarise(time_2mmol = n())
    
    return(d$time_2mmol)
  }
}

#------------4mmol--------------------------------------------
calculate_time_4mmol_w <- function(d,w_4mmol){
  if (sum(names(d) =="WATTS") == 0| is.na(w_4mmol)) {
    return(as.numeric(NA))
  }else{
    d<-d %>% select(WATTS) %>% 
      filter(WATTS >= w_4mmol) %>%
      summarise(time_4mmol = n())
    
    return(d$time_4mmol)
  }
}

#------------below w_min--------------------------------------------
calculate_time_below_w_min <- function(d, w_min = 50){
  if (sum(names(d) =="WATTS") == 0) {
    return(as.numeric(NA))
  }else{
    d<-d %>% select(WATTS) %>% 
      filter(WATTS <= w_min) %>%
      summarise(time_below_w_min = n())
    
    return(d$time_below_w_min)
  }
}




#----------Notebook 3_Aktivitäten_analysieren_v4.Rmd--------------------------------------
calculate_hr_mean <- function(d){
  if (sum(names(d) =="HR") == 0) {
    return(as.numeric(NA))
  }else{
    d<-d %>% select(HR) %>% 
      
      summarise(hr_mean = as.integer(mean(HR,na.rm = TRUE)))
    
    return(d$hr_mean)
  }
}


calculate_w_mean <- function(d){
  if (sum(names(d) =="WATTS") == 0) {
    return(as.numeric(NA))
  }else{
    d<-d %>% select(WATTS) %>% 
      
      summarise(watt_mean = mean(WATTS,na.rm = TRUE))
    
    return(d$watt_mean)
  }
}

calculate_NP <- function(d){
  if (sum(names(d) =="WATTS") == 0) {
    return(as.numeric(NA))
  }else{
    d<-d %>% select(WATTS) %>% 
      mutate(watt = as.ts(WATTS,frequency = 1), #Timeseries machen
             watt_average =pracma::movavg(watt,25,type = "e"),#  ma(watt,order =60),  #gleitendender bzw. expon mittelwert
             watt_4 = watt_average^4) %>%  #4potenz
      summarise(watt_np = (mean(watt_4, na.rm=TRUE))^(1/4))
    
    return(d$watt_np)
  }
  
  #TSS = (Sek x NP® x IF®)/(FTP x 3600) x 100
}

calculate_IF <- function(d,FTP){
  if (sum(names(d) =="WATTS") == 0) {
    return(as.numeric(NA))
  }else{
    d<-d %>% select(WATTS) %>% 
      mutate(watt = as.ts(WATTS,frequency = 1), #Timeseries machen
             watt_average =pracma::movavg(watt,25,type = "e"),#  ma(watt,order =60),  #gleitendender bzw. expon mittelwert
             watt_4 = watt_average^4) %>%  #4potenz
      summarise(watt_mean = mean(WATTS),
                watt_np = (mean(watt_4, na.rm=TRUE))^(1/4),
                watt_average = mean(watt_average, na.rm=TRUE),
                sek = n()#
      ) %>%
      mutate(intensity = watt_np/FTP)
    return(d$intensity)
  }
}

calculate_TSS <- function(d,FTP){
  if (sum(names(d) =="WATTS") == 0) {
    return(as.numeric(NA))
  }else{
    d<-d %>% select(WATTS) %>% 
      mutate(watt = as.ts(WATTS,frequency = 1), #Timeseries machen
             watt_average =pracma::movavg(watt,25,type = "e"),#  ma(watt,order =60),  #gleitendender bzw. expon mittelwert
             watt_4 = watt_average^4) %>%  #4potenz
      summarise(watt_mean = mean(WATTS),
                watt_np = (mean(watt_4, na.rm=TRUE))^(1/4),
                watt_average = mean(watt_average, na.rm=TRUE),
                sek = n()#
      ) %>%
      mutate(intensity = watt_np/FTP,
             TSS = as.integer((sek* watt_np * intensity)/(FTP * 3600)*100))
    return(d$TSS)
  }
  
  #TSS = (Sek x NP® x IF®)/(FTP x 3600) x 100
}


# BESTE Watt über x minuten
calculate_best_watt <- function(d,min){
  
  dauer <- d %>% summarise(n())
  if(dauer < 60*min |
     sum(names(d) =="WATTS") == 0){
    return(as.numeric(NA))
  }else{
    
    d<- d %>% select(SECS,WATTS) %>% 
      mutate(watt = as.ts(WATTS,frequency = 1), #Timeseries machen
             #watt_average = rollmean(WATTS,x*60)
             watt_average =pracma::movavg(watt,min*60, type = c("s"))
      ) %>% summarise(beste_watt = max(watt_average))
    return(d$beste_watt)
  }
}

calculate_best_hr5 <- function(d){ 
  if(sum(names(d) =="HR") == 0){
    return(as.numeric(NA))
  }else{
    d<-d %>% mutate(hr = as.ts(HR,frequency = 1), #Timeseries machen
                    hr_5 = pracma::movavg(hr,60*5, type = c("s"))) %>%
      summarise(hr5_mean = mean(hr_5),
                hr5_median = median(hr_5),
                hr5_sd = sd(hr_5))
    return(d)
    
  }
}

calculate_best_w5 <- function(d){ 
  if(sum(names(d) =="WATTS") == 0){
    return(as.numeric(NA))
  }else{
    d<-d %>% filter(WATTS >50) %>%   # sonst ergeben sich durchs bergabfahren verzerrungen?
      mutate(watt = as.ts(WATTS,frequency = 1), #Timeseries machen
             watt_5 = pracma::movavg(watt,60*5, type = c("s"))) %>%
      summarise(w5_mean = mean(watt_5),
                w5_median = median(watt_5),
                w5_sd = sd(watt_5),
                zeit_mit_w = hms::as_hms(n()))
    return(d)
    
  }
}

#================================== COUNT WATT and HR==============================

count_watt <- function(d){ 
  if(sum(names(d) =="WATTS") == 0){
    return(tibble())
  }else{
    d<-d %>% mutate(watt= as.integer(WATTS)) %>% count(watt) 
    return(d)
    
  }
}

count_hr <- function(d){ 
  if(sum(names(d) =="WATTS") == 0){
    return(tibble())
  }else{
    d<-d %>% mutate(hr= as.integer(HR)) %>% count(hr)
    return(d)
    
  }
}



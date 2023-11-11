# Processes the intervals main function

update_intervals <-function(path_activities_gc, path_intervals){
  #updates the interval result list saved at path_intervals
  #path_activities_gc .. path of the GoldenCheetah data
  
  library("jsonlite")
  
  result_old <- read_csv2(path_intervalls)
  max_date <- result_old %>% summarise(max(date)) %>% pull() 
  
  
  
  print(str_c("last processed intervals: ", max_date))
  
  #if there is new data process and save it
  if (max_date < Sys.Date()) {
    max_date = max_date +1
    
    #get all new files
    files_sel <- choose_files(path_activities_raw, max_date,Sys.Date())
    
    result_list <- list()
    for (i in seq_along(files_sel$file_name)) {
      
      file_sel <- files_sel[i,]
      result_list[[i]] <- read_intervals(path_activities_gc, file_sel)
    }
    result_new <- bind_rows(result_list)
    
    result_new <- result_old %>% bind_rows(result_new) %>% arrange(desc(date))
    result_new %>% write_csv2(path_intervals) 
    
  }else{
    result_new <- result_old
  }
  
  
  
  return(result_new)
}


aggregate_activity_intervals <- function(df){
  #aggregate the intervals
  
  if (sum(names(df) =="HR") == 0) { df <- df %>% mutate(HR = 0)}
  if (sum(names(df) =="WATTS") == 0) { df <- df %>% mutate(WATTS = 0)}
  if (sum(names(df) =="CAD") == 0) { df <- df %>% mutate(CAD = 0)}
  #aggregae the ridedata grouped by interval
  result <- df %>% group_by(interval_name) %>% filter(!is.na(WATTS) &
                                                        WATTS > 0 ) %>% 
    summarise(watt = round(mean(WATTS,na.rm = TRUE)),
              hr = round(mean(HR, na.rm = TRUE)),
              interval_hr_max = max(HR, na.rm = TRUE),
              interval_hr_min = min(HR, na.rm = TRUE),
              cadence = round(mean(CAD, na.rm = TRUE)),
              secs = n(),
              min = round(secs/60,1))
  return(result)
}


#================= Extract useful intervals================================

extract_intervals <- function(df_intervals, device_list, ftp_values,intensity_min = 0.88){
  # extracts the intervalls which are over intensity_min
  # uses only data specified in the device_list
  # intensity = watt_interval / watt_ftp
  
  int_max_sst <- 0.97
  int_max_ftp <- 1.1
  
  df_intervals <- df_intervals %>% 
    mutate(device = str_remove_all(device, " ")) %>%
    filter(device %in% device_list) %>% 
    left_join(ftp_values, by = "date") %>%
    mutate(intensity = watt / w_5mmol) %>% 
    
    filter(intensity > intensity_min) %>%
    
    mutate(min_sst = case_when(intensity < int_max_sst ~ min,
                               TRUE ~ 0),
           min_ftp = case_when(intensity >= int_max_sst  &
                               intensity < int_max_ftp~ min,
                               TRUE ~ 0),
           min_vo2max = case_when(intensity >= int_max_ftp ~ min,
                               TRUE ~ 0)
           )
  
  return(df_intervals)
}


#===========================================================================================================

# ======================================= choose the files to process based on dates
choose_files <- function(file_path, start_date = "2015-01-01", end_date = Sys.Date()){
  # choose the files to process based on dates
  #returns a tibble with file names from the file_path between start_date and end_date
  
  
  files <- tibble(file_name = list.files(file_path)) %>% mutate(file_date = str_sub(file_name,1,10))
  
  date_selected <- tibble(file_date = seq.Date(as.Date(start_date),
                                               as.Date(end_date), 
                                               by = "day") )
  date_selected <- date_selected %>% mutate(file_date = str_replace_all(as.character(file_date),"-","_"))
  
  files_selected_all <- files  %>% right_join(date_selected, 
                                              by= "file_date") %>% 
    filter(!is.na(file_name)) %>% #if the date doesn`t exist in the data
    select(file_name) 
  
  return(files_selected_all)
  
}


#=============== read data and aggregate it
read_and_agg_intervals <- function(pfad_aktivity,  start_date, end_date){
  library("jsonlite")
  
  files_selected <- choose_files(pfad_aktivity, start_date, end_date)  
  
  result_list <- list()
  for (i in seq_along(files_selected$file_name)) {
    
    datei <- str_c(pfad_aktivity,files_selected$file_name[i])
    data_ride <- read_json(datei)
    ridedata_list <- data_ride[[1]]$SAMPLES
    ridedata <- bind_rows(ridedata_list)
    result_list[[i]] <- get_intervals(data_ride)                  
  }
  
  result <- bind_rows(result_list)
  
  return(result)
  
}


read_intervals <- function(pfad_aktivity, file_sel){
                    # read the intervalls and aggregate them
  
  data_ride <- read_gc_data(pfad_aktivity, file_sel)
  
  # check if there is any ridedata
  if (!is.null(data_ride[[1]]$SAMPLES)) {
    # check if there is any intervall information
    if (!is.null(data_ride$RIDE$INTERVALS)) {
        
        ridedata <- get_activity_data(data_ride)
        
        intervals <- get_activity_intervals(data_ride)    
        
        intervals <- label_intervals(ridedata, intervals)
        
        intervals_agg <- aggregate_activity_intervals(intervals)
        result <- add_activity_info(intervals_agg, data_ride, file_sel)
        
        result <- result %>% select(date, workout_code, interval_name, min, everything())
        
        
        return(result)
    }
  }

  
}


#=============================== GC Intervall activity data =============================  
#------------------------- Get Intervall data:------------------------------------------

read_gc_data <- function(pfad_aktivity, file_name){
  #read the json data 
  #returns a list of different tibbles of data (e.g. data_ride[[1]]$SAMPLES .. WATT, SECS of the ride)
  library("jsonlite")
  
  datei <- str_c(pfad_aktivity,file_name)
  data_ride <- read_json(datei)
  
  return(data_ride)
  
}

get_activity_data <- function(data_ride){
  #get the ridedata (for every second: watt, hr, ...)
  
  ridedata_list <- data_ride[[1]]$SAMPLES
  ridedata <- bind_rows(ridedata_list) 
  
  min_second <- min(ridedata$SECS)
  max_second <- max(ridedata$SECS)
  
  all_seconds <- tibble(SECS = as.integer(seq(min_second,max_second,by= 1)))
  ridedata <- ridedata %>% full_join(all_seconds)
  
  
  return(ridedata)
  
}

get_activity_intervals <- function(data_ride){
  # get the intervall information of the loaded json data
  # get the name and the start second
  
  intervals_gc_list <- data_ride$RIDE$INTERVALS
  
  intervals_n <- intervals_gc_list %>% length()
  
  intervals_list <- c()
  for (i in c(1:intervals_n)) {
    intervals_list[[i]] <- tibble(
      interval_name = intervals_gc_list[[i]]$NAME,
      interval_start = as.integer(intervals_gc_list[[i]]$START),
      #interval_ptest = intervals_gc_list[[i]]$PTEST
    ) %>% 
      mutate(interval_start = abs(interval_start),
             interval_name = case_when(is.null(interval_name) ~ " ",
                                       TRUE ~ interval_name))
    
  }
  
  interval <- intervals_list %>% bind_rows() %>% arrange(interval_start)
  
  return(interval)
  
}

label_intervals <- function(ridedata, intervals){
  #label the ridedata with the intervals
  
  library(zoo)
  #join with intervals
  ridedata_joined <- ridedata %>% left_join(intervals, 
                                            by = c("SECS" = "interval_start")) %>%
    arrange(SECS)     
  
  #fill NA
  ridedata_interval <- ridedata_joined %>% 
    #filter so that the first value is not Null
    filter(SECS >= !!intervals$interval_start[1] ) %>%
    mutate(interval_name = na.locf(interval_name),
           #interval_ptest = na.locf(interval_ptest)
    )
  return(ridedata_interval)
}




add_activity_info <- function(df, data_ride, file_name){
  
  if (sum(names(data_ride$RIDE$TAGS) =="Workout Code") == 0) { 
        workout_code <- ""}else{
        workout_code <- as.character(data_ride$RIDE$TAGS$`Workout Code`)}
  
  if (sum(names(data_ride$RIDE$TAGS) =="Rad") == 0) { 
      device_name <- ""}else{
        device_name <- as.character(data_ride$RIDE$TAGS$Rad)}
  
  
  #add the main acitvity infos to the intervals
  df <- df %>% mutate(file_name = !!file_name$file_name,
                      workout_code = !!workout_code,
                      device = !!device_name,
                      start_time = data_ride$RIDE$STARTTIME,
                      date  = as.Date(start_time))
  
  return(df)
}

#================================ALTE ======================================

get_intervals_ALT <- function(data_ride){
  
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
  
  #TSS

  
  
  return(interval_aggregation)
}
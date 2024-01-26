### LOAD AND PREPROCESS DATA FOR MODEL ###
model_data <- readRDS("Raw_WSP_Setlists_1985_2024.rds") %>%
  mutate(
    set_num = case_when(
      set == 'E' ~ "99",
      TRUE ~ set
    ),
    set = as.numeric(set_num),
    weekday = weekdays(date)
  ) %>%
  group_by(link, show_index) %>%
  mutate(
    min_set = min(as.numeric(set)),
    max_set = min(as.numeric(set)),
    set = if_else(set == 0 & min_set == 0 & max_set %in% c(99,0), 1, set)
  ) %>%
  select(-c(min_set, max_set, set_num)) %>%
  filter(!is.na(run_index)) %>%
  ungroup() %>%
  group_by(run_index) %>%
  mutate(show_in_run  = (show_index -min(show_index))+1) %>%
  ungroup() %>%
  filter(date != '2023-01-13')


# Task 1: Build Tweak Function (Takes Setlist And Pre-Processes Data Into Usable Model Input)
manipulate_train <- function(test_date = future_shows$date[[1]]){
  
  # Create Show-Specific Variables
  if(test_date > max(model_data$date)){
    future_show_df <- future_shows %>% filter(date == test_date)
    
    test_state <- future_show_df$state[[1]]
    test_city <- future_show_df$city[[1]]
    test_venue <- future_show_df$venue_full[[1]]
    test_show_in_run <- future_show_df$show_in_run[[1]]
  } else {
    old_show_row <- model_data %>% filter(date == test_date)
    
    test_state <- model_data$state[[1]]
    test_city <- old_show_row$city[[1]]
    test_venue <- old_show_row$venue_full[[1]]
    test_show_in_run <- old_show_row$show_in_run[[1]]
  }
  
  next_show_day = weekdays(as.Date(test_date))
  
  
  df <- model_data %>%
    filter(date < test_date) %>%
    filter(song_name %notin% c('','DRUMS', 'JAM')) %>%
    arrange(run_index, show_index, year_index, song_index)
  
  # Next Show + Run
  next_show_index = max(df$show_index) + 1
  next_run_index = ifelse(max(df$date) != as.Date(test_date) - 1, max(df$run_index) + 1, max(df$run_index))
  
  ## BY SHOW
  tot_shows <- n_distinct(df$link)
  tot_shows_last_6_months <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365/2)) %>% select(link) %>% distinct())
  tot_shows_last_year <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365)) %>% select(link) %>% distinct())
  tot_shows_last_2_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*2)) %>% select(link) %>% distinct())
  tot_shows_last_4_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*4)) %>% select(link) %>% distinct())
  tot_shows_last_10_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*10)) %>% select(link) %>% distinct())
  
  tot_shows_mikey_years <- n_distinct(df %>% filter(date <= "2002-08-10") %>% select(link) %>% distinct())
  tot_shows_jimmy_years <- n_distinct(df %>% filter(date >= "2006-08-03") %>% select(link) %>% distinct())
  
  # Location
  tot_shows_same_state <- n_distinct(df %>% filter(state == test_state) %>% select(link) %>% distinct())
  tot_shows_same_city <- n_distinct(df %>% filter(city == test_city) %>% select(link) %>% distinct())
  tot_shows_same_venue <- n_distinct(df %>% filter(venue_full == test_venue) %>% select(link) %>% distinct())
  
  # Day of Week
  tot_shows_same_day <- n_distinct(df %>% filter(weekday == next_show_day) %>% select(link) %>% distinct())
  
  ## BY RUNS
  tot_runs <- max(df$run_index)
  tot_runs_last_6_months <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365/2)) %>% select(run_index) %>% distinct())
  tot_runs_last_year <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365)) %>% select(run_index) %>% distinct())
  tot_runs_last_2_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*2)) %>% select(run_index) %>% distinct())
  tot_runs_last_4_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*4)) %>% select(run_index) %>% distinct())
  tot_runs_last_10_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*10)) %>% select(run_index) %>% distinct())
  
  # Location
  tot_runs_same_state <- n_distinct(df %>% filter(state == test_state) %>% select(run_index) %>% distinct())
  tot_runs_same_city <- n_distinct(df %>% filter(city == test_city) %>% select(run_index) %>% distinct())
  tot_runs_same_venue <- n_distinct(df %>% filter(venue_full == test_venue) %>% select(run_index) %>% distinct())
  
  # Show In Run
  tot_shows_same_in_run <- n_distinct(df %>% filter(show_in_run == test_show_in_run) %>% select(link) %>% distinct())
  tot_shows_same_day_in_run <- n_distinct(df %>% filter(weekday == next_show_day & show_in_run == test_show_in_run) %>% select(link) %>% distinct())
  
  # Song Statistics
  clean_train <- df %>%
    mutate(
      # Last X Years
      is_last_6_months = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= (365/2) , 1, 0),
      is_last_year = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365 , 1, 0),
      is_last_2_years = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365*2 , 1, 0),
      is_last_4_years = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365*4 , 1, 0),
      is_last_10_years = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365*10 , 1, 0),
      
      # By Guitarist
      is_mikey_show = ifelse(date <= "2002-08-10", 1, 0),
      is_jimmy_show = ifelse(date >= "2006-08-03", 1, 0),
      
      # By Location
      is_same_state = ifelse(city == test_state, 1, 0),
      is_same_city = ifelse(city == test_city, 1, 0),
      is_same_venue = ifelse(venue_full == test_venue, 1, 0),
      
      # By Day Type 
      is_same_day = ifelse(weekday == next_show_day, 1, 0),
      is_same_in_run = ifelse(show_in_run == test_show_in_run, 1, 0),
      is_same_day_in_run = is_same_day*is_same_in_run
      ) %>%
    group_by(song_name) %>%
    mutate(
      # LTP BY SHOW
      
      ltp =   next_show_index - max(show_index),
      ltp_2 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 2), 
      ltp_3 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 3),
      ltp_4 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 4),
      ltp_5 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 5),
      ltp_6 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 6),
      ltp_7 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 7),
      ltp_8 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 8),
      ltp_9 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 9),
      ltp_10= next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 10),
    
      # LTP BY RUN
      
      ltp_run =   next_run_index - max(run_index),
      ltp_2_run = next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 2),
      ltp_3_run = next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 3),
      ltp_4_run = next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 4),
      ltp_5_run = next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 5),
      ltp_6_run = next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 6),
      ltp_7_run = next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 7),
      ltp_8_run = next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 8),
      ltp_9_run = next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 9),
      ltp_10_run =next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 10)
    )  %>%
    summarise(
      ### TOTAL COUNTS ###
      
      ## SHOWS
      
      # Time
      n_shows_all_time = n_distinct(link),
      n_shows_last_6_months = sum(is_last_6_months),
      n_shows_last_year = sum(is_last_year),
      n_shows_last_2_years = sum(is_last_2_years),
      n_shows_last_4_years = sum(is_last_4_years),
      n_shows_last_10_years = sum(is_last_10_years),
      
      # Guitarist
      n_shows_mikey_years = sum(is_mikey_show),
      n_shows_jimmy_years = sum(is_jimmy_show),
      
      # Location
      n_shows_same_state = sum(is_same_state),
      n_shows_same_city = sum(is_same_city),
      n_shows_same_venue = sum(is_same_venue),
      
      # Day
      n_shows_same_day = sum(is_same_day),
      n_shows_same_in_run = sum(is_same_in_run),
      n_shows_same_day_in_run = sum(is_same_day_in_run),

      ## RUNS
      
      # Time
      n_runs_all_time = n_distinct(run_index),
      n_runs_last_6_months = (n_distinct(run_index * is_last_6_months))-1,
      n_runs_last_year = (n_distinct(run_index * is_last_year))-1,
      n_runs_last_2_years = (n_distinct(run_index * is_last_2_years))-1,
      n_runs_last_4_years = (n_distinct(run_index * is_last_4_years))-1,
      n_runs_last_10_years = (n_distinct(run_index * is_last_10_years))-1,
      
      # Location
      n_runs_same_state = (n_distinct(run_index * is_same_state))-1,
      n_runs_same_city = (n_distinct(run_index * is_same_city))-1,
      n_runs_same_venue = (n_distinct(run_index * is_same_venue))-1,
      
      ### PERCENT OF WHOLE ###
      
      ## SHOWS
      
      # Time
      pct_shows_all_time = n_shows_all_time / tot_shows,
      pct_shows_last_6_months = n_shows_last_6_months / tot_shows_last_6_months,
      pct_shows_last_year = n_shows_last_year / tot_shows_last_year,
      pct_shows_last_2_years = n_shows_last_2_years / tot_shows_last_2_years,
      pct_shows_last_4_years = n_shows_last_4_years / tot_shows_last_4_years,
      pct_shows_last_10_years = n_shows_last_10_years / tot_shows_last_10_years,
      
      # Guitarist
      pct_shows_mikey_years = n_shows_mikey_years / tot_shows_mikey_years,
      pct_shows_jimmy_years = n_shows_jimmy_years / tot_shows_jimmy_years,
      
      # Location
      pct_shows_same_state = n_shows_same_state / tot_shows_same_state,
      pct_shows_same_city = n_shows_same_city / tot_shows_same_city,
      pct_shows_same_venue = n_shows_same_venue / tot_shows_same_venue,
      
      # Day
      pct_shows_same_day = n_shows_same_day / tot_shows_same_day,
      pct_shows_same_in_run = n_shows_same_in_run / tot_shows_same_in_run,
      pct_shows_same_day_in_run = n_shows_same_day_in_run / tot_shows_same_day_in_run,
      
      ## RUNS ##
      
      # Time
      pct_runs_all_time = n_runs_all_time / tot_runs,
      pct_runs_last_6_months = n_runs_last_6_months / tot_runs_last_6_months,
      pct_runs_last_year = n_runs_last_year / tot_runs_last_year,
      pct_runs_last_2_years = n_runs_last_2_years / tot_runs_last_2_years,
      pct_runs_last_4_years = n_runs_last_4_years / tot_runs_last_4_years,
      pct_runs_last_10_years = n_runs_last_10_years / tot_runs_last_10_years,
      
      # Location
      pct_runs_same_state = n_runs_same_state / tot_runs_same_state,
      pct_runs_same_city = n_runs_same_city / tot_runs_same_city,
      pct_runs_same_venue = n_runs_same_venue / tot_runs_same_venue,
      
      ### DIFFERENCES IN PERCENT ###
      
      ## Shows
      
      # Time (Arbitrary)
      diff_6mo_2year = pct_shows_last_6_months - pct_shows_last_2_years,
      diff_year_alltime = pct_shows_last_year - pct_shows_all_time,
      diff_2year_10year = pct_shows_last_2_years - pct_shows_last_10_years,
      
      # Guitarist
      diff_jimmy_mikey_shows = pct_shows_jimmy_years - pct_shows_mikey_years,
      
      # Location
      diff_shows_same_state = pct_shows_same_state - pct_shows_all_time,
      diff_shows_same_city = pct_shows_same_city - pct_shows_all_time,
      diff_shows_same_venue = pct_shows_same_venue - pct_shows_all_time,
      diff_recent_shows_same_state = pct_shows_same_state - pct_shows_last_10_years,
      diff_recent_shows_same_city = pct_shows_same_city - pct_shows_last_10_years,
      diff_recent_shows_same_venue = pct_shows_same_venue - pct_shows_last_10_years,
      
      diff_runs_same_state = pct_runs_same_state - pct_runs_all_time,
      diff_runs_same_city = pct_runs_same_city - pct_runs_all_time,
      diff_runs_same_venue = pct_runs_same_venue - pct_runs_all_time,
      diff_recent_shows_same_state = pct_runs_same_state - pct_runs_last_10_years,
      diff_recent_shows_same_city = pct_runs_same_city - pct_runs_last_10_years,
      diff_recent_shows_same_venue = pct_runs_same_venue - pct_runs_last_10_years,
      
      # Day
      diff_shows_same_day = pct_shows_same_day - pct_shows_all_time,
      diff_recent_shows_same_day = pct_shows_same_day - pct_shows_last_10_years,
      diff_shows_same_in_run = pct_shows_same_in_run - pct_shows_all_time,
      diff_recent_shows_same_in_run = pct_shows_same_in_run - pct_shows_last_10_years,
      diff_shows_same_day_in_run = pct_shows_same_day_in_run - pct_shows_all_time,
      diff_recent_shows_same_day_in_run = pct_shows_same_day_in_run - pct_shows_last_10_years,
      
      ### NORMALIZE LTP###
      
      # By Show
      ltp =   max(ltp),
      ltp_2 = max(ltp_2),
      ltp_3 = max(ltp_3),
      ltp_4 = max(ltp_4),
      ltp_5 = max(ltp_5),
      ltp_6 = max(ltp_6),
      ltp_7 = max(ltp_7),
      ltp_8 = max(ltp_8),
      ltp_9 = max(ltp_9),
      ltp_10= max(ltp_10),
      
      # By Run
      ltp_run =   max(ltp_run),
      ltp_2_run = max(ltp_2_run),
      ltp_3_run = max(ltp_3_run),
      ltp_4_run = max(ltp_4_run),
      ltp_5_run = max(ltp_5_run),
      ltp_6_run = max(ltp_6_run),
      ltp_7_run = max(ltp_7_run),
      ltp_8_run = max(ltp_8_run),
      ltp_9_run = max(ltp_9_run),
      ltp_10_run= max(ltp_10_run)
    ) %>%
    ungroup() %>%
    mutate(
      eligible = if_else(rowSums(!is.na(select(., starts_with("ltp_")))) >= 9, 1, 0)
    )
  
  eligible_songs <- clean_train %>%
    filter(eligible == 1) %>%
    group_by(song_name) %>%
    mutate(
      
      # LTP (Show) Differences
      
      diff_ltp_1_2 = ltp_2 - ltp,
      diff_ltp_2_3 = ltp_3 - ltp_2,
      diff_ltp_3_4 = ltp_4 - ltp_3,
      diff_ltp_4_5 = ltp_5 - ltp_4,
      diff_ltp_5_6 = ltp_6 - ltp_5,
      diff_ltp_6_7 = ltp_7 - ltp_6,
      diff_ltp_7_8 = ltp_8 - ltp_7,
      diff_ltp_8_9 = ltp_9 - ltp_8,
      diff_ltp_9_10 = ltp_10 - ltp_9,
      avg_ltp = sum(ltp, diff_ltp_1_2, diff_ltp_2_3, diff_ltp_3_4,
                    diff_ltp_4_5, diff_ltp_5_6, diff_ltp_6_7,
                    diff_ltp_7_8, diff_ltp_8_9, diff_ltp_9_10, na.rm = TRUE)/sum(!is.na(ltp), !is.na(diff_ltp_1_2), !is.na(diff_ltp_2_3), !is.na(diff_ltp_3_4),
                                                                                 !is.na(diff_ltp_4_5), !is.na(diff_ltp_5_6), !is.na(diff_ltp_6_7), 
                                                                                 !is.na(diff_ltp_7_8), !is.na(diff_ltp_8_9), !is.na(diff_ltp_9_10)),
      recent_avg_ltp = sum(ltp, diff_ltp_1_2, diff_ltp_2_3, diff_ltp_3_4, diff_ltp_4_5, diff_ltp_5_6,
                           na.rm = TRUE)/sum(!is.na(ltp), !is.na(diff_ltp_1_2), !is.na(diff_ltp_2_3), !is.na(diff_ltp_3_4),
                                             !is.na(diff_ltp_4_5), !is.na(diff_ltp_5_6)),
      # LTP Metrics
      ltp_diff =  abs(ltp - avg_ltp),
      ltp_diff = if_else(ltp - avg_ltp > 0, ltp_diff, if_else(ltp - avg_ltp < 0, ltp_diff, 0.01)),
      ltp_ratio = ltp_diff / avg_ltp,
      
      # Recent LTP Metrics
      recent_ltp_diff = abs(ltp - recent_avg_ltp),
      recent_ltp_diff = if_else(ltp - avg_ltp > 0, recent_ltp_diff, if_else(ltp - avg_ltp < 0, recent_ltp_diff, 0.01)),
      recent_ltp_ratio = recent_ltp_diff / recent_avg_ltp,
      
      # Other LTP Metrics
      played_last_show = if_else(ltp == 1, 1, 0),
      overdue_show = if_else(ltp - avg_ltp > 0, ltp_diff, 0),
      overdue_metric = if_else(ltp - avg_ltp > 0, ltp_diff*pct_runs_last_year, 0),
      
      # LTP (RUN) Differences
      diff_ltp_1_2_run = ltp_2_run - ltp_run,
      diff_ltp_2_3_run = ltp_3_run - ltp_2_run,
      diff_ltp_3_4_run = ltp_4_run - ltp_3_run,
      diff_ltp_4_5_run = ltp_5_run - ltp_4_run,
      diff_ltp_5_6_run = ltp_6_run - ltp_5_run,
      diff_ltp_6_7_run = ltp_7_run - ltp_6_run,
      diff_ltp_7_8_run = ltp_8_run - ltp_7_run,
      diff_ltp_8_9_run = ltp_9_run - ltp_8_run,
      diff_ltp_9_10_run = ltp_10_run - ltp_9_run,
      avg_ltp_run = sum(ltp_run, diff_ltp_1_2_run, diff_ltp_2_3_run, diff_ltp_3_4_run,
                        diff_ltp_4_5_run, diff_ltp_5_6_run, diff_ltp_6_7_run,
                        diff_ltp_7_8_run, diff_ltp_8_9_run, diff_ltp_9_10_run, na.rm = TRUE)/sum(!is.na(ltp_run), !is.na(diff_ltp_1_2_run), !is.na(diff_ltp_2_3_run), !is.na(diff_ltp_3_4_run),
                                                                                                 !is.na(diff_ltp_4_5_run), !is.na(diff_ltp_5_6_run), !is.na(diff_ltp_6_7_run), 
                                                                                                 !is.na(diff_ltp_7_8_run), !is.na(diff_ltp_8_9_run), !is.na(diff_ltp_9_10_run)),
      recent_avg_ltp_run = sum(ltp_run, diff_ltp_1_2_run, diff_ltp_2_3_run, diff_ltp_3_4_run, diff_ltp_4_5_run, diff_ltp_5_6_run,
                               na.rm = TRUE)/sum(!is.na(ltp_run), !is.na(diff_ltp_1_2_run), !is.na(diff_ltp_2_3_run), !is.na(diff_ltp_3_4_run),
                                                 !is.na(diff_ltp_4_5_run), !is.na(diff_ltp_5_6_run)),
      
      # LTP Metrics
      ltp_run_diff =  abs(ltp_run - avg_ltp_run),
      ltp_run_diff = if_else(ltp_run - avg_ltp_run > 0, ltp_run_diff, if_else(ltp_run - avg_ltp_run < 0, ltp_run_diff, 0.001)),
      ltp_run_ratio = ltp_run_diff / avg_ltp_run,
      
      # Recent LTP Metrics
      recent_ltp_run_diff = abs(ltp_run - recent_avg_ltp_run),
      recent_ltp_run_diff = if_else(ltp_run - recent_avg_ltp_run > 0, recent_ltp_run_diff, if_else(ltp_run - recent_avg_ltp_run < 0, recent_ltp_run_diff, 0.001)),
      recent_ltp_run_ratio = recent_ltp_run_diff / recent_avg_ltp_run,
      
      # Played Last Run
      played_this_run = if_else(ltp_run == 0, 1, 0),
      played_last_run = if_else(ltp_run == 1, 1, 0),
      overdue_run = if_else(ltp_run - avg_ltp_run > 0, ltp_run_diff, 0),
      overdue_run_metric = if_else(ltp_run - avg_ltp_run > 0, ltp_run_diff*pct_runs_last_year, 0),
      
      # Score Metrics
      raw_score = ((pct_shows_last_6_months * 10) + (pct_shows_last_year * 8) + (pct_shows_last_2_years * 6) + (pct_shows_last_10_years * 4) + (pct_shows_all_time * 2) + diff_shows_same_city + diff_shows_same_day + diff_shows_same_day_in_run)/(ltp_diff),
      raw_run_score = ((pct_runs_last_6_months * 10) + (pct_runs_last_year * 8) + (pct_runs_last_2_years * 6) + (pct_runs_last_10_years * 4) + (pct_runs_all_time * 2) + diff_runs_same_city + diff_shows_same_day + diff_shows_same_day_in_run)/(ltp_run_diff),
      raw_score = if_else(played_this_run == 1, 0, raw_score),
      raw_run_score = if_else(played_this_run == 1, 0, raw_run_score)
    ) %>%
    ungroup()
  
  eligible_songs$city = test_city
  eligible_songs$date = test_date
  eligible_songs$venue_full = test_venue
  eligible_songs$show_in_run = test_show_in_run
  
  return(eligible_songs)
}

# Task 2: Create Model Input File For Next Show (Best one of the year):
sell_sell_table <- manipulate_train(test_date = "2024-01-20")

# Task 3: Create Model Input Files For Last X Shows To Test Model:
create_train_set <- function(end_date = max(model_data$date), train_n = 500){
  start_time <- Sys.time()
  # Get Train/Test Date List
  train_dates <- model_data %>% filter(date <= end_date) %>% select(date) %>% arrange(desc(date)) %>% unique() %>% head(train_n) %>% pull()
  print(paste0("Now Loading ", train_n, " Concerts From ", train_dates[[1]]," to ", train_dates[[length(train_dates)]]))
  
  list_of_dfs <- c()
  
  # Create Loop?
  for(i in 1:length(train_dates)){
    predict_table <- manipulate_train(test_date = train_dates[i])
    
    setlist <- model_data %>% filter(date == train_dates[i]) %>% select(song_name) %>% unique() %>% pull()
    
    final_tbl <- predict_table %>% mutate(played = if_else(song_name %in% setlist, 1, 0))
    
    list_of_dfs[[i]] <- final_tbl
  }
  
  train_table <- bind_rows(list_of_dfs)
  
  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
  print(paste0("Loading of ", train_n, " Concerts From ", train_dates[[1]]," to ", train_dates[[length(train_dates)]], " Completed in ", round(elapsed_time, 2)," Minutes"))

  return(train_table)
  
}
model_table <- create_train_set()

# Task 4: Function For Building and Testing Model + Metrics
build_model <- function(test_dte, n_shows = 400, rounds = 25, m_depth = 5, rt = 0.3){
  set.seed(87)
  
  ## Pre-Processing ##
  
  # Split
  test_data <- model_table %>% filter(date == test_dte)
  train_data <- model_table %>% filter(date < test_dte)
  filt_dates <- train_data %>% select(date) %>% filter(date < test_dte) %>% arrange(date) %>% unique() %>% tail(n_shows) %>% pull()
  train_data <- model_table %>% filter(date %in% filt_dates)

  
  # Keep Indicies
  song_index <- test_data$song_name
  date_index <- test_data$date
  city_index <- test_data$city
  
  # Features And Targets
  features <- names(train_data)[!names(train_data) %in% c("played", "city", "date", "venue_full", "song_name")]
  target <- "played"
  
  ## TRAIN ##
  
  # Train the xgboost model
  xgb_model <- xgboost(data = as.matrix(train_data[, features]),
                       label = as.numeric(train_data[[target]]),
                       objective = "binary:logistic",
                       eval.metric = 'logloss',
                       max.depth=m_depth,
                       nrounds = rounds,
                       eta = rt,
                       verbose = 0)
  
  ## PREDICT ##
  
  pred_vec <- predict(xgb_model, as.matrix(test_data[, features]))
  
  model_predictions <<- data.frame(
    date = date_index,
    city = city_index,
    song_name = song_index,
    pred = pred_vec, actual = test_data[[target]]
    ) %>%
    mutate(
      log_pred = rescale(log(pred + 1))
    ) %>%
    arrange(desc(log_pred), desc(actual))
  
  #print(model_predictions %>% head(5))
  
  ## METRICS ##
  
  feature_importance <- xgb.importance(feature_names = features, model = xgb_model)
  top_features <- feature_importance %>% head(25)
  
  model_predictions <- model_predictions %>% mutate(optimal_pred = if_else(log_pred >= 0.4, 1, 0))
  confusion_matrix <- table(Actual = model_predictions$actual, Predicted = model_predictions$optimal_pred)
  print(confusion_matrix)
  
  acc <- mean(model_predictions$optimal_pred == model_predictions$actual)
  prec <- sum(model_predictions$optimal_pred == 1 & model_predictions$actual == 1) / sum(model_predictions$optimal_pred == 1)
  recall <- sum(model_predictions$optimal_pred == 1 & model_predictions$actual == 1) / sum(model_predictions$actual == 1)
  f1_score <- 2 * prec * recall / (prec + recall)
  roc_curve <- roc(model_predictions$actual, model_predictions$optimal_pred, quiet = TRUE)
  auc_roc <- auc(roc_curve)
  
  print(paste0("Show Date: ", test_dte, " | Accuracy: ", round(acc, 3), " | Precision: ", round(prec, 3), " | Recall: ", round(recall, 3), " | F1 Score: ", round(f1_score, 3), " | AUC: ", round(auc_roc,3)))
  
  correct_a0_p0 <- sum(model_predictions$actual == 0 & model_predictions$optimal_pred == 0)
  correct_a1_p1 <- sum(model_predictions$actual == 1 & model_predictions$optimal_pred == 1)
  correct_a1_p0 <- sum(model_predictions$actual == 1 & model_predictions$optimal_pred == 0)
  correct_a0_p1 <- sum(model_predictions$actual == 0 & model_predictions$optimal_pred == 1)
  
  feat <- top_features %>% select(Feature) %>% pull()
  gain <- top_features %>% select(Gain) %>% pull()
  cov <- top_features %>% select(Cover) %>% pull()
  freq <- top_features %>% select(Frequency) %>% pull()
  
  ## SAVE ACC METRICS ##
  metrics_df <- data.frame(
    date = test_dte,
    train_start = filt_dates[[1]],
    train_end = filt_dates[[length(filt_dates)]],
    city = city_index[[1]],
    accuracy = acc,
    precision = prec,
    recall = recall,
    f1 = f1_score,
    auc = as.numeric(auc_roc),
    n_shows = n_shows,
    max_depth = m_depth,
    rounds = rounds,
    eta = rt,
    correct_a0_p0 = correct_a0_p0,
    correct_a1_p1 = correct_a1_p1,
    incorrect_a1_p0 = correct_a1_p0,
    incorrect_a0_p1 = correct_a0_p1,
    features_top = feat,
    features_gain = gain,
    features_cover = cov,
    features_freq = freq
  )
  
  model_predictions$city <- city_index
  
  
  return(list(metrics_df, model_predictions))
  
}

# Task 5: Loop Models For Years You Want To Test (Default is 2023+2024)
# Produces Two Outputs In General Envrionment (acc_metrics)
  # Model Accuracy Metrics For Each DataFrame (all_song_predictions_df)
  # Setlist of Predictions and Actual For Each Show Tested
loop_model <- function(yrs = c(2022, 2023, 2024)){
  
  # Get Test Dates #
  test_dates <- model_table %>% select(date) %>% filter(year(date) %in% yrs & date != "2023-01-13") %>% unique() %>% arrange(desc(date)) %>% pull()
  metrics_list_dfs <- c()
  predict_songs_dfs <- c()
  
  # Loop Model Build #
    for(i in 1:length(test_dates)){
      return_list <- build_model(test_dates[[i]])
      metrics_list_dfs[[i]] <- return_list[[1]]
      predict_songs_dfs[[i]] <- return_list[[2]]
    }
  all_metrics <- bind_rows(metrics_list_dfs)
  
  acc_metrics <<-all_metrics %>% arrange(desc(date)) %>%
    select(-starts_with('features_')) %>%
    filter(date != '2023-01-13') %>%
    unique()
  
  feat_metrics <<- all_metrics_df %>%
    group_by(features_top) %>%
    summarise(
      n_times = n(),
      avg_gain = mean(features_gain),
      avg_cover = mean(features_cover),
      avg_freq = mean(features_freq),
      weight = n_times * avg_gain,
    ) %>%
    arrange(desc(weight))
  
  all_song_predictions_df <<- bind_rows(predict_songs_dfs) %>%
    arrange(desc(date), desc(pred), desc(actual))

}
loop_model()

# Task 6: "Apply" Model To Next Show Incorporating X Most Recent Shows
make_predictions <- function(next_show_date, next_show_city){
  ## Pre-Processing ##
  
  # Split
  test_data <- manipulate_train(test_date = next_show_date)
  train_data <- model_table %>% filter(date < next_show_date)

  # Keep Indicies
  song_index <- test_data$song_name
  date_index <- next_show_date
  city_index <- next_show_city
  
  # Features And Targets
  features <- names(train_data)[!names(train_data) %in% c("played", "venue_full", "city", "date", "song_name")]
  target <- "played"
  
  ## TRAIN ##
  
  # Train the xgboost model
  xgb_model <- xgboost(data = as.matrix(train_data[, features]),
                       label = as.numeric(train_data[[target]]),
                       objective = "binary:logistic",
                       eval.metric = 'logloss',
                       max.depth=5,
                       nrounds = 25,
                       eta = 0.3,
                       verbose = 0)
  
  pred_vec <- predict(xgb_model, as.matrix(test_data[, features]))
  
  # Run Optimal Thresholds From Model Train
  get_optimal_thresh <- function(){
    df <- data.frame(actual = all_song_predictions_df$actual,
                     pred = all_song_predictions_df$pred)
    # Create a data frame to store results
    threshold_results <- data.frame(threshold = numeric(),
                                    recall = numeric(),
                                    specificity = numeric(),
                                    precision = numeric(),
                                    f1_score = numeric(),
                                    auc_score = numeric(),
                                    accuracy = numeric(),
                                    combo_score = numeric)
    # Loop through possible threshold values
    max_t <- round(max(df$pred)-0.03, 2)
    min_t <- round(min(df$pred)+0.01, 2)
    print(paste0("Testing Thresholds Between ",min_t," and ",max_t))
    for (threshold in seq(min_t, max_t, by = 0.001)) {
      # Reclassify based on the current threshold
      threshold_predictions <- ifelse(df$pred >= threshold, 1, 0)
      
      # Calculate confusion matrix
      confusion_matrix <- table(Actual = df$actual, Predicted = threshold_predictions)
      
      # Calculate Accuracy Metrics
      recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
      specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
      f1_score <- 2 * (precision * recall) / (precision + recall)
      auc_score <- auc(df$actual, threshold_predictions, quiet = TRUE)
      combo_score <- precision + f1_score + accuracy + auc_score
      
      # Store results in the data frame
      threshold_results <- rbind(threshold_results, 
                                 data.frame(threshold = threshold,
                                            recall = recall,
                                            specificity = specificity,
                                            precision = precision,
                                            f1_score = f1_score,
                                            auc_score = auc_score,
                                            accuracy = accuracy,
                                            combo_score = combo_score))
    }
    
    # Rescale Combination + Smooth Points
    threshold_results$auc_score <- as.numeric(threshold_results$auc_score)
    threshold_results <- threshold_results %>%
      mutate(
        combo_score = rescale(combo_score)
      )
    
    # Save to GenEnv
    threshold_df <<- threshold_results
    
    
    # Plot Optimal Thresholds
    piv_df <- pivot_longer(threshold_results,
                           cols = -threshold,
                           names_to = "Metric",
                           values_to = "Value")
    max_df <- piv_df %>%
      group_by(Metric) %>%
      mutate(smooth = predict(loess(Value~threshold, span=.5))) %>%
      slice_max(order_by = smooth) %>%
      select(Metric, threshold, Value, smooth)
    
    plt <- ggplot(piv_df, aes(x = threshold, y = Value, color = Metric)) +
      geom_point(data = max_df, aes(x = threshold, y = smooth, color = Metric), size = 3) +
      geom_smooth(aes(group = Metric, color = Metric), span = 0.5, method = "loess", se = FALSE, linetype = "solid") +
      labs(title = "WSP Setlist Model - Evaluation Metric by Threshold",
           subtitle = "Based on Testing Model Results of Concerts From 2022-2024 (92 Shows) | Each Show Trained on Last 400 Concerts",
           x = "Threshold", y = "Smoothed Eval Metric Score (0-1)") +
      theme_bw() +
      theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
        ) +
      xlim(min_t, max_t)
    print(plt)
    
    # Print the optimal threshold
    print(paste("Optimal Threshold (F1 Score):", max_df[max_df$Metric == 'f1_score', 'threshold']))
    print(paste("Optimal Threshold (Recall):", max_df[max_df$Metric == 'recall', 'threshold']))
    print(paste("Optimal Threshold (Precision):", max_df[max_df$Metric == 'precision', 'threshold']))
    print(paste("Optimal Threshold (Accuracy):", max_df[max_df$Metric == 'accuracy', 'threshold']))
    print(paste("Optimal Threshold (AUC):", max_df[max_df$Metric == 'auc_score', 'threshold']))
    print(paste("Optimal Threshold (Combination):", max_df[max_df$Metric == 'combo_score', 'threshold']))
    
    return(max_df %>% ungroup())
    
  }
  opt_df <- get_optimal_thresh()
  
  tier_1_threshold <- opt_df %>% filter(Metric == 'precision') %>% select(threshold) %>% pull() # Optimizing Precision
  tier_2_threshold <- opt_df %>% filter(Metric == 'f1_score') %>% select(threshold) %>% pull()  # Optimizing Precision + Recall
  tier_3_threshold <- 0.0612 # Optimizing AUC

  new_preds <- data.frame(
    date = date_index,
    city = city_index,
    song_name = song_index,
    pred = pred_vec
  ) %>%
    mutate(
      pred_class = if_else(pred >= tier_1_threshold, "1_High_Confidence",
                           if_else(pred >= tier_2_threshold & pred < tier_1_threshold, "2_Medium_Confidence", 
                                   if_else(pred >= tier_3_threshold & pred < tier_2_threshold, "3_InPlay", NA))),
      pred = round(pred * 100, 2)
    )
  
  new_preds$ltp_freq_score <- round(rescale(test_data$raw_score),3)
  new_preds$overdue_metric <- round(test_data$overdue_metric,2)
  new_preds$ltp <- test_data$ltp
  new_preds$ltp_2 <- test_data$ltp_2
  new_preds$ltp_3 <- test_data$ltp_3
  new_preds$ltp_diff <- round(test_data$ltp_diff,2)
  new_preds$diff_shows_same_day <- round(test_data$diff_shows_same_day,3)
  new_preds$diff_shows_same_city <- round(test_data$diff_shows_same_city,3)
  
  print(new_preds %>% arrange(desc(pred)) %>% head(5))
  return(new_preds)
}
sell_sell <- make_predictions("2024-02-15", "CHICAGO") %>% arrange(desc(pred))



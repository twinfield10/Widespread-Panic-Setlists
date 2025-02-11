### PREPROCESS DATA ###
set.seed(87)

### 1) Load + Re-Index Shows ###
prep_model_input <- function(df = dim_all, song_df = fact_song){
  
  # Re-Index Show/Run/ShowInRun
  model_dim <- df %>%
    filter(is_radio != 1 & is_soundcheck != 1) %>%
    select(-show_index, -run_index, -show_in_run, -year_index) %>%
    # Show Index
    arrange(year, month, day) %>% rowid_to_column('show_index') %>%
    # Run Index
    arrange(date, venue_name) %>% group_by(venue_name, run_index = cumsum(c(1, diff(date) != 1))) %>% ungroup() %>%
    # Show In Run Index
    arrange(date, run_index) %>% group_by(run_index) %>% mutate(show_in_run  = (show_index -min(show_index))+1) %>% ungroup() %>%
    arrange(year, show_index) %>% group_by(year) %>% mutate(year_index = row_number()) %>% ungroup() %>%
    arrange(show_index)
  
  # Tables To Global Env
  dim_future <<- model_dim %>% filter(is_fut == 1) %>% print.data.frame()
  model_dim <<- model_dim %>% filter(is_fut == 0)
  
  # Check Counts
  dist_links = n_distinct(model_dim$link)
  dist_idx  =  n_distinct(model_dim$show_index)
  if(dist_links == dist_idx){
    print(paste0(dist_idx, " Distinct Shows in Model Input Table (model_dim)"))
  } else {
    print("Links and Show Index Not Equal")
    print(paste0(dist_links, " Distinct Links"))
    print(paste0(dist_idx, " Distinct Show Indicies"))
  }
  
  model_data <<- model_dim %>%
    inner_join(song_df, by = c('link')) %>%
    filter(!is.na(date)) %>%
    group_by(song_name) %>%
    mutate(
      ftp_date = min(date),
      ftp_show = min(show_index),
      ftp_run = min(run_index),
      ftp_city = city[which.min(date)],
      ftp_venue = venue_full[which.min(date)]
    ) %>%
    ungroup() %>%
    arrange(show_index, song_index)
  
}
prep_model_input(
  df = readRDS("./Data/WSP_Show_Dim_Table_1986_to_2025.rds"),
  song_df = readRDS("./Data/WSP_Song_Fact_Table_1986_to_2025.rds")
)
# Output:
# model_data - Table combining show info and setlists that will be used for train data
# model_dim - Table containing info about historical shows 
# dim_future - Table containing info about upcoming shows

### 2) Manipulate Data To Create Input Tables ###
# a)  Function: Create Input Table For Single Show
    # Input: Date
    # Output: DataFrame of Songs and Song Stats as of input date
manipulate_train <- function(test_date = dim_future$date[[1]]){
  
  # Create Show-Specific Variables
  if(test_date > max(model_data$date)){
    future_show_df <- dim_future %>% filter(date == test_date)
    
    test_state <- future_show_df$state[[1]]
    test_city <- future_show_df$city[[1]]
    test_venue <- future_show_df$venue_full[[1]]
    test_show_index <- future_show_df$show_index[[1]]
    test_run_index <- future_show_df$run_index[[1]]
    test_show_in_run <- future_show_df$show_in_run[[1]]
  } else {
    old_show_row <- model_data %>% filter(date == test_date)
    
    test_state <- old_show_row$state[[1]]
    test_city <- old_show_row$city[[1]]
    test_venue <- old_show_row$venue_full[[1]]
    test_show_index <- old_show_row$show_index[[1]]
    test_run_index <- old_show_row$run_index[[1]]
    test_show_in_run <- old_show_row$show_in_run[[1]]
  }
  next_show_day = weekdays(as.Date(test_date))
  
  
  df <- model_data %>%
    filter(date < test_date) %>%
    filter(song_name %notin% c('','DRUMS', 'JAM')) %>%
    arrange(run_index, show_index, year_index, song_index) %>%
    group_by(state) %>% mutate(state_show_index = cumsum(!duplicated(show_index)), state_run_index = cumsum(!duplicated(run_index))) %>% ungroup() %>%
    group_by(city) %>% mutate(city_show_index = cumsum(!duplicated(show_index)), city_run_index = cumsum(!duplicated(run_index))) %>% ungroup() %>%
    group_by(venue_full) %>% mutate(venue_show_index = cumsum(!duplicated(show_index)), venue_run_index = cumsum(!duplicated(run_index))) %>% ungroup()
  
  # Next Show + Run
  next_show_index = test_show_index
  next_run_index = test_run_index
  
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
  tot_runs <- n_distinct(df$run_index)
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
      is_same_state = ifelse(state == test_state, 1, 0),
      is_same_city = ifelse(city == test_city, 1, 0),
      is_same_venue = ifelse(venue_full == test_venue, 1, 0),
      
      # By Day Type 
      is_same_day = ifelse(weekday == next_show_day, 1, 0),
      is_same_in_run = ifelse(show_in_run == test_show_in_run, 1, 0),
      is_same_day_in_run = is_same_day*is_same_in_run,
      
      # By Setlist Location
      is_set_1 = ifelse(set == 1, 1, 0),
      is_set_2 = ifelse(set == 2, 1, 0),
      is_encore = ifelse(set == 99, 1, 0),
      is_opener = ifelse(song_index == 1, 1, 0)
      ) %>%
    group_by(song_name) %>%
    mutate(
      
      # LTP BY SHOW
      ltp = (next_show_index - max(show_index)),
      ltp_2 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 2), 
      ltp_3 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 3),
      ltp_4 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 4),
      ltp_5 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 5),
      ltp_6 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 6),
      ltp_7 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 7),
      ltp_8 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 8),
      ltp_9 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 9),
      ltp_10= next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 10),
      across(starts_with('ltp_'), ~ if_else(is.na(.), next_show_index, .)),
    
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
      ltp_10_run =next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 10),
      across(starts_with('ltp_') & ends_with('_run'), ~ if_else(is.na(.), next_run_index, .)),
    )  %>%
    summarise(
      ftp = max(ftp_date),
      ### TOTAL COUNTS SINCE DEBUT - Will Change Percentages ###
      tot_shows_since_debut = next_show_index - min(ftp_show),
      tot_runs_since_debut = next_run_index - min(ftp_run),
      
      #tot_shows_same_city = n_distinct(link[is_same_city == 1 & show_index >= ftp_show]),
      #tot_shows_same_venue = n_distinct(link[is_same_venue == 1 & show_index >= ftp_show]),
      #tot_shows_same_day = n_distinct(link[is_same_day == 1 & show_index >= ftp_show]),
      
      #tot_runs_same_state = n_distinct(run_index[is_same_state == 1 & run_index >= ftp_run]),
      #tot_runs_same_city = n_distinct(run_index[is_same_city == 1 & run_index >= ftp_run]),
      #tot_runs_same_venue = n_distinct(run_index[is_same_venue == 1 & run_index >= ftp_run]),
      #tot_runs_same_day = n_distinct(run_index[is_same_day == 1 & run_index >= ftp_run]),
      
      #tot_shows_same_in_run = n_distinct(run_index[is_same_in_run == 1 & run_index >= ftp_run]),
      #tot_shows_same_day_in_run = n_distinct(run_index[is_same_day_in_run == 1 & run_index >= ftp_run]),

      
      ## SHOWS
      
      # Time
      n_shows_all_time = n_distinct(link),
      n_shows_last_6_months =  n_distinct(link[is_last_6_months == 1]),
      n_shows_last_year = n_distinct(link[is_last_year == 1]),
      n_shows_last_2_years = n_distinct(link[is_last_2_years == 1]),
      n_shows_last_4_years = n_distinct(link[is_last_4_years == 1]),
      n_shows_last_10_years = n_distinct(link[is_last_10_years == 1]),
      
      # Guitarist
      n_shows_mikey_years = n_distinct(link[is_mikey_show == 1]),
      n_shows_jimmy_years = n_distinct(link[is_jimmy_show == 1]),
      
      # Location
      n_shows_same_state = n_distinct(link[is_same_state == 1]),
      n_shows_same_city = n_distinct(link[is_same_city == 1]),
      n_shows_same_venue = n_distinct(link[is_same_venue == 1]),
      
      # Day
      n_shows_same_day = n_distinct(link[is_same_day == 1]),
      n_shows_same_in_run = n_distinct(link[is_same_in_run == 1]),
      n_shows_same_day_in_run = n_distinct(link[is_same_day_in_run == 1]),
      
      # Setlist Location
      n_shows_set_1 = n_distinct(link[is_set_1 == 1]),
      n_shows_set_2 = n_distinct(link[is_set_2 == 1]),
      n_shows_encore = n_distinct(link[is_encore == 1]),
      n_shows_opener = n_distinct(link[is_opener == 1]),

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
      
      # Since Debut
      pct_shows_since_debut = n_shows_all_time / tot_shows_since_debut,
      pct_runs_since_debut = n_runs_all_time / tot_runs_since_debut,
      
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
      
      # Setlist Location
      pct_shows_set_1 = n_shows_set_1 / n_shows_all_time,
      pct_shows_set_2 = n_shows_set_2 / n_shows_all_time,
      pct_shows_encore = n_shows_encore / n_shows_all_time,
      pct_shows_opener = n_shows_opener / n_shows_all_time,
      
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
      
      # Fill NA
      across(starts_with('pct_'), ~ if_else(is.na(.), 0, .)),
      
      ### DIFFERENCES IN PERCENT ###
      
      ## Shows
      
      # Time (Arbitrary)
      diff_6mo_2year = pct_shows_last_6_months - pct_shows_last_2_years,
      diff_year_alltime = pct_shows_last_year - pct_shows_all_time,
      diff_2year_10year = pct_shows_last_2_years - pct_shows_last_10_years,
      diff_year_debut = pct_shows_last_year - pct_shows_since_debut,
      
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
      diff_recent_runs_same_state = pct_runs_same_state - pct_runs_last_10_years,
      diff_recent_runs_same_city = pct_runs_same_city - pct_runs_last_10_years,
      diff_recent_runs_same_venue = pct_runs_same_venue - pct_runs_last_10_years,
      
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
      eligible = if_else(rowSums(!is.na(select(., starts_with("ltp_")))) >= 3, 1, 0)
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
  eligible_songs$show_index = next_show_index
  
  return(eligible_songs)
}

# b) Apply manipulate_train for next show (i.e., Model Test Data)
sell_sell_table <- manipulate_train()

# c) Apply manipulate_train for previous n shows to use as Train Data
create_train_set <- function(end_date = max(model_data$date), train_n = 500){
  start_time <- Sys.time()
  
  # Get Train/Test Date List
  train_dates <- model_data %>%
    filter(date <= end_date) %>%
    distinct(date) %>%
    arrange(desc(date)) %>%
    slice_head(n = train_n) %>%
    pull(date)
  
  # Print Start TIme
  print(
    paste0(
      "Now Loading ",
      train_n,
      " Concerts From ",
      train_dates[[1]],
      " to ",
      train_dates[[length(train_dates)]],
      " At ",
      format(Sys.time(),"%H:%M:%S")
      )
    )
  
  # List of Train Sets
  list_of_dfs <- map(train_dates, function(train_date) {
    predict_table <- manipulate_train(test_date = train_date) %>%
      filter(ftp < train_date)
    
    setlist <- model_data %>%
      filter(date == train_date) %>%
      distinct(song_name) %>%
      pull(song_name)
    
    predict_table %>%
      mutate(played = as.integer(song_name %in% setlist)) %>%
      select(-ftp)
  })
  
  train_table <- bind_rows(list_of_dfs)
  
  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
  print(paste0("Loading of ", train_n, " Concerts From ", train_dates[[1]],
               " to ", train_dates[[length(train_dates)]],
               " Completed in ", round(elapsed_time, 2)," Minutes"))

  return(train_table)
  
}
model_table <- create_train_set()

### TRAIN MODEL - NO HYPERTUNING ###
train_model <- function(input = model_table, resample = TRUE){
  
  # Separate Columns
  target <- 'played'
  id_cols <- c('song_name', 'show_index', 'city', 'date', 'venue_full')
  feature_cols <- setdiff(names(input), c(target, id_cols))
  
  # Remove Inf
  input <- input %>% filter(!is.infinite(pct_runs_since_debut))
  
  # Build Input Vectors
  input <- if(resample == TRUE){
    input$played <- as.factor(input$played)
    smote(played ~ ., input %>% select(all_of(feature_cols), played), perc.over = 2, k = 5, perc.under = 2)
  } else {
    input
  }

  
  X <- input %>%
    select(all_of(feature_cols)) %>%
    as.matrix()
  y <- as.numeric(as.character(input[[target]]))
  
  # Split into Train/Test
  train_index <- sample(seq_len(nrow(input)), size = 0.8 * nrow(input))
  X_train <- X[train_index, ]
  X_test  <- X[-train_index, ]
  y_train <- y[train_index]
  y_test  <- y[-train_index]
  
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dtest  <- xgb.DMatrix(data = X_test, label = y_test)
  
  # Create Baseline Metrics For Eval
  baseline_prob <- mean(y_train)
  baseline_log_loss <- logLoss(y_test, rep(baseline_prob, length(y_test)))
  
  # Train Model
  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = 6,
    eta = 0.1
  )
  
  model <- xgboost(
    data = dtrain,
    params = params,
    nrounds = 100,
    verbose = 0
  )
  
  # Make Predictions
  y_pred <- predict(model, dtest)
  
  # Evaluate Thresholds
  thresholds <- seq(0, 1, by = 0.01)
  results <- data.frame(threshold = thresholds, accuracy = numeric(length(thresholds)))
  
  for (t in thresholds) {
    y_pred_class <- ifelse(y_pred > t, 1, 0)
    conf_matrix <- confusionMatrix(as.factor(y_pred_class), as.factor(y_test))
    results$accuracy[which(results$threshold == t)] <- conf_matrix$overall["Accuracy"]
  }
  
  # Plot the accuracy vs threshold
  plot(results$threshold, results$accuracy, type = "l", col = "blue", 
       xlab = "Threshold", ylab = "Accuracy", main = "Accuracy vs Threshold")
  
  
  y_pred_class <- ifelse(y_pred > 0.5, 1, 0)
  
  # Evaluate Model
  conf_matrix <- confusionMatrix(as.factor(y_pred_class), as.factor(y_test))
  accuracy <- conf_matrix$overall["Accuracy"]
  auc_score <- auc(y_test, y_pred)
  model_logloss <- logLoss(y_test, y_pred)
  
  # Log Loss Calc
  print(paste("Baseline Probability:", baseline_prob))
  print(paste("Baseline Log Loss:", round(baseline_log_loss, 4)))
  print(paste("XGBoost Log Loss:", round(model_logloss, 4)))
  print(paste("Log Loss Improvement:", round(baseline_log_loss - model_logloss, 4)))
  
  # Other Calcs
  print(conf_matrix)
  print(paste("Accuracy:", round(accuracy, 4)))
  print(paste("AUC:", round(auc_score, 4)))
  
  return(model)
}
# Resampled Model
trained_xgb_resample <- train_model(resample = TRUE)
# No Resample
trained_xgb_raw <- train_model(resample = FALSE)

### APPLY MODEL ###
apply_model<- function(mdl = trained_xgb_raw, input = sell_sell_table){
  target <- 'played'
  id_cols <- c('song_name', 'show_index', 'city', 'date', 'venue_full', 'ftp')
  feature_cols <- setdiff(names(input), c(target, id_cols))
  
  id_df <- input[, id_cols]
  new_features <- input %>%
    select(all_of(feature_cols)) %>%
    as.matrix()
  
  new_dmatrix <- xgb.DMatrix(data = new_features)
  new_predictions <- predict(mdl, new_dmatrix)
  input$pred <- new_predictions
  
  return(input %>% arrange(-pred))
  
}


### CREATE SHOW TABLES ###

# Create Predictions Given Dates
AC_N1 <- apply_model(input = manipulate_train(test_date = "2025-02-15"))
AC_N2 <- apply_model(input = manipulate_train(test_date = "2025-02-16"))
AC_N3 <- apply_model(input = manipulate_train(test_date = "2025-02-17"))

# Combine
AC_ALL <- AC_N1 %>%
  left_join(AC_N2 %>% select(song_name, pred) %>% rename(N2 = pred), by = c('song_name')) %>%
  left_join(AC_N3 %>% select(song_name, pred) %>% rename(N3 = pred), by = c('song_name')) %>%
  mutate(Mean = (pred + N2 + N3) / 3)


##### TABLES #####

# Single Night
build_next_show_table <- function(data = sell_sell, n_preds = 10){
  keep_overall_cols <- c('song_name', 'pred', 'pct_shows_since_debut', 'pct_shows_all_time',
                         'pct_shows_mikey_years', 'pct_shows_jimmy_years', 'diff_jimmy_mikey_shows',
                         #'pct_shows_same_venue', 'diff_shows_same_venue',
                         'pct_shows_same_day', 'diff_shows_same_day',
                         'ltp', 'ltp_2', 'ltp_3', 'avg_ltp', 'ltp_diff', 'ltp_ratio',
                         'raw_score', 'raw_run_score', 'overdue_run_metric')
  
  
  show_date <- unique(data$date)
  show_city <- unique(data$city)
  show_venue <- unique(data$venue_full)
  #show_state <- unique(data$state)
  show_sir <- unique(data$show_in_run)
  
  lab <- "Widespread Panic Setlist Predictions"
  sub_lab <- paste0("Predictions for ", show_date, " @ ", show_venue, " (N",show_sir,")")
  
  df <- data %>% arrange(-pred) %>% select(all_of(keep_overall_cols)) %>% head(n_preds)
  
  gt_obj <- df %>%
    gt() %>%
    # Spanners
    tab_spanner(
      label = "Song Frequency (% of Shows)",
      columns = c(pct_shows_since_debut, pct_shows_all_time,
                  pct_shows_mikey_years, pct_shows_jimmy_years, diff_jimmy_mikey_shows,
                  #pct_shows_same_venue, diff_shows_same_venue,
                  pct_shows_same_day, diff_shows_same_day),
      id = "FREQ"
    ) %>%
    tab_spanner(
      label = "Last Time Played",
      columns = c(ltp, ltp_2, ltp_3, avg_ltp, ltp_diff, ltp_ratio),
      id = 'LTP'
    ) %>%
    tab_spanner(
      label = "Metrics",
      columns = c(raw_score, raw_run_score, overdue_run_metric),
      id = "METRICS"
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("right", "left", "bottom"),
          color = "#333F48",
          weight = px(2)
        ),
        cell_text(
          weight = "bold",
          style = "italic",
          color = "white"
        ),
        cell_fill(
          color = "#BF5700"
        )
      ),
      locations = cells_column_spanners(spanners = c("FREQ", "LTP", "METRICS"))
    ) %>%
    # Song + Pred Columns Format
    tab_style(
      locations = cells_body(columns = c(song_name)),
      style = list(
        css(
          text_align = "left",
          font_weight = "bold",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(pred)),
      style = list(
        css(
          text_align = "center",
          font_weight = "bold",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    # Song Frequency Columns
    tab_style(
      locations = cells_body(columns = c(pct_shows_since_debut,pct_shows_all_time,pct_shows_mikey_years,pct_shows_jimmy_years,diff_jimmy_mikey_shows,
                                         #pct_shows_same_venue, diff_shows_same_venue,
                                         pct_shows_same_day, diff_shows_same_day)),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(diff_jimmy_mikey_shows,
                                         #diff_shows_same_venue,
                                         diff_shows_same_day,
                                         ltp_ratio)),
      style = list(
        css(
          text_align = "center",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    # LTP Columns
    tab_style(
      locations = cells_body(columns = c(ltp, ltp_2, ltp_3, avg_ltp, ltp_diff)),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    # Metric Columns
    tab_style(
      locations = cells_body(columns = c(raw_score, raw_run_score, overdue_run_metric)),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    # Number Format
    fmt_percent(
      columns = c(pct_shows_since_debut,pct_shows_all_time,pct_shows_mikey_years,pct_shows_jimmy_years,diff_jimmy_mikey_shows,
                  #pct_shows_same_venue, diff_shows_same_venue,
                  pct_shows_same_day, diff_shows_same_day,
                  pred),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(avg_ltp,ltp_diff),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(ltp_ratio, raw_score,	raw_run_score,	overdue_run_metric),
      decimals = 2
    ) %>%
    # Column Labels
    tab_style(
      locations = cells_column_labels(),
      style = list(
        css(
          color = "black",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black"
        )
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(song_name, pred)),
      style = list(
        css(
          color = "white",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black",
          border_right = "2px solid #333F48",
          background_color = "#BF5700"
        )
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(diff_jimmy_mikey_shows,
                                                  #diff_shows_same_venue,
                                                  diff_shows_same_day,
                                                  ltp_ratio, overdue_run_metric)),
      style = list(
        css(
          color = "black",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    # Color Scales
    data_color(
      columns = c(pred),
      fn = scales::col_numeric(
        palette = colorRamp(c("#BCCFB4", "#09622A"), interpolate="spline"),
        domain = c(0,1)
      )
    ) %>%
    data_color(
      columns = c(#diff_shows_same_venue,
        diff_shows_same_day),
      fn = scales::col_numeric(
        palette = colorRamp(c('#B70005FF', '#EA332FFF', '#EF6A63FF', '#F8BEB0FF','#FFFFFF', '#CDE1C2FF','#55974CFF', '#287A22FF', '#17692CFF'), interpolate="spline"),
        domain = c(-0.2,0.2)
      )
    ) %>%
    # Header
    tab_header(
      title = md(
        paste0("<span style='color:white'>**",lab,"**</style>")
      ),
      subtitle = md(
        paste0("<span style='color:white'>***",sub_lab,"***</style>")
      )
    ) %>%
    # Rename Labels
    cols_label(
      song_name = "Song",
      pct_shows_since_debut = "Since Debut",
      pct_shows_all_time = "All Time",
      pct_shows_mikey_years = "Mikey Shows",
      pct_shows_jimmy_years = "Jimmy Shows",
      diff_jimmy_mikey_shows = "Jimmy - Mikey",
      #pct_shows_same_venue = "Venue",
      #diff_shows_same_venue = "+/-",
      pct_shows_same_day = "Day",
      diff_shows_same_day = "+/-",
      ltp = "1",
      ltp_2 = "2",
      ltp_3  = "3",
      avg_ltp = "AVG",
      ltp_diff = "LTP-AVG",
      ltp_ratio = "LTP/AVG",
      raw_score = "Show Score",
      raw_run_score = "Run Score",
      overdue_run_metric = "Overdue Score",
      pred = "Pred"
    ) %>%
    tab_options(
      table.border.top.style = "solid",
      table.border.top.width = "3px",
      table.border.top.color = "#333F48",
      table.border.right.style = "solid",
      table.border.right.width = "3px",
      table.border.right.color = "#333F48",
      table.border.bottom.style = "solid",
      table.border.bottom.width = "3px",
      table.border.bottom.color = "#333F48",
      table.border.left.style = "solid",
      table.border.left.width = "3px",
      table.border.left.color = "#333F48",
      heading.background.color = "#BF5700"
    )
  
  return(gt_obj)
  
}
build_next_show_table(data = AC_N2, n_preds = 25)

# Single Run
build_next_run_table <- function(data = AC_ALL, n_preds = 10){
  
  keep_run_cols <- c('song_name', 'Mean', 'pred', 'N2', 'N3', 'pct_shows_since_debut', 'pct_shows_all_time',
                     'pct_shows_mikey_years', 'pct_shows_jimmy_years', 'diff_jimmy_mikey_shows',
                     #'pct_shows_same_venue', 'diff_shows_same_venue',
                     'ltp', 'ltp_2', 'ltp_3', 'avg_ltp', 'ltp_diff', 'ltp_ratio',
                     'raw_score', 'raw_run_score', 'overdue_run_metric')
  
  
  show_date <- unique(data$date)
  show_city <- unique(data$city)
  show_venue <- unique(data$venue_full)
  
  lab <- "Widespread Panic Setlist Predictions"
  sub_lab <- paste0("Predictions for ", show_venue)
  
  df <- data %>% arrange(-Mean) %>% head(n_preds) %>% select(all_of(keep_run_cols))
  
  gt_obj <- df %>%
    gt() %>%
    # Spanners
    tab_spanner(
      label = "Predictions",
      columns = c(Mean, pred, N2, N3),
      id = 'PRED'
    ) %>%
    tab_spanner(
      label = "Song Frequency (% of Shows)",
      columns = c(pct_shows_since_debut, pct_shows_all_time,
                  pct_shows_mikey_years, pct_shows_jimmy_years, diff_jimmy_mikey_shows#,
                  #pct_shows_same_venue, diff_shows_same_venue
      ),
      id = "FREQ"
    ) %>%
    tab_spanner(
      label = "Last Time Played",
      columns = c(ltp, ltp_2, ltp_3, avg_ltp, ltp_diff, ltp_ratio),
      id = 'LTP'
    ) %>%
    tab_spanner(
      label = "Metrics",
      columns = c(raw_score, raw_run_score, overdue_run_metric),
      id = "METRICS"
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("right", "left", "bottom"),
          color = "#333F48",
          weight = px(2)
        ),
        cell_text(
          weight = "bold",
          style = "italic",
          color = "white"
        ),
        cell_fill(
          color = "#BF5700"
        )
      ),
      locations = cells_column_spanners(spanners = c("PRED","FREQ", "LTP", "METRICS"))
    ) %>%
    # Song + Pred Columns Format
    tab_style(
      locations = cells_body(columns = c(song_name)),
      style = list(
        css(
          text_align = "left",
          font_weight = "bold",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(Mean)),
      style = list(
        css(
          text_align = "center",
          font_style = "itallic",
          font_weight = "bold",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(N3)),
      style = list(
        css(
          text_align = "center",
          font_style = "itallic",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(pred, N2)),
      style = list(
        css(
          text_align = "center",
          font_style = "itallic"
        )
      )
    ) %>%
    # Song Frequency Columns
    tab_style(
      locations = cells_body(columns = c(pct_shows_since_debut,pct_shows_all_time,pct_shows_mikey_years,pct_shows_jimmy_years,diff_jimmy_mikey_shows,
                                         #pct_shows_same_venue, diff_shows_same_venue,
      )
      ),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(N3,
                                         diff_jimmy_mikey_shows,
                                         #diff_shows_same_venue,
                                         ltp_ratio)),
      style = list(
        css(
          text_align = "center",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    # LTP Columns
    tab_style(
      locations = cells_body(columns = c(ltp, ltp_2, ltp_3, avg_ltp, ltp_diff)),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    # Metric Columns
    tab_style(
      locations = cells_body(columns = c(raw_score, raw_run_score, overdue_run_metric)),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    # Number Format
    fmt_percent(
      columns = c(pct_shows_since_debut,pct_shows_all_time,pct_shows_mikey_years,pct_shows_jimmy_years,diff_jimmy_mikey_shows,
                  #pct_shows_same_venue, diff_shows_same_venue,
                  Mean, pred, N2, N3),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(avg_ltp,ltp_diff),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(ltp_ratio, raw_score,	raw_run_score,	overdue_run_metric),
      decimals = 2
    ) %>%
    # Column Labels
    tab_style(
      locations = cells_column_labels(),
      style = list(
        css(
          color = "black",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black"
        )
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(song_name)),
      style = list(
        css(
          color = "white",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black",
          border_right = "2px solid #333F48",
          background_color = "#BF5700"
        )
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(N3,
                                                  diff_jimmy_mikey_shows,
                                                  #diff_shows_same_venue,
                                                  ltp_ratio, overdue_run_metric)),
      style = list(
        css(
          color = "black",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    # Color Scales
    data_color(
      columns = c(Mean, pred, N2, N3),
      fn = scales::col_numeric(
        palette = colorRamp(c("#BCCFB4", "#09622A"), interpolate="spline"),
        domain = c(0,1)
      )
    ) %>%
    #data_color(
    #  columns = c(#diff_shows_same_venue,),
    #  fn = scales::col_numeric(
    #    palette = colorRamp(c('#B70005FF', '#EA332FFF', '#EF6A63FF', '#F8BEB0FF','#FFFFFF', '#CDE1C2FF','#55974CFF', '#287A22FF', '#17692CFF'), interpolate="spline"),
    #    domain = c(-0.2,0.2)
    #  )
    #) %>%
    # Header
    tab_header(
      title = md(
        paste0("<span style='color:white'>**",lab,"**</style>")
      ),
      subtitle = md(
        paste0("<span style='color:white'>***",sub_lab,"***</style>")
      )
    ) %>%
    # Rename Labels
    cols_label(
      song_name = "Song",
      pct_shows_since_debut = "Since Debut",
      pct_shows_all_time = "All Time",
      pct_shows_mikey_years = "Mikey Shows",
      pct_shows_jimmy_years = "Jimmy Shows",
      diff_jimmy_mikey_shows = "Jimmy - Mikey",
      #pct_shows_same_venue = "Venue",
      #diff_shows_same_venue = "+/-",
      ltp = "1",
      ltp_2 = "2",
      ltp_3  = "3",
      avg_ltp = "AVG",
      ltp_diff = "LTP-AVG",
      ltp_ratio = "LTP/AVG",
      raw_score = "Show Score",
      raw_run_score = "Run Score",
      overdue_run_metric = "Overdue Score",
      pred = "N1"
    ) %>%
    tab_options(
      table.border.top.style = "solid",
      table.border.top.width = "3px",
      table.border.top.color = "#333F48",
      table.border.right.style = "solid",
      table.border.right.width = "3px",
      table.border.right.color = "#333F48",
      table.border.bottom.style = "solid",
      table.border.bottom.width = "3px",
      table.border.bottom.color = "#333F48",
      table.border.left.style = "solid",
      table.border.left.width = "3px",
      table.border.left.color = "#333F48",
      heading.background.color = "#BF5700"
    )
  
  return(gt_obj)
  
}
build_next_run_table(AC_ALL, n_preds = 20)

# Bustouts on Run
build_next_run_rare_table <- function(data = AC_ALL, n_preds = 10){
  keep_run_cols <- c('song_name', 'Mean', 'pred', 'N2', 'N3', 'pct_shows_since_debut', 'pct_shows_all_time',
                     'pct_shows_mikey_years', 'pct_shows_jimmy_years', 'diff_jimmy_mikey_shows',
                     #'pct_shows_same_venue', 'diff_shows_same_venue',
                     'ltp', 'ltp_2', 'ltp_3', 'avg_ltp', 'ltp_diff', 'ltp_ratio',
                     'raw_score', 'raw_run_score', 'overdue_run_metric')
  
  
  show_date <- unique(data$date)
  show_city <- unique(data$city)
  show_venue <- unique(data$venue_full)
  
  lab <- "Widespread Panic Setlist Predictions - Bust Outs"
  sub_lab <- paste0("Bust Out Predictions for ", show_venue, " - Songs LTP > 15 & < 10% Played Since Debut")
  
  df <- data %>% filter(pct_shows_since_debut < 0.10, pct_shows_since_debut > 0.02, ltp > 15) %>% arrange(-(Mean / pct_shows_since_debut)) %>% head(n_preds) %>% select(all_of(keep_run_cols))
  
  gt_obj <- df %>%
    gt() %>%
    # Spanners
    tab_spanner(
      label = "Predictions",
      columns = c(Mean, pred, N2, N3),
      id = 'PRED'
    ) %>%
    tab_spanner(
      label = "Song Frequency (% of Shows)",
      columns = c(pct_shows_since_debut, pct_shows_all_time,
                  pct_shows_mikey_years, pct_shows_jimmy_years, diff_jimmy_mikey_shows#,
                  #pct_shows_same_venue, diff_shows_same_venue
      ),
      id = "FREQ"
    ) %>%
    tab_spanner(
      label = "Last Time Played",
      columns = c(ltp, ltp_2, ltp_3, avg_ltp, ltp_diff, ltp_ratio),
      id = 'LTP'
    ) %>%
    tab_spanner(
      label = "Metrics",
      columns = c(raw_score, raw_run_score, overdue_run_metric),
      id = "METRICS"
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("right", "left", "bottom"),
          color = "#333F48",
          weight = px(2)
        ),
        cell_text(
          weight = "bold",
          style = "italic",
          color = "white"
        ),
        cell_fill(
          color = "#BF5700"
        )
      ),
      locations = cells_column_spanners(spanners = c("PRED","FREQ", "LTP", "METRICS"))
    ) %>%
    # Song + Pred Columns Format
    tab_style(
      locations = cells_body(columns = c(song_name)),
      style = list(
        css(
          text_align = "left",
          font_weight = "bold",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(Mean)),
      style = list(
        css(
          text_align = "center",
          font_style = "itallic",
          font_weight = "bold",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(N3)),
      style = list(
        css(
          text_align = "center",
          font_style = "itallic",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(pred, N2)),
      style = list(
        css(
          text_align = "center",
          font_style = "itallic"
        )
      )
    ) %>%
    # Song Frequency Columns
    tab_style(
      locations = cells_body(columns = c(pct_shows_since_debut,pct_shows_all_time,pct_shows_mikey_years,pct_shows_jimmy_years,diff_jimmy_mikey_shows,
                                         #pct_shows_same_venue, diff_shows_same_venue,
      )
      ),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(N3,
                                         diff_jimmy_mikey_shows,
                                         #diff_shows_same_venue,
                                         ltp_ratio)),
      style = list(
        css(
          text_align = "center",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    # LTP Columns
    tab_style(
      locations = cells_body(columns = c(ltp, ltp_2, ltp_3, avg_ltp, ltp_diff)),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    # Metric Columns
    tab_style(
      locations = cells_body(columns = c(raw_score, raw_run_score, overdue_run_metric)),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    # Number Format
    fmt_percent(
      columns = c(pct_shows_since_debut,pct_shows_all_time,pct_shows_mikey_years,pct_shows_jimmy_years,diff_jimmy_mikey_shows,
                  #pct_shows_same_venue, diff_shows_same_venue,
                  Mean, pred, N2, N3),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(avg_ltp,ltp_diff),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(ltp_ratio, raw_score,	raw_run_score,	overdue_run_metric),
      decimals = 2
    ) %>%
    # Column Labels
    tab_style(
      locations = cells_column_labels(),
      style = list(
        css(
          color = "black",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black"
        )
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(song_name)),
      style = list(
        css(
          color = "white",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black",
          border_right = "2px solid #333F48",
          background_color = "#BF5700"
        )
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(N3,
                                                  diff_jimmy_mikey_shows,
                                                  #diff_shows_same_venue,
                                                  ltp_ratio, overdue_run_metric)),
      style = list(
        css(
          color = "black",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    # Color Scales
    data_color(
      columns = c(Mean, pred, N2, N3),
      fn = scales::col_numeric(
        palette = colorRamp(c("#BCCFB4", "#09622A"), interpolate="spline"),
        domain = c(0,0.4),
      ),
    ) %>%
    # Header
    tab_header(
      title = md(
        paste0("<span style='color:white'>**",lab,"**</style>")
      ),
      subtitle = md(
        paste0("<span style='color:white'>***",sub_lab,"***</style>")
      )
    ) %>%
    # Rename Labels
    cols_label(
      song_name = "Song",
      pct_shows_since_debut = "Since Debut",
      pct_shows_all_time = "All Time",
      pct_shows_mikey_years = "Mikey Shows",
      pct_shows_jimmy_years = "Jimmy Shows",
      diff_jimmy_mikey_shows = "Jimmy - Mikey",
      #pct_shows_same_venue = "Venue",
      #diff_shows_same_venue = "+/-",
      ltp = "1",
      ltp_2 = "2",
      ltp_3  = "3",
      avg_ltp = "AVG",
      ltp_diff = "LTP-AVG",
      ltp_ratio = "LTP/AVG",
      raw_score = "Show Score",
      raw_run_score = "Run Score",
      overdue_run_metric = "Overdue Score",
      pred = "N1"
    ) %>%
    tab_options(
      table.border.top.style = "solid",
      table.border.top.width = "3px",
      table.border.top.color = "#333F48",
      table.border.right.style = "solid",
      table.border.right.width = "3px",
      table.border.right.color = "#333F48",
      table.border.bottom.style = "solid",
      table.border.bottom.width = "3px",
      table.border.bottom.color = "#333F48",
      table.border.left.style = "solid",
      table.border.left.width = "3px",
      table.border.left.color = "#333F48",
      heading.background.color = "#BF5700"
    )
  
  return(gt_obj)
  
}
build_next_run_rare_table(AC_ALL, n_preds = 25)



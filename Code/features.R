# features.R - Feature engineering for setlist prediction model
# Transforms raw show/song data into model-ready features

library(tidyverse)
library(lubridate)

`%notin%` <- Negate(`%in%`)

#' Prepare model input data
#'
#' @param dim_df Show dimension table
#' @param song_df Song fact table
#' @return List with model_data, model_dim, dim_future
prep_model_input <- function(dim_df, song_df) {
  
  # Re-index shows (exclude radio/soundcheck)
  model_dim <- dim_df %>%
    filter(is_radio != 1 & is_soundcheck != 1) %>%
    select(-show_index, -run_index, -show_in_run, -year_index) %>%
    arrange(year, month, day) %>% 
    rowid_to_column('show_index') %>%
    arrange(date, venue_name) %>% 
    group_by(venue_name, run_index = cumsum(c(1, diff(date) != 1))) %>% 
    ungroup() %>%
    arrange(date, run_index) %>% 
    group_by(run_index) %>% 
    mutate(show_in_run = (show_index - min(show_index)) + 1) %>% 
    ungroup() %>%
    arrange(year, show_index) %>% 
    group_by(year) %>% 
    mutate(year_index = row_number()) %>% 
    ungroup() %>%
    arrange(show_index)
  
  # Split historical and future
  dim_future_tbl <- model_dim %>% filter(is_fut == 1)
  model_dim <- model_dim %>% filter(is_fut == 0)
  
  message(paste0(nrow(model_dim), " historical shows prepared for modeling"))
  
  # Join songs with show data
  model_data <- model_dim %>%
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
  
  return(list(
    model_data = model_data,
    model_dim = model_dim,
    dim_future = dim_future_tbl
  ))
}

#' Create feature set for a single show prediction
#'
#' @param test_date Date of show to predict
#' @param model_data Historical model data
#' @param dim_future Future shows dimension
#' @return Data frame with features for each song
create_features_for_show <- function(test_date, model_data, dim_future) {
  
  # Get show metadata
  if (test_date > max(model_data$date)) {
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
  
  next_show_day <- weekdays(as.Date(test_date))
  next_show_index <- test_show_index
  next_run_index <- test_run_index
  
  # Filter to historical data before test date
  df <- model_data %>%
    filter(date < test_date) %>%
    filter(song_name %notin% c('', 'DRUMS', 'JAM')) %>%
    arrange(run_index, show_index, year_index, song_index) %>%
    group_by(state) %>% 
    mutate(
      state_show_index = cumsum(!duplicated(show_index)), 
      state_run_index = cumsum(!duplicated(run_index))
    ) %>% 
    ungroup() %>%
    group_by(city) %>% 
    mutate(
      city_show_index = cumsum(!duplicated(show_index)), 
      city_run_index = cumsum(!duplicated(run_index))
    ) %>% 
    ungroup() %>%
    group_by(venue_full) %>% 
    mutate(
      venue_show_index = cumsum(!duplicated(show_index)), 
      venue_run_index = cumsum(!duplicated(run_index))
    ) %>% 
    ungroup()
  
  # Calculate totals for context
  tot_shows <- n_distinct(df$link)
  tot_shows_last_6_months <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365/2)) %>% select(link) %>% distinct())
  tot_shows_last_year <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= 365) %>% select(link) %>% distinct())
  tot_shows_last_2_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*2)) %>% select(link) %>% distinct())
  tot_shows_last_4_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*4)) %>% select(link) %>% distinct())
  tot_shows_last_10_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*10)) %>% select(link) %>% distinct())
  
  tot_shows_mikey_years <- n_distinct(df %>% filter(date <= "2002-08-10") %>% select(link) %>% distinct())
  tot_shows_jimmy_years <- n_distinct(df %>% filter(date >= "2006-08-03") %>% select(link) %>% distinct())
  
  tot_shows_same_state <- n_distinct(df %>% filter(state == test_state) %>% select(link) %>% distinct())
  tot_shows_same_city <- n_distinct(df %>% filter(city == test_city) %>% select(link) %>% distinct())
  tot_shows_same_venue <- n_distinct(df %>% filter(venue_full == test_venue) %>% select(link) %>% distinct())
  tot_shows_same_day <- n_distinct(df %>% filter(weekday == next_show_day) %>% select(link) %>% distinct())
  
  tot_runs <- n_distinct(df$run_index)
  tot_runs_last_6_months <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365/2)) %>% select(run_index) %>% distinct())
  tot_runs_last_year <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= 365) %>% select(run_index) %>% distinct())
  tot_runs_last_2_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*2)) %>% select(run_index) %>% distinct())
  tot_runs_last_4_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*4)) %>% select(run_index) %>% distinct())
  tot_runs_last_10_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*10)) %>% select(run_index) %>% distinct())
  
  tot_runs_same_state <- n_distinct(df %>% filter(state == test_state) %>% select(run_index) %>% distinct())
  tot_runs_same_city <- n_distinct(df %>% filter(city == test_city) %>% select(run_index) %>% distinct())
  tot_runs_same_venue <- n_distinct(df %>% filter(venue_full == test_venue) %>% select(run_index) %>% distinct())
  
  tot_shows_same_in_run <- n_distinct(df %>% filter(show_in_run == test_show_in_run) %>% select(link) %>% distinct())
  tot_shows_same_day_in_run <- n_distinct(df %>% filter(weekday == next_show_day & show_in_run == test_show_in_run) %>% select(link) %>% distinct())
  
  # Calculate song-level features
  clean_train <- df %>%
    mutate(
      # Last X Years
      is_last_6_months = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= (365/2), 1, 0),
      is_last_year = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365, 1, 0),
      is_last_2_years = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365*2, 1, 0),
      is_last_4_years = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365*4, 1, 0),
      is_last_10_years = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365*10, 1, 0),
      
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
      is_same_day_in_run = is_same_day * is_same_in_run,
      
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
      ltp_10 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 10),
      across(starts_with('ltp_'), ~ if_else(is.na(.), next_show_index, .)),
      
      # LTP BY RUN
      ltp_run = next_run_index - max(run_index),
      ltp_2_run = next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 2),
      ltp_3_run = next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 3),
      ltp_4_run = next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 4),
      ltp_5_run = next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 5),
      ltp_6_run = next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 6),
      ltp_7_run = next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 7),
      ltp_8_run = next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 8),
      ltp_9_run = next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 9),
      ltp_10_run = next_run_index - nth(sort(unique(run_index), decreasing = TRUE), 10),
      across(starts_with('ltp_') & ends_with('_run'), ~ if_else(is.na(.), next_run_index, .))
    ) %>%
    summarise(
      ftp = max(ftp_date),
      
      ### TOTAL COUNTS SINCE DEBUT ###
      tot_shows_since_debut = next_show_index - min(ftp_show),
      tot_runs_since_debut = next_run_index - min(ftp_run),
      
      ## SHOWS
      # Time
      n_shows_all_time = n_distinct(link),
      n_shows_last_6_months = n_distinct(link[is_last_6_months == 1]),
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
      n_runs_last_6_months = (n_distinct(run_index * is_last_6_months)) - 1,
      n_runs_last_year = (n_distinct(run_index * is_last_year)) - 1,
      n_runs_last_2_years = (n_distinct(run_index * is_last_2_years)) - 1,
      n_runs_last_4_years = (n_distinct(run_index * is_last_4_years)) - 1,
      n_runs_last_10_years = (n_distinct(run_index * is_last_10_years)) - 1,
      
      # Location
      n_runs_same_state = (n_distinct(run_index * is_same_state)) - 1,
      n_runs_same_city = (n_distinct(run_index * is_same_city)) - 1,
      n_runs_same_venue = (n_distinct(run_index * is_same_venue)) - 1,
      
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
      
      ## RUNS
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
      # Time
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
      
      ### NORMALIZE LTP ###
      # By Show
      ltp = max(ltp),
      ltp_2 = max(ltp_2),
      ltp_3 = max(ltp_3),
      ltp_4 = max(ltp_4),
      ltp_5 = max(ltp_5),
      ltp_6 = max(ltp_6),
      ltp_7 = max(ltp_7),
      ltp_8 = max(ltp_8),
      ltp_9 = max(ltp_9),
      ltp_10 = max(ltp_10),
      
      # By Run
      ltp_run = max(ltp_run),
      ltp_2_run = max(ltp_2_run),
      ltp_3_run = max(ltp_3_run),
      ltp_4_run = max(ltp_4_run),
      ltp_5_run = max(ltp_5_run),
      ltp_6_run = max(ltp_6_run),
      ltp_7_run = max(ltp_7_run),
      ltp_8_run = max(ltp_8_run),
      ltp_9_run = max(ltp_9_run),
      ltp_10_run = max(ltp_10_run)
    ) %>%
    ungroup() %>%
    mutate(
      eligible = if_else(rowSums(!is.na(select(., starts_with("ltp_")))) >= 3, 1, 0)
    )
  
  # Calculate advanced LTP metrics for eligible songs
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
      avg_ltp = sum(ltp, diff_ltp_1_2, diff_ltp_2_3, diff_ltp_3_4, diff_ltp_4_5, diff_ltp_5_6, 
                    diff_ltp_6_7, diff_ltp_7_8, diff_ltp_8_9, diff_ltp_9_10, na.rm = TRUE) / 
                sum(!is.na(ltp), !is.na(diff_ltp_1_2), !is.na(diff_ltp_2_3), !is.na(diff_ltp_3_4),
                    !is.na(diff_ltp_4_5), !is.na(diff_ltp_5_6), !is.na(diff_ltp_6_7), 
                    !is.na(diff_ltp_7_8), !is.na(diff_ltp_8_9), !is.na(diff_ltp_9_10)),
      recent_avg_ltp = sum(ltp, diff_ltp_1_2, diff_ltp_2_3, diff_ltp_3_4, diff_ltp_4_5, diff_ltp_5_6,
                           na.rm = TRUE) / 
                       sum(!is.na(ltp), !is.na(diff_ltp_1_2), !is.na(diff_ltp_2_3), !is.na(diff_ltp_3_4),
                           !is.na(diff_ltp_4_5), !is.na(diff_ltp_5_6)),
      
      # LTP Metrics
      ltp_diff = abs(ltp - avg_ltp),
      ltp_diff = if_else(ltp - avg_ltp > 0, ltp_diff, if_else(ltp - avg_ltp < 0, ltp_diff, 0.01)),
      ltp_ratio = ltp_diff / avg_ltp,
      
      # Recent LTP Metrics
      recent_ltp_diff = abs(ltp - recent_avg_ltp),
      recent_ltp_diff = if_else(ltp - avg_ltp > 0, recent_ltp_diff, if_else(ltp - avg_ltp < 0, recent_ltp_diff, 0.01)),
      recent_ltp_ratio = recent_ltp_diff / recent_avg_ltp,
      
      # Other LTP Metrics
      played_last_show = if_else(ltp == 1, 1, 0),
      overdue_show = if_else(ltp - avg_ltp > 0, ltp_diff, 0),
      overdue_metric = if_else(ltp - avg_ltp > 0, ltp_diff * pct_runs_last_year, 0),
      
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
      avg_ltp_run = sum(ltp_run, diff_ltp_1_2_run, diff_ltp_2_3_run, diff_ltp_3_4_run, diff_ltp_4_5_run, 
                        diff_ltp_5_6_run, diff_ltp_6_7_run, diff_ltp_7_8_run, diff_ltp_8_9_run, 
                        diff_ltp_9_10_run, na.rm = TRUE) / 
                    sum(!is.na(ltp_run), !is.na(diff_ltp_1_2_run), !is.na(diff_ltp_2_3_run), 
                        !is.na(diff_ltp_3_4_run), !is.na(diff_ltp_4_5_run), !is.na(diff_ltp_5_6_run), 
                        !is.na(diff_ltp_6_7_run), !is.na(diff_ltp_7_8_run), !is.na(diff_ltp_8_9_run), 
                        !is.na(diff_ltp_9_10_run)),
      recent_avg_ltp_run = sum(ltp_run, diff_ltp_1_2_run, diff_ltp_2_3_run, diff_ltp_3_4_run, 
                               diff_ltp_4_5_run, diff_ltp_5_6_run, na.rm = TRUE) / 
                           sum(!is.na(ltp_run), !is.na(diff_ltp_1_2_run), !is.na(diff_ltp_2_3_run), 
                               !is.na(diff_ltp_3_4_run), !is.na(diff_ltp_4_5_run), !is.na(diff_ltp_5_6_run)),
      
      # LTP Run Metrics
      ltp_run_diff = abs(ltp_run - avg_ltp_run),
      ltp_run_diff = if_else(ltp_run - avg_ltp_run > 0, ltp_run_diff, if_else(ltp_run - avg_ltp_run < 0, ltp_run_diff, 0.001)),
      ltp_run_ratio = ltp_run_diff / avg_ltp_run,
      
      # Recent LTP Run Metrics
      recent_ltp_run_diff = abs(ltp_run - recent_avg_ltp_run),
      recent_ltp_run_diff = if_else(ltp_run - recent_avg_ltp_run > 0, recent_ltp_run_diff, 
                                    if_else(ltp_run - recent_avg_ltp_run < 0, recent_ltp_run_diff, 0.001)),
      recent_ltp_run_ratio = recent_ltp_run_diff / recent_avg_ltp_run,
      
      # Played Last Run
      played_this_run = if_else(ltp_run == 0, 1, 0),
      played_last_run = if_else(ltp_run == 1, 1, 0),
      overdue_run = if_else(ltp_run - avg_ltp_run > 0, ltp_run_diff, 0),
      overdue_run_metric = if_else(ltp_run - avg_ltp_run > 0, ltp_run_diff * pct_runs_last_year, 0),
      
      # Score Metrics
      raw_score = ((pct_shows_last_6_months * 10) + (pct_shows_last_year * 8) + (pct_shows_last_2_years * 6) + 
                   (pct_shows_last_10_years * 4) + (pct_shows_all_time * 2) + diff_shows_same_city + 
                   diff_shows_same_day + diff_shows_same_day_in_run) / (ltp_diff),
      raw_run_score = ((pct_runs_last_6_months * 10) + (pct_runs_last_year * 8) + (pct_runs_last_2_years * 6) + 
                       (pct_runs_last_10_years * 4) + (pct_runs_all_time * 2) + diff_runs_same_city + 
                       diff_shows_same_day + diff_shows_same_day_in_run) / (ltp_run_diff),
      raw_score = if_else(played_this_run == 1, 0, raw_score),
      raw_run_score = if_else(played_this_run == 1, 0, raw_run_score)
    ) %>%
    ungroup()
  
  # Add show metadata
  eligible_songs$city <- test_city
  eligible_songs$date <- test_date
  eligible_songs$venue_full <- test_venue
  eligible_songs$show_in_run <- test_show_in_run
  eligible_songs$show_index <- next_show_index
  
  message(paste0("✓ Features created for ", test_date, " @ ", test_venue, " (", nrow(eligible_songs), " eligible songs)"))
  
  return(eligible_songs)
}

#' Create training dataset from multiple historical shows
#'
#' @param end_date Latest date to include
#' @param train_n Number of shows to include
#' @param model_data Historical model data
#' @param dim_future Future shows
#' @return Training dataset with features and labels
create_train_set <- function(end_date, train_n, model_data, dim_future) {
  start_time <- Sys.time()
  
  # Get train dates
  train_dates <- model_data %>%
    filter(date <= end_date) %>%
    distinct(date) %>%
    arrange(desc(date)) %>%
    slice_head(n = train_n) %>%
    pull(date)
  
  message(paste0("\nBuilding training set from ", train_n, " concerts (", 
                 min(train_dates), " to ", max(train_dates), ")...\n"))
  
  # Create features for each show
  list_of_dfs <- map(train_dates, function(train_date) {
    predict_table <- create_features_for_show(train_date, model_data, dim_future) %>%
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
  
  message(paste0("✓ Training set created: ", nrow(train_table), " rows in ", 
                 round(elapsed_time, 2), " minutes\n"))
  
  return(train_table)
}

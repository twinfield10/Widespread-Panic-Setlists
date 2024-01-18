### Model Test ##
full_data %>% filter(date == '2016-10-30') %>% print.data.frame()
full_data %>% filter(song_name == 'ROADHOUSE BLUES')
# Train Manipulate Function #
manipulate_train <- function(test_date, test_city){
  
  df <- full_data %>% filter(date < test_date)
  
  tot_shows <- n_distinct(df$link)
  tot_shows_last_6_months <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365/2)) %>% select(link) %>% distinct())
  tot_shows_last_year <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365)) %>% select(link) %>% distinct())
  tot_shows_last_2_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*2)) %>% select(link) %>% distinct())
  tot_shows_last_4_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*4)) %>% select(link) %>% distinct())
  tot_shows_last_10_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*102)) %>% select(link) %>% distinct())
  tot_shows_mikey_years <- n_distinct(df %>% filter(date <= "2002-08-10") %>% select(link) %>% distinct())
  tot_shows_same_city <- n_distinct(df %>% filter(city == test_city) %>% select(link) %>% distinct())
  next_show_index = max(df$show_index) + 1
  
  # Song Statistics
  clean_train <- df %>%
    filter(song_name %notin% c('','DRUMS', 'JAM')) %>%
    arrange(show_index, year_index, song_index) %>%
    mutate(
      is_last_6_months = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= (365/2) , 1, 0),
      is_last_year = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365 , 1, 0),
      is_last_2_years = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365*2 , 1, 0),
      is_last_4_years = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365*4 , 1, 0),
      is_last_10_years = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365*10 , 1, 0),
      is_mikey_show = ifelse(date <= "2002-08-10", 1, 0),
      is_same_city = ifelse(city == test_city, 1, 0)
    ) %>%
    #arrange(desc(show_index)) %>%
    group_by(song_name) %>%
    mutate(
      ltp =   next_show_index - max(show_index),
      ltp_2 = if_else(is.na(next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 2) & !is.na(ltp)), tot_shows - ltp, next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 2)),
      ltp_3 = if_else(is.na(next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 3) & !is.na(ltp)), tot_shows - ltp_2, next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 3)),
      ltp_4 = if_else(is.na(next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 4) & !is.na(ltp)), tot_shows - ltp_3, next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 4)),
      ltp_5 = if_else(is.na(next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 5) & !is.na(ltp)), tot_shows - ltp_4, next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 5)),
      ltp_6 = if_else(is.na(next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 6) & !is.na(ltp)), tot_shows - ltp_5, next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 6)),
      ltp_7 = if_else(is.na(next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 7) & !is.na(ltp)), tot_shows - ltp_6, next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 7)),
      ltp_8 = if_else(is.na(next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 8) & !is.na(ltp)), tot_shows - ltp_7, next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 8)),
      ltp_9 = if_else(is.na(next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 9) & !is.na(ltp)), tot_shows - ltp_8, next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 9)),
      ltp_10= if_else(is.na(next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 10) & !is.na(ltp)), tot_shows - ltp_9, next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 10))
    ) %>%
    summarise(
      n_shows_all_time = n_distinct(link),
      n_shows_last_6_months = sum(is_last_6_months),
      n_shows_last_year = sum(is_last_year),
      n_shows_last_2_years = sum(is_last_2_years),
      n_shows_last_4_years = sum(is_last_4_years),
      n_shows_last_10_years = sum(is_last_10_years),
      n_shows_mikey_years = sum(is_mikey_show),
      n_shows_same_city = sum(is_same_city),
      pct_shows_all_time = n_shows_all_time / tot_shows,
      pct_shows_last_6_months = n_shows_last_6_months / tot_shows_last_6_months,
      pct_shows_last_year = n_shows_last_year / tot_shows_last_year,
      pct_shows_last_2_years = n_shows_last_2_years / tot_shows_last_2_years,
      pct_shows_last_4_years = n_shows_last_4_years / tot_shows_last_4_years,
      pct_shows_last_10_years = n_shows_last_10_years / tot_shows_last_10_years,
      pct_shows_mikey_years = n_shows_mikey_years / tot_shows_mikey_years,
      pct_shows_same_city = n_shows_same_city / tot_shows_same_city,
      diff_shows_same_city = pct_shows_same_city - pct_shows_all_time,
      off_shelf = if_else(n_shows_last_4_years > 4 & n_shows_all_time > 10, TRUE, FALSE),
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
      ltp_diff =  abs(ltp - avg_ltp),
      overdue = if_else(ltp - avg_ltp > 0, 1, 0),
      ltp_diff = if_else(ltp - avg_ltp > 0, ltp_diff, if_else(ltp - avg_ltp < 0, ltp_diff, 0.008)),
      recnt_adj_due = (pct_shows_last_year - pct_shows_last_6_months),
      score = ((pct_shows_last_6_months * 18) + (pct_shows_last_year * 12) + (pct_shows_last_2_years * 6) + (pct_shows_all_time * 3) + diff_shows_same_city)/(ltp_diff)
    ) %>%
    ungroup() %>%
    mutate(
      score = round(rescale(log(score)+1)*100,2),
      predict_type = case_when(
        # 1) The Basics: (played at more than 20% of shows in last year and difference is < 1)
        (off_shelf == TRUE & (score > 85 | (score > 75 & ltp_diff < 1 & ltp > 3 & pct_shows_last_year > .10 & ltp < 10) | (score > 75 & ltp > 4 & ltp < 15 & n_shows_all_time > 10))) ~ '1_High_Confidence',
        # 2) Song is Due
        (recnt_adj_due > 0.03 & ltp > 8 & score > 60) ~ '2_Medium_Confidence',
        # 3) Low Confidence - Not quite a bust out but song hasn't been played in a bit
        (ltp <= 50 & ltp >= 15 & (ltp_diff < avg_ltp/20)) ~ '3_Low_Confidence',
        # 4) Bust Outs: (Songs that haven't been played in a while but LTP lines up)
        (ltp > 50 & ltp < 150 & (ltp_diff < avg_ltp/10) & score > 45) ~ '4_Bustout',
        TRUE ~ NA
      )
    )%>%
    #filter(n_shows_all_time > 1) %>%
    arrange(desc(score)) %>%
    select(song_name, ltp, ltp_2, ltp_3, pct_shows_all_time, pct_shows_last_year, ltp, avg_ltp, ltp_diff, score, predict_type)
return(clean_train)
}
STL_Predict <- manipulate_train(test_date = '2024-01-19', test_city = 'ST. LOUIS')
STL_Predict_Slim <- STL_Predict %>% filter(!is.na(predict_type))
manipulate_train(test_date = '2023-12-29', test_city = 'ATLANTA') %>% filter(song_name == 'SECOND SKIN')
check_test <- function(show_date, show_city){
  
  df <- full_data %>%
    filter(date == show_date)
  
  cty = df$city %>% unique()
  pred_df <- manipulate_train(test_date = show_date, test_city = cty)
  
  df <- df %>%
    filter(song_name %notin% c('DRUMS', 'JAM')) %>%
    select(date, state, city, venue_name, set, song_name, into, song_index, song_note_detail) %>%
    left_join(pred_df %>% select(song_name, score, predict_type, ltp, avg_ltp, ltp_diff), by = c('song_name')) %>%
    arrange(song_index)
  
  return(df)
}



n1_final_df <- check_test('2023-12-29')
n2_final_df <- check_test('2023-12-30')
n3_final_df <- check_test('2023-12-31')

fox_test <- rbind(check_test('2023-12-29'), check_test('2023-12-30'), check_test('2023-12-31'))

test_historic <- function(n = 5){
  check_dates <- full_data %>% arrange(desc(date)) %>% unique() %>% head(n) %>% select(date)  %>% pull()
  check_list <- c()
  for(i in check_dates){
    check_list[[i]] <- check_test(check_dates[i])
  }
  
  final_df <- bind_rows(check_list)
  
  metric_df <- final_df %>%
    group_by(predict_type) %>%
    summarise(
      count = n()
    )
  print(metric_df)
  return(final_df)
}

test_df <- test_historic()


stl_n1_final_df <- check_test('2024-01-19', 'ST. LOUIS')
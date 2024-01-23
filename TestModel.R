# Load File
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
  ungroup()

### PREPROCESS DATA FOR MODEL ###

# Task 1: Build LTP Input and Columns

# Train Manipulate Function #
manipulate_train <- function(test_date, test_city){
  
  next_show_day = weekdays(as.Date(test_date))
  
  df <- model_data %>%
    filter(date < test_date) %>%
    filter(song_name %notin% c('','DRUMS', 'JAM')) %>%
    arrange(run_index ,show_index, year_index, song_index)
  
  # Shows
  tot_shows <- n_distinct(df$link)
  tot_shows_last_6_months <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365/2)) %>% select(link) %>% distinct())
  tot_shows_last_year <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365)) %>% select(link) %>% distinct())
  tot_shows_last_2_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*2)) %>% select(link) %>% distinct())
  tot_shows_last_4_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*4)) %>% select(link) %>% distinct())
  tot_shows_last_10_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*10)) %>% select(link) %>% distinct())
  tot_shows_mikey_years <- n_distinct(df %>% filter(date <= "2002-08-10") %>% select(link) %>% distinct())
  tot_shows_jimmy_years <- n_distinct(df %>% filter(date >= "2006-08-03") %>% select(link) %>% distinct())
  tot_shows_same_city <- n_distinct(df %>% filter(city == test_city) %>% select(link) %>% distinct())
  
  #Day of Week
  tot_shows_same_day <- n_distinct(df %>% filter(weekday == next_show_day) %>% select(link) %>% distinct())
  tot_shows_monday <- n_distinct(df %>% filter(weekday == 'Monday') %>% select(link) %>% distinct())
  tot_shows_tuesday <- n_distinct(df %>% filter(weekday == 'Tuesday') %>% select(link) %>% distinct())
  tot_shows_wednesday <- n_distinct(df %>% filter(weekday == 'Wednesday') %>% select(link) %>% distinct())
  tot_shows_thursday <- n_distinct(df %>% filter(weekday == 'Thursday') %>% select(link) %>% distinct())
  tot_shows_friday <- n_distinct(df %>% filter(weekday == 'Friday') %>% select(link) %>% distinct())
  tot_shows_saturday <- n_distinct(df %>% filter(weekday == 'Saturday') %>% select(link) %>% distinct())
  tot_shows_sunday <- n_distinct(df %>% filter(weekday == 'Sunday') %>% select(link) %>% distinct())
  
  # Runs
  tot_runs <- max(df$run_index)
  tot_runs_last_6_months <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365/2)) %>% select(run_index) %>% distinct())
  tot_runs_last_year <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365)) %>% select(run_index) %>% distinct())
  tot_runs_last_2_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*2)) %>% select(run_index) %>% distinct())
  tot_runs_last_4_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*4)) %>% select(run_index) %>% distinct())
  tot_runs_last_10_years <- n_distinct(df %>% filter(difftime(test_date, date, units = "days") <= (365*10)) %>% select(run_index) %>% distinct())
  tot_runs_same_city <- n_distinct(df %>% filter(city == test_city) %>% select(run_index) %>% distinct())
  
  # Next Show + Run
  next_show_index = max(df$show_index) + 1
  next_run_index = max(df$run_index) + 1
  
  
  # Song Statistics
  clean_train <- df %>%
    mutate(
      is_last_6_months = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= (365/2) , 1, 0),
      is_last_year = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365 , 1, 0),
      is_last_2_years = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365*2 , 1, 0),
      is_last_4_years = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365*4 , 1, 0),
      is_last_10_years = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365*10 , 1, 0),
      is_mikey_show = ifelse(date <= "2002-08-10", 1, 0),
      is_jimmy_show = ifelse(date <= "2006-08-03", 1, 0),
      is_same_city = ifelse(city == test_city, 1, 0),
      
      is_same_day = ifelse(weekday == next_show_day, 1, 0),
      is_monday = ifelse(weekday == 'Monday', 1, 0),
      is_tuesday = ifelse(weekday == 'Tuesday', 1, 0),
      is_wednesday = ifelse(weekday == 'Wednesday', 1, 0),
      is_thursday = ifelse(weekday == 'Thursday', 1, 0),
      is_friday = ifelse(weekday == 'Friday', 1, 0),
      is_saturday = ifelse(weekday == 'Saturday', 1, 0),
      is_sunday = ifelse(weekday == 'Sunday', 1, 0),
      
      run_is_last_6_months = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= (365/2) , 1, 0),
      run_is_last_year = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365 , 1, 0),
      run_is_last_2_years = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365*2 , 1, 0),
      run_is_last_4_years = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365*4 , 1, 0),
      run_is_last_10_years = ifelse(!is.na(date) & difftime(test_date, date, units = "days") <= 365*10 , 1, 0),
      run_is_same_city = ifelse(city == test_city, 1, 0)
    ) %>%
    group_by(song_name) %>%
    mutate(
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
      n_shows_all_time = n_distinct(link),
      n_shows_last_6_months = sum(is_last_6_months),
      n_shows_last_year = sum(is_last_year),
      n_shows_last_2_years = sum(is_last_2_years),
      n_shows_last_4_years = sum(is_last_4_years),
      n_shows_last_10_years = sum(is_last_10_years),
      n_shows_mikey_years = sum(is_mikey_show),
      n_shows_jimmy_years = sum(is_jimmy_show),
      n_shows_same_city = sum(is_same_city),
      
      n_shows_same_day = sum(is_same_day),
      n_shows_monday = sum(is_monday),
      n_shows_tuesday = sum(is_tuesday),
      n_shows_wednesday = sum(is_wednesday),
      n_shows_thursday = sum(is_thursday),
      n_shows_friday = sum(is_friday),
      n_shows_saturday = sum(is_saturday),
      n_shows_sunday = sum(is_sunday),
      
      n_runs_all_time = n_distinct(run_index),
      n_runs_last_6_months = (n_distinct(run_index * is_last_6_months))-1,
      n_runs_last_year = (n_distinct(run_index * is_last_year))-1,
      n_runs_last_2_years = (n_distinct(run_index * is_last_2_years))-1,
      n_runs_last_4_years = (n_distinct(run_index * is_last_4_years))-1,
      n_runs_last_10_years = (n_distinct(run_index * is_last_10_years))-1,
      n_runs_same_city = (n_distinct(run_index * is_same_city))-1,

      pct_shows_all_time = n_shows_all_time / tot_shows,
      pct_shows_last_6_months = n_shows_last_6_months / tot_shows_last_6_months,
      pct_shows_last_year = n_shows_last_year / tot_shows_last_year,
      pct_shows_last_2_years = n_shows_last_2_years / tot_shows_last_2_years,
      pct_shows_last_4_years = n_shows_last_4_years / tot_shows_last_4_years,
      pct_shows_last_10_years = n_shows_last_10_years / tot_shows_last_10_years,
      pct_shows_mikey_years = n_shows_mikey_years / tot_shows_mikey_years,
      pct_shows_jimmy_years = n_shows_jimmy_years / tot_shows_jimmy_years,
      pct_shows_same_city = n_shows_same_city / tot_shows_same_city,
      
      pct_shows_same_day = n_shows_same_day / tot_shows_same_day,
      pct_shows_monday = n_shows_monday / tot_shows_monday,
      pct_shows_tuesday = n_shows_tuesday / tot_shows_tuesday,
      pct_shows_wednesday = n_shows_wednesday / tot_shows_wednesday,
      pct_shows_thursday = n_shows_thursday / tot_shows_thursday,
      pct_shows_friday = n_shows_friday / tot_shows_friday,
      pct_shows_saturday = n_shows_saturday / tot_shows_saturday,
      pct_shows_sunday = n_shows_sunday / tot_shows_sunday,
      
      pct_runs_all_time = n_runs_all_time / tot_runs,
      pct_runs_last_6_months = n_runs_last_6_months / tot_runs_last_6_months,
      pct_runs_last_year = n_runs_last_year / tot_runs_last_year,
      pct_runs_last_2_years = n_runs_last_2_years / tot_runs_last_2_years,
      pct_runs_last_4_years = n_runs_last_4_years / tot_runs_last_4_years,
      pct_runs_last_10_years = n_runs_last_10_years / tot_runs_last_10_years,
      pct_runs_same_city = n_runs_same_city / tot_runs_same_city,
      
      diff_shows_same_city = pct_shows_same_city - pct_shows_all_time,
      diff_runs_same_city = pct_runs_same_city - pct_runs_all_time,
      
      diff_shows_same_day = pct_shows_same_day - pct_shows_all_time,
      
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
      ltp_diff =  abs(ltp - avg_ltp),
      ltp_diff = if_else(ltp - avg_ltp > 0, ltp_diff, if_else(ltp - avg_ltp < 0, ltp_diff, 0.008)),
      played_last_show = if_else(ltp == 1, 1, 0),
      overdue_show = if_else(ltp - avg_ltp > 0, ltp_diff, 0),
      
      # LTP (Show) Differences
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
      
      ltp_run_diff =  abs(ltp_run - avg_ltp_run),
      ltp_run_diff = if_else(ltp - avg_ltp > 0, ltp_diff, if_else(ltp - avg_ltp < 0, ltp_diff, 0.008)),
      played_last_run = if_else(ltp_run == 1, 1, 0),
      overdue_run = if_else(ltp - avg_ltp > 0, ltp_diff, 0),
      
      # Other Metrics
      recnt_adj_due = (pct_shows_last_year - pct_shows_last_6_months),
      raw_score = ((pct_shows_last_6_months * 18) + (pct_shows_last_year * 12) + (pct_shows_last_2_years * 6) + (pct_shows_all_time * 3) + diff_shows_same_city)/(ltp_diff),
      raw_run_score = ((pct_runs_last_6_months * 18) + (pct_runs_last_year * 12) + (pct_runs_last_2_years * 6) + (pct_runs_all_time * 3) + diff_runs_same_city)/(ltp_run_diff)
    ) %>%
    ungroup()
  
  #score_creation <- eligible_songs %>%  
    #mutate(
      #raw_score = score,
      #raw_run_score = run_score,
      #score = round(rescale(log(score)+1)*100,2),
      #run_score = round(rescale(log(run_score)+1)*100,2),
      #predict_type = case_when(
        # 1) The Basics: (played at more than 20% of shows in last year and difference is < 1)
       # ((score > 85 | (score > 75 & ltp_diff < 1 & ltp > 3 & pct_shows_last_year > .10 & ltp < 10) | (score > 75 & ltp > 4 & ltp < 15 & n_shows_all_time > 10))) ~ '1_High_Confidence',
        # 2) Song is Due
      #  (recnt_adj_due > 0.03 & ltp > 8 & score > 60) ~ '2_Medium_Confidence',
        # 3) Low Confidence - Not quite a bust out but song hasn't been played in a bit
       # (ltp <= 50 & ltp >= 15 & (ltp_diff < avg_ltp/20)) ~ '3_Low_Confidence',
        # 4) Bust Outs: (Songs that haven't been played in a while but LTP lines up)
        #(ltp > 50 & ltp < 150 & (ltp_diff < avg_ltp/10) & score > 45) ~ '4_Bustout',
        #TRUE ~ NA
      #)
    #)%>%
    #arrange(desc(score))
  
return(eligible_songs)
}
predict_df <- manipulate_train(test_date = "2024-01-18", test_city = "ST. LOUIS")
rare_songs <- predict_df %>% filter(n_shows_all_time <= 10)

create_train_set <- function(end_date = '2024-01-20', beg_date = '2023-01-01'){
  
  # Get Train/Test Date List
  train_dates <- model_data %>% filter(date < end_date & date > beg_date) %>% select(date) %>% unique() %>% pull()
  test_dates <- model_data %>% filter(date >= end_date) %>% select(date) %>% unique() %>% pull()
  
  print(paste0("Now Loading ", length(train_dates), " Concerts For Train"))
  print(paste0("Now Loading ", length(test_dates), " Concerts For Test"))
  
  all_dates <- c(train_dates, test_dates)
  
  list_of_dfs <- c()
  
  # Create Loop?
  for(i in 1:length(all_dates)){
    cty = model_data %>% filter(date == all_dates[i]) %>% select(city) %>% unique() %>% pull()
    predict_table <- manipulate_train(test_date = all_dates[i], test_city = cty)
    predict_table$date <- all_dates[i]
    predict_table$city <- cty
    predict_table$datatype <- ifelse(all_dates[i] %in% train_dates, 'Train', 'Test')
    
    setlist <- model_data %>% filter(date == all_dates[i] & city == cty) %>% select(song_name) %>% unique() %>% pull()
    
    final_tbl <- predict_table %>% mutate(played = if_else(song_name %in% setlist, 1, 0))
    
    list_of_dfs[[i]] <- final_tbl
  }
  
  train_table <- bind_rows(list_of_dfs)

  return(train_table)
  
}
check_model_table <- create_train_set()

build_model <- function(){
  set.seed(123)  # for reproducibility
  test_data <- check_model_table %>% filter(datatype == 'Test')
  train_data <- check_model_table %>% filter(datatype == 'Train')

  
  test_index <- test_data$song_name
  
  features <- names(train_data)[!names(train_data) %in% c("played", "city", "date", "datatype", "song_name")]
  target <- "played"
  
  # Train the xgboost model
  xgb_model <- xgboost(data = as.matrix(train_data[, features]),
                       label = as.numeric(train_data[[target]]),
                       objective = "binary:logistic",
                       eval.metric ='logloss',
                       max.depth=3,
                       nrounds = 15,
                       verbose = 1)
  
  # Make predictions on the test set
  pred_vec <- predict(xgb_model, as.matrix(test_data[, features]))
  
  
  
  model_predictions <<- data.frame(
    song_name = test_index,
    pred = pred_vec, actual = test_data[[target]]) %>%
    arrange(desc(pred), desc(actual)) %>%
    mutate(
      log_pred = rescale(log(pred + 1))
    )
  print(model_predictions)
  
  # Evaluate model performance (you might use different metrics based on your problem)
  confusion_matrix <- table(Actual = test_data[[target]], Predicted = round(model_predictions$log_pred))
  print(confusion_matrix)
  
  # Create a data frame to store results
  threshold_results <- data.frame(threshold = numeric(),
                                  sensitivity = numeric(),
                                  specificity = numeric(),
                                  youden_j = numeric())
  
  # Loop through possible threshold values
  for (threshold in seq(0, 1, by = 0.001)) {
    # Reclassify based on the current threshold
    threshold_predictions <- ifelse(model_predictions$log_pred >= threshold, 1, 0)
    
    # Calculate confusion matrix
    confusion_matrix <- table(Actual = as.numeric(model_predictions$actual), Predicted = threshold_predictions)
    
    # Check if confusion matrix is of expected dimensions
    if (nrow(confusion_matrix) == 2 && ncol(confusion_matrix) == 2) {
      # Calculate sensitivity, specificity, and Youden's J
      sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
      specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
      youden_j <- sensitivity + specificity - 1
      
      # Store results in the data frame
      threshold_results <- rbind(threshold_results, 
                                 data.frame(threshold = threshold,
                                            sensitivity = sensitivity,
                                            specificity = specificity,
                                            youden_j = youden_j))
    } else {
      warning("Confusion matrix is not of expected dimensions.")
    }
  }
  
  # Find the threshold that maximizes Youden's J
  if (nrow(threshold_results) > 0) {
    optimal_threshold <- threshold_results$threshold[which.max(threshold_results$youden_j)]
    
    # Print the optimal threshold
    print(paste("Optimal Threshold:", optimal_threshold))
  } else {
    warning("No valid threshold results.")
  }
  
  # Optionally, you can visualize feature importance
  feature_importance <- xgb.importance(feature_names = features, model = xgb_model)
  #print(feature_importance)
  
  model_predictions <<- model_predictions %>% mutate(optimal_pred = if_else(log_pred > optimal_threshold, 1, 0))
  
  opt_confusion_matrix <- table(Actual = model_predictions$actual, Predicted = model_predictions$optimal_pred)
  print("Optimal Threshold Confusion Matrix")
  print(opt_confusion_matrix)
  
  print("Accuracy Score:")
  print(mean(model_predictions$optimal_pred == model_predictions$actual))
}
build_model()
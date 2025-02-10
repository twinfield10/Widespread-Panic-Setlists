## Depreciated

## BUILD MODEL:

## Option 1: Loop Mini Models ##
# Task 1: Function For Building and Testing Model + Metrics
build_model <- function(test_dte, n_shows = 50, rounds = 100, m_depth = 6, rt = 0.1){
  
  ## Pre-Processing ##
  # Split
  test_data <- model_table %>% filter(date == test_dte)
  train_data <- model_table %>% filter(date < test_dte & show_index >= (test_data$show_index[[1]] - n_shows))
  filt_dates <- train_data %>% select(date) %>% filter(date < test_dte) %>% arrange(date) %>% unique() %>% pull()
  
  # Keep Indicies
  song_index <- test_data$song_name
  date_index <- test_data$date
  city_index <- test_data$city
  
  # Features And Targets
  features <- names(train_data)[!names(train_data) %in% c("played", "city", "date", "venue_full", "song_name", "show_index")]
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
    pred = pred_vec,
    actual = test_data[[target]]
  ) %>%
    arrange(desc(pred), desc(actual))
  
  
  ## METRICS ##
  
  feature_importance <- xgb.importance(feature_names = features, model = xgb_model)
  top_features <- feature_importance %>% head(25)
  
  model_predictions <- model_predictions %>% mutate(optimal_pred = ifelse(dense_rank(desc(pred)) <= 22, 1, 0))
  confusion_matrix <- table(Actual = model_predictions$actual, Predicted = model_predictions$optimal_pred)
  print(confusion_matrix)
  
  acc <- mean(model_predictions$optimal_pred == model_predictions$actual)
  prec <- sum(model_predictions$optimal_pred == 1 & model_predictions$actual == 1) / sum(model_predictions$optimal_pred == 1)
  recall <- sum(model_predictions$optimal_pred == 1 & model_predictions$actual == 1) / sum(model_predictions$actual == 1)
  f1_score <- 2 * prec * recall / (prec + recall)
  roc_curve <- pROC::roc(model_predictions$actual, model_predictions$optimal_pred, quiet = TRUE)
  auc_roc <- pROC::auc(roc_curve)
  
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
  
  gc()
  
  
  return(list(metrics_df, model_predictions))
  
}
build_model("2024-06-24")
# Task 2: Loop Models For Years You Want To Test (Default is 2023+2024)
# Produces Two Outputs In General Envrionment (acc_metrics)
# Model Accuracy Metrics For Each DataFrame (all_song_predictions_df)
# Setlist of Predictions and Actual For Each Show Tested
loop_model <- function(test_shows = 10){
  
  # Get Test Dates #
  test_dates <- model_table %>% select(date) %>% arrange(desc(date)) %>% unique() %>% head(test_shows) %>% pull()
  metrics_list_dfs <- c()
  predict_songs_dfs <- c()
  
  # Loop Model Build #
  for(i in 1:length(test_dates)){
    return_list <- build_model(test_dates[[i]])
    metrics_list_dfs[[i]] <- return_list[[1]]
    predict_songs_dfs[[i]] <- return_list[[2]]
    
    print(predict_songs_dfs[[i]] %>% select(date, song_name, pred, actual) %>% filter(actual == 1) %>% arrange(desc(pred)) %>% head(10))
  }
  all_metrics <- bind_rows(metrics_list_dfs)
  
  acc_metrics <<- all_metrics %>% arrange(desc(date)) %>%
    select(-starts_with('features_')) %>%
    unique()
  
  feat_metrics <<- all_metrics %>%
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
    arrange(desc(date), desc(pred), desc(actual)) %>%
    left_join(
      model_table, by = c('date', 'city', 'song_name')
    )
  
}
loop_model()

# Evaluate Model
evaluate_model <- function(data = all_song_predictions_df){
  y_true <- data$actual
  y_pred <- data$pred
  y_bin_pred <- data$ optimal_pred
  
  # AUC
  roc_curve <- roc(y_true, y_pred)
  auc_value <- pROC::auc(roc_curve)
  
  # Log Loss
  logloss_value <- logLoss(y_true, y_pred)
  
  # Rec, Prec, F1
  recall <- sum(y_bin_pred == 1 & y_true == 1) / sum(y_true == 1)
  precision <- sum(y_bin_pred == 1 & y_true == 1) / sum(y_bin_pred == 1)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  accuracy <- sum(y_bin_pred == y_true) / length(y_true)
  
  # Print the results
  cat("AUC:", auc_value, "\n")
  cat("LogLoss:", logloss_value, "\n")
  cat("Recall:", recall, "\n")
  cat("Precision:", precision, "\n")
  cat("F1 Score:", f1_score, "\n")
  cat("Accuracy:", accuracy, "\n")
  
  # Plot ROC
  plot_roc_df <- data.frame(
    false_positive_rate = 1 - roc_curve$specificities,
    true_positive_rate = roc_curve$sensitivities
  )
  
  yr_st <- format(min(acc_metrics$date), "%Y")
  yr_end <- format(max(acc_metrics$date), "%Y")
  shws <- max(acc_metrics$n_shows)
  
  p <- ggplot(plot_roc_df, aes(x = false_positive_rate, y = true_positive_rate)) +
    geom_line(color = "blue", size = 1.25) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1.5) +
    labs(title = "WSP Setlist Model - AUC Curve",
         subtitle = paste0("Based on Testing Model Results of Concerts From ",yr_st,"-", yr_end," | Each Show Trained on Last ",shws," Concerts"),
         x = "False Positive Rate", y = "True Positive Rate") +
    annotate("text", x=0.90, y=0.1, label = paste0("AUC Score: ", round(auc_value, 3)),
             color = "black", size= 5) +
    theme_minimal()
  
  total_acc_metrics <- acc_metrics %>%
    group_by() %>%
    summarise(
      correct_a0_p0 = sum(correct_a0_p0),
      correct_a1_p1 = sum(correct_a1_p1),
      incorrect_a1_p0 = sum(incorrect_a1_p0),
      incorrect_a0_p1 = sum(incorrect_a0_p1),
      precision = correct_a1_p1 / (correct_a1_p1+incorrect_a0_p1),
      recall = correct_a1_p1 / (incorrect_a1_p0+correct_a1_p1),
      f1_score = 2 * precision * recall / (precision + recall)
    )
  
  print(total_acc_metrics)
  print(p)
}
evaluate_model()

# Task 3: "Apply" Model To Next Show Incorporating X Most Recent Shows
make_predictions <- function(data = sell_sell_table, target_var = 'played'){
  ## Pre-Processing ##
  
  # Split
  test_data <- data
  train_data <- model_table %>% filter(date < max(test_data$date) & show_index >= (sell_sell_table$show_index[[1]] - 400))
  
  # Keep Indicies
  song_index <- test_data$song_name
  date_index <- test_data$date[[1]]
  city_index <- test_data$city[[1]]
  
  # Features And Targets
  target <- "played"
  features <- names(train_data)[!names(train_data) %in% c("played", "city", "date", "venue_full", "song_name", "show_index")] #
  
  ## TRAIN ##
  
  # Train the Setlist Prediction xgboost Model
  xgb_model <- xgboost(data = as.matrix(train_data[, features]),
                       label = as.numeric(train_data[[target]]),
                       objective = "binary:logistic",
                       eval.metric = 'logloss',
                       max.depth=7,
                       nrounds = 250,
                       eta = 0.05,
                       verbose = 0)
  
  pred_vec <- predict(xgb_model, as.matrix(test_data[, features]))
  
  prediction_df_test <- data.frame(
    date = test_data$date,
    city = test_data$city,
    song_name = song_index,
    pred = pred_vec
  ) %>%
    arrange(desc(pred))
  
  print(paste0("Top 5 Song Predictions for ", date_index, " @ ", city_index))
  print(prediction_df_test %>% head(5))
  
  prediction_df_test <- prediction_df_test %>%
    left_join(data, by = c('date', 'city', 'song_name'))
  
  ## Train + Predict Opener
  
  
  return(prediction_df_test)
  
  
}
sell_sell <- make_predictions(data = manipulate_train(dim_future$date[[1]])) %>% arrange(desc(pred))

STONES <- make_predictions(data = manipulate_train("2024-06-20")) %>% arrange(desc(pred))
RRX_N1 <- make_predictions(data = manipulate_train("2024-06-21")) %>% arrange(desc(pred))
RRX_N2 <- make_predictions(data = manipulate_train("2024-06-22")) %>% arrange(desc(pred))
RRX_N3 <- make_predictions(data = manipulate_train("2024-06-23")) %>% arrange(desc(pred))


# Save Tables
all_song_predictions_df <- all_song_predictions_df %>% select(date, city, song_name, pred, actual, optimal_pred) %>%
  left_join(model_table, by = c('date', 'city', 'song_name'))

sell_sell_table %>%
  select(song_name, pct_shows_opener, n_shows_opener) %>%
  filter(n_shows_opener <= 1) %>%
  arrange(-pct_shows_opener) %>%
  head(10) %>%
  print.data.frame()

# Save - 2021-2024 Song Prediction
write_rds(all_song_predictions_df, "./Data/SongPredictions.rds")
write_csv(all_song_predictions_df, "./Data/SongPredictions.csv")

# Save - Accuracy Metrics
write_rds(acc_metrics, "./Data/AccuracyMetrics.rds")
write_csv(acc_metrics, "./Data/AccuracyMetrics.csv")

# Save - Single Show
write_csv(STONES, "./Data/Predictions/RRX_2024/N0_20240620.csv")
write_csv(RRX_N1, "./Data/Predictions/RRX_2024/N1_20240621.csv")
write_csv(RRX_N2, "./Data/Predictions/RRX_2024/N2_20240622.csv")
write_csv(RRX_N3, "./Data/Predictions/RRX_2024/N3_20240623.csv")
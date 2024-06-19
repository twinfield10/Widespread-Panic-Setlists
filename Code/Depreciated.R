## Optimal Thresholds
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
                                  combo_score = numeric())
  # Loop through possible threshold values
  max_t <- round(max(df$pred)-0.06, 2)
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


## Option 2: Build Large Dataset and Split Train/Test Normally ##
# Task 1: 
build_large_model <- function(data){
  
  # Features And Targets
  response_column = 'played'
  X <- names(data)[!names(data) %in% c("played", "city", "date", "venue_full", "song_name", "show_index")]
  y <- data[, response_column, drop = FALSE]
  y <- as.factor(as.character(y))
  
  # Split Data Into Training and Testing Sets
  set.seed(123)
  train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  # Features And Targets
  features <- names(train_data)[!names(train_data) %in% c("played", "city", "date", "venue_full", "song_name", "show_index")]
  target <- "played"
  
  ## TRAIN ##
  
  # Define the hyperparameter grid
  hyperparameters <- expand.grid(
    eta = c(0.5),
    nrounds = c(100),
    max_depth = c(3, 5, 7),
    gamma = c(0),
    colsample_bytree = c(0.85, 0.95),
    min_child_weight = c(1, 3, 5),
    subsample = c(1)
  )
  
  # Train the xgboost model
  xgb_model <- train(
    x = as.matrix(train_data[, features]),
    y = train_data[[target]],
    method = "xgbTree",
    trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
    tuneGrid = hyperparameters,
    metric = "logLoss",
    verbose = TRUE
  )
  
  best_hyperparameters <- xgb_model$bestTune
  
  best_hyperparameters$eta <- 0.01
  
  final_model <- xgboost(
    data = as.matrix(train_data[, features]),
    label = train_data[[target]],
    objective = "binary:logistic",
    nrounds = 10000,
    early_stopping_rounds = 100,
    eval_metric = "logloss",
    max_depth = best_hyperparameters$max_depth,
    min_child_weight = best_hyperparameters$min_child_weight,
    colsample_bytree = best_hyperparameters$colsample_bytree,
    gamma = best_hyperparameters$gamma,
    eta = best_hyperparameters$eta,
    verbose = 0
  )
  
  print(best_hyperparameters)
  
  ## PREDICT ##
  pred_probs <- predict(final_model, as.matrix(test_data[, features]))
  
  # Extract the response variable as a numeric vector
  y_true <- as.numeric(test_data[[response_column]])
  
  # Evaluate the model
  conf_matrix <- confusionMatrix(table(predicted = ifelse(pred_probs > 0.5, 1, 0), actual = y_true))
  
  # Precision, recall, and F1 score
  precision <- conf_matrix$byClass["Pos Pred Value"]
  recall <- conf_matrix$byClass["Sensitivity"]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # AUC
  roc_curve <- roc(y_true, pred_probs)
  auc_value <- pROC::auc(roc_curve)
  
  # Logloss
  logloss_value <- logLoss(y_true, as.numeric(pred_probs))
  
  # Print the results
  cat("Precision:", precision, "\n")
  cat("Recall:", recall, "\n")
  cat("F1 Score:", f1_score, "\n")
  cat("AUC:", auc_value, "\n")
  cat("LogLoss:", logloss_value, "\n")
}
build_large_model(data = model_table)
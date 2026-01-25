# model.R - XGBoost model training, persistence, and prediction
# Handles model lifecycle: train, save, load, predict

library(xgboost)
library(pROC)
library(caret)
library(Metrics)
library(performanceEstimation)

source('./Code/storage.R')

#' Train XGBoost model for setlist prediction
#'
#' @param input Training data frame
#' @param resample Apply SMOTE resampling for class balance
#' @param params XGBoost hyperparameters
#' @return Trained xgboost model
train_model <- function(input, resample = TRUE, params = NULL) {
  
  set.seed(87)
  
  # Default parameters
  if (is.null(params)) {
    params <- list(
      objective = "binary:logistic",
      eval_metric = "logloss",
      max_depth = 6,
      eta = 0.1
    )
  }
  
  # Separate target and features
  target <- 'played'
  id_cols <- c('song_name', 'show_index', 'city', 'date', 'venue_full')
  feature_cols <- setdiff(names(input), c(target, id_cols))
  
  # Remove infinite values
  input <- input %>% filter(!is.infinite(pct_runs_since_debut))
  
  # Apply SMOTE resampling if requested
  if (resample) {
    message("Applying SMOTE resampling for class balance...")
    input$played <- as.factor(input$played)
    input <- smote(played ~ ., input %>% select(all_of(feature_cols), played), 
                   perc.over = 2, k = 5, perc.under = 2)
  }
  
  # Build matrices
  X <- input %>% select(all_of(feature_cols)) %>% as.matrix()
  y <- as.numeric(as.character(input[[target]]))
  
  # Train/test split
  train_index <- sample(seq_len(nrow(input)), size = 0.8 * nrow(input))
  X_train <- X[train_index, ]
  X_test <- X[-train_index, ]
  y_train <- y[train_index]
  y_test <- y[-train_index]
  
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dtest <- xgb.DMatrix(data = X_test, label = y_test)
  
  # Baseline metrics
  baseline_prob <- mean(y_train)
  baseline_log_loss <- logLoss(y_test, rep(baseline_prob, length(y_test)))
  
  message("Training XGBoost model...")
  
  # Train
  model <- xgboost(
    data = dtrain,
    params = params,
    nrounds = 100,
    verbose = 0
  )
  
  # Evaluate
  y_pred <- predict(model, dtest)
  y_pred_class <- ifelse(y_pred > 0.5, 1, 0)
  
  conf_matrix <- confusionMatrix(as.factor(y_pred_class), as.factor(y_test))
  accuracy <- conf_matrix$overall["Accuracy"]
  auc_score <- auc(y_test, y_pred)
  model_logloss <- logLoss(y_test, y_pred)
  
  # Print metrics
  message("\n=== Model Performance ===")
  message(paste("Baseline Log Loss:", round(baseline_log_loss, 4)))
  message(paste("XGBoost Log Loss:", round(model_logloss, 4)))
  message(paste("Log Loss Improvement:", round(baseline_log_loss - model_logloss, 4)))
  message(paste("Accuracy:", round(accuracy, 4)))
  message(paste("AUC:", round(auc_score, 4)))
  message("========================\n")
  
  # Attach metadata
  attr(model, "trained_on") <- Sys.Date()
  attr(model, "feature_cols") <- feature_cols
  attr(model, "resample") <- resample
  attr(model, "metrics") <- list(
    accuracy = accuracy,
    auc = auc_score,
    logloss = model_logloss
  )
  
  return(model)
}

#' Save trained model to local storage and S3
#'
#' @param model Trained xgboost model
#' @param model_name Model identifier (default: timestamped)
#' @return TRUE if successful
save_model <- function(model, model_name = NULL) {
  
  if (is.null(model_name)) {
    model_name <- paste0("wsp_setlist_model_", format(Sys.Date(), "%Y%m%d"))
  }
  
  filename <- paste0(model_name, ".rds")
  
  message(paste0("Saving model: ", filename))
  
  # Save locally and to S3
  success <- save_rds_with_s3(
    data = model,
    filename = filename,
    local_dir = "./models/",
    s3_dir = "models/"
  )
  
  if (success) {
    message("✓ Model saved successfully")
  } else {
    warning("✗ Model save failed")
  }
  
  return(success)
}

#' Load trained model from local storage or S3
#'
#' @param model_name Model identifier
#' @return Trained xgboost model or NULL
load_model <- function(model_name = "latest") {
  
  if (model_name == "latest") {
    # Find most recent model
    local_models <- list.files("./models/", pattern = "wsp_setlist_model_.*\\.rds$", full.names = TRUE)
    
    if (length(local_models) > 0) {
      latest_model <- local_models[which.max(file.mtime(local_models))]
      filename <- basename(latest_model)
      message(paste0("Loading latest model: ", filename))
    } else {
      # Try S3
      message("No local models found, attempting S3...")
      # This would require listing S3 objects - simplified for now
      stop("No models found. Train a model first.")
    }
  } else {
    filename <- paste0(model_name, ".rds")
  }
  
  # Load from local or S3
  model <- load_rds_with_s3(
    filename = filename,
    local_dir = "./models/",
    s3_dir = "models/"
  )
  
  if (!is.null(model)) {
    message("✓ Model loaded successfully")
    
    # Print metadata if available
    if (!is.null(attr(model, "trained_on"))) {
      message(paste("  Trained on:", attr(model, "trained_on")))
    }
    if (!is.null(attr(model, "metrics"))) {
      metrics <- attr(model, "metrics")
      message(paste("  Accuracy:", round(metrics$accuracy, 4)))
      message(paste("  AUC:", round(metrics$auc, 4)))
    }
  }
  
  return(model)
}

#' Apply trained model to generate predictions
#'
#' @param model Trained xgboost model
#' @param input Feature data frame
#' @return Input data frame with 'pred' column added
apply_model <- function(model, input) {
  
  target <- 'played'
  id_cols <- c('song_name', 'show_index', 'city', 'date', 'venue_full', 'ftp')
  
  # Get feature columns from model metadata
  if (!is.null(attr(model, "feature_cols"))) {
    feature_cols <- attr(model, "feature_cols")
  } else {
    # Fallback: infer from input
    feature_cols <- setdiff(names(input), c(target, id_cols))
  }
  
  # Ensure all required features are present
  missing_features <- setdiff(feature_cols, names(input))
  if (length(missing_features) > 0) {
    stop(paste("Missing features:", paste(missing_features, collapse = ", ")))
  }
  
  # Build feature matrix
  new_features <- input %>%
    select(all_of(feature_cols)) %>%
    as.matrix()
  
  new_dmatrix <- xgb.DMatrix(data = new_features)
  
  # Generate predictions
  new_predictions <- predict(model, new_dmatrix)
  input$pred <- new_predictions
  
  message(paste0("✓ Predictions generated for ", nrow(input), " songs"))
  
  return(input %>% arrange(-pred))
}

#' Get or train model with caching
#'
#' @param train_data Training data (if model needs to be trained)
#' @param force_retrain Force retraining even if cached model exists
#' @param resample Apply SMOTE resampling
#' @return Trained model
get_or_train_model <- function(train_data = NULL, force_retrain = FALSE, resample = TRUE) {
  
  if (!force_retrain) {
    # Try to load cached model
    tryCatch({
      model <- load_model("latest")
      if (!is.null(model)) {
        message("Using cached model")
        return(model)
      }
    }, error = function(e) {
      message("No cached model found, training new model...")
    })
  }
  
  # Train new model
  if (is.null(train_data)) {
    stop("train_data required to train a new model")
  }
  
  message("Training new model...")
  model <- train_model(train_data, resample = resample)
  
  # Save for future use
  save_model(model)
  
  return(model)
}

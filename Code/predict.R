# predict.R - High-level prediction pipeline
# Orchestrates feature engineering, model loading/training, and prediction generation

library(tidyverse)

source('./Code/features.R')
source('./Code/model.R')
source('./Code/data_pipeline.R')

#' Generate predictions for a future show
#'
#' @param show_date Date of show to predict
#' @param data_list Pre-loaded data (optional, will load if NULL)
#' @param model Pre-trained model (optional, will load/train if NULL)
#' @param force_retrain Force model retraining
#' @return Data frame with predictions
predict_show <- function(show_date, data_list = NULL, model = NULL, force_retrain = FALSE) {
  
  # Load data if not provided
  if (is.null(data_list)) {
    message("Loading setlist data...")
    
    # Try to load from local/S3
    dim_all <- load_rds_with_s3('WSP_Show_Dim_Table_1986_to_2026.rds', local_dir = "./Data/")
    fact_song <- load_rds_with_s3('WSP_Song_Fact_Table_1986_to_2026.rds', local_dir = "./Data/")
    
    if (is.null(dim_all) || is.null(fact_song)) {
      stop("Could not load data. Run load_all_data() first.")
    }
    
    # Prep for modeling
    data_list <- prep_model_input(dim_all, fact_song)
  }
  
  # Load or train model
  if (is.null(model)) {
    if (!force_retrain) {
      message("Loading model...")
      tryCatch({
        model <- load_model("latest")
      }, error = function(e) {
        message("No model found. Set force_retrain=TRUE to train a new model.")
        stop(e)
      })
    } else {
      message("Training new model...")
      # Create training set from last 100 shows
      train_data <- create_train_set(
        end_date = max(data_list$model_data$date),
        train_n = 100,
        model_data = data_list$model_data,
        dim_future = data_list$dim_future
      )
      model <- train_model(train_data, resample = TRUE)
      save_model(model)
    }
  }
  
  # Generate features for the show
  message(paste0("Generating features for ", show_date, "..."))
  features <- create_features_for_show(
    test_date = show_date,
    model_data = data_list$model_data,
    dim_future = data_list$dim_future
  )
  
  # Apply model
  predictions <- apply_model(model, features)
  
  return(predictions)
}

#' Generate predictions for multiple shows
#'
#' @param show_dates Vector of show dates
#' @param data_list Pre-loaded data (optional)
#' @param model Pre-trained model (optional)
#' @return List of prediction data frames
predict_multiple_shows <- function(show_dates, data_list = NULL, model = NULL) {
  
  # Load data once
  if (is.null(data_list)) {
    message("Loading setlist data...")
    dim_all <- load_rds_with_s3('WSP_Show_Dim_Table_1986_to_2026.rds', local_dir = "./Data/")
    fact_song <- load_rds_with_s3('WSP_Song_Fact_Table_1986_to_2026.rds', local_dir = "./Data/")
    
    if (is.null(dim_all) || is.null(fact_song)) {
      stop("Could not load data. Run load_all_data() first.")
    }
    
    data_list <- prep_model_input(dim_all, fact_song)
  }
  
  # Load model once
  if (is.null(model)) {
    message("Loading model...")
    model <- load_model("latest")
  }
  
  # Generate predictions for each show
  predictions_list <- map(show_dates, function(date) {
    message(paste0("\nPredicting for ", date, "..."))
    predict_show(date, data_list = data_list, model = model)
  })
  
  names(predictions_list) <- show_dates
  
  return(predictions_list)
}

#' Quick prediction for next upcoming show
#'
#' @param model Pre-trained model (optional)
#' @return Predictions for next show
predict_next_show <- function(model = NULL) {
  
  # Load future shows
  dim_all <- load_rds_with_s3('WSP_Show_Dim_Table_1986_to_2026.rds', local_dir = "./Data/")
  
  future_shows <- dim_all %>% 
    filter(is_fut == 1, date >= Sys.Date()) %>%
    arrange(date)
  
  if (nrow(future_shows) == 0) {
    stop("No upcoming shows found in the database")
  }
  
  next_show_date <- future_shows$date[1]
  next_show_venue <- future_shows$venue_full[1]
  
  message(paste0("Predicting for next show: ", next_show_date, " @ ", next_show_venue))
  
  predictions <- predict_show(next_show_date, model = model, force_retrain = FALSE)
  
  return(predictions)
}

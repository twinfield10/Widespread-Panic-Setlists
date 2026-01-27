# run_train_model.R - Train setlist prediction model
# Usage: Rscript run_train_model.R [train_n]
#
# Arguments:
#   train_n - Number of recent shows to use for training (default: 100)
#
# Example:
#   Rscript run_train_model.R        # Train on last 100 shows
#   Rscript run_train_model.R 150    # Train on last 150 shows

library(tidyverse)

source('./Code/features.R')
source('./Code/model.R')
source('./Code/storage.R')

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
train_n <- if (length(args) >= 1) as.integer(args[1]) else 100

message("============================================")
message("   WSP Setlist Prediction Model Training")
message("============================================\n")

# Step 1: Load data
message("=== Step 1: Loading Data ===")
dim_all <- load_rds_with_s3('WSP_Show_Dim_Table_1986_to_2026.rds', local_dir = "./Data/")
fact_song <- load_rds_with_s3('WSP_Song_Fact_Table_1986_to_2026.rds', local_dir = "./Data/")

if (is.null(dim_all) || is.null(fact_song)) {
  stop("Could not load data. Run run_full_scrape.R first.")
}

message(paste0("  Shows loaded: ", nrow(dim_all)))
message(paste0("  Songs loaded: ", nrow(fact_song)))

# Step 2: Prep model input
message("\n=== Step 2: Preparing Model Input ===")
data_list <- prep_model_input(dim_all, fact_song)

message(paste0("  Historical shows: ", n_distinct(data_list$model_data$show_index)))
message(paste0("  Unique songs: ", n_distinct(data_list$model_data$song_name)))

# Step 3: Create training set
message("\n=== Step 3: Creating Training Set ===")
message(paste0("  Training on last ", train_n, " shows..."))

end_date <- max(data_list$model_data$date)
train_data <- create_train_set(
  end_date = end_date,
  train_n = train_n,
  model_data = data_list$model_data,
  dim_future = data_list$dim_future
)

message(paste0("  Training rows: ", nrow(train_data)))
message(paste0("  Positive samples (played=1): ", sum(train_data$played == 1)))
message(paste0("  Negative samples (played=0): ", sum(train_data$played == 0)))

# Step 4: Train model
message("\n=== Step 4: Training Model ===")
model <- train_model(train_data, resample = TRUE)

# Step 5: Save model
message("\n=== Step 5: Saving Model ===")
save_model(model)

message("\n============================================")
message("           Training Complete!")
message("============================================")
message("\nTo generate predictions, run in R:")
message("  source('./Code/predict.R')")
message("  preds <- predict_next_show()")
message("  head(preds %>% select(song_name, pred), 20)")

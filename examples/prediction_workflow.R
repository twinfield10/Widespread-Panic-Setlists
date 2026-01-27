# prediction_workflow.R - Example workflow using modular pipeline
# Demonstrates Phase 3 modular design for setlist prediction

library(tidyverse)

# Load modular pipeline
#source('./Code/data_pipeline.R')
source('./Code/predict.R')


###############################################################################
# WORKFLOW 1: Quick Prediction for Next Show
###############################################################################

# Simplest way - predict next upcoming show using cached model
predictions <- predict_next_show()

# View top predictions
predictions %>% 
  select(song_name, pred, ltp, pct_shows_all_time) %>%
  head(20)

###############################################################################
# WORKFLOW 2: Predict Specific Show
###############################################################################

# Predict Red Rocks N1
rrx_n1 <- predict_show("2025-06-27")

# View predictions
rrx_n1 %>%
  arrange(-pred) %>%
  select(song_name, pred, ltp, pct_shows_same_venue) %>%
  head(25)

###############################################################################
# WORKFLOW 3: Predict Full Run (Multiple Shows)
###############################################################################

# Red Rocks run (3 nights)
rrx_dates <- c("2025-06-27", "2025-06-28", "2025-06-29")

rrx_run <- predict_multiple_shows(rrx_dates)

# View predictions for each night
rrx_run$`2025-06-27` %>% arrange(-pred) %>% select(song_name, pred) %>% head(10)
rrx_run$`2025-06-28` %>% arrange(-pred) %>% select(song_name, pred) %>% head(10)
rrx_run$`2025-06-29` %>% arrange(-pred) %>% select(song_name, pred) %>% head(10)

# Calculate mean prediction across run
run_summary <- rrx_run$`2025-06-27` %>%
  select(song_name) %>%
  mutate(
    N1 = rrx_run$`2025-06-27`$pred,
    N2 = rrx_run$`2025-06-28`$pred,
    N3 = rrx_run$`2025-06-29`$pred,
    Mean = (N1 + N2 + N3) / 3
  ) %>%
  arrange(-Mean)

# Top songs for the run overall
run_summary %>% head(20)

###############################################################################
# WORKFLOW 4: Update Data and Retrain Model
###############################################################################

# Update with latest shows
data_list <- update_all_data()

# Save updated data
save_setlists(
  data_list$songs,
  rbind(data_list$dim_historical, data_list$dim_future)
)

# Retrain model with updated data
# NOTE: Requires full implementation of create_train_set in features.R
# Placeholder:
# source('./Code/features.R')
# source('./Code/model.R')
# 
# model_input <- prep_model_input(
#   rbind(data_list$dim_historical, data_list$dim_future),
#   data_list$songs
# )
# 
# train_data <- create_train_set(
#   end_date = max(data_list$dim_historical$date),
#   train_n = 500,
#   model_data = model_input$model_data,
#   dim_future = model_input$dim_future
# )
# 
# new_model <- train_model(train_data, resample = TRUE)
# save_model(new_model)

###############################################################################
# WORKFLOW 5: Compare Model Predictions
###############################################################################

# Load different model versions
# current_model <- load_model("wsp_setlist_model_20260125")
# previous_model <- load_model("wsp_setlist_model_20260110")

# Generate predictions with both
# current_preds <- predict_show("2025-06-27", model = current_model)
# previous_preds <- predict_show("2025-06-27", model = previous_model)

# Compare
# comparison <- current_preds %>%
#   select(song_name, current_pred = pred) %>%
#   left_join(
#     previous_preds %>% select(song_name, previous_pred = pred),
#     by = "song_name"
#   ) %>%
#   mutate(delta = current_pred - previous_pred) %>%
#   arrange(-abs(delta))

###############################################################################
# Notes:
# - All functions use caching to avoid re-scraping or re-training
# - Models are automatically saved to S3 for team collaboration
# - Data is loaded from S3 if not available locally
# - Feature engineering happens on-demand for each prediction
###############################################################################

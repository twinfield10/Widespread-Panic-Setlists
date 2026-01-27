# Widespread Panic Setlist Predictor

A machine learning pipeline for predicting Widespread Panic setlists using historical concert data from 1986-present.

## Overview

This project scrapes setlist data from Everyday Companion, engineers 100+ features per song, and trains an XGBoost model to predict which songs are likely to be played at upcoming shows. The model achieves ~92% accuracy and 0.98 AUC by learning patterns in song rotation, venue history, and temporal trends.

## Quick Start

### 1. Install Dependencies

```r
install.packages(c(
  "tidyverse", "xgboost", "pROC", "caret",
  "Metrics", "performanceEstimation", "rvest",
  "httr", "aws.s3", "lubridate"
))
```

For web scraping, you'll also need Playwright:
```bash
npm install playwright
npx playwright install chromium
```

### 2. Scrape Data (or use existing)

```bash
# Full scrape from 1986 to current year
Rscript run_full_scrape.R

# Or specify year range
Rscript run_full_scrape.R 2020 2026
```

### 3. Train Model

```bash
# Train on last 100 shows (default)
Rscript run_train_model.R

# Or customize training window
Rscript run_train_model.R 150
```

### 4. Generate Predictions

```r
source('./Code/predict.R')

# Predict next upcoming show
preds <- predict_next_show()

# Predict specific show
preds <- predict_show("2026-02-14")

# Predict full run
preds <- predict_multiple_shows(c("2026-02-14", "2026-02-15", "2026-02-16"))

# View top predictions
preds %>%
  select(song_name, pred, ltp, pct_shows_all_time) %>%
  head(20)
```

## Project Structure

```
.
├── Code/
│   ├── features.R          # Feature engineering (100+ features per song)
│   ├── model.R             # XGBoost training, saving, loading, prediction
│   ├── predict.R           # High-level prediction pipeline
│   ├── data_pipeline.R     # Data loading and update workflows
│   ├── clean.R             # Data cleaning and normalization
│   ├── storage.R           # Local and S3 persistence
│   ├── scrape.R            # HTTP-based scraping (deprecated)
│   └── scrape_playwright.R # Playwright-based scraping (current)
├── Data/
│   ├── WSP_Show_Dim_Table_1986_to_2026.rds   # Show metadata
│   └── WSP_Song_Fact_Table_1986_to_2026.rds  # Setlist data
├── models/
│   └── wsp_setlist_model_YYYYMMDD.rds        # Trained models
├── examples/
│   └── prediction_workflow.R                  # Example usage
├── run_full_scrape.R       # Data scraping script
└── run_train_model.R       # Model training script
```

## Data Schema

### Show Dimension Table
| Column | Description |
|--------|-------------|
| `link` | Unique show URL identifier |
| `date` | Show date |
| `state`, `city`, `venue_name` | Location data |
| `run_index`, `show_in_run` | Run identification |
| `is_fut` | Future show flag (0=historical, 1=future) |

### Song Fact Table
| Column | Description |
|--------|-------------|
| `link` | Reference to show |
| `set` | Set number (1, 2, 99=encore) |
| `song_index` | Position in setlist |
| `song_name` | Song title |
| `into` | Segue indicator |

## Feature Engineering

The model uses 100+ features per song, including:

**Temporal Features**
- Time windows: last 6 months, 1 year, 2 years, 4 years, 10 years
- Guitarist era: Mikey years (pre-2002) vs Jimmy years (2006+)
- Day of week, position in run

**Frequency Features**
- `n_shows_*`: Count of shows where song was played
- `pct_shows_*`: Percentage of shows in category
- Run-based equivalents for all metrics

**Last Time Played (LTP) Features**
- `ltp` through `ltp_10`: Shows since last 1-10 plays
- `avg_ltp`, `recent_avg_ltp`: Average rotation
- `overdue_metric`: How overdue a song is vs its average

**Location Features**
- Same venue, city, state history
- Venue-specific play percentages

**Composite Scores**
- `raw_score`: Weighted frequency / LTP deviation
- Various difference features (recent vs historical trends)

## Model Details

- **Algorithm**: XGBoost (binary classification)
- **Resampling**: SMOTE for class imbalance
- **Train/Test Split**: 80/20 random
- **Hyperparameters**: max_depth=6, eta=0.1, nrounds=100

**Typical Performance**:
- Accuracy: ~92%
- AUC: ~0.98
- Log Loss: ~0.20 (vs 0.68 baseline)

## Configuration

### Environment Variables

For S3 storage (optional):
```bash
export AWS_ACCESS_KEY_ID="your-key"
export AWS_SECRET_ACCESS_KEY="your-secret"
export AWS_S3_BUCKET="wsmfp-setlists"
```

### Customizing Training

Edit `run_train_model.R` or pass command-line args:

```bash
# Train on more historical data
Rscript run_train_model.R 200

# Train on fewer (more recent) shows
Rscript run_train_model.R 50
```

### Customizing Features

Edit `Code/features.R` to add or modify features. Key functions:
- `create_features_for_show()`: Main feature generation
- `create_train_set()`: Builds training data from historical shows

## Workflows

### Update After New Shows

```r
source('./Code/data_pipeline.R')

# Scrape and append new shows
data_list <- update_all_data()

# Save updated data
save_setlists(data_list$songs,
              rbind(data_list$dim_historical, data_list$dim_future))
```

Then retrain:
```bash
Rscript run_train_model.R
```

### Compare Model Versions

```r
source('./Code/predict.R')

# Load different model versions
model_v1 <- load_model("wsp_setlist_model_20260115")
model_v2 <- load_model("wsp_setlist_model_20260127")

# Compare predictions
preds_v1 <- predict_show("2026-02-14", model = model_v1)
preds_v2 <- predict_show("2026-02-14", model = model_v2)
```

## Data Sources

- **Everyday Companion**: https://everydaycompanion.com (primary source)
- Historical data spans 1986-present (~3,100+ shows)

## License

MIT

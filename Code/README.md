# Code Directory

This directory contains modular R scripts for scraping, cleaning, and analyzing Widespread Panic setlist data.

## Module Overview

### Core Pipeline Modules (Phase 2 Refactor)

#### `scrape.R`
Web scraping functions with error handling and retry logic.

**Key Functions:**
- `safe_fetch_html()` - Fetch HTML with automatic retry on failure
- `fetch_html_cached()` - Fetch with local HTML caching to avoid re-scraping
- `scrape_year_tour()` - Scrape tour dates for a single year
- `scrape_setlist()` - Scrape setlist details for a single show
- `clear_cache()` - Clear cached HTML files

**Features:**
- Automatic retry with exponential backoff
- User agent rotation to avoid blocking
- Local HTML caching for faster re-runs
- Comprehensive error handling

#### `clean.R`
Data cleaning and validation functions.

**Key Functions:**
- `clean_tour_data()` - Standardize tour date formats and venue names
- `apply_tour_corrections()` - Apply known edge cases and URL fixes
- `apply_venue_corrections()` - Standardize venue and city names
- `add_run_indexing()` - Calculate run_index and show_in_run
- `flag_radio_shows()` - Identify radio/studio performances
- `clean_song_data()` - Normalize song data
- `extract_show_metadata()` - Extract show-level metadata from songs

**Features:**
- Centralized venue/city name corrections
- Edge case handling for unusual date formats
- Consistent data normalization

#### `storage.R`
AWS S3 integration for data persistence.

**Key Functions:**
- `save_rds_with_s3()` - Save locally + auto-backup to S3
- `load_rds_with_s3()` - Load from local, fallback to S3
- `save_csv_with_s3()` - CSV version
- `upload_to_s3()` / `download_from_s3()` - Direct S3 operations

**Features:**
- Automatic S3 backup on save
- Local cache with S3 fallback
- Environment variable-based credentials

#### `data_pipeline.R`
High-level data loading pipeline using modular functions.

**Key Functions:**
- `load_all_data()` - Load all historical data from scratch
- `update_all_data()` - Incremental update with latest shows
- `save_setlists()` - Save to local + S3

**Usage:**
```r
source('./Code/data_pipeline.R')

# Load all data (uses cache by default)
data_list <- load_all_data(start_year = 1986, end_year = 2024)

# OR update with latest shows only
data_list <- update_all_data()

# Save to local + S3
save_setlists(data_list$songs, 
              rbind(data_list$dim_historical, data_list$dim_future))
```

### Legacy Scripts (Pre-Refactor)

#### `SetlistProcessing.R`
Original monolithic script (now deprecated). Use `data_pipeline.R` instead for new work.

#### `Build Model.R`
XGBoost model training and prediction generation.

#### `Plot Model Stats.R`
Visualization and metrics plotting.

#### `SongCatelogAnalysis.R`
Song-level statistical analysis.

## File Organization

```
Code/
├── scrape.R           # Web scraping (modular, Phase 2)
├── clean.R            # Data cleaning (modular, Phase 2)
├── storage.R          # S3 integration (Phase 1)
├── data_pipeline.R    # High-level pipeline (Phase 2)
├── SetlistProcessing.R  # Legacy (deprecated)
├── Build Model.R      # Model training
├── Plot Model Stats.R # Visualization
└── SongCatelogAnalysis.R  # Song analysis
```

## Cache Management

HTML scraping cache is stored in `./cache/html/` and ignored by git. To clear cache:

```r
source('./Code/scrape.R')
clear_cache()
```

## Error Handling

All scraping functions include:
- Try-catch blocks for network errors
- Automatic retry with delays
- NULL return on failure (graceful degradation)
- Warning messages for debugging

Failed scrapes are logged but don't crash the pipeline.

## Performance

**Caching Benefits:**
- First run: ~5-10 minutes (scrapes web)
- Subsequent runs: ~30-60 seconds (uses cached HTML)
- Update runs: ~10-30 seconds (only new shows)

**S3 Benefits:**
- Team members can download pre-processed data
- No need to re-scrape when switching machines
- Automatic backup of all data files

### Modeling Modules (Phase 3 Refactor)

#### `features.R`
Feature engineering for setlist prediction.

**Key Functions:**
- `prep_model_input()` - Prepare show/song data for modeling
- `create_features_for_show()` - Engineer features for a single show prediction
- `create_train_set()` - Build training dataset from historical shows

**Features:**
- Song frequency metrics (all-time, by era, by location, by day)
- Last-time-played (LTP) calculations
- Run-based metrics
- Overdue scores

#### `model.R`
XGBoost model lifecycle management.

**Key Functions:**
- `train_model()` - Train XGBoost with optional SMOTE resampling
- `save_model()` - Save to local + S3 with metadata
- `load_model()` - Load from local or S3 fallback
- `apply_model()` - Generate predictions
- `get_or_train_model()` - Smart caching (load if exists, train if needed)

**Features:**
- Model versioning with timestamps
- Metadata tracking (trained_on, metrics, feature_cols)
- Automatic S3 backup
- Performance metrics (accuracy, AUC, log loss)

#### `predict.R`
High-level prediction orchestration.

**Key Functions:**
- `predict_show()` - Generate predictions for a single show
- `predict_multiple_shows()` - Batch predictions
- `predict_next_show()` - Quick prediction for next upcoming show

**Usage:**
```r
source('./Code/predict.R')

# Predict next upcoming show
predictions <- predict_next_show()

# Predict specific show
predictions <- predict_show("2025-06-27")  # Red Rocks N1

# Batch predict multiple shows
preds <- predict_multiple_shows(c("2025-06-27", "2025-06-28", "2025-06-29"))
```

**Smart Caching:**
- Loads trained model from cache (local or S3)
- Reuses data across multiple predictions
- Only retrain when forced or no cached model exists

## Phase 4 Plans

- GitHub Actions for automated data updates
- Scheduled model retraining
- Automated prediction generation for upcoming shows

# Widespread Panic Setlist Predictions

Machine learning model to predict setlists for Widespread Panic concerts based on historical data (1986-2025).

## Features

- Web scraping of setlist data from [Everyday Companion](http://everydaycompanion.com/)
- Data storage with local caching and AWS S3 backup
- XGBoost model for song prediction
- Prediction tables with venue, day-of-week, and last-time-played statistics

## Setup

### Prerequisites

- R (≥ 4.0)
- AWS account with S3 access
- Required R packages:
  ```r
  install.packages(c("tidyverse", "rvest", "lubridate", "scales", "purrr",
                     "xgboost", "pROC", "caret", "Metrics", "performanceEstimation",
                     "ggplot2", "gt", "paletteer", "aws.s3"))
  ```

### AWS Configuration

1. Follow instructions in [`CREDENTIALS_SETUP.md`](CREDENTIALS_SETUP.md) to configure AWS credentials
2. Ensure you have access to the S3 bucket configured in your `.Renviron` file

## Usage

### Load Data

```r
# Source the main script
source('./Code/SetlistProcessing.R')

# Load all historical data (scrapes from web, takes ~5-10 minutes)
data_list <- load_all_data(start = 1986, end = 2024)

# OR update with latest shows only
data_list <- update_all_data()
```

Data is automatically saved locally (`./Data/`) and backed up to S3.

### Generate Predictions

```r
# Source model building script
source('./Code/Build Model.R')

# Model is trained on historical shows
# Predictions are generated for upcoming concerts

# View predictions
build_next_show_table(data = AC_N1, n_preds = 25)
```

## Project Structure

```
.
├── Code/
│   ├── SetlistProcessing.R    # Data scraping and cleaning
│   ├── storage.R               # S3 integration utilities
│   ├── Build Model.R           # XGBoost model training
│   ├── Plot Model Stats.R      # Visualization
│   └── SongCatelogAnalysis.R   # Song-level analysis
├── Data/                       # Local data cache (RDS files)
├── CREDENTIALS_SETUP.md        # AWS setup guide
└── README.md
```

## Security

- **Never commit credentials to git!**
- AWS credentials are loaded from `.Renviron` (ignored by git)
- See [`CREDENTIALS_SETUP.md`](CREDENTIALS_SETUP.md) for details

## Data Sources

All setlist data is scraped from [Everyday Companion](http://everydaycompanion.com/), the definitive source for Widespread Panic setlists.

## License

MIT

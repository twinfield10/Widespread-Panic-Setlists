# Scrape > Clean > Save Workflow

Complete workflow for scraping WSP setlist data using Playwright, cleaning it, and saving to local + AWS S3.

## Prerequisites

### Install Playwright
```bash
pip3 install playwright
python3 -m playwright install chromium
```

### Configure AWS Credentials
See `CREDENTIALS_SETUP.md` for details. Ensure `.Renviron` contains:
```
AWS_ACCESS_KEY_ID=your_access_key
AWS_SECRET_ACCESS_KEY=your_secret_key
AWS_DEFAULT_REGION=us-east-2
AWS_S3_BUCKET=wsmfp-setlists
```

## Quick Start

### Full Scrape (All Years)
```bash
Rscript run_full_scrape.R
```

### Specific Year Range
```bash
Rscript run_full_scrape.R 2020 2024
```

### Test Playwright Scraper
```bash
Rscript test_playwright.R
```

## Workflow Steps

The complete workflow (`run_full_scrape.R`) performs:

### 1. **Scrape & Clean**
- Uses Playwright to fetch HTML (human-like behavior)
- Caches HTML locally (`cache/html/`)  
- Parses tour dates and setlists
- Cleans venue names, dates, and song data
- Applies known corrections and edge cases

### 2. **Save Local + S3**
- Saves `.rds` files to `./Data/`
- Automatically uploads to S3 bucket
- Files:
  - `WSP_Song_Fact_Table_YYYY_to_YYYY.rds`
  - `WSP_Show_Dim_Table_YYYY_to_YYYY.rds`

### 3. **Verify**
- Checks S3 bucket for uploaded files
- Prints summary statistics
- Reports any errors

## Caching

**First Run:**
- Scrapes ~3000 shows from web
- Takes 5-10 minutes
- Caches all HTML to `cache/html/`

**Subsequent Runs:**
- Loads from cache
- Takes 30-60 seconds
- Only scrapes new shows

**Clear Cache:**
```r
source('./Code/scrape.R')
clear_cache()
```

## Output

### Local Files
```
Data/
├── WSP_Song_Fact_Table_1986_to_2025.rds
└── WSP_Show_Dim_Table_1986_to_2025.rds
```

### S3 Bucket
```
s3://wsmfp-setlists/
├── WSP_Song_Fact_Table_1986_to_2025.rds
└── WSP_Show_Dim_Table_1986_to_2025.rds
```

## Modules

The workflow uses these modular components:

- **`Code/fetch_page.py`** - Playwright scraper (Python)
- **`Code/scrape.R`** - Scraping functions with caching
- **`Code/clean.R`** - Data cleaning and normalization
- **`Code/storage.R`** - S3 upload/download
- **`Code/data_pipeline.R`** - High-level orchestration

## Error Handling

**Network Failures:**
- Automatic retry with exponential backoff (3 attempts)
- Falls back to NULL (doesn't crash pipeline)
- Logged warnings for debugging

**AWS Errors:**
- Workflow continues even if S3 upload fails
- Local files always saved
- Error messages indicate credential issues

## Example Output

```
========================================
WSP Setlist Scrape Workflow
========================================
Date: 2026-01-25
Time: 14:30:00
Years: 1986 - 2025
Using: Playwright scraper
========================================

STEP 1: Scraping and cleaning data...
✓ Data loaded successfully
  Songs: 105,234
  Historical shows: 3,156
  Future shows: 28

STEP 2: Saving to local storage and AWS S3...
✓ Saved locally: ./Data/WSP_Song_Fact_Table_1986_to_2025.rds
✓ Uploaded to s3://wsmfp-setlists/WSP_Song_Fact_Table_1986_to_2025.rds
✓ Saved locally: ./Data/WSP_Show_Dim_Table_1986_to_2025.rds
✓ Uploaded to s3://wsmfp-setlists/WSP_Show_Dim_Table_1986_to_2025.rds
✓ Data saved successfully

STEP 3: Verifying S3 upload...
✓ S3 upload verified

Files in S3:
  2026-01-25 14:30:45 WSP_Song_Fact_Table_1986_to_2025.rds
  2026-01-25 14:30:48 WSP_Show_Dim_Table_1986_to_2025.rds

========================================
WORKFLOW COMPLETE
========================================

Summary:
  Total songs scraped: 105,234
  Historical shows: 3,156
  Future shows: 28
  Year range: 1986-2025

Local files:
  ./Data/WSP_Song_Fact_Table_1986_to_2025.rds
  ./Data/WSP_Show_Dim_Table_1986_to_2025.rds

S3 backup:
  s3://wsmfp-setlists/

✓ Ready for modeling and predictions
========================================
```

## Troubleshooting

### Playwright Not Found
```bash
pip3 install playwright
python3 -m playwright install chromium
```

### AWS Permission Denied
- Check `.Renviron` file exists
- Verify credentials are correct
- Ensure region is `us-east-2`

### Scrape Failures
- Check internet connection
- Clear cache and retry
- Check `everydaycompanion.com` is accessible

## Next Steps

After scraping, you can:
1. **Train model**: See `examples/prediction_workflow.R`
2. **Generate predictions**: `source('./Code/predict.R')`
3. **Update with latest shows**: Use `update_all_data()`

#!/usr/bin/env Rscript
# run_full_scrape.R - Complete scrape > clean > save workflow with Playwright
# Usage: Rscript run_full_scrape.R [start_year] [end_year]

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

# Source modules
source('./Code/data_pipeline.R')
source('./Code/storage.R')

# Parse command line args
args <- commandArgs(trailingOnly = TRUE)
start_year <- if (length(args) >= 1) as.numeric(args[1]) else 1986
end_year <- if (length(args) >= 2) as.numeric(args[2]) else as.numeric(format(Sys.Date(), "%Y"))

###############################################################################
# FULL SCRAPE WORKFLOW
###############################################################################

message("\n========================================")
message("WSP Setlist Scrape Workflow")
message("========================================")
message(paste0("Date: ", Sys.Date()))
message(paste0("Time: ", format(Sys.time(), "%H:%M:%S")))
message(paste0("Years: ", start_year, " - ", end_year))
message(paste0("Using: Playwright scraper"))
message("========================================\n")

###############################################################################
# STEP 1: SCRAPE & CLEAN DATA
###############################################################################

message("STEP 1: Scraping and cleaning data...")
message("This may take 5-10 minutes on first run")
message("Subsequent runs will use cached HTML (30-60 seconds)\n")

tryCatch({
  data_list <- load_all_data(
    start_year = start_year,
    end_year = end_year,
    use_cache = TRUE  # Uses cached HTML when available
  )
  
  message("\n✓ Data loaded successfully")
  message(paste0("  Songs: ", nrow(data_list$songs)))
  message(paste0("  Historical shows: ", nrow(data_list$dim_historical)))
  message(paste0("  Future shows: ", nrow(data_list$dim_future)))
  
}, error = function(e) {
  message("\n✗ ERROR in data loading:")
  message(paste0("  ", e$message))
  quit(status = 1)
})

###############################################################################
# STEP 2: SAVE TO LOCAL + S3
###############################################################################

message("\nSTEP 2: Saving to local storage and AWS S3...")

tryCatch({
  # Combine historical and future for full dimension table
  dim_all <- rbind(data_list$dim_historical, data_list$dim_future)
  
  # Save with S3 backup
  save_setlists(
    song_df = data_list$songs,
    all_df = dim_all
  )
  
  message("\n✓ Data saved successfully")
  
}, error = function(e) {
  message("\n✗ ERROR in saving:")
  message(paste0("  ", e$message))
  message("\nNote: Data was scraped but not saved. Check AWS credentials.")
  quit(status = 1)
})

###############################################################################
# STEP 3: VERIFY S3 UPLOAD
###############################################################################

message("\nSTEP 3: Verifying S3 upload...")

tryCatch({
  # List S3 bucket contents
  bucket <- Sys.getenv("AWS_S3_BUCKET", unset = "wsmfp-setlists")
  region <- Sys.getenv("AWS_DEFAULT_REGION", unset = "us-east-2")
  
  cmd <- sprintf("aws s3 ls s3://%s/ --region %s", bucket, region)
  s3_files <- system(cmd, intern = TRUE)
  
  # Check for our files
  rds_pattern <- paste0("WSP.*", end_year, "\\.rds")
  our_files <- grep(rds_pattern, s3_files, value = TRUE)
  
  if (length(our_files) > 0) {
    message("\n✓ S3 upload verified")
    message("\nFiles in S3:")
    for (file in our_files) {
      message(paste0("  ", file))
    }
  } else {
    message("\n⚠ Warning: Expected files not found in S3")
    message("S3 bucket contents:")
    for (file in s3_files) {
      message(paste0("  ", file))
    }
  }
  
}, error = function(e) {
  message("\n⚠ Could not verify S3 upload:")
  message(paste0("  ", e$message))
  message("Data may still be uploaded - check S3 bucket manually")
})

###############################################################################
# STEP 4: SUMMARY
###############################################################################

message("\n========================================")
message("WORKFLOW COMPLETE")
message("========================================")
message("\nSummary:")
message(paste0("  Total songs scraped: ", nrow(data_list$songs)))
message(paste0("  Historical shows: ", nrow(data_list$dim_historical)))
message(paste0("  Future shows: ", nrow(data_list$dim_future)))
message(paste0("  Year range: ", min(data_list$dim_historical$year), "-", max(data_list$dim_historical$year)))
message("\nLocal files:")
message(paste0("  ./Data/WSP_Song_Fact_Table_", start_year, "_to_", end_year, ".rds"))
message(paste0("  ./Data/WSP_Show_Dim_Table_", start_year, "_to_", end_year, ".rds"))
message("\nS3 backup:")
message(paste0("  s3://", Sys.getenv("AWS_S3_BUCKET", "wsmfp-setlists"), "/"))
message("\n✓ Ready for modeling and predictions")
message("========================================\n")

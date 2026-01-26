#!/usr/bin/env Rscript
# test_playwright.R - Quick test of Playwright scraper

suppressPackageStartupMessages({
  library(tidyverse)
})

source('./Code/scrape.R')
source('./Code/clean.R')

message("\n========================================")
message("Testing Playwright Scraper")
message("========================================\n")

# Test 1: Scrape a single year's tour dates
message("Test 1: Scraping tour dates for 1999...")

tryCatch({
  tour_data <- scrape_year_tour(1999, use_cache = TRUE)
  
  if (!is.null(tour_data) && nrow(tour_data) > 0) {
    message(paste0("✓ Success: Scraped ", nrow(tour_data), " shows"))
    message("\nSample data:")
    print(head(tour_data, 3))
  } else {
    message("✗ Failed: No data returned")
  }
}, error = function(e) {
  message(paste0("✗ Error: ", e$message))
})

# Test 2: Scrape a single setlist
message("\n\nTest 2: Scraping setlist for 1999-07-01...")

tryCatch({
  setlist_url <- "http://everydaycompanion.com/setlists/19990701a.asp"
  setlist <- scrape_setlist(setlist_url, use_cache = TRUE)
  
  if (!is.null(setlist) && nrow(setlist) > 0) {
    message(paste0("✓ Success: Scraped ", nrow(setlist), " songs"))
    message("\nSample setlist:")
    print(head(setlist %>% select(set, song_name, into), 10))
  } else {
    message("✗ Failed: No setlist data returned")
  }
}, error = function(e) {
  message(paste0("✗ Error: ", e$message))
})

# Test 3: Clean the data
message("\n\nTest 3: Cleaning tour data...")

tryCatch({
  if (!is.null(tour_data)) {
    cleaned <- clean_tour_data(tour_data)
    
    if (!is.null(cleaned) && nrow(cleaned) > 0) {
      message(paste0("✓ Success: Cleaned ", nrow(cleaned), " shows"))
      message("\nSample cleaned data:")
      print(head(cleaned %>% select(date, city, venue_name, is_radio), 3))
    } else {
      message("✗ Failed: No cleaned data")
    }
  } else {
    message("⚠ Skipped: No tour data to clean")
  }
}, error = function(e) {
  message(paste0("✗ Error: ", e$message))
})

message("\n========================================")
message("Test Complete")
message("========================================\n")

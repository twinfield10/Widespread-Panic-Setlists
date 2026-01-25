# data_pipeline.R - High-level data loading pipeline
# Uses modular scrape.R and clean.R functions

# Load required libraries
library(tidyverse)
library(purrr)

# Source modular functions
source('./Code/scrape.R')
source('./Code/clean.R')
source('./Code/storage.R')

`%notin%` <- Negate(`%in%`)

#' Load all historical setlist data
#'
#' @param start_year Start year (default: 1986)
#' @param end_year End year (default: current year)
#' @param use_cache Use cached HTML to avoid re-scraping
#' @return List containing: songs, historical shows, future shows
load_all_data <- function(start_year = 1986, end_year = as.numeric(format(Sys.Date(), "%Y")), use_cache = TRUE) {
  
  start_time <- Sys.time()
  
  # Years to process (excluding 2004 - no tour that year)
  years <- setdiff(start_year:end_year, 2004)
  
  message(paste0("\nLoading tour data for ", length(years), " years (", start_year, "-", end_year, ")..."))
  
  # Scrape tour dates for each year
  tour_list <- map(years, function(yr) {
    scrape_year_tour(yr, use_cache = use_cache)
  })
  
  # Filter out NULL results (failed scrapes)
  tour_list <- Filter(Negate(is.null), tour_list)
  
  if (length(tour_list) == 0) {
    stop("Failed to load any tour data")
  }
  
  # Combine and clean tour data
  tour_data <- bind_rows(tour_list) %>%
    clean_tour_data() %>%
    arrange(year, month, day) %>%
    rowid_to_column('show_index') %>%
    add_run_indexing() %>%
    flag_radio_shows() %>%
    select(link, date, date_num, year, month, day, state, city, venue_name, 
           venue_full, run_index, show_index, show_in_run, year_index, venue, is_radio)
  
  # Split historical and future
  show_dim <- tour_data %>% filter(date < Sys.Date(), year >= start_year, year <= end_year)
  fut_dim <- tour_data %>% filter(date >= Sys.Date())
  
  message(paste0("\n", length(show_dim$date), " historical & ", length(fut_dim$date), 
                 " future shows loaded. Now scraping setlists...\n"))
  
  # Scrape setlists for historical shows
  songs_list <- map(show_dim$link, function(link) {
    scrape_setlist(link, use_cache = use_cache)
  })
  
  # Filter out NULL results
  songs_list <- Filter(Negate(is.null), songs_list)
  
  if (length(songs_list) == 0) {
    stop("Failed to load any setlist data")
  }
  
  # Combine and clean songs
  songs <- bind_rows(songs_list) %>% clean_song_data()
  
  # Extract show metadata
  dim_songs <- extract_show_metadata(songs)
  
  # Remove show_notes from songs table (it's in dim now)
  songs <- songs %>% select(-show_notes)
  
  # Build future shows dimension
  slim_fut <- fut_dim %>%
    mutate(
      is_soundcheck = 0,
      is_opening_act = 0,
      show_notes = "",
      n_songs = 0,
      weekday = weekdays(date),
      is_fut = 1
    ) %>%
    arrange(date)
  
  # Build historical shows dimension
  dim <- show_dim %>%
    left_join(dim_songs, by = 'link') %>%
    mutate(
      is_soundcheck = case_when(
        grepl("\\[Soundcheck; ", show_notes) ~ 1,
        TRUE ~ 0
      ),
      is_opening_act = case_when(
        str_detect(tolower(show_notes), "opened for") ~ 1,
        TRUE ~ 0
      ),
      weekday = weekdays(date),
      is_fut = 0
    ) %>%
    arrange(show_index) %>%
    mutate(
      n_songs = if_else(is.na(n_songs), 0, n_songs),
      show_notes = if_else(is.na(show_notes), "", show_notes)
    ) %>%
    arrange(show_index) %>%
    rbind(slim_fut)
  
  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  message(paste0('\n✓ Successfully loaded ', length(unique(dim$link)), ' Widespread Panic shows (', 
                 nrow(songs), ' total songs) in ', round(elapsed_time, 2), ' minutes from ', 
                 min(dim$year), ' to ', max(dim$year), '\n'))
  
  ret_list <- list(
    songs = songs,
    dim_historical = dim %>% filter(is_fut == 0),
    dim_future = dim %>% filter(is_fut == 1)
  )
  
  return(ret_list)
}

#' Update data with latest shows only
#'
#' @param use_cache Use cached HTML
#' @return List containing updated songs and show dimensions
update_all_data <- function(use_cache = TRUE) {
  
  # Load previous data from S3/local
  song_filename <- 'WSP_Song_Fact_Table_1986_to_2024.rds'
  dim_hist_filename <- 'WSP_Show_Dim_Table_1986_to_2024.rds'
  
  prev_song <- load_rds_with_s3(song_filename, local_dir = "./Data/")
  prev_dim_hist <- load_rds_with_s3(dim_hist_filename, local_dir = "./Data/")
  
  if (is.null(prev_song) || is.null(prev_dim_hist)) {
    stop("Could not load previous data from local or S3. Run load_all_data() first.")
  }
  
  # Determine last show date
  last_show <- max(prev_dim_hist$date)
  
  message(paste0("Last show in database: ", last_show))
  message("Checking for new shows...\n")
  
  # Scrape current year tour data
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  tour_data_raw <- scrape_year_tour(current_year, use_cache = use_cache)
  
  if (is.null(tour_data_raw)) {
    message("No new tour data available")
    return(list(prev_song, prev_dim_hist, data.frame()))
  }
  
  tour_data <- tour_data_raw %>%
    clean_tour_data() %>%
    arrange(year, month, day) %>%
    rowid_to_column('show_index') %>%
    add_run_indexing() %>%
    flag_radio_shows()
  
  update_dim <- tour_data %>% filter(date < Sys.Date() & date > last_show)
  fut_dim <- tour_data %>% filter(date >= Sys.Date())
  
  if (nrow(update_dim) == 0) {
    message("✓ All historical shows up to date")
    return(list(prev_song, prev_dim_hist, fut_dim))
  }
  
  message(paste0("Found ", nrow(update_dim), " new shows to scrape\n"))
  
  # Scrape new setlists
  start_time <- Sys.time()
  load_links <- update_dim %>% 
    filter(date > last_show & link %notin% unique(prev_song$link)) %>% 
    pull(link) %>% 
    unique()
  
  update_songs_list <- map(load_links, function(link) {
    scrape_setlist(link, use_cache = use_cache)
  })
  
  update_songs_list <- Filter(Negate(is.null), update_songs_list)
  
  if (length(update_songs_list) == 0) {
    warning("No new setlist data scraped")
    return(list(prev_song, prev_dim_hist, fut_dim))
  }
  
  # Clean and combine
  songs <- bind_rows(update_songs_list) %>% clean_song_data()
  dim_songs <- extract_show_metadata(songs)
  all_songs <- rbind(prev_song, songs %>% select(-show_notes))
  
  # Build updated dimension
  slim_fut <- fut_dim %>%
    mutate(
      is_soundcheck = 0,
      is_opening_act = 0,
      show_notes = "",
      n_songs = 0,
      weekday = weekdays(date),
      is_fut = 1
    ) %>%
    arrange(date)
  
  new_dim <- update_dim %>%
    left_join(dim_songs, by = 'link') %>%
    mutate(
      is_soundcheck = case_when(grepl("\\[Soundcheck; ", show_notes) ~ 1, TRUE ~ 0),
      is_opening_act = case_when(str_detect(tolower(show_notes), "opened for") ~ 1, TRUE ~ 0),
      weekday = weekdays(date),
      is_fut = 0,
      n_songs = if_else(is.na(n_songs), 0, n_songs),
      show_notes = if_else(is.na(show_notes), "", show_notes)
    ) %>%
    arrange(show_index)
  
  dim <- rbind(prev_dim_hist, new_dim, slim_fut)
  
  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  message(paste0('\n✓ Successfully updated ', length(unique(new_dim$link)), 
                 ' shows (', nrow(songs), ' songs) in ', round(elapsed_time, 2), ' minutes'))
  
  ret_list <- list(
    songs = all_songs,
    dim_historical = dim %>% filter(is_fut == 0),
    dim_future = dim %>% filter(is_fut == 1)
  )
  
  return(ret_list)
}

#' Save setlist data locally and to S3
#'
#' @param song_df Song fact table
#' @param all_df Combined show dimension table
save_setlists <- function(song_df, all_df) {
  start_year <- min(as.numeric(substr(all_df$link, 39, 42)))
  end_year <- max(as.numeric(substr(all_df$link, 39, 42)))
  
  fact_filename <- paste0('WSP_Song_Fact_Table_', start_year, "_to_", end_year, ".rds")
  dim_filename <- paste0('WSP_Show_Dim_Table_', start_year, "_to_", end_year, ".rds")
  
  message("Saving Song Fact Table...")
  save_rds_with_s3(song_df, fact_filename, local_dir = "./Data/")
  
  message("Saving Show Dimension Table...")
  save_rds_with_s3(all_df, dim_filename, local_dir = "./Data/")
  
  message("✓ Setlist data saved locally and backed up to S3")
}

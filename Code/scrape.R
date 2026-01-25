# scrape.R - Web scraping functions with error handling and retry logic
# Scrapes setlist data from everydaycompanion.com

library(rvest)
library(httr)


#' Playwright wrapper
#' 
#' @param url URL to fetch
fetch_html_playwright <- function(url) {
  cmd <- sprintf(
    "python3 Code/fetch_page.py %s",
    shQuote(url)
  )
  
  html_text <- system(cmd, intern = TRUE)
  html <- read_html(paste(html_text, collapse = "\n"))

  return(html)
}

#' Safely fetch HTML from a URL using Playwright only
#'
#' @param url URL to fetch
#' @param max_retries Maximum number of retry attempts
#' @param retry_delay Delay between retries in seconds
#' @return HTML document or NULL on failure
safe_fetch_html <- function(
  url,
  max_retries = 3,
  retry_delay = 2
) {
  for (attempt in seq_len(max_retries)) {
    tryCatch({
      message("Playwright attempt ", attempt, " for ", url)

      html_doc <- fetch_html_playwright(url)

      # Sanity check: make sure we actually got HTML
      if (!inherits(html_doc, "xml_document")) {
        stop("Playwright did not return a valid HTML document")
      }

      return(html_doc)

    }, error = function(e) {
      if (attempt < max_retries) {
        message(
          "Attempt ", attempt, " failed for ", url, ": ",
          e$message,
          ". Retrying in ", retry_delay, "s..."
        )
        Sys.sleep(retry_delay)
      } else {
        warning(
          "Playwright failed for ", url,
          " after ", max_retries, " attempts: ",
          e$message
        )
      }
    })
  }

  NULL
}


#' Cache HTML to local file
#'
#' @param html HTML document
#' @param cache_key Unique identifier for the cached file
#' @param cache_dir Directory to store cached HTML
cache_html <- function(html, cache_key, cache_dir = "./cache/html/") {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  cache_path <- file.path(cache_dir, paste0(cache_key, ".html"))
  
  tryCatch({
    xml2::write_html(html, cache_path)
    return(TRUE)
  }, error = function(e) {
    warning(paste0("Failed to cache HTML for ", cache_key, ": ", e$message))
    return(FALSE)
  })
}

#' Load HTML from cache
#'
#' @param cache_key Unique identifier
#' @param cache_dir Cache directory
#' @return HTML document or NULL if not cached
load_cached_html <- function(cache_key, cache_dir = "./cache/html/") {
  cache_path <- file.path(cache_dir, paste0(cache_key, ".html"))
  
  if (file.exists(cache_path)) {
    tryCatch({
      return(read_html(cache_path))
    }, error = function(e) {
      warning(paste0("Failed to load cached HTML for ", cache_key, ": ", e$message))
      return(NULL)
    })
  }
  return(NULL)
}

#' Fetch HTML with caching
#'
#' @param url URL to fetch
#' @param cache_key Unique cache identifier
#' @param use_cache Whether to use cached version
#' @return HTML document or NULL
fetch_html_cached <- function(url, cache_key, use_cache = TRUE) {
  if (use_cache) {
    cached <- load_cached_html(cache_key)
    if (!is.null(cached)) {
      message(paste0("✓ Using cached HTML for ", cache_key))
      return(cached)
    }
  }
  
  message(paste0("Fetching ", url))
  html <- safe_fetch_html(url)
  
  if (!is.null(html) && use_cache) {
    cache_html(html, cache_key)
  }
  
  return(html)
}

#' Scrape tour dates for a single year
#'
#' @param year Year to scrape (e.g., 1986)
#' @param use_cache Use cached HTML if available
#' @return Data frame of tour dates or NULL on failure
scrape_year_tour <- function(year, use_cache = TRUE) {
  base_url <- 'http://everydaycompanion.com/'
  yr <- substring(as.character(year), nchar(year) - 1)
  year_link <- paste0(base_url, 'asp/tour', yr, '.asp')
  cache_key <- paste0("tour_", year)
  
  html <- fetch_html_cached(year_link, cache_key, use_cache)
  
  if (is.null(html)) {
    return(NULL)
  }
  
  tryCatch({
    tour_string <- html %>%
      html_element("p") %>%
      html_text2()
    
    if (is.na(tour_string) || tour_string == "") {
      warning(paste0("No tour data found for ", year))
      return(NULL)
    }
    
    tour_string <- gsub("\\?\\?", "00", tour_string)
    
    # Split into dates and venues
    venues <- str_trim(strsplit(tour_string, "\\d{2}/\\d{2}/\\d{2}\\?*", perl = TRUE)[[1]])
    venues <- Filter(function(x) x != "", venues)
    dates <- str_extract_all(tour_string, "\\d{2}/\\d{2}/\\d{2}\\b\\?*")[[1]]
    
    tour_data <- data.frame(
      date = unlist(dates),
      venue = unlist(venues),
      year = year
    )
    
    message(paste0("✓ Scraped ", nrow(tour_data), " shows for ", year))
    return(tour_data)
    
  }, error = function(e) {
    warning(paste0("Failed to parse tour data for ", year, ": ", e$message))
    return(NULL)
  })
}

#' Scrape setlist for a single show
#'
#' @param setlist_link URL to setlist page
#' @param use_cache Use cached HTML if available
#' @return Data frame of songs or NULL on failure
scrape_setlist <- function(setlist_link, use_cache = TRUE) {
  setlist_link <- tolower(setlist_link)
  cache_key <- gsub("[^a-z0-9]", "_", setlist_link)
  
  html <- fetch_html_cached(setlist_link, cache_key, use_cache)
  
  if (is.null(html)) {
    return(NULL)
  }
  
  tryCatch({
    setlist_tbl <- html %>% html_table()
    
    if (length(setlist_tbl) < 6) {
      warning(paste0("Unexpected table structure for ", setlist_link))
      return(NULL)
    }
    
    setlist_raw <- setlist_tbl[[6]] %>%
      mutate(
        X1 = str_replace_all(X1, "ï", "i")
      ) %>%
      mutate(
        set = case_when(
          substr(X1, 1, 3) == "??" ~ 'Details',
          substr(X1, 1, 3) == "0: " ~ '0',
          substr(X1, 1, 3) == "1: " ~ '1',
          substr(X1, 1, 3) == "2: " ~ '2',
          substr(X1, 1, 3) == "3: " ~ '3',
          substr(X1, 1, 3) == "4: " ~ '4',
          substr(X1, 1, 3) == "E: " ~ 'E',
          substr(X1, 1, 3) == "E1:" ~ 'E',
          substr(X1, 1, 3) == "E2:" ~ 'E',
          substr(X1, 1, 3) == "E3:" ~ 'E',
          grepl("^\\d{2}/", substr(X1, 1, 3)) ~ "Details",
          substr(X1, 1, 1) == "*" ~ "Song_Notes",
          substr(X1, 1, 1) == "[" ~ "Show_Notes",
          TRUE ~ "Other"
        )
      ) %>%
      rename("Raw" = 'X1') %>%
      mutate(
        Raw = case_when(
          set %in% c('0', '1', '2', '3', '4', 'E') ~ str_replace(Raw, "^.{3}", ""),
          TRUE ~ Raw
        ),
        Raw = if_else(set == 'Other', paste0("* ", str_replace(Raw, ".*\\*", "")), Raw)
      )
    
    # Parse songs
    songs <- setlist_raw %>%
      filter(set %notin% c('Details', 'Song_Notes', 'Show_Notes', 'Other')) %>%
      separate_rows(Raw, sep = ",") %>%
      mutate(into = ifelse(grepl(" > ", Raw), 1, 0)) %>%
      separate_rows(Raw, sep = " > ") %>%
      mutate(
        song_name = toupper(trimws(Raw)),
        notes_id = str_count(song_name, "\\*"),
        song_notes_key = if_else(notes_id == 0, "", strrep("*", times = notes_id)),
        link = setlist_link,
        song_name = toupper(str_replace_all(song_name, "\\*", "")),
        song_name = case_when(
          song_name %in% c('???', 'ARU/WSP JAM') ~ 'JAM',
          song_name == 'THIS MUST BE THE PLACE (NA<EF>VE MELODY)' ~ 'THIS MUST BE THE PLACE (NAIEVE MELODY)',
          song_name == 'W<CR>M' ~ 'WURM',
          song_name %in% c('LAWYERS', 'GUNS', 'AND MONEY') ~ 'LAWYERS GUNS AND MONEY',
          TRUE ~ song_name
        )
      ) %>%
      unique() %>%
      rowid_to_column('song_index') %>%
      select('link', 'set', 'song_name', 'into', 'song_index', 'song_notes_key', 'notes_id')
    
    # Parse notes
    raw_notes <- setlist_raw %>%
      filter(set %in% c("Song_Notes", "Show_Notes", "Other"))
    
    if ('Other' %in% setlist_raw$set) {
      other_row <- raw_notes %>%
        filter(set == 'Other') %>%
        separate_rows(Raw, sep = "\r\n") %>%
        mutate(set = case_when(
          substr(trimws(Raw), 1, 1) == "*" ~ "Song_Notes",
          substr(trimws(Raw), 1, 1) == "[" ~ "Show_Notes",
          TRUE ~ "Other"
        )) %>%
        filter(set != 'Other')
      
      raw_notes <- raw_notes %>%
        filter(set != 'Other') %>%
        rbind(other_row)
    }
    
    # Show notes
    show_notes_df <- if ("Show_Notes" %in% raw_notes$set) {
      data.frame(
        link = setlist_link,
        show_notes = raw_notes %>% filter(set == "Show_Notes") %>% select(Raw) %>% pull()
      )
    } else {
      data.frame(link = setlist_link, show_notes = "")
    }
    
    # Song notes
    notes_df <- if ('Song_Notes' %in% raw_notes$set) {
      notes_str <- raw_notes %>%
        filter(set %in% c('Song_Notes')) %>%
        select(Raw) %>%
        pull()
      
      notessplit <- strsplit(notes_str, "(?<=[A-Za-z])\\*", perl = TRUE)[[1]]
      for (n in seq_along(notessplit)) {
        if (n == 1) {
          notessplit[n] <- notessplit[n]
        } else if (n > 1) {
          notessplit[n] <- paste0("*", notessplit[n])
        }
      }
      
      data.frame(link = setlist_link, 'strings' = notessplit) %>%
        separate(strings, into = c("song_notes_key", "song_note_detail"), sep = " ", extra = "merge") %>%
        mutate(
          song_notes_key = trimws(song_notes_key),
          song_note_detail = toupper(trimws(song_note_detail))
        )
    } else {
      data.frame(link = setlist_link, song_notes_key = NA, song_note_detail = "")
    }
    
    # Join and return
    songs <- songs %>%
      left_join(show_notes_df, by = c("link")) %>%
      left_join(notes_df, by = c("link", "song_notes_key")) %>%
      select(-c(song_notes_key, notes_id))
    
    message(paste0("✓ Scraped ", nrow(songs), " songs from ", setlist_link))
    return(songs)
    
  }, error = function(e) {
    warning(paste0("Failed to parse setlist for ", setlist_link, ": ", e$message))
    return(NULL)
  })
}

#' Clear HTML cache
#'
#' @param cache_dir Cache directory
clear_cache <- function(cache_dir = "./cache/html/") {
  if (dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE)
    message("✓ Cache cleared")
  } else {
    message("Cache directory does not exist")
  }
}

# General
library(tidyverse)
library(rvest)
library(lubridate)
library(scales)
library(purrr)
`%notin%` = Negate(`%in%`)

# Modeling
library(xgboost)
library(pROC)
library(caret)
library(Metrics)
library(performanceEstimation)

# Plotting
library(ggplot2)
library(gt)
library(paletteer)

###################################
## DEFINE LOAD + CLEAN FUNCTIONS ##
###################################

# Setlist And Tour Date Processing Functions
process_dim <- function(st_yr = 1986 , end_yr = 2024){
  
  base_url <- 'http://everydaycompanion.com/'
  tour_list <- as.list(st_yr:end_yr)
  tour_list <- tour_list[tour_list != 2004] # :(
  tour_df_list = list()
  
  
  for (i in seq_along(tour_list)) {
    yr <- substring(as.character(tour_list[[i]]), nchar(tour_list[[i]]) - 1)
    year_link <- paste0(base_url, 'asp/tour', yr, '.asp')
    print(year_link)
    tour_string <- read_html(year_link, encoding = "latin1") %>%
      html_element("p") %>%
      html_text2()
    tour_string <- gsub("\\?\\?", "00", tour_string)
    
    # Split the string into individual dates
    venues <- str_trim(strsplit(tour_string, "\\d{2}/\\d{2}/\\d{2}\\?*", perl = TRUE)[[1]])
    venues <- Filter(function(x) x != "", venues)
    dates <- str_extract_all(tour_string, "\\d{2}/\\d{2}/\\d{2}\\b\\?*")[[1]]
    
    tour_data <- data.frame(
      date = unlist(dates),
      venue = unlist(venues))
    tour_data$link <- paste0(base_url,'setlists/', format(as.Date(tour_data$date, format = "%m/%d/%y"), "%Y%m%d"), "a.asp")
    
    # Change Unknown Dates To Update Links
    tour_data <- tour_data %>%
      mutate(
        link = case_when(
          (date == '00/00/85') & (venue == 'A-Frame, Weymanda Court, Athens, GA') ~ 'http://everydaycompanion.com/setlists/19850000a.asp',
          (date == '00/00/86') & (venue == 'Phi Delta Theta House, University of Georgia, Athens, GA') ~ 'NODATA',
          (date == '00/00/86') & (venue == '** UNKNOWN **, ** UNKNOWN **, UU') ~ 'NODATA',
          (date == '03/00/86') & (venue == 'Uptown Lounge, Athens, GA') ~ 'http://everydaycompanion.com/setlists/19860300a.asp',
          (date == '04/00/86') & (venue == '** UNKNOWN **, Atlanta, GA') ~ 'http://everydaycompanion.com/setlists/19860400a.asp',
          (date == '07/00/86') & (venue == 'Washington Park, Macon, GA') ~ 'http://everydaycompanion.com/setlists/19860700a.asp',
          (date == '01/00/87') & (venue == '40 Watt Club, Athens, GA') ~ 'http://everydaycompanion.com/setlists/19870100a.asp',
          (date == '10/00/87') & (venue == 'Uptown Lounge, Athens, GA') ~ 'http://everydaycompanion.com/setlists/19871000a.asp',
          (date == '00/00/87') & (venue == '** UNKNOWN **, ** UNKNOWN **, UU') ~ 'http://everydaycompanion.com/setlists/19870000a.asp',
          (date == '09/00/88') & (venue == 'Phi Kappa Phi House, Presbyterian College, Clinton, SC') ~ 'NODATA',
          (date == '09/00/88') & (venue == "O'Neilly's Pub, Macon, GA") ~ 'http://everydaycompanion.com/setlists/19880900a.asp',
          (date == '00/00/89') & (venue == "W.C. Don's, Jackson, MS") ~ 'http://everydaycompanion.com/setlists/19890000a.asp',
          (date == '01/00/89') & (venue == 'Phi Delta Theta House, University of Georgia, Athens, GA') ~ 'http://everydaycompanion.com/setlists/19890100a.asp',
          (date == '04/00/89') & (venue == 'Sigma Alpha Epsilon House, Tuscaloosa, AL') ~ 'http://everydaycompanion.com/setlists/19890400b.asp',
          (date == '05/00/89') & (venue == 'The Brewery, Raleigh, NC') ~ 'http://everydaycompanion.com/setlists/19890500a.asp',
          (date == '09/00/89') & (venue == "Edgar's Campus Bar, Clemson University, Clemson, SC") ~ 'http://everydaycompanion.com/setlists/19890900a.asp',
          (date == '10/00/89') & (venue == 'Elmo House, Charlottesville, VA') ~ 'http://everydaycompanion.com/setlists/19891000a.asp',
          (date == '00/00/90') & (venue == "Johnny D's, Somerville, MA") ~ 'http://everydaycompanion.com/setlists/19900000a.asp',
          (date == '08/00/90') & (venue == 'Excelsior Mill, Atlanta, GA') ~ 'http://everydaycompanion.com/setlists/19900800a.asp',
          (date == '00/00/91') & (venue == 'Hollins University, Roanoke, VA') ~ 'NODATA',
          (date == '03/21/87') & (venue == 'The Rookery, Macon, GA') ~ 'NODATA',
          (date %in% c('02/15/24', '02/16/24', '02/17/24')) & (venue == 'Chicago Theatre, Chicago, IL') ~ 'NODATA',
          
          # Multiple Shows in One Day (change Link To 'b')
          (date == '07/21/91') & (venue == 'Sheridan Opera House, Telluride, CO') ~ 'http://everydaycompanion.com/setlists/19910721b.asp',
          (date == '08/20/91') & (venue == "Toad's Place, New Haven, CT") ~ 'http://everydaycompanion.com/setlists/19910820b.asp',
          (date == '09/21/92') & (venue == "Woodsmen of the World Hall, Eugene, OR") ~ 'http://everydaycompanion.com/setlists/19920921b.asp',
          (date == '02/23/93') & (venue == "Newport Music Hall, Columbus, OH") ~ 'http://everydaycompanion.com/setlists/19930223b.asp',
          (date == '05/03/93') & (venue == "First Avenue, Minneapolis, MN") ~ 'http://everydaycompanion.com/setlists/19930503b.asp',
          (date == '05/12/93') & (venue == "Horizontal Boogie Bar, Rochester, NY") ~ 'http://everydaycompanion.com/setlists/19930512b.asp',
          (date == '05/15/93') & (venue == "Avalon, Boston, MA") ~ 'http://everydaycompanion.com/setlists/19930515b.asp',
          (date == '03/15/94') & (venue == "Avalon, Boston, MA") ~ 'http://everydaycompanion.com/setlists/19940315b.asp',
          (date == '07/14/94') & (venue == "The Vic Theatre, Chicago, IL") ~ 'http://everydaycompanion.com/setlists/19940714b.asp',
          (date == '11/05/94') & (venue == "Arnold Hall, US Air Force Academy, Colorado Springs, CO") ~ 'http://everydaycompanion.com/setlists/19941105b.asp',
          (date == '11/06/94') & (venue == "Theater, Lory Student Center, Colorado State University, Fort Collins, CO") ~ 'http://everydaycompanion.com/setlists/19941106b.asp',
          (date == '11/11/94') & (venue == "Roseland Theater, Portland, OR") ~ 'http://everydaycompanion.com/setlists/19941111b.asp',
          (date == '03/25/95') & (venue == "Michigan State University Auditorium, East Lansing, MI") ~ 'http://everydaycompanion.com/setlists/19950325b.asp',
          (date == '04/08/95') & (venue == "Irving Plaza, New York, NY") ~ 'http://everydaycompanion.com/setlists/19950408b.asp',
          (date == '05/06/95') & (venue == "Chastain Park, Atlanta, GA") ~ 'http://everydaycompanion.com/setlists/19950506b.asp',
          (date == '07/14/95') & (venue == "Cain's Main Street Stage, Tulsa, OK") ~ 'http://everydaycompanion.com/setlists/19950714b.asp',
          (date == '07/18/95') & (venue == "Alberta Bair Theater, Billings, MT") ~ 'http://everydaycompanion.com/setlists/19950718b.asp',
          (date == '07/22/95') & (venue == "Roseland Theater, Portland, OR") ~ 'http://everydaycompanion.com/setlists/19950722b.asp',
          (date == '07/29/95') & (venue == "Snow King Center, Jackson, WY") ~ 'http://everydaycompanion.com/setlists/19950729b.asp',
          (date == '04/12/97') & (venue == "Backyard, Bee Cave, TX") ~ 'http://everydaycompanion.com/setlists/19970412b.asp',
          (date == '09/16/97') & (venue == "Virginia Theater, Champaign, IL") ~ 'http://everydaycompanion.com/setlists/19970916b.asp',
          (date == '09/17/97') & (venue == "Shryock Auditorium, Southern Illinois University, Carbondale, IL") ~ 'http://everydaycompanion.com/setlists/19970917b.asp',
          (date == '03/19/98') & (venue == "Chesterfield Café, Paris, FR") ~ 'http://everydaycompanion.com/setlists/19980319b.asp',
          (date == '07/01/99') & (venue == "House of Blues, West Hollywood, CA") ~ 'http://everydaycompanion.com/setlists/19990701b.asp',
          (date == '09/30/99') & (venue == "Backyard, Bee Cave, TX") ~ 'http://everydaycompanion.com/setlists/19990930b.asp',
          (date == '11/17/99') & (venue == "Orpheum Theater, Boston, MA") ~ 'http://everydaycompanion.com/setlists/19991117b.asp',
          (date == '07/30/00') & (venue == "Alpine Stage, Bolton Valley Resort, Bolton, VT") ~ 'http://everydaycompanion.com/setlists/20000730b.asp',
          (date == '07/21/01') & (venue == "Harbor Center, Portsmouth, VA") ~ 'http://everydaycompanion.com/setlists/20010721b.asp',
          (date == '10/16/01') & (venue == "Paramount Theater, Seattle, WA") ~ 'http://everydaycompanion.com/setlists/20011016b.asp',
          (date == '10/24/01') & (venue == "KGSR 107.1FM Studios, Austin, TX") ~ 'http://everydaycompanion.com/setlists/20011024b.asp',
          (date == '10/24/01') & (venue == "Frank Erwin Center, Austin, TX") ~ 'http://everydaycompanion.com/setlists/20011024c.asp',
          (date == '11/01/01') & (venue == "Roy Wilkins Civic Auditorium, St. Paul, MN") ~ 'http://everydaycompanion.com/setlists/20011101b.asp',
          (date == '11/08/01') & (venue == "Orpheum Theater, Boston, MA") ~ 'http://everydaycompanion.com/setlists/20011108b.asp',
          (date == '04/11/03') & (venue == "UIC Pavilion, Chicago, IL") ~ 'http://everydaycompanion.com/setlists/20030411b.asp',
          (date == '07/16/03') & (venue == "Harbor Center, Portsmouth, VA") ~ 'http://everydaycompanion.com/setlists/20030716b.asp',
          (date == '07/22/03') & (venue == "Paolo Soleri, Santa Fe, NM") ~ 'http://everydaycompanion.com/setlists/20030722b.asp',
          (date == '10/03/03') & (venue == "Backyard, Bee Cave, TX") ~ 'http://everydaycompanion.com/setlists/20031003b.asp',
          (date == '04/08/05') & (venue == "Chicago Theatre, Chicago, IL") ~ 'http://everydaycompanion.com/setlists/20050408b.asp',
          (date == '04/14/05') & (venue == "Radio City Music Hall, New York, NY") ~ 'http://everydaycompanion.com/setlists/20050414b.asp',
          (date == '08/01/06') & (venue == "The Palace Theatre, Louisville, KY") ~ 'http://everydaycompanion.com/setlists/20060801b.asp',
          (date == '11/02/06') & (venue == "Backyard, Bee Cave, TX") ~ 'http://everydaycompanion.com/setlists/20061102b.asp',
          (date == '04/29/10') & (venue == "Howlin' Wolf, New Orleans, LA") ~ 'http://everydaycompanion.com/setlists/20100429b.asp',
          (date == '06/24/10') & (venue == "Twist and Shout Records, Denver, CO") ~ 'http://everydaycompanion.com/setlists/20100624b.asp',
          (date == '07/26/10') & (venue == "Tennessee Theater, Knoxville, TN") ~ 'http://everydaycompanion.com/setlists/20100726b.asp',
          (date == '10/04/10') & (venue == "Ryman Auditorium, Nashville, TN") ~ 'http://everydaycompanion.com/setlists/20101004b.asp',
          (date == '04/17/13') & (venue == "Palace Theater, Louisville, KY") ~ 'http://everydaycompanion.com/setlists/20130417b.asp',
          (date == '01/25/19') & (venue == "Hard Rock Hotel and Casino, Riviera Maya, MX") ~ 'http://everydaycompanion.com/setlists/20190125b.asp',
          TRUE ~ link
        )
      ) %>%
      filter(link != 'NODATA') %>%
      unique() %>%
      mutate(
        date_num = str_extract(link, "\\d+"),
        year = as.numeric(substr(date_num, 1, 4)),
        month = as.numeric(substr(date_num, 5, 6)),
        day = as.numeric(substr(date_num, 7, 8)),
        date = as.Date(sprintf("%02d/%02d/%04d", month, day, year), format = "%m/%d/%Y")
      ) %>%
      rowwise() %>%
      mutate(
        venue_split = str_split(venue, ", "),
        len_split = length(venue_split),
        state = venue_split[len_split],
        city = venue_split[len_split - 1],
        venue_name = if_else(len_split == 3, paste(venue_split[1]),
                             if_else(len_split == 4, paste(venue_split[1:(len_split-2)], collapse = ", "), NA)),
        venue_full = venue
      ) %>%
      mutate(
        city = toupper(city),
        venue_name = toupper(venue_name),
        venue_full = toupper(venue_full)
      ) %>%
      rowid_to_column('year_index') %>%
      select(-c(venue_split, len_split)) %>%
      mutate(
        is_radio = case_when(
          grepl("\\b\\d+\\.\\d+FM\\b", venue_full) ~ 1,
          grepl("\\b\\d+\\.\\d\\b", venue_full) ~ 1,
          grepl("NBC STUDIOS", venue_full) ~ 1,
          grepl("ED SULLIVAN THEATER", venue_full) ~ 1,
          grepl("STUDIO 6B, ROCKAFELLER CENTER", venue_full) ~ 1,
          grepl("CNN STUDIOS", venue_full) ~ 1,
          grepl(" STUDIO", venue_full) ~ 1,
          grepl(" RECORD", venue_full) ~ 1,
          TRUE ~ 0
        ),
        venue_name = case_when(
          (venue_name == 'ADAMS CENTER') & (state == 'MT') ~ 'ADAMS FIELDHOUSE, UNIVERSITY OF MONTANA',
          (venue_name == 'AUDITORIUM THEATRE') & (city == 'CHICAGO') ~ 'AUDITORIUM THEATER, ROOSEVELT UNIVERSITY',
          (venue_name == 'BAYFRONT ARENA') & (state == 'FL') ~ 'BAYFRONT AUDITORIUM',
          (venue_name == "FLEET PAVILION") & (city == 'BOSTON') ~ "CAESAR'S TAHOE",
          (venue_name == "CAESAR'S TAHOE SHOWROOM") & (state == 'NV') ~ "CAESAR'S TAHOE",
          TRUE ~ venue_name
        ),
        city = case_when(
          (venue_name == '23 EAST CABARET') & (state == 'PA') ~ 'PHILADELPHIA',
          (venue_name == "CAESAR'S TAHOE") ~ 'LAKE TAHOE',
          (venue_name == 'CYNTHIA WOODS MITCHELL PAVILLION') ~ 'THE WOODLANDS',
          (city %in% c('N. LITTLE ROCK', 'NORTH LITTLE ROCK')) ~ 'LITTLE ROCK',
          (city == 'MT. CRESTED BUTTE') ~ 'CRESTED BUTTE',
          (city == 'SNOWMASS VILLAGE') ~ 'SNOWMASS',
          (city == 'ELON COLLEGE') ~ 'ELON',
          (city == 'N. MYRTLE BEACH') ~ 'MYRTLE BEACH',
          TRUE ~ city
        )
      )
    
    # Append the data frame to the list
    tour_df_list[[i]] <- tour_data
  }
  
  # Combine DataFrames From Loop List
  tour_data <- bind_rows(tour_df_list) %>% arrange(year, month, day) %>% rowid_to_column('show_index')
  
  # Create Run Index + Show In Run Index
  tour_data <- tour_data %>%
    arrange(date, venue_name) %>%  # Sort data by 'venue' and 'date'
    group_by(venue_name, run_index = cumsum(c(1, diff(date) != 1))) %>%  # Create groups based on consecutive dates and venue
    ungroup() %>%
    arrange(date, run_index) %>%
    group_by(run_index) %>%
    mutate(show_in_run  = (show_index -min(show_index))+1) %>%
    ungroup()  %>%
    select(link, date, date_num, year, month, day, state, city, venue_name, venue_full, run_index, show_index, show_in_run, year_index, venue, is_radio)
  
  return(tour_data)
  
}
process_setlist <- function(setlist_link) {
  
  setlist_link <- tolower(setlist_link)

    setlist_tbl <- read_html(setlist_link) %>%
      html_table()
    
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
        Raw = if_else(set == 'Other', paste0("* ", str_replace(Raw, ".*\\*", "")),Raw)
      )
    
    # Create Songs DataFrame
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
    
    raw_notes <- setlist_raw %>%
      filter(set %in% c("Song_Notes", "Show_Notes", "Other"))
    
    raw_notes <- if('Other' %in% setlist_raw$set){
      other_row <- raw_notes %>%
        filter(set == 'Other') %>%
        separate_rows(Raw, sep = "\r\n") %>%
        mutate(set = case_when(
          substr(trimws(Raw), 1, 1) == "*" ~ "Song_Notes",
          substr(trimws(Raw), 1, 1) == "[" ~ "Show_Notes",
          TRUE ~ "Other"
        )) %>%
        filter(set != 'Other')
      
      raw_notes %>%
        filter(set != 'Other') %>%
        rbind(other_row)
    } else {
      raw_notes
    }
    
    # Create Song Notes DataFrame
    show_notes_df <- if("Show_Notes" %in% raw_notes$set){
      data.frame(
        link = setlist_link,
        show_notes = raw_notes %>% filter(set == "Show_Notes") %>% select(Raw) %>% pull()
      )
    } else {
      data.frame(
        link = setlist_link,
        show_notes = ""
      )
    }
    
    # Create Notes DataFrame
    notes_df <- if ('Song_Notes' %in% raw_notes$set){
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
      
      
      data.frame(
        link = setlist_link,
        'strings' = notessplit
      ) %>%
        separate(strings, into = c("song_notes_key", "song_note_detail"), sep = " ", extra = "merge") %>%
        mutate(
          song_notes_key = trimws(song_notes_key),
          song_note_detail = toupper(trimws(song_note_detail))
        )
    }
    else{
      data.frame(
        link = setlist_link,
        song_notes_key = NA,
        song_note_detail = ""
      )
    }
    
    # Join Notes To Songs
    songs <- songs %>%
      left_join(show_notes_df, by = c("link")) %>%
      left_join(notes_df, by = c("link","song_notes_key")) %>%
      select(-c(song_notes_key, notes_id))
    
    print(paste0("Now Loading ", setlist_link))
    
    return(songs)
}

# Functions 1: Load All Show Information via EveryDayCompanion
load_all_data <- function(start = 1986 , end = 2025) {
  
  # Load Dim Stage 1
  tour_data <- process_dim(st_yr = start, end_yr = end)
  
  # Split Historical and Future
  show_dim <- tour_data %>% filter(date < Sys.Date()) %>% filter(year >= start & year <= end)
  fut_dim <- tour_data %>% filter(date >= Sys.Date())
    
  # Peek
  print(paste0(length(show_dim$date)," Historical & ", length(fut_dim$date)," Future Shows And EDC Links Loaded - Now Loading Setlists"))
  print(show_dim %>% arrange(-show_index) %>% head())
  
  # Load Setlists
  start_time <- Sys.time()
  
  # Historical Song
  songs <- map_dfr(show_dim$link, process_setlist)
  
  # Songs
  songs <-  songs %>%
    mutate(
      song_note_detail = if_else(song_note_detail == "", NA, song_note_detail),
      show_notes = if_else(show_notes == "", NA, show_notes),
      set_num = case_when(
        set == 'E' ~ "99",
        TRUE ~ set
      ),
      set = as.numeric(set_num)
    ) %>%
    group_by(link) %>%
    mutate(
      min_set = min(as.numeric(set)),
      max_set = max(as.numeric(set)),
      set = if_else(set == 0 & min_set == 0 & max_set %in% c(99,0), 1, set)
    ) %>%
    ungroup() %>%
    select(link, set, song_index, song_name, into, song_note_detail, show_notes)
  
  dim_songs <- songs %>%
    group_by(link, show_notes) %>%
    mutate(n_songs = max(song_index)) %>%
    ungroup() %>%
    select(link, show_notes, n_songs) %>%
    unique()
  
  songs <- songs %>% select(-show_notes)
  
  # Show Information
  # Slim Future
  Slim_Fut <- fut_dim %>%
    mutate(
      is_soundcheck = 0,
      is_opening_act = 0,
      show_notes = "",
      n_songs = 0,
      weekday = weekdays(date),
      is_fut = 1
    ) %>%
    arrange(date)
  
  
  dim <- show_dim %>%
    left_join(dim_songs, by = 'link') %>%
    mutate(
      is_radio = case_when(
        grepl("\\b\\d+\\.\\d+FM\\b", venue_full) ~ 1,
        grepl("\\b\\d+\\.\\d\\b", venue_full) ~ 1,
        grepl("NBC STUDIOS", venue_full) ~ 1,
        grepl("ED SULLIVAN THEATER", venue_full) ~ 1,
        grepl("STUDIO 6B, ROCKAFELLER CENTER", venue_full) ~ 1,
        grepl("CNN STUDIOS", venue_full) ~ 1,
        grepl(" STUDIO", venue_full) ~ 1,
        grepl(" RECORD", venue_full) ~ 1,
        TRUE ~ 0
      ),
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
    rbind(Slim_Fut)
    
  
  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
  print(paste0('Successfully Loaded ',length(unique(dim$link)),' Widespread Panic Shows (', nrow(songs),' Total Songs) in ', round(elapsed_time, 2),' Minutes From ', min(dim$year), ' to ', max(dim$year)))
  
  ret_list <- list(songs,
                   dim %>% filter(is_fut == 0),
                   dim %>% filter(is_fut == 1)
                   )
  # Return
  return(ret_list)
}

# Function 2: Update Most Recent Shows
update_all_data <- function(){
  # Load Previous Data
  song_path <- './Data/WSP_Song_FactTable_1986_to_2024.rds'
  dim_hist_path <- './Data/WSP_Dim_Show_Historical_1986_to_2024.rds'
  dim_fut_path <- './Data/WSP_Dim_Show_Future_2024_to_2024.rds'
  
  prev_dim_hist <- readRDS('./Data/WSP_Dim_Show_Historical_1986_to_2024.rds')
  prev_dim_fut <- readRDS(dim_fut_path)
  
  
  # Set Up Dim For Update
  last_show = max(prev_dim_hist$date)
  tour_data <- process_dim(st_yr = 1986, end_yr = 2025)
  
  update_dim <- tour_data %>% filter(date < Sys.Date() & date > last_show)
  fut_dim <- tour_data %>% filter(date >= Sys.Date())
  
  if(nrow(update_dim) == 0){
    print("All Historical Shows Up to Date")
    return(list(readRDS(song_path),
                readRDS(dim_hist_path),
                fut_dim)
    )
    
  } else {
    # Peek
    print(paste0("Now Updating ", length(update_dim$date)," Shows | ", length(fut_dim$date)," Future Shows And EDC Links Loaded - Now Loading Setlists"))
    print(update_dim %>% arrange(-show_index) %>% head())
    
    # Load Setlists
    start_time <- Sys.time()
    prev_song <- readRDS(song_path)
    load_links <- update_dim %>% filter(date > last_show & link %notin% unique(prev_song$link)) %>% pull(link) %>% unique()
    update_songs <- map_dfr(load_links, process_setlist)
    
    # Manipulate Setlists
    # Songs
    songs <-  update_songs %>%
      mutate(
        song_note_detail = if_else(song_note_detail == "", NA, song_note_detail),
        show_notes = if_else(show_notes == "", NA, show_notes),
        set_num = case_when(
          set == 'E' ~ "99",
          TRUE ~ set
        ),
        set = as.numeric(set_num)
      ) %>%
      group_by(link) %>%
      mutate(
        min_set = min(as.numeric(set)),
        max_set = max(as.numeric(set)),
        set = if_else(set == 0 & min_set == 0 & max_set %in% c(99,0), 1, set)
      ) %>%
      ungroup() %>%
      select(link, set, song_index, song_name, into, song_note_detail, show_notes)
    
    dim_songs <- songs %>%
      group_by(link, show_notes) %>%
      mutate(n_songs = max(song_index)) %>%
      ungroup() %>%
      select(link, show_notes, n_songs) %>%
      unique()
    
    all_songs <- rbind(prev_song, songs %>% select(-show_notes))
    
    # Slim Future
    Slim_Fut <- fut_dim %>%
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
        is_radio = case_when(
          grepl("\\b\\d+\\.\\d+FM\\b", venue_full) ~ 1,
          grepl("\\b\\d+\\.\\d\\b", venue_full) ~ 1,
          grepl("NBC STUDIOS", venue_full) ~ 1,
          grepl("ED SULLIVAN THEATER", venue_full) ~ 1,
          grepl("STUDIO 6B, ROCKAFELLER CENTER", venue_full) ~ 1,
          grepl("CNN STUDIOS", venue_full) ~ 1,
          grepl(" STUDIO", venue_full) ~ 1,
          grepl(" RECORD", venue_full) ~ 1,
          TRUE ~ 0
        ),
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
      arrange(show_index)
    
    dim <- rbind(prev_dim_hist, new_dim, Slim_Fut)
    
    
    end_time <- Sys.time()
    elapsed_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
    print(paste0('Successfully Updated ',length(unique(new_dim$link)),' Widespread Panic Shows (', nrow(songs),' Total Songs) in ', round(elapsed_time, 2),' Minutes From ', min(new_dim$date), ' to ', max(new_dim$date)))
    
    ret_list <- list(all_songs,
                     dim %>% filter(is_fut == 0),
                     dim %>% filter(is_fut == 1)
    )
    # Return
    return(ret_list)
  }
  
  
}


## Run:
# Load All From Scratch
data_list <- load_all_data(start = 1986, end = 2024)

# Update Setlist Data
#data_list <- update_all_data()


# Create Tables

## Song Fact Table
fact_song <- data_list[[1]]

## Dim Historical Show (All Show Data Related To Historical Concerts)
dim_historical <- data_list[[2]]

## Dim Future Show (All Show Data Related To Future Concerts - Manual if EC Not Updated)
dim_future <- if(length(data_list[[3]]$link)>0){
  data_list[[3]]
} else {
  read_csv("./Data/20250209_PanicFutureDim - FutureDim.csv", show_col_types = FALSE) %>%
    mutate(
      date_num = as.character(date_num),
      show_notes = if_else(is.na(show_notes), "", show_notes)
    ) %>%
    filter(date >= Sys.Date())
}

## Combine All
dim_all <- rbind(dim_historical, dim_future)


# Save Tables
save_setlists <- function(song_df = fact_song, all_df = dim_all){
  # Create Paths
  start_year = min(as.numeric(substr(all_df$link, 39,42)))
  end_year = max(as.numeric(substr(all_df$link, 39,42)))
  
  fact_path = paste0('./Data/WSP_Song_Fact_Table_', start_year, "_to_", end_year, ".rds")
  dim_path = paste0('./Data/WSP_Show_Dim_Table_', start_year, "_to_", end_year, ".rds")
  
  # Save
  saveRDS(fact_song, file = fact_path)
  saveRDS(all_df, file = dim_path)
  
  # Print
  print(paste0("Setlist Data Saved To ", fact_path))
  print(paste0("Show Data Saved To ", dim_path))
}
save_setlists()



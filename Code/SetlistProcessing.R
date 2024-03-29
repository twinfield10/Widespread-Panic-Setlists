# General
library(tidyverse)
library(rvest)
library(lubridate)
library(scales)
`%notin%` = Negate(`%in%`)

# Modeling
library(xgboost)
library(pROC)
library(caret)
library(Metrics)
# Plotting
library(ggplot2)

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
  
  return(bind_rows(tour_df_list))
  
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
load_all_data <- function(start = 1986 , end = 2024) {
  
  # Load Dim Stage 1
  tour_data <- process_dim(st_yr = start, end_yr = end)
  
  # Split Historical and Future
  show_dim <- subset(dim_show, date < Sys.Date()) %>% filter(year >= start & year <= end)
  fut_dim <- subset(dim_show, date >= Sys.Date())
    
  # Peek
  print(paste0(length(show_dim$date)," Historical & ", length(fut_dim$date)," Future Shows And EDC Links Loaded - Now Loading Setlists"))
  print(tour_data %>% head())
  
  # Load Setlists
  start_time <- Sys.time()

  songs <- map_dfr(show_dim$link, process_setlist)
  
  dim_songs <- songs %>%
    group_by(link, show_notes) %>%
    mutate(n_songs = max(song_index)) %>%
    ungroup() %>%
    select(link, show_notes, n_songs) %>%
    unique()
  
  # Songs
  songs <- songs %>%
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
    select(link, set, song_index, song_name, into, song_note_detail)
  
  # Show Information
  # Slim Future
  Slim_Fut <- dim_future %>%
    select(-c(year_index, run_index, show_index, show_in_run)) %>%
    mutate(
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
      weekday = weekdays(date),
      is_fut = 0
    ) %>%
    arrange(show_index) %>%
    mutate(
      n_songs = if_else(is.na(n_songs), 0, n_songs),
      show_notes = if_else(is.na(show_notes), "", show_notes)
    ) %>%
    arrange(show_index) %>%
    filter(is_soundcheck != 1 & is_radio != 1) %>%
    select(-c(year_index, run_index, show_index, show_in_run, is_radio, is_soundcheck)) %>%
    rbind(Slim_Fut) %>%
    # Show Index
    mutate(show_index = row_number()) %>% arrange(show_index) %>%
    # Year Index
    group_by(year) %>% mutate(year_index = row_number()) %>% ungroup() %>%
    # Run Index
    mutate(run_index = 1 + cumsum(
      venue_full != lag(venue_full, default = first(venue_full)) |
        date != lag(date + 1, default = first(date)))) %>% arrange(show_index) %>%
    # Show In Run
    group_by(run_index) %>% mutate(show_in_run  = (show_index -min(show_index))+1) %>% ungroup()
    
  
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
data_list <- load_all_data(start = 1986, end = 2024)

# Create Tables
fact_song <- data_list[[1]]
dim_historical <- data_list[[2]]
dim_future <- data_list[[3]]
dim_all <- rbind(dim_historical, dim_future)

# Save Tables
saveRDS(fact_song, file = paste0("./Data/WSP_Song_FactTable_", min(as.numeric(substr(fact_song$link, 39,42))),  "_to_", max(as.numeric(substr(fact_song$link, 39,42))), ".rds"))
saveRDS(dim_historical, file = paste0("./Data/WSP_Dim_Show_Historical_", min(as.numeric(substr(dim_historical$link, 39,42))),  "_to_", max(as.numeric(substr(dim_historical$link, 39,42))), ".rds"))
saveRDS(dim_future, file = paste0("./Data/WSP_Dim_Show_Future_", min(as.numeric(substr(dim_future$link, 39,42))),  "_to_", max(as.numeric(substr(dim_future$link, 39,42))), ".rds"))

# Free Up Space
#rm(fact_song, dim_historical, dim_future)
gc()

# Function 2: Process + Clean Single Show Setlist via EveryDayCompanion

# Function 3: Loop Over process_setlist to create DataFrame
load_all_setlists <- function(st_yr = 1986 , end_yr = 2024){
  start_time <- Sys.time()
  
  # Download Calendar Using Links
  show_dim <- dim_historical %>% filter(year >= st_yr & year <= end_yr)
  data <- map_dfr(show_dim$link, process_setlist)
  
  # Mutate Table For Indexes
  data <- data %>%
    mutate(
      set_num = case_when(
        set == 'E' ~ "99",
        TRUE ~ set
      ),
      set = as.numeric(set_num),
      weekday = weekdays(date)
    ) %>%
    group_by(link, show_index) %>%
    mutate(
      min_set = min(as.numeric(set)),
      max_set = min(as.numeric(set)),
      set = if_else(set == 0 & min_set == 0 & max_set %in% c(99,0), 1, set)
    ) %>%
    select(-c(min_set, max_set, set_num)) %>%
    filter(!is.na(run_index)) %>%
    ungroup()
  
  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
  print(paste0('Successfully Loaded ',length(unique(data$link)),' Widespread Panic Shows (', nrow(data),' Total Rows/Songs) in ', round(elapsed_time, 2),' Minutes From ', min(data$year), ' to ', max(data$year)))
  print(data %>% head())
  
  # Save
  ## SAVE FILE FOR MODEL AND ANALYSIS
  saveRDS(data, file = paste0("./Data/Raw_WSP_Setlists_",st_yr,"_",end_yr,".rds"))
}

# Function 4: Update Setlist DataFrame
update_setlist <- function(yr = 2024){
  exist_df <- readRDS("./Data/Raw_WSP_Setlists_1986_2024.rds") %>% filter(year <= (yr-1))
  
  max_ri <- max(exist_df$run_index)
  max_si <- max(exist_df$show_index)
  
  new_shows <- load_tour_dates(st_yr = yr, end_yr = 2024)
  
  new_df <- map_dfr(new_shows$link, process_setlist) %>%
    mutate(
      run_index = run_index + max_ri,
      show_index = show_index + max_si
    )
  
  final_df <- rbind(exist_df, new_df) %>% unique() %>% arrange(year, month, day, run_index, show_index, set, song_index)
  
  saveRDS(final_df, file = "./Data/Raw_WSP_Setlists_1986_2024.rds")
}

########################################
## LOAD DATA INTO GENERAL ENVIRONMENT ##
########################################

# 1) Load All Show Information (Historical And Future) as Dimensional Table
dim_show <- load_tour_dates()

# 2) Separate Into Future and Historical + Remove Full DataFrame
dim_historical <- subset(dim_show, date < Sys.Date())
dim_future <- subset(dim_show, date >= Sys.Date())

# 3) Load All Setlists (Saves To RDS and CSV) **EST TIME TO LOAD = **
load_all_setlists(st_yr = 2022)


##########################
## UPDATE EXISTING DATA ##
##########################

# 1) Update Data With New Setlists (Saves To RDS and CSV)
update_setlist()





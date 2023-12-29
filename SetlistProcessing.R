library(tidyverse)
library(rvest)
library(lubridate)
`%notin%` = Negate(`%in%`)

## LOAD TOUR DATES AND VENUES ##
load_tour_dates <- function(st_yr = 1985 , end_yr = 2023) {
  base_url <- 'http://everydaycompanion.com/'
  tour_list <- as.list(st_yr:end_yr)
  tour_list <- tour_list[tour_list != 2004] # :(
  tour_df_list = list()
  
  # Function To Loop Setlist URLs
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
          (date == '00/00/86') & (venue == '** UNKNOWN **, ** UNKNOWN **, UU') ~ 'http://everydaycompanion.com/setlists/19860000a.asp',
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
          (date == '04/00/89') & (venue == 'Sigma Alpha Epsilon House, Tuscaloosa, AL') ~ 'http://everydaycompanion.com/setlists/19890500a.asp',
          (date == '05/00/89') & (venue == 'The Brewery, Raleigh, NC') ~ 'http://everydaycompanion.com/setlists/19890500a.asp',
          (date == '09/00/89') & (venue == "Edgar's Campus Bar, Clemson University, Clemson, SC") ~ 'http://everydaycompanion.com/setlists/19890900a.asp',
          (date == '10/00/89') & (venue == 'Elmo House, Charlottesville, VA') ~ 'http://everydaycompanion.com/setlists/19891000a.asp',
          (date == '00/00/90') & (venue == "Johnny D's, Somerville, MA") ~ 'http://everydaycompanion.com/setlists/19900000a.asp',
          (date == '08/00/90') & (venue == 'Excelsior Mill, Atlanta, GA') ~ 'http://everydaycompanion.com/setlists/19900800a.asp',
          (date == '00/00/91') & (venue == 'Hollins University, Roanoke, VA') ~ 'NODATA',
          TRUE ~ link
        )
      ) %>%
      filter(link != 'NODATA') %>%
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
      select(-c(venue_split, len_split))
    
    # Append the data frame to the list
    tour_df_list[[i]] <- tour_data
  }

  # Combine DataFrames From Loop List
  combined_tour_data <- bind_rows(tour_df_list) %>% arrange(year, month, day) %>% rowid_to_column('show_index')
  
  # Return Final DataFrame
  return(combined_tour_data)
}

# Create "Schedule" Table
combined_tour_data <- load_tour_dates()


## CREATE SETLIST DF ##

# Build Function For Looping Over Setlists
process_setlist <- function(setlist_link) {
  
  tryCatch({
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
          grepl("^\\d{2}/", substr(X1, 1, 3)) ~ "Details",
          TRUE ~ "Notes"
        )
      ) %>%
      rename("Raw" = 'X1') %>%
      mutate(
        Raw = case_when(
        set %in% c('0', '1', '2', '3', '4', 'E') ~ str_replace(Raw, "^.{3}", ""),
        TRUE ~ Raw
      )
      ) 
  
    songs <- setlist_raw %>%
      filter(set %notin% c('Details', 'Notes')) %>%
      separate_rows(Raw, sep = ",") %>%
      mutate(into = ifelse(grepl(" > ", Raw), 1, 0)) %>%
      separate_rows(Raw, sep = " > ") %>%
      mutate(
        song_name = toupper(trimws(Raw)),
        notes_id = str_count(song_name, "\\*"),
        song_notes_key = if_else(notes_id == 0, "", strrep("*", times = notes_id)),
        ) %>%
      rowid_to_column('song_index') %>%
      select('set', 'song_name', 'into', 'song_index', 'song_notes_key', 'notes_id')
  
    # Mutate
    songs <- mutate(songs, link = setlist_link) %>%
      left_join(combined_tour_data, by = 'link') %>%
      mutate(
        song_name = toupper(str_replace_all(song_name, "\\*", ""))
        ) %>%
      mutate(
        song_name = case_when(
          song_name %in% c('???', 'ARU/WSP JAM') ~ 'JAM',
          song_name == 'THIS MUST BE THE PLACE (NA<EF>VE MELODY)' ~ 'THIS MUST BE THE PLACE (NAIEVE MELODY)',
          song_name == 'W<CR>M' ~ 'WURM',
          TRUE ~ song_name
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
      ) %>%
      select(date, year, month, day, state, city, venue_name, set, song_name, into, show_index, year_index, song_index, song_notes_key, link, venue_full, notes_id)
    
    is_notes <- sum(songs$notes_id)
    
    # Create Notes DataFrame
    if (is_notes > 0){
    notes_str <- setlist_raw %>%
      filter(set %in% c('Notes')) %>%
      select(Raw) %>%
      pull()
    
    notes_split <- strsplit(notes_str, "(?<=[A-Za-z])\\*", perl = TRUE)[[1]]
    for (n in seq_along(notes_split)) {
      if (n == 1) {
        notes_split[n] <- notes_split[n]
      } else if (n > 1) {
        notes_split[n] <- paste0("*", notes_split[n])
      }
    }

    notes_df <- tibble('strings' = notes_split) %>%
      separate(strings, into = c("song_notes_key", "song_note_detail"), sep = " ", extra = "merge") %>%
      mutate(
        song_notes_key = trimws(song_notes_key),
        song_note_detail = toupper(trimws(song_note_detail))
      )
    }
    else{
      notes_df <- tibble() %>%
        mutate(
          song_notes_key = "",
          song_note_detail = ""
        )
    }
    
    # Join Notes To Songs
    songs <- songs %>%
      left_join(notes_df, by = c("song_notes_key")) %>%
      select(-c(song_notes_key, notes_id))
      

    return(songs)
  }, error = function(e) {
    
    tryCatch({
      # Print the error message along with setlist_link causing the error
      cat("Error processing songs in setlist_link:", setlist_link, "\n")
      cat("Error message:", conditionMessage(e), "\n")
      # Load Tour Data For Basic Show Info:
      data <- combined_tour_data %>%
        filter(link == setlist_link) %>%
        mutate(
          set = NA,
          song_name = NA,
          into = NA,
          song_notes_key = NA
        ) %>%
        select(date, year, month, day, state, city, venue_name, set, song_name, into, show_index, year_index, song_index, song_notes_key, link, venue_full)
    }, error = function(e){
      # Print the error message along with setlist_link causing the error
      cat("Error processing tour data in setlist_link:", setlist_link, "\n")
      cat("Error message:", conditionMessage(e), "\n")
      # Return a data frame with NAs to indicate the error
      return(data.frame(date = NA, year = NA, month = NA, day = NA, state = NA, city = NA, venue_name = NA, set = NA, song_name = NA, into = NA, show_index = NA, year_index = NA, song_index = NA, link = NA, venue_full = NA))
    })
  })
}

# Process each setlist link and combine the results
start_time <- Sys.time()
full_data <- map_dfr(combined_tour_data$link, process_setlist)
rm(tour_df_list)
end_time <- Sys.time()
elapsed_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
print(paste0('Successfully Loaded ',length(unique(full_data$link)),' Widespread Panic Shows (', nrow(full_data),' Total Rows/Songs) in ', round(elapsed_time, 2),' Minutes From ', min(full_data$year), ' to ', max(full_data$year)))
print(full_data %>% head())


## SAVE FILE FOR MODEL AND ANALYSIS
saveRDS(full_data, file = "Raw_WSP_Setlists_1985_2023.rds")
write_csv(full_data, file = "Raw_WSP_Setlists_1985_2023.csv")
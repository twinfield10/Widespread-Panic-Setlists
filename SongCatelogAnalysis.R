library(scales)

# Load File
full_data <- readRDS("Raw_WSP_Setlists_1985_2023.rds") %>%
  mutate(
    set = if_else(substr(set, 1, 1) == 'E', 99, as.numeric(set))
  ) %>%
  group_by(link, show_index) %>%
  mutate(
    min_set = min(as.numeric(set)),
    max_set = min(as.numeric(set)),
    set = if_else(set == 0 & min_set == 0 & max_set %in% c(99,0), 1, set)
  ) %>%
  select(-c(min_set, max_set)) %>%
  ungroup()

### PREPROCESS DATA FOR MODEL ###

# Task 1: Build LTP Input and Columns
next_show = '2024-01-19'

# Constants
tot_shows <- n_distinct(full_data$link)
tot_shows_last_6_months <- n_distinct(full_data %>% filter(difftime(next_show, date, units = "days") <= (365/2)) %>% select(link) %>% distinct())
tot_shows_last_year <- n_distinct(full_data %>% filter(difftime(next_show, date, units = "days") <= (365)) %>% select(link) %>% distinct())
tot_shows_last_2_years <- n_distinct(full_data %>% filter(difftime(next_show, date, units = "days") <= (365*2)) %>% select(link) %>% distinct())
tot_shows_last_4_years <- n_distinct(full_data %>% filter(difftime(next_show, date, units = "days") <= (365*4)) %>% select(link) %>% distinct())
tot_shows_last_10_years <- n_distinct(full_data %>% filter(difftime(next_show, date, units = "days") <= (365*102)) %>% select(link) %>% distinct())
next_show_index = max(full_data$show_index) + 1

# Song Statistics
song_catalog <- full_data %>%
  filter(song_name != '') %>%
  arrange(show_index, year_index, song_index) %>%
  mutate(
    is_last_6_months = ifelse(!is.na(date) & difftime(next_show, date, units = "days") <= (365/2) , 1, 0),
    is_last_year = ifelse(!is.na(date) & difftime(next_show, date, units = "days") <= 365 , 1, 0),
    is_last_2_years = ifelse(!is.na(date) & difftime(next_show, date, units = "days") <= 365*2 , 1, 0),
    is_last_4_years = ifelse(!is.na(date) & difftime(next_show, date, units = "days") <= 365*4 , 1, 0),
    is_last_10_years = ifelse(!is.na(date) & difftime(next_show, date, units = "days") <= 365*10 , 1, 0),
  ) %>%
  #arrange(desc(show_index)) %>%
  group_by(song_name) %>%
  mutate(
    ltp =   next_show_index - max(show_index),
    ltp_2 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 2),
    ltp_3 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 3),
    ltp_4 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 4),
    ltp_5 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 5),
    ltp_6 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 6),
    ltp_7 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 7),
    ltp_8 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 8),
    ltp_9 = next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 9),
    ltp_10= next_show_index - nth(sort(unique(show_index), decreasing = TRUE), 10)
  ) %>%
  summarise(
    n_shows_all_time = n_distinct(link),
    n_shows_last_6_months = sum(is_last_6_months),
    n_shows_last_year = sum(is_last_year),
    n_shows_last_2_years = sum(is_last_2_years),
    n_shows_last_4_years = sum(is_last_4_years),
    n_shows_last_10_years = sum(is_last_10_years),
    pct_shows_all_time = n_shows_all_time / tot_shows,
    pct_shows_last_6_months = n_shows_last_6_months / tot_shows_last_6_months,
    pct_shows_last_year = n_shows_last_year / tot_shows_last_year,
    pct_shows_last_2_years = n_shows_last_2_years / tot_shows_last_2_years,
    pct_shows_last_4_years = n_shows_last_4_years / tot_shows_last_4_years,
    pct_shows_last_10_years = n_shows_last_10_years / tot_shows_last_10_years,
    off_shelf = if_else(pct_shows_last_4_years > 0 & n_shows_all_time > 10, TRUE, FALSE),
    ltp =   max(ltp),
    ltp_2 = max(ltp_2),
    ltp_3 = max(ltp_3),
    ltp_4 = max(ltp_4),
    ltp_5 = max(ltp_5),
    ltp_6 = max(ltp_6),
    ltp_7 = max(ltp_7),
    ltp_8 = max(ltp_8),
    ltp_9 = max(ltp_9),
    ltp_10= max(ltp_10),
    diff_ltp_1_2 = ltp_2 - ltp,
    diff_ltp_2_3 = ltp_3 - ltp_2,
    diff_ltp_3_4 = ltp_4 - ltp_3,
    diff_ltp_4_5 = ltp_5 - ltp_4,
    diff_ltp_5_6 = ltp_6 - ltp_5,
    diff_ltp_6_7 = ltp_7 - ltp_6,
    diff_ltp_7_8 = ltp_8 - ltp_7,
    diff_ltp_8_9 = ltp_9 - ltp_8,
    diff_ltp_9_10 = ltp_10 - ltp_9,
    avg_ltp = sum(ltp, diff_ltp_1_2, diff_ltp_2_3, diff_ltp_3_4,
                       diff_ltp_4_5, diff_ltp_5_6, diff_ltp_6_7,
                       diff_ltp_7_8, diff_ltp_8_9, diff_ltp_9_10, na.rm = TRUE)/sum(!is.na(ltp), !is.na(diff_ltp_1_2), !is.na(diff_ltp_2_3), !is.na(diff_ltp_3_4),
                                                                                    !is.na(diff_ltp_4_5), !is.na(diff_ltp_5_6), !is.na(diff_ltp_6_7), 
                                                                                    !is.na(diff_ltp_7_8), !is.na(diff_ltp_8_9), !is.na(diff_ltp_9_10)),
    ltp_diff = abs(ltp - avg_ltp)
  ) %>%
  ungroup()

## Analyze Song Library - Check Songs With LTP Difference less than 1
song_catalog %>%
  filter(off_shelf == TRUE & n_shows_all_time > 10 & ltp_diff < 1) %>%
  mutate(
    ltp_diff = if_else(ltp_diff == 0, 0.008, ltp_diff),
    score = ((pct_shows_last_6_months * 24) + (pct_shows_last_year * 12) + (pct_shows_last_2_years * 6) + (pct_shows_all_time * 3))/ltp_diff
  ) %>%
  arrange(desc(score)) %>%
  select(song_name, pct_shows_all_time, pct_shows_last_6_months, pct_shows_last_year, pct_shows_last_2_years, ltp, avg_ltp, ltp_diff, score) %>%
  print.data.frame()

# Bust-Outs:
song_catalog %>%
  filter(ltp != 1 & ltp_diff > 1 & pct_shows_last_2_years < .10) %>%
  mutate(
    ltp_diff = if_else(ltp_diff == 0, 0.008, ltp_diff),
    score = ((pct_shows_last_6_months * 24) + (pct_shows_last_year * 12) + (pct_shows_last_2_years * 6) + (pct_shows_all_time * 3))/ltp_diff
  ) %>%
  arrange(desc(score)) %>%
  select(song_name, pct_shows_all_time, pct_shows_last_6_months, pct_shows_last_year, pct_shows_last_2_years, ltp, avg_ltp, ltp_diff, score) %>%
  head(20) %>%
  print.data.frame()

song_catalog <- song_catalog %>%
  mutate(
    score = round(rescale(log(score)+1)*100,2),
    predict_type = case_when(
      # 1) The Basics: (played at more than 20% of shows in last year and difference is < 1)
      (off_shelf == TRUE & pct_shows_last_year > .2 & ltp_diff < 1 & score > 70) | (score > 70 & ltp > 3 & ltp < 20) ~ '1_High_Confidence',
      # 2) Old Mikey Tunes Played More: (songs that have not been played a lot since Mikey passed)
      (ltp > 5 & (pct_shows_mikey_years - pct_shows_last_2_years) > .10 & ltp_diff < 5) ~ '2_Mikey_Era_Bustout',
      # 3) Bust Outs: (Songs that haven't been played in a while but LTP lines up)
      (pct_shows_all_time < 0.03 & pct_shows_mikey_years > 0 & pct_shows_last_4_years > 0 & ltp > 50 & n_shows_all_time > 20) ~ '3_Bustout',
      # 4) Rareties x HIgh Score
      (song_name %in% c('THE WAKER','SANDBOX', "TRAVELIN' MAN", "DON'T TELL THE BAND", "GALLEON")) ~ '4_Rare_But_Possible',
      # 5) Popular and not played last 3 Shows
      (score > 60 & score <= 70 & ltp_diff < 3 & ltp_diff >= 1 & ltp > 2 & pct_shows_last_6_months > .15 & overdue == 0) ~ '5_Popular_But_Early',
      TRUE ~ NA
      )
    )%>%
  arrange(desc(score)) %>%
  select(song_name, pct_shows_all_time, pct_shows_last_6_months, pct_shows_last_year, pct_shows_last_2_years, ltp, avg_ltp, ltp_diff, score, predict_type)

song_catalog %>%
   filter(!is.na(predict_type)) %>%
   arrange(desc(score)) %>%
   mutate(
     rescaled_score = round(rescale(log(score)+1)*100,2)
   ) %>%
   select(song_name, pct_shows_all_time, pct_shows_last_year, ltp, avg_ltp, ltp_diff, score, rescaled_score, predict_type) %>%
   print.data.frame()

# Task 2: Create a column for the song played before
#prepro_data <- prepro_data %>%
#  arrange(year, month, day, link, set, index) %>%
#  group_by(link) %>%
#  mutate(
#    prev_song = lag(song_name, default = "OPENER")
#  ) %>%
#  ungroup()
#
## Task 3: Create a binary column 'set_open'
#prepro_data <- prepro_data %>%
#  mutate(
#    set_open = as.integer(set != lag(set, default = first(set)))
#  )
  
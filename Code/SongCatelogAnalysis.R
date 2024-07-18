library(gt)
library(paletteer)
## Most Current Song Catelog Table with Stats ##

sell_sell_table %>%
  select(song_name, pct_shows_since_debut, pct_shows_same_city) %>%
  mutate(diff_shows_same_city = pct_shows_same_city - pct_shows_since_debut) %>%
  arrange(-diff_shows_same_city) %>%
  filter(song_name == 'CITY OF DREAMS') %>%
  head(10)

write.csv(sell_sell, './Data/RRXN1Preds.csv')

# Song Predictions Since 2021 (all_song_predictions_df)
tommy_shows <- c("2023-12-29", "2022-09-18", '2022-09-17', '2022-09-16', '2022-07-24', '2022-07-23', '2022-07-22', '2022-05-07', '2022-05-06', '2021-08-07', '2021-08-07', '2021-08-06')
# Rarest Songs
all_song_predictions_df %>%
  filter(actual == 1) %>%
  arrange(-ltp) %>%
  select(date, venue_full, song_name, ltp, pred, actual) %>%
  head(20) %>%
  print.data.frame()

# Popular Predictions Based on Prediction Vs Since Debut
RRX_N1 %>%
  filter(n_shows_all_time > 10 & pred <= 0.1) %>%
  mutate(
    pred_diff = pred - pct_shows_since_debut,
    pred_diff_pct = pred_diff / pct_shows_since_debut
  ) %>%
  select(song_name, n_shows_all_time, ltp, pred, pct_shows_since_debut, pred_diff, pred_diff_pct) %>%
  arrange(-pred_diff_pct) %>%
  head(10)

# By Same Day
all_song_predictions_df %>%
  mutate(
    weekday = weekdays(date)
  ) %>%
  group_by(song_name, weekday) %>%
  summarise(
    day_pred = mean(pred),
    day_cnt = sum(actual)
  ) %>%
  ungroup() %>%
  filter(day_cnt > 1) %>%
  rbind(
    all_song_predictions_df %>%
      mutate(
        weekday = weekdays(date)
      ) %>%
      group_by(song_name) %>%
      summarise(
        weekday = "Total",
        day_pred = mean(pred),
        day_cnt = n()
      ) %>%
      ungroup() %>%
      filter(day_cnt > 1)
  ) %>%
  select(song_name, weekday, day_pred, day_cnt) %>%
  pivot_wider(id_cols = song_name, names_from = weekday, values_from = c(day_pred, day_cnt)) %>%
  mutate(
    Diff_Friday = day_pred_Friday - day_pred_Total,
    Diff_Monday = day_pred_Monday - day_pred_Total,
    Diff_Saturday = day_pred_Saturday - day_pred_Total,
    Diff_Sunday = day_pred_Sunday - day_pred_Total,
    Diff_Thursday = day_pred_Thursday - day_pred_Total,
    Diff_Tuesday = day_pred_Tuesday - day_pred_Total,
    Diff_Wednesday = day_pred_Wednesday - day_pred_Total,
    
    Pct_Friday = Diff_Friday/day_pred_Total,
    Pct_Monday = Diff_Monday/day_pred_Total,
    Pct_Saturday = Diff_Saturday/day_pred_Total,
    Pct_Sunday = Diff_Sunday/day_pred_Total,
    Pct_Thursday = Diff_Thursday/day_pred_Total,
    Pct_Tuesday = Diff_Tuesday/day_pred_Total,
    Pct_Wednesday = Diff_Wednesday/day_pred_Total
  ) %>%
  select(song_name, ends_with('_Thursday'), ends_with("Total")) %>%
  arrange(-Pct_Thursday) %>%
  head(10)
  

# Weirdly Popular Relative To Previous Scores
all_song_predictions_df %>%
  group_by(song_name) %>%
  summarise(
    played = sum(actual),
    mean = mean(pred),
    median = median(pred),
    std = sd(pred)
  ) %>%
  ungroup() %>%
  left_join(RRX_N1 %>%
              select(song_name, pred) %>%
              rename(curr_pred = pred), by = c('song_name')) %>%
  mutate(
    diff_mean = curr_pred - mean,
    diff_median = curr_pred - median,
    stds_above = diff_mean / std,
    is_2std = if_else(curr_pred > ((2*std) + mean), 1, 0),
    is_1std = if_else(curr_pred > ((std) + mean), 1, 0)
  ) %>%
  arrange(-stds_above, -diff_mean) %>%
  filter(played > 5 & mean < .07) %>%
  head(10)

keep_overall_cols <- c('song_name', 'pred', 'pct_shows_since_debut', 'pct_shows_all_time',
                       'pct_shows_mikey_years', 'pct_shows_jimmy_years', 'diff_jimmy_mikey_shows',
                       'pct_shows_same_venue', 'diff_shows_same_venue',
                       'pct_shows_same_day', 'diff_shows_same_day',
                       'ltp', 'ltp_2', 'ltp_3', 'avg_ltp', 'ltp_diff', 'ltp_ratio',
                       'raw_score', 'raw_run_score', 'overdue_run_metric')

keep_diff_cols <- c('song_name', 'pred', 'pct_shows_since_debut',
                    'pct_shows_same_state', 'diff_shows_same_state',
                    'pct_shows_same_city', 'diff_shows_same_city',
                    'pct_shows_same_venue', 'diff_shows_same_venue',
                    'pct_shows_same_day', 'pct_shows_same_day',
                    'pct_shows_same_in_run', 'pct_shows_same_in_run')


# Pretty Table Function
build_next_show_table <- function(data = sell_sell, n_preds = 10){
  show_date <- unique(data$date)
  show_city <- unique(data$city)
  show_venue <- unique(data$venue_full)
  show_state <- unique(data$state)
  show_sir <- unique(data$show_in_run)
  
  lab <- "Widespread Panic Setlist Predictions"
  sub_lab <- paste0("Predictions for ", show_date, " @ ", show_venue, " (N",show_sir,")")
  
  df <- data %>% arrange(-pred) %>% select(all_of(keep_overall_cols)) %>% head(n_preds)
  
  gt_obj <- df %>%
    gt() %>%
    # Spanners
    tab_spanner(
      label = "Song Frequency (% of Shows)",
      columns = c(pct_shows_since_debut, pct_shows_all_time,
                  pct_shows_mikey_years, pct_shows_jimmy_years, diff_jimmy_mikey_shows,
                  pct_shows_same_venue, diff_shows_same_venue,
                  pct_shows_same_day, diff_shows_same_day),
      id = "FREQ"
    ) %>%
    tab_spanner(
      label = "Last Time Played",
      columns = c(ltp, ltp_2, ltp_3, avg_ltp, ltp_diff, ltp_ratio),
      id = 'LTP'
    ) %>%
    tab_spanner(
      label = "Metrics",
      columns = c(raw_score, raw_run_score, overdue_run_metric),
      id = "METRICS"
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("right", "left", "bottom"),
          color = "#333F48",
          weight = px(2)
        ),
        cell_text(
          weight = "bold",
          style = "italic",
          color = "white"
        ),
        cell_fill(
          color = "#BF5700"
        )
      ),
      locations = cells_column_spanners(spanners = c("FREQ", "LTP", "METRICS"))
    ) %>%
    # Song + Pred Columns Format
    tab_style(
      locations = cells_body(columns = c(song_name)),
      style = list(
        css(
          text_align = "left",
          font_weight = "bold",
          border_right = "2px solid #333F48"
          )
        )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(pred)),
      style = list(
        css(
          text_align = "center",
          font_weight = "bold",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    # Song Frequency Columns
    tab_style(
      locations = cells_body(columns = c(pct_shows_since_debut,pct_shows_all_time,pct_shows_mikey_years,pct_shows_jimmy_years,diff_jimmy_mikey_shows,
                                         pct_shows_same_venue, diff_shows_same_venue,
                                         pct_shows_same_day, diff_shows_same_day)),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(diff_jimmy_mikey_shows,
                                         diff_shows_same_venue,
                                         diff_shows_same_day,
                                         ltp_ratio)),
      style = list(
        css(
          text_align = "center",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    # LTP Columns
    tab_style(
      locations = cells_body(columns = c(ltp, ltp_2, ltp_3, avg_ltp, ltp_diff)),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    # Metric Columns
    tab_style(
      locations = cells_body(columns = c(raw_score, raw_run_score, overdue_run_metric)),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    # Number Format
    fmt_percent(
      columns = c(pct_shows_since_debut,pct_shows_all_time,pct_shows_mikey_years,pct_shows_jimmy_years,diff_jimmy_mikey_shows,
                  pct_shows_same_venue, diff_shows_same_venue,
                  pct_shows_same_day, diff_shows_same_day,
                  pred),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(avg_ltp,ltp_diff),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(ltp_ratio, raw_score,	raw_run_score,	overdue_run_metric),
      decimals = 2
    ) %>%
    # Column Labels
    tab_style(
      locations = cells_column_labels(),
      style = list(
        css(
          color = "black",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black"
        )
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(song_name, pred)),
      style = list(
        css(
          color = "white",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black",
          border_right = "2px solid #333F48",
          background_color = "#BF5700"
        )
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(diff_jimmy_mikey_shows,
                                                  diff_shows_same_venue,
                                                  diff_shows_same_day,
                                                  ltp_ratio, overdue_run_metric)),
      style = list(
        css(
          color = "black",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    # Color Scales
    data_color(
      columns = c(pred),
      fn = scales::col_numeric(
        palette = colorRamp(c("#BCCFB4", "#09622A"), interpolate="spline"),
        domain = c(0,0.85)
      )
    ) %>%
    data_color(
      columns = c(diff_shows_same_venue,
                  diff_shows_same_day),
      fn = scales::col_numeric(
        palette = colorRamp(c('#B70005FF', '#EA332FFF', '#EF6A63FF', '#F8BEB0FF','#FFFFFF', '#CDE1C2FF','#55974CFF', '#287A22FF', '#17692CFF'), interpolate="spline"),
        domain = c(-0.2,0.2)
      )
    ) %>%
    # Header
    tab_header(
      title = md(
        paste0("<span style='color:white'>**",lab,"**</style>")
      ),
      subtitle = md(
        paste0("<span style='color:white'>*",sub_lab,"*</style>")
      )
    ) %>%
    # Rename Labels
    cols_label(
      song_name = "Song",
      pct_shows_since_debut = "Since Debut",
      pct_shows_all_time = "All Time",
      pct_shows_mikey_years = "Mikey Shows",
      pct_shows_jimmy_years = "Jimmy Shows",
      diff_jimmy_mikey_shows = "Jimmy - Mikey",
      pct_shows_same_venue = "Venue",
      diff_shows_same_venue = "+/-",
      pct_shows_same_day = "Day",
      diff_shows_same_day = "+/-",
      ltp = "1",
      ltp_2 = "2",
      ltp_3  = "3",
      avg_ltp = "AVG",
      ltp_diff = "LTP-AVG",
      ltp_ratio = "LTP/AVG",
      raw_score = "Show Score",
      raw_run_score = "Run Score",
      overdue_run_metric = "Overdue Score",
      pred = "Prediction"
    ) %>%
    tab_options(
      table.border.top.style = "solid",
      table.border.top.width = "3px",
      table.border.top.color = "#333F48",
      table.border.right.style = "solid",
      table.border.right.width = "3px",
      table.border.right.color = "#333F48",
      table.border.bottom.style = "solid",
      table.border.bottom.width = "3px",
      table.border.bottom.color = "#333F48",
      table.border.left.style = "solid",
      table.border.left.width = "3px",
      table.border.left.color = "#333F48",
      heading.background.color = "#BF5700"
    )
  
  return(gt_obj)
    
}
build_next_show_table(data = RRX_N2)

build_result_show_table <- function(data = sell_sell, n_preds = 30){
  show_date <- unique(data$date)
  show_city <- unique(data$city)
  show_venue <- unique(data$venue_full)
  show_state <- unique(data$state)
  show_sir <- unique(data$show_in_run)
  show_idx <- unique(data$show_index)
  
  lab <- "Widespread Panic Setlist Predictions"
  sub_lab <- paste0("Predictions for ", show_date, " @ ", show_venue, " (N",show_sir,")")
  
  correct <- model_data %>% filter(show_index == show_idx) %>% pull(song_name)
  
  df <- data %>% filter(song_name %in% correct) %>% arrange(-pred) %>% select(all_of(keep_overall_cols)) %>% head(n_preds)
  
  
  gt_obj <- df %>%
    gt() %>%
    # Spanners
    tab_spanner(
      label = "Song Frequency (% of Shows)",
      columns = c(pct_shows_since_debut, pct_shows_all_time,
                  pct_shows_mikey_years, pct_shows_jimmy_years, diff_jimmy_mikey_shows,
                  pct_shows_same_venue, diff_shows_same_venue,
                  pct_shows_same_day, diff_shows_same_day),
      id = "FREQ"
    ) %>%
    tab_spanner(
      label = "Last Time Played",
      columns = c(ltp, ltp_2, ltp_3, avg_ltp, ltp_diff, ltp_ratio),
      id = 'LTP'
    ) %>%
    tab_spanner(
      label = "Metrics",
      columns = c(raw_score, raw_run_score, overdue_run_metric),
      id = "METRICS"
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("right", "left", "bottom"),
          color = "#333F48",
          weight = px(2)
        ),
        cell_text(
          weight = "bold",
          style = "italic",
          color = "white"
        ),
        cell_fill(
          color = "#BF5700"
        )
      ),
      locations = cells_column_spanners(spanners = c("FREQ", "LTP", "METRICS"))
    ) %>%
    # Song + Pred Columns Format
    tab_style(
      locations = cells_body(columns = c(song_name)),
      style = list(
        css(
          text_align = "left",
          font_weight = "bold",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(song_name), rows = (df$song_name %in% correct)),
      style = list(
        css(
          background_color = "#1A8D4D",
          font_weight = "bold",
          text_align = "left",
          color = '#FFFFFF',
          border_right = "2px solid #333F48"
          )
        )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(pred)),
      style = list(
        css(
          text_align = "center",
          font_weight = "bold",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    # Song Frequency Columns
    tab_style(
      locations = cells_body(columns = c(pct_shows_since_debut,pct_shows_all_time,pct_shows_mikey_years,pct_shows_jimmy_years,diff_jimmy_mikey_shows,
                                         pct_shows_same_venue, diff_shows_same_venue,
                                         pct_shows_same_day, diff_shows_same_day)),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(diff_jimmy_mikey_shows,
                                         diff_shows_same_venue,
                                         diff_shows_same_day,
                                         ltp_ratio)),
      style = list(
        css(
          text_align = "center",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    # LTP Columns
    tab_style(
      locations = cells_body(columns = c(ltp, ltp_2, ltp_3, avg_ltp, ltp_diff)),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    # Metric Columns
    tab_style(
      locations = cells_body(columns = c(raw_score, raw_run_score, overdue_run_metric)),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    # Number Format
    fmt_percent(
      columns = c(pct_shows_since_debut,pct_shows_all_time,pct_shows_mikey_years,pct_shows_jimmy_years,diff_jimmy_mikey_shows,
                  pct_shows_same_venue, diff_shows_same_venue,
                  pct_shows_same_day, diff_shows_same_day,
                  pred),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(avg_ltp,ltp_diff),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(ltp_ratio, raw_score,	raw_run_score,	overdue_run_metric),
      decimals = 2
    ) %>%
    # Column Labels
    tab_style(
      locations = cells_column_labels(),
      style = list(
        css(
          color = "black",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black"
        )
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(song_name, pred)),
      style = list(
        css(
          color = "white",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black",
          border_right = "2px solid #333F48",
          background_color = "#BF5700"
        )
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(diff_jimmy_mikey_shows,
                                                  diff_shows_same_venue,
                                                  diff_shows_same_day,
                                                  ltp_ratio, overdue_run_metric)),
      style = list(
        css(
          color = "black",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black",
          border_right = "2px solid #333F48"
        )
      )
    ) %>%
    # Color Scales
    data_color(
      columns = c(pred),
      fn = scales::col_numeric(
        palette = colorRamp(c("#BCCFB4", "#09622A"), interpolate="spline"),
        domain = c(0,0.85)
      )
    ) %>%
    data_color(
      columns = c(diff_shows_same_venue,
                  diff_shows_same_day),
      fn = scales::col_numeric(
        palette = colorRamp(c('#B70005FF', '#EA332FFF', '#EF6A63FF', '#F8BEB0FF','#FFFFFF', '#CDE1C2FF','#55974CFF', '#287A22FF', '#17692CFF'), interpolate="spline"),
        domain = c(-0.2,0.2)
      )
    ) %>%
    # Header
    tab_header(
      title = md(
        paste0("<span style='color:white'>**",lab,"**</style>")
      ),
      subtitle = md(
        paste0("<span style='color:white'>*",sub_lab,"*</style>")
      )
    ) %>%
    # Rename Labels
    cols_label(
      song_name = "Song",
      pct_shows_since_debut = "Since Debut",
      pct_shows_all_time = "All Time",
      pct_shows_mikey_years = "Mikey Shows",
      pct_shows_jimmy_years = "Jimmy Shows",
      diff_jimmy_mikey_shows = "Jimmy - Mikey",
      pct_shows_same_venue = "Venue",
      diff_shows_same_venue = "+/-",
      pct_shows_same_day = "Day",
      diff_shows_same_day = "+/-",
      ltp = "1",
      ltp_2 = "2",
      ltp_3  = "3",
      avg_ltp = "AVG",
      ltp_diff = "LTP-AVG",
      ltp_ratio = "LTP/AVG",
      raw_score = "Show Score",
      raw_run_score = "Run Score",
      overdue_run_metric = "Overdue Score",
      pred = "Prediction"
    ) %>%
    tab_options(
      table.border.top.style = "solid",
      table.border.top.width = "3px",
      table.border.top.color = "#333F48",
      table.border.right.style = "solid",
      table.border.right.width = "3px",
      table.border.right.color = "#333F48",
      table.border.bottom.style = "solid",
      table.border.bottom.width = "3px",
      table.border.bottom.color = "#333F48",
      table.border.left.style = "solid",
      table.border.left.width = "3px",
      table.border.left.color = "#333F48",
      heading.background.color = "#BF5700"
    )
  
  return(gt_obj)
  
}
build_result_show_table(data = RRX_N1)

build_location_table <- function(data = sell_sell, n_preds = 15){
  show_date <- unique(data$date)
  show_city <- unique(data$city)
  show_venue <- unique(data$venue_full)
  show_state <- unique(data$state)
  show_sir <- unique(data$show_in_run)
  
  lab <- "Widespread Panic Setlist Predictions - Location Data"
  sub_lab <- paste0("Predictions for ", format(show_date, "%A, %B %d, %Y"), " @ ", show_venue, " (N",show_sir,")")
  
  grp_df <- all_song_predictions_df %>%
    filter(city != "MORRISON") %>%
    group_by(song_name) %>%
    summarise(
      t_shows = n(),
      played = sum(actual),
      mean = mean(pred),
      median = median(pred),
      std = sd(pred)
    ) %>%
    ungroup() %>%
    left_join(sell_sell %>% select(all_of(keep_diff_cols)) %>% rename(curr_pred = pred), by = c('song_name')) %>%
    mutate(
      diff_mean = curr_pred - mean,
      diff_median = curr_pred - median,
      stds_above = diff_mean / std,
      is_2std = if_else(curr_pred > ((2*std) + mean), 1, 0),
      is_1std = if_else(curr_pred > ((std) + mean), 1, 0)
    ) %>%
    arrange(-diff_shows_same_venue) %>%
    #filter(played > 5 & mean < .07) %>%
    select(-t_shows, -played, diff_mean, diff_median, std, is_2std, is_1std) %>%
    head(10) %>%
    print.data.frame()
  
  df <- data %>% select(all_of(keep_diff_cols)) %>% arrange() %>% head(n_preds)
}
build_location_table()
library(gt)
library(paletteer)
## Most Current Song Catelog Table with Stats ##

sell_sell_table %>%
  head(5)

# Song Predictions Since 2021 (all_song_predictions_df)
tommy_shows <- c("2023-12-29", "2022-09-18", '2022-09-17', '2022-09-16', '2022-07-24', '2022-07-23', '2022-07-22', '2022-05-07', '2022-05-06', '2021-08-07', '2021-08-07', '2021-08-06')
# Rarest Songs
all_song_predictions_df %>%
  filter(actual == 1 & date %in% tommy_shows) %>%
  arrange(-ltp) %>%
  select(date, venue_full, song_name, ltp, pred, actual) %>%
  head(20) %>%
  print.data.frame()

# Weirdly Popular Relative To Previous Scores
all_song_predictions_df %>%
  group_by(song_name) %>%
  summarise(
    t_shows = n(),
    played = sum(actual),
    mean = mean(pred),
    median = median(pred),
    std = sd(pred)
  ) %>%
  ungroup() %>%
  left_join(sell_sell %>% select(song_name, pred) %>% rename(curr_pred = pred), by = c('song_name')) %>%
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
                       'ltp', 'ltp_2', 'ltp_3', 'avg_ltp', 'ltp_diff', 'ltp_ratio',
                       'raw_score', 'raw_run_score', 'overdue_run_metric')

keep_diff_cols <- c('song_name', 'pct_shows_since_debut',
                    'pct_shows_same_state', 'diff_shows_same_state',
                    'pct_shows_same_city', 'pct_shows_same_city',
                    'pct_shows_same_venue', 'pct_shows_same_venue',
                    'pct_shows_same_day', 'pct_shows_same_day',
                    'pct_shows_same_in_run', 'pct_shows_same_in_run',
                    'pred'
                    )


# Pretty Table Function
build_next_show_table <- function(data = sell_sell, n_preds = 22){
  show_date <- unique(data$date)
  show_city <- unique(data$city)
  show_venue <- unique(data$venue_full)
  show_state <- unique(data$state)
  show_sir <- unique(data$show_in_run)
  
  lab <- "Widespread Panic Setlist Predictions"
  sub_lab <- paste0("Predictions for ", format(show_date, "%A, %B %d, %Y"), " @ ", show_venue, " (N",show_sir,")")
  
  df <- data %>% select(all_of(keep_overall_cols)) %>% head(n_preds)
  
  gt_obj <- df %>%
    gt() %>%
    # Spanners
    tab_spanner(
      label = "Song Frequency (% of Shows)",
      columns = c(pct_shows_since_debut, pct_shows_all_time, pct_shows_mikey_years, pct_shows_jimmy_years, diff_jimmy_mikey_shows),
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
          color = "#BBBBBB",
          weight = px(2)
        ),
        cell_text(
          weight = "bold",
          style = "italic",
          color = "white"
        ),
        cell_fill(
          color = "#2F4985"
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
          border_right = "2px solid #BBBBBB"
          )
        )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(pred)),
      style = list(
        css(
          text_align = "center",
          font_weight = "bold",
          border_right = "2px solid #BBBBBB"
        )
      )
    ) %>%
    # Song Frequency Columns
    tab_style(
      locations = cells_body(columns = c(pct_shows_since_debut,pct_shows_all_time,pct_shows_mikey_years,pct_shows_jimmy_years,diff_jimmy_mikey_shows)),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(diff_jimmy_mikey_shows)),
      style = list(
        css(
          text_align = "center",
          border_right = "2px solid #BBBBBB"
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
    tab_style(
      locations = cells_body(columns = c(ltp_ratio)),
      style = list(
        css(
          text_align = "center",
          border_right = "2px solid #BBBBBB"
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
      columns = c(pct_shows_since_debut,pct_shows_all_time,pct_shows_mikey_years,pct_shows_jimmy_years,diff_jimmy_mikey_shows, pred),
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
          border_right = "2px solid #BBBBBB",
          background_color = "#2F4985"
        )
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(diff_jimmy_mikey_shows, ltp_ratio, overdue_run_metric)),
      style = list(
        css(
          color = "black",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black",
          border_right = "2px solid #BBBBBB"
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
      diff_jimmy_mikey_shows = "Jimmy - Mikey Diff",
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
      table.border.top.color = "#BBBBBB",
      table.border.right.style = "solid",
      table.border.right.width = "3px",
      table.border.right.color = "#BBBBBB",
      table.border.bottom.style = "solid",
      table.border.bottom.width = "3px",
      table.border.bottom.color = "#BBBBBB",
      table.border.left.style = "solid",
      table.border.left.width = "3px",
      table.border.left.color = "#BBBBBB",
      heading.background.color = "#001C5D"
    )
  
  return(gt_obj)
    
}
build_next_show_table()
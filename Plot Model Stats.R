## Plot Metrics ##



library(ggplot2)

# Plot the distribution of 'pred'
all_song_predictions_df <- all_song_predictions_df %>%
  mutate(
    pred_class = if_else(pred >= 0.5, "1_High_Confidence",
                         if_else(pred >= 0.349 & pred < 0.5, "2_Medium_Confidence", 
                                 if_else(pred >= 0.205 & pred < 0.349, "3_Low_Confidence", NA)))
  )

all_song_predictions_df %>%
  group_by(pred_class) %>%
  summarise(
    count = n(),
    mean_pred = mean(pred),
    mean_played = mean(actual),
    sum_pred = sum(pred),
    sum_actual = sum(actual)
  )
ggplot(all_song_predictions_df %>% mutate(quartile = cut(rescale(pred), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c(1, 2, 3, 4), include.lowest = TRUE))
       ,aes(x = log_pred)) +
  geom_density(fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Song Prediction Score", x = "Predicted Probability", y = "Frequency") +
  theme_minimal() +
  facet_wrap(~quartile, ncol = 4)



ggplot(acc_metrics, aes(x = Lab, y = precision)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Precision Over Time", x = "Date", y = "Precision") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 80, hjust = 1))

ggplot(acc_metrics, aes(x = as.factor(date), y = precision)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Precision Over Time", x = "Date", y = "Precision") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 80, hjust = 1)) +
  facet_wrap(~city, scale = "free_x")
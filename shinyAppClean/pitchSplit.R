library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

get_pitch_type_percentages <- function(pitch_data) {
  pitch_data <- pitch_data %>%
    filter(!is.na(balls) & !is.na(strikes) & !is.na(auto_pitch_type))
  
  if (nrow(pitch_data) == 0) {
    return(data.frame(Pitch_Type = character(0), Count = character(0), Percentage = numeric(0)))
  }
  
  pitch_data$count <- paste(pitch_data$balls, "-", pitch_data$strikes)
  
  # pitch_summary <- pitch_data %>%
  #   group_by(count, auto_pitch_type) %>%
  #   summarise(pitch_count = n(), .groups = "drop") %>%
  #   spread(key = auto_pitch_type, value = pitch_count, fill = 0) %>%
  #   mutate(across(-count, ~ . / sum(.) * 100)) %>%
  #   arrange(count)
  
  pitch_summary <- pitch_data %>%
    group_by(count, auto_pitch_type) %>%
    summarise(pitch_count = n(), .groups = "drop") %>%
    # Summarize the total number of pitches for each count (combination of Balls-Strikes)
    group_by(count) %>%
    mutate(total_pitches_in_count = sum(pitch_count)) %>%
    ungroup() %>%
    # Spread data so each pitch type becomes a separate column
    spread(key = auto_pitch_type, value = pitch_count, fill = 0) %>%
    # Calculate the percentage for each pitch type within the count
    mutate(across(-count & -total_pitches_in_count, ~ round(. / total_pitches_in_count * 100, 2))) %>%
    arrange(count) %>%
    select(-total_pitches_in_count)  # Optional: Remove the temporary total_pitches_in_count column
  
  return(pitch_summary)
  
}
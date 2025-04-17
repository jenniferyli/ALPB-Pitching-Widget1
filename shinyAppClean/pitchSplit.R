library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

# get_pitch_type_percentages <- function(pitch_data) {
#   pitch_data <- pitch_data %>%
#     filter(!is.na(balls) & !is.na(strikes) & !is.na(auto_pitch_type))
#   
#   if (nrow(pitch_data) == 0) {
#     return(data.frame(Pitch_Type = character(0), Count = character(0), Percentage = numeric(0)))
#   }
#   
#   pitch_data$Count <- paste(pitch_data$balls, "-", pitch_data$strikes)
#   
#   
#   pitch_summary <- pitch_data %>%
#     group_by(Count, auto_pitch_type) %>%
#     summarise(pitch_count = n(), .groups = "drop") %>%
#     group_by(Count) %>%
#     mutate(total_pitches_in_count = sum(pitch_count)) %>%
#     ungroup() %>%
#     spread(key = auto_pitch_type, value = pitch_count, fill = 0) %>%
#     mutate(across(-Count & -total_pitches_in_count, ~ round(. / total_pitches_in_count * 100, 1))) %>%
#     arrange(Count) %>%
#     select(-total_pitches_in_count)  
#   
#   return(pitch_summary)
#   
# }

get_pitch_type_percentages <- function(pitch_data, tag) {
  pitch_data <- pitch_data %>%
    filter(!is.na(balls) & !is.na(strikes) & !is.na(.data[[tag]]) & .data[[tag]] != "Undefined")
  
  if (nrow(pitch_data) == 0) {
    return(data.frame(Pitch_Type = character(0), Count = character(0), Percentage = numeric(0)))
  }
  
  pitch_data$Count <- paste(pitch_data$balls, "-", pitch_data$strikes)
  
  
  pitch_summary <- pitch_data %>%
    group_by(Count, .data[[tag]]) %>%
    summarise(pitch_count = n(), .groups = "drop") %>%
    group_by(Count) %>%
    mutate(total_pitches_in_count = sum(pitch_count)) %>%
    ungroup() %>%
    spread(key = .data[[tag]], value = pitch_count, fill = 0) %>%
    mutate(across(-Count & -total_pitches_in_count, ~ round(. / total_pitches_in_count * 100, 1))) %>%
    arrange(Count) %>%
    select(-total_pitches_in_count)  
  
  return(pitch_summary)
  
}
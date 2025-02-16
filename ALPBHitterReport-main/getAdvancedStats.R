library(dplyr)

# function that takes in selected player data and calculates/returns the advanced swing and launch stats

get_advanced_stats <- function(data, percentile_data, id, throws, date_low, date_high, include_percentiles = TRUE) {
  
  # if there is no data present, return a blank table
  
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  # manipulate the data so that we have useful flags for calculating the advanced stats
  
  batter_data <- data %>%
    filter(BatterId == id) %>%
    filter(PitcherThrows == throws) %>%
    filter(Date >= date_low & Date <= date_high) %>%
    dplyr::select(BatterId, BatterSide, Balls, Strikes, Outs, PitchCall, KorBB, PlayResult, TaggedHitType, PlateLocHeight, PlateLocSide, ExitSpeed, Angle, Direction) %>%
    mutate(
      is_swing = ifelse(PitchCall %in% c("InPlay", "FoulBallNotFieldable", "StrikeSwinging", "FoulBallFieldable"), TRUE, FALSE),
      is_miss = ifelse(PitchCall == "StrikeSwinging", TRUE, FALSE),
      is_strike = ifelse((PlateLocHeight >= 1.5) & (PlateLocHeight <= 3.5) & (PlateLocSide >= -(10/12)) & (PlateLocSide <= (10/12)), TRUE, FALSE),
      is_in_play = ifelse(PitchCall == "InPlay", TRUE, FALSE)
    ) %>%
    filter(!(is_in_play & is.na(ExitSpeed))) %>%
    filter(!is.na(PlateLocHeight))

  # aggregate the flags and metrics to calculate the swing and launch advanced stats that we need
  # NOTE THE AS.CHARACTER CASTS SO THAT THE PERCENTILES ROW CAN WORK PROPERLY IF YOU NEED TO DO
  # FURTHER CALCULATIONS WITH THESE NUMBERS, THEY SHOULD BE BACK IN THE FORM OF NUMERICS
  
  advanced_stats_data <- batter_data %>%
    group_by(BatterId) %>%
    summarise(
      balls_in_play = as.character(sum(is_in_play)),
      whiff_pct = as.character(round(100 * sum(is_miss, na.rm = T) / sum(is_swing, na.rm = T), 1)),
      chase_pct = as.character(round(100 * sum(ifelse(is_swing & !is_strike, 1, 0), na.rm = T) / sum(!is_strike, na.rm = T), 1)),
      z_swing = as.character(round(100 * sum(ifelse(is_swing & is_strike, 1, 0), na.rm = T) / sum(is_strike, na.rm = T), 1)),
      fp_swing = as.character(round(100 * sum(ifelse(is_swing & (Balls == 0) & (Strikes == 0), 1, 0), na.rm = T) / n(), 1)),
      avg_ev = as.character(round(ifelse(sum(is_in_play) == 0, NA, mean(ExitSpeed[is_in_play], na.rm = TRUE)), 1)),
      max_ev = as.character(round(ifelse(sum(is_in_play) == 0, NA, max(ExitSpeed[is_in_play], na.rm = TRUE)), 1)),
      nth_ev = round(ifelse(sum(is_in_play) == 0, NA, quantile(ExitSpeed[is_in_play], probs = 0.9, na.rm = TRUE)), 1),
      med_la = as.character(round(ifelse(sum(is_in_play) == 0, NA, median(Angle[is_in_play], na.rm = TRUE)), 1)),
      ground_ball_pct = round(100 * sum(ifelse(TaggedHitType[is_in_play] == "GroundBall", 1, 0), na.rm = TRUE) / sum(is_in_play, na.rm = T), 1),
      line_drive_pct = round(100 * sum(ifelse(TaggedHitType[is_in_play] == "LineDrive", 1, 0), na.rm = TRUE) / sum(is_in_play, na.rm = T), 1),
      pop_up_pct = round(100 * sum(ifelse(TaggedHitType[is_in_play] == "Popup", 1, 0), na.rm = TRUE) / sum(is_in_play, na.rm = T), 1),
      fly_ball_pct = round(100 * sum(ifelse(TaggedHitType[is_in_play] == "FlyBall", 1, 0), na.rm = TRUE) / sum(is_in_play, na.rm = T), 1),
      pull_pct = round(100 * sum(ifelse((Direction[is_in_play] >= -45) & (Direction[is_in_play] < -15), 1, 0), na.rm = TRUE) / sum(is_in_play, na.rm = T), 1),
      center_pct = round(100 * sum(ifelse((Direction[is_in_play] >= -15) & (Direction[is_in_play] <= 15), 1, 0), na.rm = TRUE) / sum(is_in_play, na.rm = T), 1),
      oppo_pct = round(sum(100 * ifelse((Direction[is_in_play] > 15) & (Direction[is_in_play] <= 45), 1, 0), na.rm = TRUE) / sum(is_in_play, na.rm = T), 1)
    )
  
  if (include_percentiles) {
    
    # pull the corresponding data for swing stats from the big dataframe
    # and get the percentile data so that it can be binded to the bottom row
    
    swing_percentile_data <- percentile_data %>%
      filter(BatterId == id) %>%
      dplyr::select(-BatterId, -max_ev, -avg_ev)
    
    swing_stats <- advanced_stats_data %>%
      dplyr::select(whiff_pct, chase_pct, z_swing, fp_swing) %>%
      bind_rows(swing_percentile_data) %>%
      dplyr::rename(
        `Whiff %` = whiff_pct,
        `Chase %` = chase_pct,
        `Z-Swing %` = z_swing,
        `FP-Swing %` = fp_swing
      )
    
    # pull the corresponding data for launch stats from the big dataframe
    # and get the percentile data so that it can be binded to the bottom row
    
    launch_percentile_data <- percentile_data %>%
      filter(BatterId == id) %>%
      mutate(
        med_la = "",
        balls_in_play = "",
      ) %>%
      dplyr::select(-BatterId, -whiff_pct, -chase_pct, -z_swing, -fp_swing)
    
    launch_stats <- advanced_stats_data %>%
      dplyr::select(max_ev, avg_ev, med_la, balls_in_play) %>%
      bind_rows(launch_percentile_data) %>%
      dplyr::rename(
        `Max EV` = max_ev,
        `Avg EV` = avg_ev,
        `Med LA` = med_la,
        `Balls in Play` = balls_in_play
      )
    
  } else {
    
    # pull the corresponding data for swing stats from the big dataframe
    # but without the percentile stats
    
    swing_stats <- advanced_stats_data %>%
      dplyr::select(whiff_pct, chase_pct, z_swing, fp_swing) %>%
      dplyr::rename(
        `Whiff %` = whiff_pct,
        `Chase %` = chase_pct,
        `Z-Swing %` = z_swing,
        `FP-Swing %` = fp_swing
      )
    
    # pull the corresponding data for launch stats from the big dataframe
    # but without the percentile stats
    
    launch_stats <- advanced_stats_data %>%
      dplyr::select(max_ev, avg_ev, med_la, balls_in_play) %>%
      dplyr::rename(
        `Max EV` = max_ev,
        `Avg EV` = avg_ev,
        `Med LA` = med_la,
        `Balls in Play` = balls_in_play
      )
    
  }
  
  
  # launch_stats <- advanced_stats_data %>%
  #   dplyr::select(max_ev, avg_ev, med_la, balls_in_play) %>%
  #   dplyr::rename(
  #     `Max EV` = max_ev,
  #     `Avg EV` = avg_ev,
  #     `Med LA` = med_la,
  #     `Balls in Play` = balls_in_play
  #   )
  
  # launch_percentile_row <- get_sliders(get_trackman_data(), "max_ev", )
  
  # if we later want to build out more stratified and specific table, we can use the following:
  
  # ev_stats <- advanced_stats_data %>%
  #   dplyr::select(max_ev, avg_ev, nth_ev, balls_in_play) %>%
  #   dplyr::rename(
  #     `Max EV` = max_ev,
  #     `Avg EV` = avg_ev,
  #     `90th %tile EV` = nth_ev,
  #     `Balls in Play` = balls_in_play
  #   )
  # 
  # launch_stats <- advanced_stats_data %>%
  #   dplyr::select(med_la, ground_ball_pct, line_drive_pct, pop_up_pct, fly_ball_pct) %>%
  #   dplyr::rename(
  #     `Median LA` = med_la,
  #     `Ground Ball %` = ground_ball_pct,
  #     `Line Drive %` = line_drive_pct,
  #     `Pop Up %` = pop_up_pct,
  #     `Fly Ball %` = fly_ball_pct
  #   )
  # 
  # direction_stats <- advanced_stats_data %>%
  #   dplyr::select(pull_pct, center_pct, oppo_pct) %>%
  #   dplyr::rename(
  #     `Pull %` = pull_pct,
  #     `Center %` = center_pct,
  #     `Oppo %` = oppo_pct
  #   )
  # 
  # all_advanced_stats <- list(swing_stats = swing_stats, ev_stats = ev_stats, launch_stats = launch_stats, direction_stats = direction_stats)
  
  # return the swing and launch stats in a list to be used
  
  all_advanced_stats <- list(swing_stats = swing_stats, launch_stats = launch_stats)
  
  
  return(all_advanced_stats)
  
}


######## EXTRA CODE FOR TESTING ##################

# return stats in a dataframe for now

# advanced_stats_data <- data.frame(
#   avg_ev = mean(batter_, na.rm = TRUE),
#   max_ev = max(data$ExitSpeed, na.rm = TRUE)
#   #check.names = FALSE
# )

# agg <- test %>%
#   select(BatterId, BatterSide, Balls, Strikes, Outs, PitchCall, KorBB, PlayResult, PlateLocHeight, PlateLocSide, ExitSpeed, Angle, Direction) %>%
#   mutate(
#     is_swing = ifelse(PitchCall %in% c("InPlay", "FoulBallNotFieldable", "StrikeSwinging", "FoulBallFieldable"), TRUE, FALSE),
#     is_miss = ifelse(PitchCall == "StrikeSwinging", TRUE, FALSE),
#     is_strike = ifelse((PlateLocHeight >= 1.5) & (PlateLocHeight <= 3.5) & (PlateLocSide >= -(10/12)) & (PlateLocSide <= (10/12)), TRUE, FALSE),
#     is_in_play = ifelse(PitchCall == "InPlay", TRUE, FALSE)
#   ) %>%
#   filter(!(is_in_play & is.na(ExitSpeed))) %>%
#   group_by(BatterId) %>%
#   summarise(
#     balls_in_play = sum(is_in_play),
#     whiff_pct = 100 * round(sum(is_miss) / sum(is_swing), 2),
#     chase_pct = 100 * round(sum(ifelse(is_swing & !is_strike, 1, 0)) / sum(!is_strike), 2),
#     z_swing = 100 * round(sum(ifelse(is_swing & is_strike, 1, 0)) / sum(is_strike), 2),
#     fp_swing = 100 * round(sum(ifelse(is_swing & (Balls == 0) & (Strikes == 0), 1, 0)) / n(), 2),
#     avg_ev = round(ifelse(sum(is_in_play) == 0, NA, mean(ExitSpeed[is_in_play], na.rm = TRUE)), 2),
#     max_ev = round(ifelse(sum(is_in_play) == 0, NA, max(ExitSpeed[is_in_play], na.rm = TRUE)), 2),
#     nth_ev = round(ifelse(sum(is_in_play) == 0, NA, quantile(ExitSpeed[is_in_play], probs = 0.9, na.rm = TRUE)), 2),
#     med_la = round(ifelse(sum(is_in_play) == 0, NA, median(Angle[is_in_play], na.rm = TRUE)), 2),
#     ground_ball_pct = 100 * round(sum(ifelse(Angle[is_in_play] < 10, 1, 0), na.rm = TRUE) / sum(is_in_play), 2),
#     line_drive_pct = 100 * round(sum(ifelse((Angle[is_in_play] >= 10) & (Angle[is_in_play] <= 25), 1, 0), na.rm = TRUE) / sum(is_in_play), 2),
#     fly_ball_pct = 100 * round(sum(ifelse(Angle[is_in_play] > 25, 1, 0), na.rm = TRUE) / sum(is_in_play), 2),
#     pull_pct = 100 * round(sum(ifelse((Direction[is_in_play] >= -45) & (Direction[is_in_play] < -15), 1, 0), na.rm = TRUE) / sum(is_in_play), 2),
#     center_pct = 100 * round(sum(ifelse((Direction[is_in_play] >= -15) & (Direction[is_in_play] <= 15), 1, 0), na.rm = TRUE) / sum(is_in_play), 2),
#     oppo_pct = 100 * round(sum(ifelse((Direction[is_in_play] > 15) & (Direction[is_in_play] <= 45), 1, 0), na.rm = TRUE) / sum(is_in_play), 2)
#   )
# 
# mid <- test %>%
#   select(BatterId, BatterSide, PitchCall, KorBB, PlayResult, PlateLocHeight, PlateLocSide, ExitSpeed, Angle, Direction) %>%
#   mutate(
#     is_swing = ifelse(PitchCall %in% c("InPlay", "FoulBallNotFieldable", "StrikeSwinging", "FoulBallFieldable"), TRUE, FALSE),
#     is_miss = ifelse(PitchCall == "StrikeSwinging", TRUE, FALSE),
#     is_strike = ifelse((PlateLocHeight >= 1.5) & (PlateLocHeight <= 3.5) & (PlateLocSide >= -(10/12)) & (PlateLocSide <= (10/12)), TRUE, FALSE),
#     is_in_play = ifelse(PitchCall == "InPlay", TRUE, FALSE)
#   )

# alternate way to infer GB, FB, etc in the summarize condition

#ground_ball_pct = round(100 * sum(ifelse(Angle[is_in_play] < 10, 1, 0), na.rm = TRUE) / sum(is_in_play), 1),
#line_drive_pct = round(100 *sum(ifelse((Angle[is_in_play] >= 10) & (Angle[is_in_play] <= 25), 1, 0), na.rm = TRUE) / sum(is_in_play), 1),
#fly_ball_pct = round(100 *sum(ifelse(Angle[is_in_play] > 25, 1, 0), na.rm = TRUE) / sum(is_in_play), 1),
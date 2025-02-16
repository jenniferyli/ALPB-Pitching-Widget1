# Function to take in the Trackman data and return a dataframe with percentiles for the basic and advanced stats

get_percentiles <- function(data, stat_type) {
  
  # function to create the conditional coloring for the percentiles
  
  create_colored_circle_raw <- function(value) {
    if (value < 33) {
      paste0("🔴 ", value)   # Red
    } else if (value < 66) {
      paste0("🟡 ", value)   # Light red
    } else {
      paste0("🟢 ", value)   # Green
    }
  }
  
  # vectorize function so that it can be used in a mutate
  
  create_colored_circle <- Vectorize(create_colored_circle_raw)
  
  # PERCENTILES 1: BASIC COUNTING STATS
  
  if (stat_type == "basic") {
    batter_data <- data %>%
      dplyr::select(BatterId, BatterSide, Balls, Strikes, Outs, PitchCall, KorBB, PlayResult, TaggedHitType) %>%
      mutate(
        is_in_play = ifelse(PitchCall == "InPlay", TRUE, FALSE),
        is_sacrifice_fly = ifelse((TaggedHitType != "Bunt") & (PlayResult == "Sacrifice"), TRUE, FALSE),
        is_sacrifice_bunt = ifelse((TaggedHitType == "Bunt") & (PlayResult == "Sacrifice"), TRUE, FALSE),
        is_walk = ifelse(KorBB == "Walk", TRUE, FALSE),
        is_hbp = ifelse(PitchCall == "HitByPitch", TRUE, FALSE),
        is_strikeout = ifelse(KorBB == "Strikeout", TRUE, FALSE),
        is_hit = ifelse(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), TRUE, FALSE),
        is_single = ifelse(PlayResult == "Single", TRUE, FALSE),
        is_double = ifelse(PlayResult == "Double", TRUE, FALSE),
        is_triple = ifelse(PlayResult == "Triple", TRUE, FALSE),
        is_homerun = ifelse(PlayResult == "HomeRun", TRUE, FALSE)
      ) %>%
      mutate(
        is_at_bat = ifelse((is_in_play & !(is_sacrifice_fly | is_sacrifice_bunt)) | is_strikeout, 1, 0),
      )
    
    # use the flags to calculate all of the statistics we need
    
    basic_stats_data <- batter_data %>%
      dplyr::group_by(BatterId) %>%
      dplyr::summarise(
        avg = round(sum(is_hit) / sum(is_at_bat), 3),
        hr = sum(is_homerun),
        obp = round(sum(is_hit, is_walk, is_hbp) / sum(is_at_bat, is_walk, is_hbp, is_sacrifice_fly), 3),
        ops = round((sum(is_hit, is_walk, is_hbp) / sum(is_at_bat, is_walk, is_hbp, is_sacrifice_fly)) + ((sum(is_single) + 2*sum(is_double) + 3*sum(is_triple) + 4*sum(is_homerun)) / sum(is_at_bat)), 3),
        k_perc = round(100 * sum(is_strikeout) / sum(is_at_bat, is_walk, is_hbp, is_sacrifice_fly, is_sacrifice_bunt), 1),
        bb_perc = round(100 * sum(is_walk) / sum(is_at_bat, is_walk, is_hbp, is_sacrifice_fly, is_sacrifice_bunt), 1)
      )
    
    # Do the percentile calculations for basic counting stats
    
    avg_ecdf <- ecdf(basic_stats_data$avg)
    hr_ecdf <- ecdf(basic_stats_data$hr)
    obp_ecdf <- ecdf(basic_stats_data$obp)
    ops_ecdf <- ecdf(basic_stats_data$ops)
    k_ecdf <- ecdf(basic_stats_data$k_perc)
    bb_ecdf <- ecdf(basic_stats_data$bb_perc)
    
    # NOTE THAT K IS INVERSE PERCENTILE. LOW K IS BETTER, ETC
    # also rename columns at end to match the advanced stats table so we can bind them well together later
    # call the percentiles ECDF function and build a colored circle based on percentile value
    
    basic_percentiles <- basic_stats_data %>%
      filter(!is.na(avg) & !is.na(hr) & !is.na(obp) & !is.na(ops) & !is.na(k_perc) & !is.na(bb_perc)) %>%
      dplyr::select(BatterId, avg, hr, obp, ops, k_perc, bb_perc) %>%
      mutate(avg_ntile = sapply(round(avg_ecdf(avg) * 100, 0), create_colored_circle),
             hr_ntile = sapply(round(hr_ecdf(hr) * 100, 0), create_colored_circle),
             obp_ntile = sapply(round(obp_ecdf(obp) * 100, 0), create_colored_circle),
             ops_ntile = sapply(round(ops_ecdf(ops) * 100, 0), create_colored_circle),
             k_ntile = sapply(round((1 - k_ecdf(k_perc)) * 100, 0), create_colored_circle),
             bb_ntile = sapply(round(bb_ecdf(bb_perc) * 100, 0), create_colored_circle)) %>%
      dplyr::select(BatterId, avg_ntile, hr_ntile, obp_ntile, ops_ntile, k_ntile, bb_ntile) %>%
      rename(
        avg = avg_ntile,
        hr = hr_ntile,
        obp = obp_ntile,
        ops = ops_ntile,
        k_perc = k_ntile,
        bb_perc = bb_ntile
      )
    
    return(basic_percentiles)
    
  }
  
  # PERCENTILES 2: ADVANCED SWING AND LAUNCH STATS
  
  if (stat_type == "advanced") {
    batter_data <- data %>%
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
    
    advanced_stats_data <- batter_data %>%
      group_by(BatterId) %>%
      summarise(
        balls_in_play = sum(is_in_play),
        whiff_pct = round(100 * sum(is_miss, na.rm = T) / sum(is_swing, na.rm = T), 1),
        chase_pct = round(100 * sum(ifelse(is_swing & !is_strike, 1, 0), na.rm = T) / sum(!is_strike, na.rm = T), 1),
        z_swing = round(100 * sum(ifelse(is_swing & is_strike, 1, 0), na.rm = T) / sum(is_strike, na.rm = T), 1),
        fp_swing = round(100 * sum(ifelse(is_swing & (Balls == 0) & (Strikes == 0), 1, 0), na.rm = T) / n(), 1),
        avg_ev = round(ifelse(sum(is_in_play) == 0, NA, mean(ExitSpeed[is_in_play], na.rm = TRUE)), 1),
        max_ev = round(ifelse(sum(is_in_play) == 0, NA, max(ExitSpeed[is_in_play], na.rm = TRUE)), 1),
        nth_ev = round(ifelse(sum(is_in_play) == 0, NA, quantile(ExitSpeed[is_in_play], probs = 0.9, na.rm = TRUE)), 1),
        med_la = round(ifelse(sum(is_in_play) == 0, NA, median(Angle[is_in_play], na.rm = TRUE)), 1),
        ground_ball_pct = round(100 * sum(ifelse(TaggedHitType[is_in_play] == "GroundBall", 1, 0), na.rm = TRUE) / sum(is_in_play, na.rm = T), 1),
        line_drive_pct = round(100 * sum(ifelse(TaggedHitType[is_in_play] == "LineDrive", 1, 0), na.rm = TRUE) / sum(is_in_play, na.rm = T), 1),
        pop_up_pct = round(100 * sum(ifelse(TaggedHitType[is_in_play] == "Popup", 1, 0), na.rm = TRUE) / sum(is_in_play, na.rm = T), 1),
        fly_ball_pct = round(100 * sum(ifelse(TaggedHitType[is_in_play] == "FlyBall", 1, 0), na.rm = TRUE) / sum(is_in_play, na.rm = T), 1),
        pull_pct = round(100 * sum(ifelse((Direction[is_in_play] >= -45) & (Direction[is_in_play] < -15), 1, 0), na.rm = TRUE) / sum(is_in_play, na.rm = T), 1),
        center_pct = round(100 * sum(ifelse((Direction[is_in_play] >= -15) & (Direction[is_in_play] <= 15), 1, 0), na.rm = TRUE) / sum(is_in_play, na.rm = T), 1),
        oppo_pct = round(sum(100 * ifelse((Direction[is_in_play] > 15) & (Direction[is_in_play] <= 45), 1, 0), na.rm = TRUE) / sum(is_in_play, na.rm = T), 1)
      )
    
    # Do the percentile calculations for launch stats
    
    max_ev_ecdf <- ecdf(advanced_stats_data$max_ev)
    avg_ev_ecdf <- ecdf(advanced_stats_data$avg_ev)
    
    # Do the percentile calculations for swing decision stats
    
    whiff_ecdf <- ecdf(advanced_stats_data$whiff_pct)
    chase_ecdf <- ecdf(advanced_stats_data$chase_pct)
    z_swing_ecdf <- ecdf(advanced_stats_data$z_swing)
    fp_swing_ecdf <- ecdf(advanced_stats_data$fp_swing)
    
    # if you wanted to do percentile for LA + BIP you can here the same way as the others (update below too)
    # but they don't really make sense to do
    
    # med_la_ecdf <- ecdf(advanced_stats_data$med_la)
    # bip_ecdf <- ecdf(advanced_stats_data$balls_in_play)
    
    # NOTE THAT WHIFF AND CHASE ARE INVERSE PERCENTILES. LOW WHIFF IS BETTER, ETC
    # call the percentiles ECDF function and build a colored circle based on percentile value
    
    # for swing decisions first
    
    swing_percentiles <- advanced_stats_data %>%
      filter(!is.na(whiff_pct) & !is.na(chase_pct) & !is.na(z_swing) & !is.na(fp_swing)) %>%
      dplyr::select(BatterId, whiff_pct, chase_pct, z_swing, fp_swing) %>%
      mutate(whiff_ntile = sapply(round((1 - whiff_ecdf(whiff_pct)) * 100, 0), create_colored_circle),
             chase_ntile = sapply(round((1 - chase_ecdf(chase_pct)) * 100, 0), create_colored_circle),
             z_swing_ntile = sapply(round(z_swing_ecdf(z_swing) * 100, 0), create_colored_circle),
             fp_swing_ntile = sapply(round(fp_swing_ecdf(fp_swing) * 100, 0), create_colored_circle)) %>%
      dplyr::select(BatterId, whiff_ntile, chase_ntile, z_swing_ntile, fp_swing_ntile)
    
    # then launch next
    
    launch_percentiles <- advanced_stats_data %>%
      filter(!is.na(max_ev) & !is.na(avg_ev)) %>%
      dplyr::select(BatterId, max_ev, avg_ev) %>%
      mutate(max_ev_ntile = sapply(round(max_ev_ecdf(max_ev) * 100, 0), create_colored_circle),
             avg_ev_ntile = sapply(round(avg_ev_ecdf(avg_ev) * 100, 0), create_colored_circle)) %>%
      dplyr::select(BatterId, max_ev_ntile, avg_ev_ntile)
    
    # join swing and launch percentiles together into one table and
    # rename columns to match the advanced stats table so we can bind them well together later
    
    adv_percentiles <- swing_percentiles %>%
      inner_join(launch_percentiles, by = "BatterId") %>%
      rename(
        max_ev = max_ev_ntile,
        avg_ev = avg_ev_ntile,
        whiff_pct = whiff_ntile,
        chase_pct = chase_ntile,
        z_swing = z_swing_ntile,
        fp_swing = fp_swing_ntile
      )
    
    return (adv_percentiles)
  }
}
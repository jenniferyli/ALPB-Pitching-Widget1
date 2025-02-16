library(dplyr)

# function to return the basic counting stats of the selected player

get_basic_stats <- function(data, percentile_data, id, throws, date_low, date_high, include_percentiles = TRUE) {
  
  
  # if there is no data, return a blank table
  
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  # manipulate the data so that we have relevant flags that we need for future filtering and counting
  
  batter_data <- data %>%
    filter(BatterId == id) %>%
    filter(PitcherThrows == throws) %>%
    filter(Date >= date_low & Date <= date_high) %>%
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
  # NOTE THE AS.CHARACTER CASTS SO THAT THE PERCENTILES ROW CAN WORK PROPERLY IF YOU NEED TO DO
  # FURTHER CALCULATIONS WITH THESE NUMBERS, THEY SHOULD BE BACK IN THE FORM OF NUMERICS
  
  basic_stats_data <- batter_data %>%
    dplyr::group_by(BatterId) %>%
    dplyr::summarise(
      avg = as.character(format(round(sum(is_hit) / sum(is_at_bat), 3), nsmall = 3)),
      hr = as.character(sum(is_homerun)),
      obp = as.character(format(round(sum(is_hit, is_walk, is_hbp) / sum(is_at_bat, is_walk, is_hbp, is_sacrifice_fly), 3), nsmall = 3)),
      ops = as.character(format(round((sum(is_hit, is_walk, is_hbp) / sum(is_at_bat, is_walk, is_hbp, is_sacrifice_fly)) + ((sum(is_single) + 2*sum(is_double) + 3*sum(is_triple) + 4*sum(is_homerun)) / sum(is_at_bat)), 3), nsmall = 3)),
      k_perc = as.character(format(round(100 * sum(is_strikeout) / sum(is_at_bat, is_walk, is_hbp, is_sacrifice_fly, is_sacrifice_bunt), 1), nsmall = 0)),
      bb_perc = as.character(format(round(100 * sum(is_walk) / sum(is_at_bat, is_walk, is_hbp, is_sacrifice_fly, is_sacrifice_bunt), 1), nsmall = 0))
    ) 
  
  # Get the basic data and percentile data so that it can be binded to the bottom row
  
  if (include_percentiles) {
    
    basic_percentile_data <- percentile_data %>%
      dplyr::filter(BatterId == id) %>%
      dplyr::select(-BatterId)
    
    basic_stats <- basic_stats_data %>%
      dplyr::select(avg, hr, obp, ops, k_perc, bb_perc) %>%
      bind_rows(basic_percentile_data) %>%
      dplyr::rename(
        `Avg` = avg,
        `HR` = hr,
        `OBP` = obp,
        `OPS` = ops,
        `K %` = k_perc,
        `BB %` = bb_perc
      )
  } else {
    
    basic_stats <- basic_stats_data %>%
      dplyr::select(avg, hr, obp, ops, k_perc, bb_perc) %>%
      dplyr::rename(
        `Avg` = avg,
        `HR` = hr,
        `OBP` = obp,
        `OPS` = ops,
        `K %` = k_perc,
        `BB %` = bb_perc
      )
    
  }
  
  return(basic_stats)
  
  # Testing
}
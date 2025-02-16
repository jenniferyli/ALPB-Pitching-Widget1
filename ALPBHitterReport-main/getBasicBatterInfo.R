## Build Component 1 - Basic Batter Info

# Might want to rework these functions so that they only take in a
# batter ID, run the data query and then build whatever they need to do
# don't take in the Trackman data from the main server. Only pass an ID

library(dplyr)
library(ggplot2)

get_batter_info_card <- function(data, id) {
  
  batter_data <- data %>%
    filter(BatterId == id) %>%
    dplyr::select(BatterId, BatterSide, BatterTeam)
  
  # batter_data <- data %>%
  #   filter(Batter == batter_name) %>%
  #   select(BatterId, BatterSide, BatterTeam)
  
  batter_id <- as.integer(median(batter_data$BatterId))
  batter_team <- max(batter_data$BatterTeam)
  batter_side <- ifelse(length(unique(batter_data$BatterSide)) == 2, "S", ifelse(max(batter_data$BatterSide) == "Right", "R", "L"))
  
  # return very basic table for now
  
  batter_card_table <- data.frame(
    "ID" = batter_id,
    "Team" = batter_team,
    "Side" = batter_side,
    check.names = FALSE
  )
  
  return(batter_card_table)
  
}
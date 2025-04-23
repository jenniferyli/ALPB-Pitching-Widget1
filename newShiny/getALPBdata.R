# library(httr)
# library(jsonlite)
# 
# # === Function to query ALPB by first/last name ===
# get_alpb_pitcher_info <- function(fname, lname) {
#   # Construct player_name in ALPB format
#   query_name <- paste0(lname, ", ", fname)
# 
#   # API setup
#   alpb_api_key <- "IuHgm3smV65kbC6lMlMLz80DOeEkGSiV6USoQhvZ"
#   url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/players"
# 
#   # API call
#   res <- GET(
#     url,
#     query = list(player_name = query_name),
#     add_headers(`x-api-key` = alpb_api_key)
#   )
# 
#   # Parse safely
#   json_data <- content(res, as = "text", encoding = "UTF-8")
#   parsed <- fromJSON(json_data, simplifyDataFrame = FALSE)
# 
#   # Check and extract info
#   player <- parsed$data[[1]]
# 
#   if (!is.null(player) && isTRUE(player$is_pitcher)) {
#     return(tibble(
#       player_id = player$player_id,
#       pitching_hand = player$player_pitching_handedness
#     ))
#   } else {
#     return(tibble(
#       player_id = NA,
#       pitching_hand = NA
#     ))
#   }
# }
# 
# 


library(httr)
library(jsonlite)
library(tibble)

get_alpb_pitcher_info <- function(fname, lname) {
  query_name <- paste0(lname, ", ", fname)
  alpb_api_key <- "IuHgm3smV65kbC6lMlMLz80DOeEkGSiV6USoQhvZ"
  url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/players"
  
  # Error handling block
  tryCatch({
    res <- GET(
      url,
      query = list(player_name = query_name),
      add_headers(`x-api-key` = alpb_api_key)
    )
    
    json_data <- content(res, as = "text", encoding = "UTF-8")
    parsed <- fromJSON(json_data, simplifyDataFrame = FALSE)
    
    player <- parsed$data[[1]]
    
    if (!is.null(player) && isTRUE(player$is_pitcher)) {
      return(tibble(
        player_id = player$player_id,
        pitching_hand = player$player_pitching_handedness
      ))
    } else {
      return(tibble(
        player_id = "data unavailable",
        pitching_hand = "data unavailable"
      ))
    }
  }, error = function(e) {
    # On error, return fallback tibble
    return(tibble(
      player_id = "data unavailable",
      pitching_hand = "data unavailable"
    ))
  })
}


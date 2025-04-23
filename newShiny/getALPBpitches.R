library(httr)
library(jsonlite)
library(dplyr)

get_alpb_pitches_by_pitcher <- function(player_id) {
  # ALPB API key and endpoint
  alpb_api_key <- "IuHgm3smV65kbC6lMlMLz80DOeEkGSiV6USoQhvZ"
  base_url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/pitches"
  
  # Make GET request with pitcher_id as query param
  res <- GET(
    url = base_url,
    query = list(pitcher_id = player_id),
    add_headers(`x-api-key` = alpb_api_key)
  )
  
  # Check response status
  if (status_code(res) != 200) {
    cat("❌ API request failed with status:", status_code(res), "\n")
    return(NULL)
  }
  
  # Parse and extract pitch data
  json_data <- content(res, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(json_data, flatten = TRUE)
  
  if (is.null(parsed$data) || length(parsed$data) == 0) {
    cat("⚠️ No pitch data found for pitcher_id:", player_id, "\n")
    return(NULL)
  }
  
  pitch_df <- as_tibble(parsed$data)
  print(pitch_df)
  return(pitch_df)
}


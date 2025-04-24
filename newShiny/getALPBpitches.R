library(httr)
library(jsonlite)
library(dplyr)

get_alpb_pitches_by_pitcher <- function(player_id) {
  
  if (is.null(player_id) || player_id == "") {
    return(NULL)
  }
  
  
  url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/pitches"
  api_key <- "IuHgm3smV65kbC6lMlMLz80DOeEkGSiV6USoQhvZ"
  
  # First request to get total number of pages
  res <- GET(
    url,
    query = list(pitcher_id = player_id, page = 1),
    add_headers(`x-api-key` = api_key)
  )
  
  if (status_code(res) != 200) {
    warning("API request failed")
    return("ALPB data unavailable.")
  }
  
  content_text <- content(res, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(content_text, simplifyDataFrame = FALSE)
  
  # Check if data is empty
  if (is.null(parsed$data) || length(parsed$data) == 0) {
    return("ALPB data unavailable.")
  }
  
  # Determine total pages from meta
  total_pages <- parsed$meta$total
  all_data <- parsed$data
  
  # Loop through remaining pages
  if (total_pages > 1) {
    for (page in 2:total_pages) {
      res_page <- GET(
        url,
        query = list(pitcher_id = player_id, page = page),
        add_headers(`x-api-key` = api_key)
      )
      if (status_code(res_page) == 200) {
        page_data <- fromJSON(content(res_page, as = "text", encoding = "UTF-8"), simplifyDataFrame = FALSE)
        all_data <- c(all_data, page_data$data)
      }
    }
  }
  
  df <- bind_rows(all_data)
  return(df)
}


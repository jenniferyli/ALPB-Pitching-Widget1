# library(httr)
# library(jsonlite)
# 
# # === Function to get and print pitching stats for a player ===
# get_pitching_stats_only <- function(playerlinkid, seasonid) {
#   apikey <- "vIpQsngDfc6Y7WVgAcTt"
#   
#   url <- paste0(
#     "https://api.pointstreak.com/baseball/player/stats/",
#     playerlinkid, "/", seasonid, "/json"
#   )
#   
#   res <- GET(url, add_headers(apikey = apikey))
#   
#   if (status_code(res) != 200) {
#     cat("❌ API request failed. Status:", status_code(res), "\n")
#     return(NULL)
#   }
#   
#   json_data <- content(res, as = "text", encoding = "UTF-8")
#   parsed <- fromJSON(json_data, simplifyDataFrame = FALSE)
#   
#   # Access nested pitching stats
#   pitching_stats <- parsed$player$pitchingstats$season
#   
#   if (length(pitching_stats) == 0 || is.null(pitching_stats)) {
#     cat("⚠️ No pitching stats found for this player.\n")
#   } else {
#     cat("✅ Pitching Stats:\n")
#     
#     # Safely convert to a data frame and print
#     if (is.list(pitching_stats) && all(sapply(pitching_stats, is.list))) {
#       df <- bind_rows(pitching_stats)
#       print(df)
#     } else {
#       print(pitching_stats)
#     }
#   }
# }
# 
# # === Example usage ===
# get_pitching_stats_only("1743144", "34104")
# 


library(httr)
library(jsonlite)
library(dplyr)

get_pitching_stats_only <- function(playerlinkid, seasonid = "34104") {
  apikey <- "vIpQsngDfc6Y7WVgAcTt"
  
  url <- paste0("https://api.pointstreak.com/baseball/player/stats/", playerlinkid, "/", seasonid, "/json")
  res <- GET(url, add_headers(apikey = apikey))
  if (status_code(res) != 200) return(NULL)
  
  json_data <- content(res, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(json_data, simplifyDataFrame = FALSE)
  
  pitching_stats <- parsed$player$pitchingstats$season
  if (is.null(pitching_stats)) return(NULL)
  
  df <- bind_rows(pitching_stats)
  return(df)
}
get_pitching_stats_only("1743144", "34104")

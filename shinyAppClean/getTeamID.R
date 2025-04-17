library(httr)
library(xml2)

get_player_info_by_team_name <- function(team_name_input, player_name_input, league_id = 174, season_id = 34104, api_key = "vIpQsngDfc6Y7WVgAcTt") {
  
  # Step 1: Get League Structure and Team ID
  structure_url <- paste0("https://api.pointstreak.com/baseball/league/structure/", league_id, "/xml")
  structure_response <- GET(structure_url, add_headers(apikey = api_key))
  if (status_code(structure_response) != 200) stop("❌ Failed to fetch league structure")
  
  structure_xml <- content(structure_response, as = "parsed", encoding = "UTF-8")
  teams <- xml_find_all(structure_xml, ".//team")
  
  teamlinkid <- NULL
  for (team in teams) {
    name <- xml_attr(team, "teamname")
    if (!is.na(name) && tolower(trimws(name)) == tolower(trimws(team_name_input))) {
      teamlinkid <- xml_attr(team, "teamlinkid")
      break
    }
  }
  if (is.null(teamlinkid)) stop("❌ Team not found: ", team_name_input)
  
  # Step 2: Get Roster
  roster_url <- paste0("https://api.pointstreak.com/baseball/team/roster/", teamlinkid, "/", season_id, "/xml")
  roster_response <- GET(roster_url, add_headers(apikey = api_key))
  if (status_code(roster_response) != 200) stop("❌ Failed to fetch team roster")
  
  roster_xml <- content(roster_response, as = "parsed", encoding = "UTF-8")
  players <- xml_find_all(roster_xml, ".//player")
  
  for (player in players) {
    fname <- xml_text(xml_find_first(player, "./fname"))
    lname <- xml_text(xml_find_first(player, "./lname"))
    full_name <- paste(fname, lname)
    
    if (tolower(trimws(full_name)) == tolower(trimws(player_name_input))) {
      player_id <- xml_attr(player, "playerid")
      playerlink_id <- xml_attr(player, "playerlinkid")
      height <- xml_text(xml_find_first(player, "./height"))
      weight <- xml_text(xml_find_first(player, "./weight"))
      photo <- xml_text(xml_find_first(player, "./photo"))
      
      cat("✅ Player found!\n")
      cat("Full Name     :", full_name, "\n")
      cat("Player ID     :", player_id, "\n")
      cat("PlayerLink ID :", playerlink_id, "\n")
      cat("Height        :", height, "\n")
      cat("Weight        :", weight, "\n")
      cat("Photo URL     :", photo, "\n")
      
      # Step 3: Player Stats
      stats_url <- paste0("https://api.pointstreak.com/baseball/player/stats/", playerlink_id, "/", season_id, "/xml")
      stats_response <- GET(stats_url, add_headers(apikey = api_key))
      if (status_code(stats_response) != 200) {
        cat("⚠️ Could not fetch player stats\n")
        return()
      }
      stats_xml <- content(stats_response, as = "parsed", encoding = "UTF-8")
      
      bio <- xml_text(xml_find_first(stats_xml, ".//bio"))
      cat("\n📖 Bio:\n", bio, "\n")
      
      cat("\n📊 Pitching Stats:\n")
      stats_seasons <- xml_find_all(stats_xml, ".//pitchingstats/season")
      
      alpb_2024_stats <- NULL
      for (season in stats_seasons) {
        season_name <- xml_text(xml_find_first(season, "./name"))
        team_name <- xml_text(xml_find_first(season, "./teamname"))
        if (grepl("ALPB- 2024", season_name)) {
          cat("\n🎯 Season:", season_name, "| Team:", team_name, "\n")
          fields <- c("gp", "w", "l", "h", "bb", "er", "gs", "era", "sho", "sv", "ip", "so")
          alpb_2024_stats <- list()
          for (field in fields) {
            value <- xml_text(xml_find_first(season, paste0("./", field)))
            alpb_2024_stats[[field]] <- value
            cat(toupper(field), ":", value, "\n")
          }
        }
      }
      
      # Return only the 2024 stats, along with core info
      return(invisible(list(
        name = full_name,
        player_id = player_id,
        playerlink_id = playerlink_id,
        height = height,
        weight = weight,
        photo = photo,
        bio = bio,
        alpb_2024_stats = alpb_2024_stats
      )))
    }
  }
  
  cat("❌ Player not found:", player_name_input, "\n")
}



# 
# 
# library(httr)
# library(xml2)
# 
# pplayer_info_by_team_name <- function(team_name_input, player_name_input, league_id = 174, season_id = 34104, api_key = "vIpQsngDfc6Y7WVgAcTt") {
#   
#   # Step 1: Get League Structure to match team name
#   structure_url <- paste0("https://api.pointstreak.com/baseball/league/structure/", league_id, "/xml")
#   structure_response <- GET(structure_url, add_headers(apikey = api_key))
#   if (status_code(structure_response) != 200) {
#     stop("❌ Failed to fetch league structure. Status code: ", status_code(structure_response))
#   }
#   
#   structure_xml <- content(structure_response, as = "parsed", encoding = "UTF-8")
#   teams <- xml_find_all(structure_xml, ".//team")
#   
#   teamlinkid <- NULL
#   for (team in teams) {
#     name <- xml_attr(team, "teamname")
#     if (tolower(name) == tolower(team_name_input)) {
#       teamlinkid <- xml_attr(team, "teamlinkid")
#       break
#     }
#   }
#   
#   if (is.null(teamlinkid)) {
#     stop("❌ Team not found: ", team_name_input)
#   }
#   
#   # Step 2: Use teamlinkid to pull roster
#   roster_url <- paste0("https://api.pointstreak.com/baseball/team/roster/", teamlinkid, "/", season_id, "/xml")
#   roster_response <- GET(roster_url, add_headers(apikey = api_key))
#   if (status_code(roster_response) != 200) {
#     stop("❌ Failed to fetch team roster. Status code: ", status_code(roster_response))
#   }
#   
#   roster_xml <- content(roster_response, as = "parsed", encoding = "UTF-8")
#   players <- xml_find_all(roster_xml, ".//player")
#   
#   # Step 3: Match player name
#   for (player in players) {
#     fname <- xml_text(xml_find_first(player, "./fname"))
#     lname <- xml_text(xml_find_first(player, "./lname"))
#     full_name <- paste(fname, lname)
#     
#     if (tolower(full_name) == tolower(player_name_input)) {
#       player_id <- xml_attr(player, "playerid")
#       playerlink_id <- xml_attr(player, "playerlinkid")
#       height <- xml_text(xml_find_first(player, "./height"))
#       weight <- xml_text(xml_find_first(player, "./weight"))
#       photo <- xml_text(xml_find_first(player, "./photo"))
#       
#       # Step 4: Fetch player stats
#       stats_url <- paste0("https://api.pointstreak.com/baseball/player/stats/", playerlink_id, "/", season_id, "/xml")
#       stats_response <- GET(stats_url, add_headers(apikey = api_key))
#       stats <- list()
#       
#       if (status_code(stats_response) == 200) {
#         stats_xml <- content(stats_response, as = "parsed", encoding = "UTF-8")
#         stat_node <- xml_find_first(stats_xml, ".//season")
#         
#         if (!is.na(stat_node)) {
#           stats <- list(
#             games_played = xml_text(xml_find_first(stat_node, "./gp")),
#             wins = xml_text(xml_find_first(stat_node, "./w")),
#             losses = xml_text(xml_find_first(stat_node, "./l")),
#             era = xml_text(xml_find_first(stat_node, "./era")),
#             strikeouts = xml_text(xml_find_first(stat_node, "./so")),
#             innings_pitched = xml_text(xml_find_first(stat_node, "./ip"))
#           )
#         }
#       }
#       
#       cat("✅ Player found!\n")
#       cat("Full Name     :", full_name, "\n")
#       cat("Player ID     :", player_id, "\n")
#       cat("PlayerLink ID :", playerlink_id, "\n")
#       cat("Height        :", height, "\n")
#       cat("Weight        :", weight, "\n")
#       cat("Photo URL     :", photo, "\n")
#       cat("Stats         :", paste(names(stats), stats, sep = " = ", collapse = ", "), "\n")
#       
#       return(invisible(list(
#         player_id = player_id,
#         playerlink_id = playerlink_id,
#         height = height,
#         weight = weight,
#         photo = photo,
#         stats = stats
#       )))
#     }
#   }
#   
#   cat("❌ Player not found: ", player_name_input, "\n")
#   return(NULL)
# }

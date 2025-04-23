library(httr)
library(jsonlite)
library(dplyr)

# === STEP 1: Get all teams for the 2025 Spring Training season ===

# Season and API details
seasonid <- "34104"
apikey <- "vIpQsngDfc6Y7WVgAcTt"

# Request JSON from API
url <- "https://api.pointstreak.com/baseball/league/structure/174/json"
headers <- add_headers(apikey = apikey)
params <- list(seasonid = seasonid)

res <- GET(url, headers, query = params)
json_data <- content(res, as = "text", encoding = "UTF-8")
parsed <- fromJSON(json_data, simplifyDataFrame = FALSE)

# Get just the season with ID 34104
season <- Filter(function(s) s$seasonid == seasonid, parsed$league$season)[[1]]

# Extract and flatten all teams from all divisions
teams <- do.call(c, lapply(season$division, function(div) div$team))
team_df <- bind_rows(teams)

# Print team list
print(team_df)

# === STEP 2: Helper to pull players for one team ===

safe_extract <- function(field) {
  if (is.null(field)) return(NA)
  if (is.list(field)) return(as.character(unlist(field)))
  return(as.character(field))
}

get_players_for_team <- function(team) {
  teamlinkid <- team$teamlinkid
  teamname <- team$teamname

  url <- paste0("https://api.pointstreak.com/baseball/team/roster/", teamlinkid, "/", seasonid, "/json")
  res <- GET(url, add_headers(apikey = apikey))

  if (status_code(res) != 200) return(NULL)

  json_text <- content(res, as = "text", encoding = "UTF-8")
  parsed_json <- fromJSON(json_text, simplifyDataFrame = FALSE)
  players <- parsed_json$league$player

  if (is.null(players)) return(NULL)

  bind_rows(lapply(players, function(p) {
    tibble(
      playerid = safe_extract(p$playerid),
      playerlinkid = safe_extract(p$playerlinkid),
      fname = safe_extract(p$fname),
      lname = safe_extract(p$lname),
      position = safe_extract(p$position),
      height = safe_extract(p$height),
      weight = safe_extract(p$weight),
      birthday = safe_extract(p$birthday),
      bats = safe_extract(p$bats),
      throws = safe_extract(p$throws),
      hometown = safe_extract(p$hometown),
      photo = safe_extract(p$photo),
      teamlinkid = teamlinkid,
      teamname = teamname
    )
  }))
}

# === STEP 3: Loop through all teams and build the full player data ===

all_players_df <- bind_rows(lapply(teams, get_players_for_team))

pitchers_df <- all_players_df %>%
  filter(position == "P") %>%
  filter(!teamname %in% c("Staten Island Ferry Hawks", "Long Island Black Sox")) %>%
  arrange(lname)


# View sorted list
print(pitchers_df)


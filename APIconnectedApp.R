library(shiny)
library(httr)
library(jsonlite)

api_key <- "IuHgm3smV65kbC6lMlMLz80DOeEkGSiV6USoQhvZ"
players_url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/players"
pitches_url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/pitches"

get_pitcher_data <- function() {
  headers <- add_headers(`x-api-key` = api_key)

  all_data <- data.frame()
  page <- 1
  total_pages <- 1

  repeat {
    url <- paste0(players_url, "?is_pitcher=true&page=", page)
    response <- GET(url, headers)

    if (status_code(response) == 200) {
      data <- content(response, as = "text", encoding = "UTF-8")
      parsed_data <- fromJSON(data, flatten = TRUE)  # Flatten JSON

      if (!is.null(parsed_data$data)) {
        all_data <- rbind(all_data, parsed_data$data)
      }

      total_pages <- parsed_data$meta$pages
      if (page >= total_pages) break
      page <- page + 1
    } else {
      print(paste("Error fetching pitchers:", status_code(response)))
      break
    }
  }

  return(all_data)
}

get_pitch_data <- function(player_id) {
  if (is.null(player_id) || length(player_id) == 0) {
    print("⚠️ Error: Invalid player_id (NULL or empty)")
    return(data.frame(Message = "Invalid player_id"))
  }

  print(paste("Fetching pitches for Player ID:", player_id))

  headers <- add_headers(
    `x-api-key` = api_key,
    `player_id` = player_id  
  )

  response <- GET(pitches_url, headers)

  if (status_code(response) == 200) {
    data <- content(response, as = "text", encoding = "UTF-8")
    parsed_data <- fromJSON(data, flatten = TRUE)

    if (!is.null(parsed_data$data) && length(parsed_data$data) > 0) {
      pitch_data <- as.data.frame(parsed_data$data)
      print(paste("✅ First 5 pitches for Player ID:", player_id))
      print(head(pitch_data, 5))
      return(pitch_data)
    } else {
      print("⚠️ No pitch data found for this player.")
      return(data.frame(Message = "No pitch data available"))
    }
  } else {
    print(paste("API Error: Status code", status_code(response)))
    return(data.frame(Message = "API Error"))
  }
}

pitcher_data <- get_pitcher_data()

# Shiny UI
ui <- fluidPage(
  titlePanel("Select a Pitcher"),

  sidebarLayout(
    sidebarPanel(
      selectInput("selected_player", "Choose a Player:", choices = pitcher_data$player_name)
    ),

    mainPanel(
      textOutput("player_id"),
      textOutput("team_name"),
      textOutput("player_handedness")
    )
  )
)

# Shiny Server
server <- function(input, output) {

  selected_pitcher <- reactive({
    pitcher_data[pitcher_data$player_name == input$selected_player, ]
  })

  output$player_id <- renderText({
    player_id <- selected_pitcher()$player_id

    if (!is.null(player_id) && length(player_id) > 0) {
      paste("Player ID:", player_id)
    } else {
      "No data available"
    }
  })

  output$team_name <- renderText({
    team_name <- selected_pitcher()$team_name

    if (!is.null(team_name) && length(team_name) > 0) {
      paste("Team Name:", team_name)
    } else {
      "No data available"
    }
  })

  output$player_handedness <- renderText({
    handedness <- selected_pitcher()$player_pitching_handedness

    if (!is.null(handedness) && length(handedness) > 0) {
      paste("Pitching Handedness:", handedness)
    } else {
      "No data available"
    }
  })

  observeEvent(input$selected_player, {
    player_id <- selected_pitcher()$player_id

    if (!is.null(player_id) && length(player_id) > 0) {
      pitch_data <- get_pitch_data(player_id)
    } else {
      print("Error: Invalid player selection (player_id is NULL)")
    }
  })
}

shinyApp(ui = ui, server = server)

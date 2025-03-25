
library(shiny)
library(httr)
library(jsonlite)

api_key <- "IuHgm3smV65kbC6lMlMLz80DOeEkGSiV6USoQhvZ"
players_url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/players"
pitches_url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/pitches"

# Fetch pitcher data
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
      parsed_data <- fromJSON(data, flatten = TRUE)  
      
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

# Fetch and filter pitch data by date range
get_pitch_data <- function(player_id, start_date, end_date) {
  if (is.null(player_id) || length(player_id) == 0) {
    print("Error: Invalid player_id (NULL or empty)")
    return(NULL)
  }
  
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
      
      pitch_data$date <- as.Date(pitch_data$date)
      filtered_data <- subset(pitch_data, date >= start_date & date <= end_date)
      
      print(paste("\nFiltered pitches for Player ID:", player_id))
      print(head(filtered_data, 5))  # ✅ PRINT TO CONSOLE ONLY
      
      return(filtered_data)
    } else {
      print("⚠️ No pitch data found for this player.")
    }
  } else {
    print(paste("API Error: Status code", status_code(response)))
  }
  
  return(NULL)
}

# UI Card Helper
card_w_header <- function(title, body) {
  div(class = "card",
      div(class = "card-header bg-info text-white text-center font-weight-bold", title),
      div(class = "card-body d-flex justify-content-center align-items-center",
          div(style = "text-align: center; width: 100%;", body)
      )
  )
}

# Load pitcher data
pitcher_data <- get_pitcher_data()

# UI
ui <- fluidPage(
  div(style = "text-align: center;", h1("ALPB Pitchers")),
  
  fluidRow(
    column(4,
           wellPanel(
             selectInput("selected_player", "Select a Pitcher:", choices = pitcher_data$player_name),
             dateRangeInput("date_range", "Select Date Range:",
                            start = Sys.Date() - 365,
                            end = Sys.Date())
           )
    ),
    column(8,
           card_w_header("Season Stats", tableOutput("season_log"))
    )
  ),
  
  fluidRow(
    column(12,
           h3("Pitcher Information"),
           uiOutput("player_info")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  selected_pitcher <- reactive({
    pitcher_data[pitcher_data$player_name == input$selected_player, ]
  })
  
  output$player_info <- renderUI({
    player_name <- selected_pitcher()$player_name
    player_id <- selected_pitcher()$player_id
    team_name <- selected_pitcher()$team_name
    handedness <- selected_pitcher()$player_pitching_handedness
    
    if (!is.null(player_id) && length(player_id) > 0) {
      tagList(
        h4(HTML(paste(player_name))),
        div(HTML(paste("<b>Player ID:</b>", player_id))),
        div(HTML(paste("<b>Team Name:</b>", team_name))),
        div(HTML(paste("<b>Pitching Handedness:</b>", handedness))),
        div(HTML("<b>Age:</b> Pointstreak TBD")),
        div(HTML("<b>Height:</b> Pointstreak TBD")),
        div(HTML("<b>Weight:</b> Pointstreak TBD"))
      )
    } else {
      "No data available"
    }
  })
  
  output$season_log <- renderTable({
    data.frame(
      W = sample(2:10, 1),
      L = sample(2:10, 1),
      ERA = round(runif(1, 3.00, 6.00), 2),
      G = sample(20:25, 1),
      IP = round(runif(1, 4.0, 9.0), 1),
      H = sample(70:150, 1),
      R = sample(45:55, 1),
      ER = sample(35:45, 1),
      HR = sample(15:30,1),
      BB = sample(25:50,1),
      WHIP = round(runif(1, 1.10, 1.50), 2)  
    )
  })
  
  observeEvent({
    input$selected_player
    input$date_range
  }, {
    req(input$selected_player, input$date_range)
    
    player_id <- selected_pitcher()$player_id
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    if (!is.null(player_id) && length(player_id) > 0) {
      get_pitch_data(player_id, start_date, end_date)  # ✅ Only prints to console
    }
  })
}

shinyApp(ui, server)

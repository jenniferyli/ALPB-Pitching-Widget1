# library(shiny)
# library(httr)
# library(jsonlite)
# 
# # API Key and Base URLs
# api_key <- "IuHgm3smV65kbC6lMlMLz80DOeEkGSiV6USoQhvZ"
# players_url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/players"
# 
# # Function to fetch pitcher data
# get_pitcher_data <- function() {
#   headers <- add_headers(`x-api-key` = api_key)
#   
#   all_data <- data.frame()
#   page <- 1
#   total_pages <- 1  
#   
#   repeat {
#     url <- paste0(players_url, "?is_pitcher=true&page=", page)
#     response <- GET(url, headers)
#     
#     if (status_code(response) == 200) {
#       data <- content(response, as = "text", encoding = "UTF-8")
#       parsed_data <- fromJSON(data, flatten = TRUE)  
#       
#       if (!is.null(parsed_data$data)) {
#         all_data <- rbind(all_data, parsed_data$data)
#       }
#       
#       total_pages <- parsed_data$meta$pages
#       if (page >= total_pages) break
#       page <- page + 1
#     } else {
#       print(paste("Error fetching pitchers:", status_code(response)))
#       break
#     }
#   }
#   
#   return(all_data)
# }
# 
# # Fetch pitcher data before launching the app
# pitcher_data <- get_pitcher_data()
# 
# # UI
# ui <- fluidPage(
#   titlePanel("ALPB Pitchers"),
#   
#   # First row: Dropdown selection (1/3 width)
#   fluidRow(
#     column(4,   # Dropdown takes 1/3 of the row
#            wellPanel(
#              selectInput("selected_player", "Select a Pitcher:", choices = pitcher_data$player_name)
#            )
#     ),
#     column(8)   # Empty space for alignment (2/3 of the row)
#   ),
#   
#   # Second row: Pitcher Information
#   fluidRow(
#     column(12, 
#            # h3("Profile"),  
#            uiOutput("player_info") 
#     )
#   )
# )
# 
# # Server
# server <- function(input, output, session) {
#   
#   selected_pitcher <- reactive({
#     pitcher_data[pitcher_data$player_name == input$selected_player, ]
#   })
#   
#   output$player_info <- renderUI({
#     player_name <- selected_pitcher()$player_name
#     player_id <- selected_pitcher()$player_id
#     team_name <- selected_pitcher()$team_name
#     handedness <- selected_pitcher()$player_pitching_handedness
#     
#     if (!is.null(player_id) && length(player_id) > 0) {
#       tagList(
#         h4(HTML(paste(player_name))),  # Display Player Name first
#         div(HTML(paste("<b>Player ID:</b>", player_id))),
#         div(HTML(paste("<b>Team Name:</b>", team_name))),
#         div(HTML(paste("<b>Pitching Handedness:</b>", handedness))),
#         div(HTML("<b>Age:</b> Pointstreak TBD")),
#         div(HTML("<b>Height:</b> Pointstreak TBD")),
#         div(HTML("<b>Weight:</b> Pointstreak TBD"))
#       )
#     } else {
#       "No data available"
#     }
#   })
# }
# 
# shinyApp(ui, server)

library(shiny)
library(httr)
library(jsonlite)
library(DT)  # For interactive tables

# API Key and Base URLs
api_key <- "IuHgm3smV65kbC6lMlMLz80DOeEkGSiV6USoQhvZ"
players_url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/players"
pitches_url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/pitches"

# Function to fetch pitcher data
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

# Function to fetch first 5 rows of pitch data for a given player_id
get_pitch_data <- function(player_id) {
  if (is.null(player_id) || length(player_id) == 0) {
    print("⚠️ Error: Invalid player_id (NULL or empty)")
    return(data.frame(Message = "Invalid player_id"))
  }
  
  print(paste("Fetching first 5 pitches for Player ID:", player_id))
  
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
      print(head(pitch_data, 5))  # Print first 5 rows to the console
      return(head(pitch_data, 5)) # Return only first 5 rows
    } else {
      print("⚠️ No pitch data found for this player.")
      return(data.frame(Message = "No pitch data available"))
    }
  } else {
    print(paste("API Error: Status code", status_code(response)))
    return(data.frame(Message = "API Error"))
  }
}

# Fetch pitcher data before launching the app
pitcher_data <- get_pitcher_data()

# UI
ui <- fluidPage(
  titlePanel("ALPB Pitchers"),
  
  # First row: Dropdown selection (1/3 width)
  fluidRow(
    column(4,   # Dropdown takes 1/3 of the row
           wellPanel(
             selectInput("selected_player", "Select a Pitcher:", choices = pitcher_data$player_name)
           )
    ),
    column(8)   # Empty space for alignment (2/3 of the row)
  ),
  
  # Second row: Pitcher Information
  fluidRow(
    column(12, 
           h3("Pitcher Information"),  # Title for the section
           uiOutput("player_info")  # Display player details
    )
  ),
  
  # Third row: Pitch Data Table
  fluidRow(
    column(12, 
           h3("Recent Pitches"),  # Section Title
           DTOutput("pitch_data_table")  # Interactive Table
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
        h4(HTML(paste("<b>Player Name:</b>", player_name))),  # Display Player Name first
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
  
  # Fetch and display pitch data when a player is selected
  observeEvent(input$selected_player, {
    player_id <- selected_pitcher()$player_id
    
    if (!is.null(player_id) && length(player_id) > 0) {
      pitch_data <- get_pitch_data(player_id)
      
      output$pitch_data_table <- renderDT({
        datatable(
          pitch_data,
          options = list(pageLength = 5, scrollX = TRUE),
          rownames = FALSE
        )
      })
    }
  })
}

shinyApp(ui, server)



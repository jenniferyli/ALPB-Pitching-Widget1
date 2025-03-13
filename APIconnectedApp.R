# library(httr)
# library(jsonlite)
# 
# base_url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/players"
# headers <- add_headers(`x-api-key` = "IuHgm3smV65kbC6lMlMLz80DOeEkGSiV6USoQhvZ")
# 
# player_names <- c()
# 
# page <- 1
# total_pages <- 1  
# 
# repeat {
#   url <- paste0(base_url, "?is_pitcher=true&page=", page)
# 
#   response <- GET(url, headers)
#   
#   if (status_code(response) == 200) {
#     data <- content(response, as = "text", encoding = "UTF-8")
#     parsed_data <- fromJSON(data)
#     
#     player_names <- c(player_names, parsed_data$data$player_name)
#     
#     total_pages <- parsed_data$meta$pages
# 
#     if (page >= total_pages) {
#       break
#     }
#     
#     page <- page + 1
#   } else {
#     print(paste("Error:", status_code(response)))
#     break
#   }
# }
# 
# total_players <- length(player_names)
# print(player_names)
# print(total_players)

library(shiny)
library(httr)
library(jsonlite)

# Function to fetch pitcher names from API
get_pitcher_names <- function() {
  base_url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/players"
  headers <- add_headers(`x-api-key` = "IuHgm3smV65kbC6lMlMLz80DOeEkGSiV6USoQhvZ")
  
  player_names <- c()
  page <- 1
  total_pages <- 1  
  
  repeat {
    url <- paste0(base_url, "?is_pitcher=true&page=", page)
    response <- GET(url, headers)
    
    if (status_code(response) == 200) {
      data <- content(response, as = "text", encoding = "UTF-8")
      parsed_data <- fromJSON(data)
      player_names <- c(player_names, parsed_data$data$player_name)
      total_pages <- parsed_data$meta$pages
      
      if (page >= total_pages) break
      page <- page + 1
    } else {
      print(paste("Error:", status_code(response)))
      break
    }
  }
  
  return(player_names)
}

# Fetch players before launching the app
player_names <- get_pitcher_names()

# Shiny UI
ui <- fluidPage(
  titlePanel("Select a Pitcher"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_player", "Choose a Player:", choices = player_names)
    ),
    mainPanel(
      textOutput("player_output")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  output$player_output <- renderText({
    paste("You selected:", input$selected_player)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


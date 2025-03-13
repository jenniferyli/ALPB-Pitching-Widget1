library(shiny)
library(httr)
library(jsonlite)

get_pitcher_data <- function() {
  base_url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/players"
  headers <- add_headers(`x-api-key` = "IuHgm3smV65kbC6lMlMLz80DOeEkGSiV6USoQhvZ")
  
  all_data <- data.frame() 
  page <- 1
  total_pages <- 1  
  
  repeat {
    url <- paste0(base_url, "?is_pitcher=true&page=", page)
    response <- GET(url, headers)
    
    if (status_code(response) == 200) {
      data <- content(response, as = "text", encoding = "UTF-8")
      parsed_data <- fromJSON(data)
      
      all_data <- rbind(all_data, parsed_data$data)
      
      total_pages <- parsed_data$meta$pages
      
      if (page >= total_pages) break
      page <- page + 1
    } else {
      print(paste("Error:", status_code(response)))
      break
    }
  }
  
  return(all_data)
}

pitcher_data <- get_pitcher_data()

ui <- fluidPage(
  titlePanel("Select a Pitcher"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_player", "Choose a Player:", choices = pitcher_data$player_name)
    ),
    
    mainPanel(
      textOutput("player_handedness")
    )
  )
)

server <- function(input, output) {
  output$player_handedness <- renderText({
    # Find the selected player's handedness
    handedness <- pitcher_data$player_pitching_handedness[pitcher_data$player_name == input$selected_player]
    
    if (length(handedness) > 0) {
      paste("Pitching Handedness:", handedness)
    } else {
      "No data available"
    }
  })
}

shinyApp(ui = ui, server = server)

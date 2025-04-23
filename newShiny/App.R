library(shiny)
library(dplyr)

# === Load data and functions ===
source("getPointstreakPlayers.R")
source("getALPBdata.R")
source("getSeasonStats.R") 
source("getALPBPitches.R") 

# Create full name for dropdown
pitchers_df <- pitchers_df %>%
  mutate(full_name = paste(fname, lname))

# === Shiny App UI ===
ui <- fluidPage(
  titlePanel("ALPB Pitcher Lookup"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_player", "Choose a Pitcher:", choices = pitchers_df$full_name)
    ),
    
    mainPanel(
      h4("Pointstreak Data:"),
      tableOutput("player_info"),
      
      h4("ALPB Data:"),
      verbatimTextOutput("alpb_info"),
      
      h4("Pointstreak playerlinkid:"),
      verbatimTextOutput("playerlink_output"),
      
      # ✅ NEW: Season Stats Table
      h4("Pointstreak Season Pitching Stats"),
      tableOutput("season_stats_output"),
      
      # ✅ NEW: ALPB Pitches Table
      h4("ALPB Pitch-by-Pitch Data"),
      tableOutput("alpb_pitch_data")
    )
  )
)

# === Server logic ===
server <- function(input, output) {
  # Pointstreak player row
  selected_player_row <- reactive({
    pitchers_df %>%
      filter(full_name == input$selected_player)
  })
  
  # Store ALPB player ID reactively
  alpb_player_id <- reactiveVal(NULL)
  
  # Show full player info
  output$player_info <- renderTable({
    req(selected_player_row())
    selected_player_row() %>% select(-full_name)
  })
  
  # Show ALPB info
  output$alpb_info <- renderPrint({
    req(selected_player_row())
    selected <- selected_player_row()
    result <- get_alpb_pitcher_info(selected$fname, selected$lname)
    
    if (is.na(result$player_id) || result$player_id == "data unavailable") {
      alpb_player_id(NULL)
      cat("\u26A0\uFE0F ALPB Data Unavailable")
    } else {
      alpb_player_id(result$player_id)
      cat("Player ID: ", result$player_id, "\n")
      cat("Pitching Handedness: ", result$pitching_hand)
    }
  })
  
  # Show Pointstreak playerlinkid
  output$playerlink_output <- renderPrint({
    req(selected_player_row())
    cat("playerlinkid:", selected_player_row()$playerlinkid)
  })
  
  # ✅ Show season stats from getSeasonStats.R
  output$season_stats_output <- renderTable({
    req(selected_player_row())
    playerlinkid <- selected_player_row()$playerlinkid
    stats <- get_pitching_stats_only(playerlinkid)
    
    if (is.null(stats)) {
      return(tibble::tibble(message = "\u26A0\uFE0F No season stats found for this player."))
    }
    
    stats
  })
  
  output$alpb_pitch_data <- renderTable({
    req(alpb_player_id())
    pitch_df <- get_alpb_pitches_by_pitcher(alpb_player_id())
    if (is.null(pitch_df)) {
      return(tibble::tibble(message = "\u26A0\uFE0F No pitch data found for this player."))
    }
    pitch_df
  })
}

shinyApp(ui = ui, server = server)

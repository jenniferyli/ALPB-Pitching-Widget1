library(shiny)
library(httr)
library(jsonlite)
library(DT)
library(rsconnect)
library(ggplot2)
library(tidyr)

# source files
source("getPointstreakPlayers.R")
source("getALPBdata.R")
source("getSeasonStats.R") 
source("getALPBPitches.R") 

# ui card helper
card_w_header <- function(title, body) {
  div(class = "card",
      div(class = "card-header bg-info text-white text-center font-weight-bold",
          style = "padding-top: 3px; padding-bottom: 3px;",
          title),
      div(class = "card-body d-flex justify-content-center align-items-center",
          div(style = "text-align: center; width: 100%;", body)
      )
  )
}

# dropdown
pitchers_df <- pitchers_df %>%
  mutate(full_name = paste(fname, lname))

# shiny ui

ui <- fluidPage(
  fluidRow(
    column(1),
    column(10,
           div(style = "text-align: center;", h1("ALPB Pitchers")),
           
           fluidRow(
             column(4,
                    wellPanel(selectInput("selected_player", "Choose a Pitcher:", choices = pitchers_df$full_name))
             ),
             column(6,
                    card_w_header("Season Stats", tableOutput("season_log_fake"))
             ), 
             
             column(2,
                    downloadButton("download_pdf", "Download PDF")
             )
           ),
           
           fluidRow(
             column(4,
                    div(style = "margin-bottom: 20px;",  # 
                        card_w_header("Pitcher Information",
                                      div(style = "text-align: left;", uiOutput("player_info_placeholder"))  # 
                        )
                    )
             ),
             
             
             column(8,
                    card_w_header("Game Log", tableOutput("game_log"))
             )
           ),
           
           fluidRow(
             column(6,
                    card_w_header(uiOutput("scatter_header"), plotOutput("velPlot", height = "300px"))
             ),
             
             column(6,
                    card_w_header("Induced Vertical Break vs Horizontal Break", plotOutput("breakPlot", height = "300px"))
                    # card_w_header("Strike Zone", plotOutput("heatmaps", height = "300px"))
             )
           ),
           fluidRow(column(3,
                           radioButtons("break_type", "Break Type:",
                                        choices = c("Vertical Break" = "induced_vert_break",
                                                    "Horizontal Break" = "horz_break")),
                           
                           
           ), column(3,  radioButtons("tag_choice", "Pitch Tagging Method:",
                                      choices = c("Machine Tagged" = "auto_pitch_type",
                                                  "Human Tagged" = "tagged_pitch_type")
           )
           ),
           column(3,
                  uiOutput("pitch_type_ui")
           )
           ),
           fluidRow(
             column(6,
                    card_w_header("Pitch map vs RH Batters", plotOutput("heatmap_right", height = "300px"))
             ),
             
             column(6,
                    card_w_header("Pitch map vs LH Batters", plotOutput("heatmap_left", height = "300px"))
             )
           ),
           fluidRow(column(12,
                           card_w_header("Pitch Type Percentages for Each Count",DTOutput("pitchTable"))
           )),
           fluidRow(
             column(12,
                    h4("Pointstreak Data:"),
                    tableOutput("player_info"))
             ),
           fluidRow(
             column(12,
                    h4("ALPB Data:"),
                    verbatimTextOutput("alpb_info"))),
           fluidRow(
             column(12,
                    h4("Pointstreak playerlinkid:"),
                    verbatimTextOutput("playerlink_output"))),
           fluidRow(
             column(12,
                    h4("Pointstreak Season Pitching Stats"),
                    tableOutput("season_stats_output"))),
           fluidRow(
             column(12,
                    h4("ALPB Pitch-by-Pitch Data"),
                    tableOutput("alpb_pitch_data"))
           )
           
    ),
    column(1)
  )
)

# server
server <- function(input, output) {
  # Pointstreak player row
  selected_player_row <- reactive({
    pitchers_df %>%
      filter(full_name == input$selected_player)
  })

  # Store ALPB player ID reactively
  alpb_player_id <- reactiveVal(NULL)
  
  observeEvent(input$selected_player, {
    selected <- selected_player_row()
    result <- get_alpb_pitcher_info(selected$fname, selected$lname)
    
    if (is.null(result) || is.na(result$player_id) || result$player_id == "data unavailable") {
      alpb_player_id(NULL)
    } else {
      alpb_player_id(result$player_id)
    }
  })
  
  pitch_data <- reactive({
    get_alpb_pitches_by_pitcher(alpb_player_id())
  })
  
  
  output$scatter_header <- renderUI({
    if(input$break_type == "induced_vert_break") {
      tagList("Vertical Break vs Velocity")  # Dynamic header for the plot
    } else if(input$break_type == "horz_break") {
      tagList("Horizontal Break vs Velocity")  # Dynamic header for the plot
    }
  })
  
  selected_pitcher <- reactive({
    input$selected_player  # Return the full name of the selected pitcher
  })
  
  
  pitch_types <- reactive({
    data <- pitch_data()
    req(data)
    #unique(na.omit(data$auto_pitch_type)) #Can change to user specification later
    # Filter out "untagged" values
    if (input$tag_choice == "auto_pitch_type") {
      pitch_types <- unique(na.omit(data$auto_pitch_type))
    } else {
      pitch_types <- unique(na.omit(data$tagged_pitch_type))
    }
    pitch_types <- pitch_types[pitch_types != "Undefined"]
  })
  
  output$pitch_type_ui <- renderUI({
    req(pitch_types())
    selectInput("selected_pitch_type", "Select Pitch Type:",
                choices = c("All", pitch_types()), selected = "All")
  })
  
  output$player_info_placeholder <- renderUI({
    div(HTML("<b>Weight:</b> Pointstreak TBD"))
  })
  
  # # Season log: calculated from game log
  output$season_log_fake <- renderTable({
    # Convert innings from baseball notation to decimal
    IP_vals <- c(7.0, 6.1, 5.2)
    IP_decimal <- c(7.0, 6 + 1/3, 5 + 2/3)  # Keeping fractional innings as decimals
    IP_total <- sum(IP_decimal)  # No need for rounding as we want a clean total

    H_total <- sum(c(6, 5, 8))
    R_total <- sum(c(2, 3, 4))
    ER_total <- sum(c(2, 2, 3))
    HR_total <- sum(c(1, 0, 2))
    BB_total <- sum(c(1, 2, 3))

    ERA <- round((ER_total * 9) / IP_total, 2)
    WHIP <- round((H_total + BB_total) / IP_total, 2)

    # Whole numbers: Wins (W), Losses (L), Games (G)
    W <- 2
    L <- 1
    G <- 3

    # Ensure H, R, ER, HR, BB are integers (no decimals)
    data.frame(
      W = as.integer(W),
      L = as.integer(L),
      ERA = ERA,
      G = as.integer(G),
      IP = IP_total,  
      H = as.integer(H_total),  
      R = as.integer(R_total), 
      ER = as.integer(ER_total), 
      HR = as.integer(HR_total),  
      BB = as.integer(BB_total), 
      WHIP = WHIP
    )
  })
  # 
  # Game log: 3 realistic games
  output$game_log <- renderTable({
    data.frame(
      Game = 1:3,
      Rslt = c("W", "W", "L"),
      ERA = round(c((2*9)/7.0, (2*9)/(6 + 1/3), (3*9)/(5 + 2/3)), 2),
      IP = c(7.0, 6.1, 5.2),
      H = as.integer(c(6, 5, 8)),  # Keep Hits as whole numbers
      R = as.integer(c(2, 3, 4)),  # Keep Runs as whole numbers
      ER = as.integer(c(2, 2, 3)),  # Keep Earned Runs as whole numbers
      HR = as.integer(c(1, 0, 2)),  # Keep Home Runs as whole numbers
      BB = as.integer(c(1, 2, 3)),  # Keep Walks as whole numbers
      WHIP = round(c((6+1)/7.0, (5+2)/(6 + 1/3), (8+3)/(5 + 2/3)), 2)
    )
  })
  
  source('getGraphs.R')
  output$velPlot <- renderPlot({
    build_graph(pitch_data(), "rel_speed", input$break_type, input$tag_choice)
  })
  
  output$breakPlot <- renderPlot({
    build_graph(pitch_data(), "horz_break", "induced_vert_break", input$tag_choice)
  })
  source('getHeatmap.R')
  
  output$heatmap_right <- renderPlot({
    data <- pitch_data()
    req(data)
    filtered_data <- data[data$batter_side == "Right", ]
    # if (!is.null(input$selected_pitch_type) && input$selected_pitch_type != "All") {
    #   filtered_data <- filtered_data[filtered_data$auto_pitch_type == input$selected_pitch_type, ]
    # }
    if (!is.null(input$selected_pitch_type) && input$tag_choice == "auto_pitch_type" && input$selected_pitch_type != "All") {
      filtered_data <- filtered_data[filtered_data$auto_pitch_type == input$selected_pitch_type, ]
    } else if (!is.null(input$selected_pitch_type) && input$tag_choice == "tagged_pitch_type" && input$selected_pitch_type != "All") {
      filtered_data <- filtered_data[filtered_data$tagged_pitch_type == input$selected_pitch_type, ]
    }
    build_heatmap(filtered_data)
  })
  
  output$heatmap_left <- renderPlot({
    data <- pitch_data()
    req(data)
    filtered_data <- data[data$batter_side == "Left", ]
    # if (!is.null(input$selected_pitch_type) && input$selected_pitch_type != "All") {
    #   filtered_data <- filtered_data[filtered_data$auto_pitch_type == input$selected_pitch_type, ]
    # }
    if (!is.null(input$selected_pitch_type) && input$tag_choice == "auto_pitch_type" && input$selected_pitch_type != "All") {
      filtered_data <- filtered_data[filtered_data$auto_pitch_type == input$selected_pitch_type, ]
    } else if (!is.null(input$selected_pitch_type) && input$tag_choice == "tagged_pitch_type" && input$selected_pitch_type != "All") {
      filtered_data <- filtered_data[filtered_data$tagged_pitch_type == input$selected_pitch_type, ]
    }
    build_heatmap(filtered_data)
  })
  
  source('getPDFReport.R')
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0(selected_pitcher(), " Pitcher Report.pdf")
    },
    content = function(file) {
      # Generate the PDF
      #pdf_path <- get_pdf_working(pitch_data(), selected_pitcher()$player_name, input$date_range[1],input$date_range[2])
      pdf_path <- get_blank_pdf(pitch_data(), selected_pitcher(), input$tag_choice)
      # Copy it to the file path that downloadHandler expects
      file.copy(pdf_path, file)
    },
    contentType = "application/pdf"
  )
  
  source("pitchSplit.R")
  
  # Render the data table
  
  output$pitchTable <- renderDT({
    # df <- get_pitch_type_percentages(pitch_data())
    df <- get_pitch_type_percentages(pitch_data(), input$tag_choice)
    datatable(df, options = list(pageLength = 12, scrollX = TRUE))  # Display the table
  })
  
  # Show full player info
  output$player_info <- renderTable({
    req(selected_player_row())
    selected_player_row() %>% dplyr::select(-full_name)
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

  # # ✅ Show season stats from getSeasonStats.R
  output$season_stats_output <- renderTable({
    req(selected_player_row())
    playerlinkid <- selected_player_row()$playerlinkid
    stats <- get_pitching_stats_only(playerlinkid)

    if (is.null(stats)) {
      return(tibble::tibble(message = "\u26A0\uFE0F No season stats found for this player."))
    }

    stats
  })
  # 
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

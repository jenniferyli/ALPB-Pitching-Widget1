# 
# library(shiny)
# library(httr)
# library(jsonlite)
# 
# api_key <- "IuHgm3smV65kbC6lMlMLz80DOeEkGSiV6USoQhvZ"
# players_url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/players"
# pitches_url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/pitches"
# 
# # Function to create a card without a header
# card_w_no_header <- function(body) {
#   div(class = "card-body d-flex justify-content-center", body)
# }
# 
# # function to create a card container without a header but with a slight inset
# 
# card_w_inset_and_no_header <- function(body) {
#   div(class = "card",
#       div(class = "card-body d-inline-flex justify-content-center", body)
#   )
# }
# 
# # Fetch pitcher data
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
# # Fetch and filter pitch data by date range
# get_pitch_data <- function(player_id, start_date, end_date) {
#   if (is.null(player_id) || length(player_id) == 0) {
#     print("Error: Invalid player_id (NULL or empty)")
#     return(NULL)
#   }
# 
#   headers <- add_headers(
#     `x-api-key` = api_key,
#     `player_id` = player_id
#   )
# 
#   response <- GET(pitches_url, headers)
# 
#   if (status_code(response) == 200) {
#     data <- content(response, as = "text", encoding = "UTF-8")
#     parsed_data <- fromJSON(data, flatten = TRUE)
# 
#     if (!is.null(parsed_data$data) && length(parsed_data$data) > 0) {
#       pitch_data <- as.data.frame(parsed_data$data)
# 
#       pitch_data$date <- as.Date(pitch_data$date)
#       cleaned_data <- subset(pitch_data, date >= start_date & date <= end_date)
# 
#       print(paste("\nFiltered pitches for Player ID:", player_id))
#       print(head(cleaned_data, 5))  # ✅ PRINT TO CONSOLE ONLY
# 
#       return(cleaned_data)
#     } else {
#       print("⚠️ No pitch data found for this player.")
#     }
#   } else {
#     print(paste("API Error: Status code", status_code(response)))
#   }
# 
#   return(NULL)
# }
# 
# # UI Card Helper
# card_w_header <- function(title, body) {
#   div(class = "card",
#       div(class = "card-header bg-info text-white text-center font-weight-bold", title),
#       div(class = "card-body d-flex justify-content-center align-items-center",
#           div(style = "text-align: center; width: 100%;", body)
#       )
#   )
# }
# 
# # Load pitcher data
# pitcher_data <- get_pitcher_data()
# 
# # UI
# ui <- fluidPage(
#   div(style = "text-align: center;", h1("ALPB Pitchers")),
# 
#   fluidRow(
#     column(4,
#            wellPanel(
#              selectInput("selected_player", "Select a Pitcher:", choices = pitcher_data$player_name),
#              dateRangeInput("date_range", "Select Date Range:",
#                             start = Sys.Date() - 365,
#                             end = Sys.Date())
#            )
#     ),
#     column(8,
#            card_w_header("Season Stats", tableOutput("season_log"))
#     )
#   ),
# 
#   fluidRow(
#     column(12,
#            h3("Pitcher Information"),
#            uiOutput("player_info")
#     )
#   ),
#   fluidRow(
#     column(12,
#            card_w_header("Gamesplits:", tableOutput("game_log"))  # Placeholder table in full width
#     )
#   ),
# 
#   fluidRow(
#     column(6,  # Main content takes up 2/3 of the screen
#            card_w_header("Breaks", plotOutput("scatterPlot", height = "300px")) # Correct way to set height for plotOutput
#     ),
#     # column(6,
#     #        card_w_header("Heat Maps", plotOutput("heatmaps", height = "300px")) # Correct way to set height for plotOutput
#     # )
#   ),
#   fluidRow(
#     column(3,  # Sidebar takes up 1/3 of the screen
#            # textOutput("pitcher_hand"),
#            # textOutput("pitcher_id"),
#            radioButtons("break_type", "Select Break Type:",
#                         choices = c("Vertical Break" = "induced_vert_break",
#                                     "Horizontal Break" = "horz_break"),
#                         selected = "induced_vert_break")
# 
#     ),
#     column(3,  # Sidebar takes up 1/3 of the screen
#            # textOutput("pitcher_hand"),
#            # textOutput("pitcher_id"),
# 
#            radioButtons("tag_choice", "Select Pitch Tagging Method:",
#                         choices = c("Human Tagged" = "tagged_pitch_type",
#                                     "Machine Tagged" = "auto_pitch_type"),
#                         selected = "auto_pitch_type")
# 
#     ),
#     column(6,
#            card_w_no_header(selectizeInput("pitch_type", "Choose a pitch type:", choices = NULL, options = list(placeholder = 'Type to search...', onInitialize = I('function() { this.setValue(""); }')))),
#     )
#   )
# )
# 
# # Server
# server <- function(input, output, session) {
#   filter_data <- reactive({
#     player_id <- selected_pitcher()$player_id
#     start_date <- input$date_range[1]
#     end_date <- input$date_range[2]
# 
#     if (!is.null(player_id) && length(player_id) > 0) {
#       get_pitch_data(player_id, start_date, end_date)  # Get the data
#     } else {
#       NULL
#     }
#   })
#   selected_pitcher <- reactive({
#     pitcher_data[pitcher_data$player_name == input$selected_player, ]
#   })
# 
#   # filtered_data <- reactive({
#   #   req(input$selected_pitcher)
#   #   get_data() %>% filter(Pitcher == input$selected_pitcher)
#   #
#   # })
#   output$player_info <- renderUI({
#     player_name <- selected_pitcher()$player_name
#     player_id <- selected_pitcher()$player_id
#     team_name <- selected_pitcher()$team_name
#     handedness <- selected_pitcher()$player_pitching_handedness
# 
#     if (!is.null(player_id) && length(player_id) > 0) {
#       tagList(
#         h4(HTML(paste(player_name))),
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
# 
#   output$season_log <- renderTable({
#     data.frame(
#       W = sample(2:10, 1),
#       L = sample(2:10, 1),
#       ERA = round(runif(1, 3.00, 6.00), 2),
#       G = sample(20:25, 1),
#       IP = round(runif(1, 4.0, 9.0), 1),
#       H = sample(70:150, 1),
#       R = sample(45:55, 1),
#       ER = sample(35:45, 1),
#       HR = sample(15:30,1),
#       BB = sample(25:50,1),
#       WHIP = round(runif(1, 1.10, 1.50), 2)
#     )
#   })
#   output$game_log <- renderTable({
#     data.frame(
#       Game = 1:3,
#       Rslt = c("W", "W", "L"),
#       ERA = round(runif(3, 3.00, 6.00), 2),
#       G = sample(1:5, 3, replace = TRUE),
#       IP = round(runif(3, 4.0, 9.0), 1),
#       H = sample(3:10, 3, replace = TRUE),
#       R = sample(2:7, 3, replace = TRUE),
#       ER = sample(2:7, 3, replace = TRUE),
#       HR = sample(0:2, 3, replace = TRUE),
#       BB = sample(1:5, 3, replace = TRUE),
#       WHIP = round(runif(3, 1.10, 1.50), 2),
#       SO = sample(3:10, 3, replace = TRUE),
#       AVG = round(runif(3, 0.200, 0.350), 3)
#     )
#   })
# 
# 
# 
#   # Scatter plot for Break vs RelSpeed
#   output$scatterPlot <- renderPlot({
#     # filtered_data <- NULL
#     # player_id <- selected_pitcher()$player_id
#     # start_date <- input$date_range[1]
#     # end_date <- input$date_range[2]
#     # if (!is.null(player_id) && length(player_id) > 0) {
#     #   filtered_data <- get_pitch_data(player_id, start_date, end_date)
#     #   # print(head(filtered_data, 5))
#     # }
#     filtered_data <- filter_data()
#     # print("printing NOW")
#     # print(head(filtered_data, 5))
#     if (!is.null(filtered_data) & nrow(filtered_data) > 0) {
#       #Tag_name =  get(input$tag_choice)
#       filtered_data$TagStatus <- ifelse(filtered_data[[input$tag_choice]] == "Undefined" | is.na(filtered_data[[input$tag_choice]]), "Untagged", as.character(filtered_data[[input$tag_choice]]))
# 
# 
#       ggplot(filtered_data, aes(x = rel_speed, y = filtered_data[[input$break_type]], color = TagStatus)) +
#         geom_point(alpha = 0.7, size = 2) +
#         labs(title = paste(input$break_type, "vs. Velocity" ),
#              x = "Velocity",
#              y = input$break_type) +
#         theme_minimal() +
#         scale_color_manual("Pitch Tag", values = c("Fastball" = "red",
#                                                    "Four-Seam" = "red",
#                                                    "Changeup" = "blue",
#                                                    "ChangeUp" = "blue",
#                                                    "Sinker" = "green",
#                                                    "Curveball" = "yellow",
#                                                    "Slider" = "purple",
#                                                    "Splitter" = "black",
#                                                    "Cutter" = "pink",
#                                                    "Untagged" = "gray")) # Custom colors (optional)
#     }
#   })
#   # source('getHeatMap.R')
#   #
#   # # Create the heatmaps as a reactive expressions based on a dropdown
#   #
#   # # updateSelectizeInput(session, "heatPitchType", choices = c("FB", "SL", "CB", "CH"), server = TRUE)
#   #
#   # heatmap_plots <- reactive({
#   #   req(filtered_data)
#   #   build_heatmap(filtered_data)
#   # })
#   #
#   # # Render the heatmaps in plot form (will be put in a card in the UI)
#   #
#   # output$heatmaps <- renderPlot({
#   #   heatmap_plots()
#   # })
# }
# 
# shinyApp(ui, server)
# 

library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)

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

# Fetch pitch data and filter by date
get_pitch_data <- function(player_id, start_date = NULL, end_date = NULL) {
  if (is.null(player_id) || length(player_id) == 0) {
    warning("Invalid player_id provided.")
    return(NULL)
  }
  
  url <- paste0(pitches_url, "?pitcher_id=", player_id)
  headers <- add_headers(`x-api-key` = api_key)
  response <- GET(url, headers)
  
  if (status_code(response) == 200) {
    data <- content(response, as = "text", encoding = "UTF-8")
    parsed <- fromJSON(data, flatten = TRUE)
    
    if (!is.null(parsed$data) && length(parsed$data) > 0) {
      df <- as.data.frame(parsed$data)
      
      # Filter by date if provided
      if (!is.null(start_date) && !is.null(end_date) && "date" %in% names(df)) {
        df$date <- as.Date(df$date)
        df <- df[df$date >= start_date & df$date <= end_date, ]
      }
      
      cat("\n✅ Pitch data for player_id:", player_id, "\n")
      print(head(df, 10))
      return(df)
    } else {
      warning("No pitch data found for this player.")
      return(NULL)
    }
  } else {
    stop(paste("API Error:", status_code(response)))
  }
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
    column(4,
           card_w_header("Pitcher Information", uiOutput("player_info"))
    ),
    column(8,
           card_w_header("Game Log", tableOutput("game_log"))
    )
    
  ),
  
  
  fluidRow(
    column(4,
           card_w_header("Break vs Velocity", plotOutput("scatterPlot", height = "300px"))
    ),
    column(2,
           card_w_header("Graph Controls",
                         tagList(
                           radioButtons("break_type", "Break Type:",
                                        choices = c("Vertical Break" = "induced_vert_break",
                                                    "Horizontal Break" = "horz_break")),
                           radioButtons("tag_choice", "Pitch Tagging Method:",
                                        choices = c("Human Tagged" = "tagged_pitch_type",
                                                    "Machine Tagged" = "auto_pitch_type"))
                         )
           )
    ),
    column(6,            
           card_w_header("Heat Maps", plotOutput("heatmaps", height = "300px")) # Correct way to set height for plotOutput
)
  ),
  
  fluidRow(
    column(12,
           h3("Pitch Data for Selected Pitcher"),
           tableOutput("player_pitches_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  selected_pitcher <- reactive({
    pitcher_data[pitcher_data$player_name == input$selected_player, ]
  })
  
  pitch_data <- reactive({
    req(input$selected_player)
    get_pitch_data(
      selected_pitcher()$player_id,
      input$date_range[1],
      input$date_range[2]
    )
  })
  
  output$player_info <- renderUI({
    player <- selected_pitcher()
    
    if (nrow(player) > 0) {
      tagList(
        h4(HTML(player$player_name)),
        div(HTML(paste("<b>Player ID:</b>", player$player_id))),
        div(HTML(paste("<b>Team Name:</b>", player$team_name))),
        div(HTML(paste("<b>Pitching Handedness:</b>", player$player_pitching_handedness))),
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
  
  output$player_pitches_table <- renderTable({
    df <- pitch_data()
    if (!is.null(df) && nrow(df) > 0) {
      df
    } else {
      data.frame(Message = "No pitch data available for this player.")
    }
  })
  
  output$game_log <- renderTable({
        data.frame(
          Game = 1:3,
          Rslt = c("W", "W", "L"),
          ERA = round(runif(3, 3.00, 6.00), 2),
          G = sample(1:5, 3, replace = TRUE),
          IP = round(runif(3, 4.0, 9.0), 1),
          H = sample(3:10, 3, replace = TRUE),
          R = sample(2:7, 3, replace = TRUE),
          ER = sample(2:7, 3, replace = TRUE),
          HR = sample(0:2, 3, replace = TRUE),
          BB = sample(1:5, 3, replace = TRUE),
          WHIP = round(runif(3, 1.10, 1.50), 2),
          SO = sample(3:10, 3, replace = TRUE),
          AVG = round(runif(3, 0.200, 0.350), 3)
        )
      })
  
  output$scatterPlot <- renderPlot({
    df <- pitch_data()
    
    if (!is.null(df) && nrow(df) > 0 &&
        input$break_type %in% names(df) &&
        "rel_speed" %in% names(df)) {
      
      df$TagStatus <- ifelse(df[[input$tag_choice]] == "Undefined" | is.na(df[[input$tag_choice]]),
                             "Untagged", as.character(df[[input$tag_choice]]))
      
      ggplot(df, aes(x = rel_speed, y = df[[input$break_type]], color = TagStatus)) +
        geom_point(alpha = 0.7, size = 2) +
        labs(title = paste(input$break_type, "vs. Velocity"),
             x = "Velocity",
             y = input$break_type) +
        theme_minimal() +
        scale_color_manual("Pitch Tag", values = c(
          "Fastball" = "red", 
          "Four-Seam" = "red",
          "Changeup" = "blue", 
          "ChangeUp" = "blue", 
          "Sinker" = "green", 
          "Curveball" = "yellow", 
          "Slider" = "purple", 
          "Splitter" = "black", 
          "Cutter" = "pink",
          "Untagged" = "gray"
        ))
    }
  })
  source('getHeatmap.R')
  
  # Create the heatmaps as a reactive expressions based on a dropdown
  
  # updateSelectizeInput(session, "heatPitchType", choices = c("FB", "SL", "CB", "CH"), server = TRUE)
  
  heatmap_plots <- reactive({
    data <- pitch_data()
    req(data)
    build_heatmap(data)
  })
  
  # Render the heatmaps in plot form (will be put in a card in the UI)
  
  output$heatmaps <- renderPlot({
    heatmap_plots()
  })
}

shinyApp(ui, server)

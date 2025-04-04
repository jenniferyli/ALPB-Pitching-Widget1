library(shiny)
library(httr)
library(jsonlite)
library(DT)
library(rsconnect)
library(ggplot2)

api_key <- "IuHgm3smV65kbC6lMlMLz80DOeEkGSiV6USoQhvZ"
players_url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/players"
pitches_url <- "https://1ywv9dczq5.execute-api.us-east-2.amazonaws.com/ALPBAPI/pitches"

# fetch pitcher data
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

get_pitch_data <- function(player_id, start_date = NULL, end_date = NULL) {
  if (is.null(player_id) || length(player_id) == 0) {
    warning("Invalid player_id provided.")
    return(NULL)
  }
  
  headers <- add_headers(`x-api-key` = api_key)
  all_data <- data.frame()
  page <- 1
  
  repeat {
    url <- paste0(pitches_url, "?pitcher_id=", player_id, "&page=", page)
    response <- GET(url, headers)
    
    if (status_code(response) == 200) {
      data <- content(response, as = "text", encoding = "UTF-8")
      parsed <- fromJSON(data, flatten = TRUE)
      
      if (!is.null(parsed$data) && length(parsed$data) > 0) {
        all_data <- rbind(all_data, parsed$data)
        page <- page + 1
      } else {
        break  
      }
    } else {
      stop(paste("API Error:", status_code(response)))
    }
  }
  
  # filter by date
  if (nrow(all_data) > 0 && !is.null(start_date) && !is.null(end_date) && "date" %in% names(all_data)) {
    all_data$date <- as.Date(all_data$date)  # direct conversion is safe
    all_data <- all_data[all_data$date >= start_date & all_data$date <= end_date, ]
  }
  
  
  cat("\n✅ Total pitch records fetched:", nrow(all_data), "\n")
  return(all_data)
}



# UI Card Helper
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

# load pitcher data
pitcher_data <- get_pitcher_data()

ui <- fluidPage(
  fluidRow(
    column(1),
    column(10,
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
             column(6,
                    card_w_header("Season Stats", tableOutput("season_log"))
             ), 
             
             column(2,
                    card_w_header("Export as PDF", actionButton("createPDF", "Generate!", class = "btn btn-primary btn-primary bg-info"))
             )
           ),
           
           fluidRow(
             column(4,
                    div(style = "margin-bottom: 20px;",  # ⬅️ adds space below the card
                        card_w_header("Pitcher Information",
                                      div(style = "text-align: left;", uiOutput("player_info"))  # ⬅️ right-align the content
                        )
                    )
             ),
             
             
             column(8,
                    card_w_header("Game Log", tableOutput("game_log"))
             )
           ),
           
           fluidRow(
             column(6,
                    card_w_header(uiOutput("scatter_header"), plotOutput("scatterPlot", height = "300px"))
             ),
             
             column(6,            
                    card_w_header("Strike Zone", plotOutput("heatmaps", height = "300px"))
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
              )
           )
           # fluidRow(
           #   column(12,
           #          h3("Pitch Data for Selected Pitcher"),
           #          # tableOutput("player_pitches_table")
           #          DTOutput("player_pitches_table")
           #          
           #   )
           # )
    ),
    column(1)
  )
)


# server
server <- function(input, output, session) {
  
  output$scatter_header <- renderUI({
    if(input$break_type == "induced_vert_break") {
      tagList("Vertical Break vs Velocity")  # Dynamic header for the plot
    } else if(input$break_type == "horz_break") {
      tagList("Horizontal Break vs Velocity")  # Dynamic header for the plot
    }
  })
  
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
  
  #display the table 
  # output$player_pitches_table <- renderDT({
  #   df <- pitch_data()
  #   if (!is.null(df) && nrow(df) > 0) {
  #     datatable(df, options = list(pageLength = 25, scrollX = TRUE))
  #   } else {
  #     datatable(data.frame(Message = "No pitch data available for this player."))
  #   }
  # })
  
  
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
      
      ggplot(df, aes(x = rel_speed, y = .data[[input$break_type]], color = TagStatus)) +
        geom_point(alpha = 0.7, size = 2) +
        labs(
             x = "Velocity (mph)",
             y = "Break (inches)") +
        theme_minimal() +
        scale_color_manual("Pitch Tag", values = c(
          "Fastball" = "red", 
          "Four-Seam" = "red",
          "Changeup" = "blue", 
          "ChangeUp" = "blue", 
          "Sinker" = "green", 
          "Curveball" = "brown", 
          "Slider" = "purple", 
          "Splitter" = "black", 
          "Cutter" = "pink",
          "Untagged" = "gray"
        ))
    }
  })
  source('getHeatmap.R')
  
 #heatmaps
  
  heatmap_plots <- reactive({
    data <- pitch_data()
    req(data)
    build_heatmap(data)
  })

  
  output$heatmaps <- renderPlot({
    heatmap_plots()
  })
  
  source('getPDFReport.R')
  
  # when the Create PDF button is clicked, show the page spinner and call the rendering function
  # once the PDF is done rendering, show a modal explaining that it has been created and downloaded
  # ui <- page_fluid(
  #   # Existing UI components
  #   
  #   # Add Create PDF Button
  #   downloadButton("create_pdf", "Create PDF")
  # )
  
  observeEvent(input$createPDF, {
    # name = pitcher_data$player_name
    get_blank_pdf(input$selected_player, input$date_range[1],
                   input$date_range[2])
    
  })
}

shinyApp(ui, server)


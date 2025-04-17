



options(shiny.error = browser)
options(shiny.fullstacktrace = TRUE)

library(shiny)
library(httr)
library(jsonlite)
library(DT)
library(rsconnect)
library(ggplot2)
library(tidyr)

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
      parsed_data <- fromJSON(content(response, as = "text"), flatten = TRUE)
      if (!is.null(parsed_data$data)) {
        all_data <- rbind(all_data, parsed_data$data)
      }
      total_pages <- parsed_data$meta$pages
      if (page >= total_pages) break
      page <- page + 1
    } else {
      break
    }
  }
  return(all_data)
}

get_pitch_data <- function(player_id, start_date = NULL, end_date = NULL) {
  if (is.null(player_id)) return(NULL)
  headers <- add_headers(`x-api-key` = api_key)
  all_data <- data.frame()
  page <- 1

  repeat {
    url <- paste0(pitches_url, "?pitcher_id=", player_id, "&page=", page)
    response <- GET(url, headers)
    if (status_code(response) == 200) {
      parsed <- fromJSON(content(response, as = "text"), flatten = TRUE)
      if (!is.null(parsed$data) && length(parsed$data) > 0) {
        all_data <- rbind(all_data, parsed$data)
        page <- page + 1
      } else {
        break
      }
    } else {
      break
    }
  }
  if (nrow(all_data) > 0 && !is.null(start_date) && !is.null(end_date)) {
    all_data$date <- as.Date(all_data$date)
    all_data <- all_data[all_data$date >= start_date & all_data$date <= end_date, ]
  }
  return(all_data)
}

get_pointstreak_player_info <- function(team_name, player_name) {
  library(xml2)
  league_id <- 174
  season_id <- 34104
  ps_api_key <- "vIpQsngDfc6Y7WVgAcTt"

  structure_url <- paste0("https://api.pointstreak.com/baseball/league/structure/", league_id, "/xml")
  structure_resp <- GET(structure_url, add_headers(apikey = ps_api_key))
  if (status_code(structure_resp) != 200) return(NULL)

  structure_xml <- content(structure_resp, as = "parsed")
  teams <- xml_find_all(structure_xml, ".//team")

  team_id <- NULL
  for (team in teams) {
    name <- xml_attr(team, "teamname")
    if (tolower(trimws(name)) == tolower(trimws(team_name))) {
      team_id <- xml_attr(team, "teamlinkid")
      break
    }
  }

  if (is.null(team_id)) return(NULL)

  roster_url <- paste0("https://api.pointstreak.com/baseball/team/roster/", team_id, "/", season_id, "/xml")
  roster_resp <- GET(roster_url, add_headers(apikey = ps_api_key))
  if (status_code(roster_resp) != 200) return(NULL)

  roster_xml <- content(roster_resp, as = "parsed")
  players <- xml_find_all(roster_xml, ".//player")

  for (p in players) {
    fname <- xml_text(xml_find_first(p, "./fname"))
    lname <- xml_text(xml_find_first(p, "./lname"))
    full_name <- paste(fname, lname)
    if (tolower(trimws(full_name)) == tolower(trimws(player_name))) {
      return(list(
        playerid = xml_attr(p, "playerid"),
        playerlinkid = xml_attr(p, "playerlinkid"),
        height = xml_text(xml_find_first(p, "./height")),
        weight = xml_text(xml_find_first(p, "./weight")),
        photo = xml_text(xml_find_first(p, "./photo"))
      ))
    }
  }
  return(NULL)
}

card_w_header <- function(title, body) {
  div(class = "card",
      div(class = "card-header bg-info text-white text-center font-weight-bold",
          style = "padding-top: 3px; padding-bottom: 3px;",
          title),
      div(class = "card-body d-flex align-items-start",  # Removed center, added start
          div(style = "text-align: left; width: 100%;", body))  # ← key line
  )
}


pitcher_data <- get_pitcher_data()

ui <- fluidPage(
  fluidRow(
    column(1),
    column(10,
           h1("ALPB Pitchers", align = "center"),
           fluidRow(
             column(4,
                    wellPanel(
                      selectInput("selected_player", "Select a Pitcher:", choices = pitcher_data$player_name),
                      dateRangeInput("date_range", "Select Date Range:", start = Sys.Date() - 365, end = Sys.Date())
                    )
             ),
             column(6, card_w_header("Season Stats", tableOutput("season_log"))),
             column(2, downloadButton("download_pdf", "Download PDF"))
           ),
           fluidRow(
             column(4,
                    div(style = "text-align: left;",  # Ensures left-alignment
                        card_w_header("Pitcher Info", uiOutput("player_info")))
             ),
             column(8, card_w_header("Game Log", tableOutput("game_log")))
           ),
           fluidRow(
             column(6, card_w_header(uiOutput("scatter_header"), plotOutput("velPlot", height = "300px"))),
             column(6, card_w_header("Induced Vertical Break vs Horizontal Break", plotOutput("breakPlot", height = "300px")))
           ),
           fluidRow(
             column(3, radioButtons("break_type", "Break Type:", choices = c("Vertical Break" = "induced_vert_break", "Horizontal Break" = "horz_break"))),
             column(3, radioButtons("tag_choice", "Pitch Tagging Method:", choices = c("Machine Tagged" = "auto_pitch_type", "Human Tagged" = "tagged_pitch_type")))
           ),
           fluidRow(
             column(6, card_w_header("Pitch map vs RH Batters", plotOutput("heatmap_right", height = "300px"))),
             column(6, card_w_header("Pitch map vs LH Batters", plotOutput("heatmap_left", height = "300px")))
           ),
           fluidRow(column(12, card_w_header("Pitch Type Percentages for Each Count", DTOutput("pitchTable"))))
    ),
    column(1)
  )
)

server <- function(input, output, session) {
  source("getGraphs.R")
  source("getHeatMap.R")
  source("pitchSplit.R")
  source("getPDFReport.R")

  selected_pitcher <- reactive({
    pitcher_data[pitcher_data$player_name == input$selected_player, ]
  })

  pitch_data <- reactive({
    req(input$selected_player)
    get_pitch_data(selected_pitcher()$player_id, input$date_range[1], input$date_range[2])
  })

  output$scatter_header <- renderUI({
    if (input$break_type == "induced_vert_break") {
      "Vertical Break vs Velocity"
    } else {
      "Horizontal Break vs Velocity"
    }
  })

  output$player_info <- renderUI({
    player <- selected_pitcher()
    req(nrow(player) > 0)

    name_parts <- unlist(strsplit(player$player_name, ",\\s*"))
    formatted_name <- paste(name_parts[2], name_parts[1])  # "First Last"
    ps_info <- get_pointstreak_player_info(player$team_name, formatted_name)
    
  


    print(player$team_name)
    print(formatted_name)
    print(ps_info)
    print(ps_info$alpb_2024_stats)

    tagList(
      h4(player$player_name),
      div(HTML(paste("<b>Player ID:</b>", player$player_id))),
      div(HTML(paste("<b>Team Name:</b>", player$team_name))),
      div(HTML(paste("<b>Pitching Handedness:</b>", player$player_pitching_handedness))),
      if (!is.null(ps_info)) {
        tagList(
          div(HTML(paste("<b>Height:</b>", ps_info$height))),
          div(HTML(paste("<b>Weight:</b>", ps_info$weight))),
          img(src = ps_info$photo, height = "100px")
        )
      }
    )
  })

  # output$season_log <- renderTable({
  #   data.frame(
  #     W = sample(2:10, 1), 
  #     L = sample(2:10, 1), 
  #     ERA = round(runif(1, 3.00, 6.00), 2)
  #   )
  # })
  
  source("getTeamID.R")
  output$season_log <- renderTable({
    player <- selected_pitcher()
    req(nrow(player) > 0)
    
    # Format name from "Last, First" → "First Last"
    name_parts <- unlist(strsplit(player$player_name, ",\\s*"))
    formatted_name <- paste(name_parts[2], name_parts[1])
    
    # Pull full info from Pointstreak
    info <- get_player_info_by_team_name(player$team_name, formatted_name)
    print(info$alpb_2024_stats)
    
    # Return ALPB 2024 stats if available
    if (!is.null(info) && !is.null(info$alpb_2024_stats) && length(info$alpb_2024_stats) > 0) {
      stats <- info$alpb_2024_stats
      data.frame(
        GP  = stats$gp,
        W   = stats$w,
        L   = stats$l,
        ERA = stats$era,
        SV  = stats$sv,
        IP  = stats$ip,
        SO  = stats$so,
        H   = stats$h,
        BB  = stats$bb,
        ER  = stats$er,
        GS  = stats$gs,
        SHO = stats$sho,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(Message = "No ALPB 2024 stats available")
    }
  })
  
  

  
  output$game_log <- renderTable({
    data.frame(Game = 1:3, ERA = round(runif(3, 3.00, 6.00), 2), WHIP = round(runif(3, 1.10, 1.50), 2))
  })

  output$velPlot <- renderPlot({
    df <- pitch_data()
    req(!is.null(df), nrow(df) > 0)
    build_graph(df, "rel_speed", input$break_type, input$tag_choice)
  })

  output$breakPlot <- renderPlot({
    df <- pitch_data()
    req(!is.null(df), nrow(df) > 0)
    build_graph(df, "horz_break", "induced_vert_break", input$tag_choice)
  })

  output$heatmap_right <- renderPlot({
    df <- pitch_data()
    req(!is.null(df), nrow(df) > 0)
    filtered <- df[df$batter_side == "Right", ]
    req(nrow(filtered) > 0)
    build_heatmap(filtered)
  })

  output$heatmap_left <- renderPlot({
    df <- pitch_data()
    req(!is.null(df), nrow(df) > 0)
    filtered <- df[df$batter_side == "Left", ]
    req(nrow(filtered) > 0)
    build_heatmap(filtered)
  })

  output$download_pdf <- downloadHandler(
    filename = function() {
      player <- selected_pitcher()
      if (is.null(player)) return("Pitcher_Report.pdf")
      paste0(player$player_name, "_Pitcher_Report.pdf")
    },
    content = function(file) {
      player <- selected_pitcher()
      df <- pitch_data()

      if (is.null(player) || is.null(input$date_range) || is.null(df) || nrow(df) == 0) {
        file.create(file)
        return()
      }

      name <- player$player_name
      date1 <- as.character(input$date_range[1])
      date2 <- as.character(input$date_range[2])

      pdf_path <- get_blank_pdf(df, name, date1, date2)

      if (!is.null(pdf_path) && file.exists(pdf_path)) {
        file.copy(pdf_path, file)
      } else {
        file.create(file)
      }
    },
    contentType = "application/pdf"
  )

  output$pitchTable <- renderDT({
    df <- pitch_data()
    req(!is.null(df), nrow(df) > 0)
    datatable(get_pitch_type_percentages(df), options = list(pageLength = 12, scrollX = TRUE))
  })
}

shinyApp(ui, server)


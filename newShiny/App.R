library(shiny)
library(httr)
library(jsonlite)
library(DT)
library(rsconnect)
library(ggplot2)
library(tidyr)
library(shinyjs)


# source files
source("getPointstreakPlayers.R")
source("getALPBdata.R")
source("getSeasonStats.R") 
source("getALPBPitches.R") 

# ui card helper
card_w_header <- function(title, body) {
  div(class = "card",
      div(class = "card-header bg-info text-white text-center font-weight-bold",
          style = "padding-top: 5px; padding-bottom: 5px;",
          title),
      div(class = "card-body d-flex justify-content-center align-items-center",
          div(style = "text-align: center; width: 100%;", body)
      )
  )
}

# dropdown
pitchers_df <- pitchers_df %>%
  mutate(full_name = paste(fname, lname))

print(colnames(pitchers_df))

# shiny ui

ui <- fluidPage(
  
  useShinyjs(),
  
  
  fluidRow(
    column(1),
    column(10,
           div(style = "text-align: center;", h1("ALPB Pitchers")),
           
           #first row
             #pitcher dropdown
           fluidRow(

             column(4,
                    wellPanel(selectInput("selected_player", "Choose a Pitcher:", choices = pitchers_df$full_name))
             ),
             #download pdf btn
             column(6,
                    
             ),
             column(2,
                    downloadButton("download_pdf", "Download PDF")
             )

           ),

           #second row
           fluidRow(
             column(4,
                    div(style = "margin-bottom: 20px",
                        card_w_header("Pitcher Information",
                                      div(style = "text-align: left; margin-top: 2vh;",
                                          fluidRow(
                                            column(6, uiOutput("player_photo")),

                                            column(6, uiOutput("player_info_placeholder"))
                                          )
                                      )
                        )
                    )
             ),
             column(8,
                    card_w_header("Season Stats", tableOutput("season_stats_output"))
             )
           ),
           
           # single fluidRow layout
           # fluidRow(
           #   
           #   # Left column
           #   column(4,
           #          # First nested row: dropdown and download button
           #          fluidRow(
           #            column(8,
           #                   wellPanel(selectInput("selected_player", "Choose a Pitcher:", choices = pitchers_df$full_name))
           #            ),
           #            column(4,
           #                   downloadButton("download_pdf", "Download PDF")
           #            )
           #          ),
           #          # Second nested row: pitcher info
           #          fluidRow(
           #            column(12,
           #                   div(style = "margin-top: 20px;",
           #                       card_w_header("Pitcher Information",
           #                                     div(style = "text-align: left; margin-top: 2vh;",  
           #                                         fluidRow(
           #                                           column(6, uiOutput("player_photo")),
           #                                           column(6, uiOutput("player_info_placeholder"))
           #                                         )
           #                                     )
           #                       )
           #                   )
           #            )
           #          )
           #   ),
           #   
           #   # Right column: season stats
           #   column(8,
           #          card_w_header("Season Stats", tableOutput("season_stats_output"))
           #   )
           # ),
           
           
           #third row
           fluidRow(
             id = "alpbRow3",
             #vel plot
             column(6,
                    card_w_header(uiOutput("scatter_header"), plotOutput("velPlot", height = "300px"))
             ),
             
             #break graph
             column(6,
                    card_w_header("Induced Vertical Break vs Horizontal Break", plotOutput("breakPlot", height = "300px"))
                    # card_w_header("Strike Zone", plotOutput("heatmaps", height = "300px"))
             )
           ),
           
           #4th row -- buttons
           fluidRow(
             id = "alpbRow4",
             column(3,
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
           
           #5th row - heat maps
           fluidRow(
             id = "alpbRow5",
             column(6,
                    card_w_header("Pitch map vs RH Batters", plotOutput("heatmap_right", height = "300px"))
             ),
             
             column(6,
                    card_w_header("Pitch map vs LH Batters", plotOutput("heatmap_left", height = "300px"))
             )
           ),
           
           #6th row -- pitch type 
           fluidRow(
             id = "alpbRow6",
             column(12,
                           card_w_header("Pitch Type Percentages for Each Count",DTOutput("pitchTable"))
           ))
           
           # ,
           
           # fluidRow(
           #   column(12,
           #          h4("Pointstreak Data:"),
           #          tableOutput("player_info"))
           #   ),
           # fluidRow(
           #   column(12,
           #          h4("ALPB Data:"),
           #          verbatimTextOutput("alpb_info"))),
           # fluidRow(
           #   column(12,
           #          h4("Pointstreak playerlinkid:"),
           #          verbatimTextOutput("playerlink_output"))),
           # fluidRow(
           #   column(12,
           #          h4("Pointstreak Season Pitching Stats"),
           #          tableOutput("season_stats_output"))),
           # fluidRow(
           #   column(12,
           #          h4("ALPB Pitch-by-Pitch Data"),
           #          tableOutput("alpb_pitch_data"))
           # )
           
    ),
    column(1)
  )
)

# server
server <- function(input, output) {
  
  observe({
    if (is.null(alpb_player_id())) {
      shinyjs::hide("alpbRow3")
      shinyjs::hide("alpbRow4")
      shinyjs::hide("alpbRow5")
      shinyjs::hide("alpbRow6")
    } else {
      shinyjs::show("alpbRow3")
      shinyjs::show("alpbRow4")
      shinyjs::show("alpbRow5")
      shinyjs::show("alpbRow6")
    }
  })
  
  
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
    # req(alpb_player_id())  # stops here if NULL
    get_alpb_pitches_by_pitcher(alpb_player_id())
    # get_alpb_pitches_by_pitcher(NULL)
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
  
  # output$player_info_placeholder <- renderUI({
  #   tagList(
  #     div(HTML("<b>Weight:</b> Pointstreak TBD")),
  #     div(HTML("<b>Height:</b> Pointstreak TBD")),
  #     div(HTML("<b>Throws:</b> Pointstreak TBD")),
  #     div(HTML("<b>Bats:</b> Pointstreak TBD"))
  #   )
  # })
  
  output$player_info_placeholder <- renderUI({
    req(selected_player_row())  # Ensure there's a selection
    player <- selected_player_row()
    
    tagList(
      div(HTML(paste0("<b>Name:</b> ", player$full_name))),
      div(HTML(paste0("<b>Team:</b> ", player$teamname))),
      div(HTML(paste0("<b>Player ID:</b> ", player$playerid))),
      div(HTML(paste0("<b>Hometown:</b> ", player$hometown))),
      div(HTML(paste0("<b>Throws:</b> ", player$throws))),
      div(HTML(paste0("<b>Weight:</b> ", player$weight))),
      div(HTML(paste0("<b>Height:</b> ", player$height))),
      div(HTML(paste0("<b>Bats:</b> ", player$bats))),
      # div(HTML(paste0("<b>Photo:</b> ", player$photo)))
    )
  })
  
  
  
  
  source('getGraphs.R')
  output$velPlot <- renderPlot({
    data <- pitch_data()
    shiny::validate(shiny::need(!is.null(data) && nrow(data) > 0, "Need data"))
    build_graph(data, "rel_speed", input$break_type, input$tag_choice)
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
      pdf_path <- NULL
      if (is.null(alpb_player_id())){
        pdf_path<- get_no_ALPB_pdf(selected_player_row()$playerlinkid, selected_pitcher())
      } 
      else {
        pdf_path <- get_all_pdf(selected_player_row()$playerlinkid, pitch_data(), selected_pitcher(), input$tag_choice)
      }
      
      # Copy it to the file path that downloadHandler expects
      file.copy(pdf_path, file)
    },
    contentType = "application/pdf"
  )
  
  source("pitchSplit.R")
  
  # Render the data table
  
  output$pitchTable <- renderDT({
    # df <- get_pitch_type_percentages(pitch_data())
    req(pitch_data())
    df <- get_pitch_type_percentages(pitch_data(), input$tag_choice)
    datatable(df, options = list(pageLength = 12, scrollX = TRUE))  # Display the table
  })
  
  # Show full player info
  output$player_info <- renderTable({
    req(selected_player_row())
    selected_player_row() %>% dplyr::select(-full_name)
  })

  # Show ALPB info
  # output$alpb_info <- renderPrint({
  #   req(selected_player_row())
  #   selected <- selected_player_row()
  #   result <- get_alpb_pitcher_info(selected$fname, selected$lname)
  # 
  #   if (is.na(result$player_id) || result$player_id == "data unavailable") {
  #     alpb_player_id(NULL)
  #     cat("\u26A0\uFE0F ALPB Data Unavailable")
  #   } else {
  #     alpb_player_id(result$player_id)
  #     cat("Player ID: ", result$player_id, "\n")
  #     cat("Pitching Handedness: ", result$pitching_hand)
  #   }
  # })

  # Show Pointstreak playerlinkid
  # output$playerlink_output <- renderPrint({
  #   req(selected_player_row())
  #   cat("playerlinkid:", selected_player_row()$playerlinkid)
  # })

  # # ✅ Show season stats from getSeasonStats.R
  output$season_stats_output <- renderTable({
    req(selected_player_row())
    playerlinkid <- selected_player_row()$playerlinkid
    stats <- get_pitching_stats_only(playerlinkid)

    if (is.null(stats)) {
      return(tibble::tibble(message = "No season stats found for this player."))
    }

    stats
  })
  # 
  # output$alpb_pitch_data <- renderTable({
  #   req(alpb_player_id())
  #   pitch_df <- get_alpb_pitches_by_pitcher(alpb_player_id())
  #   if (is.null(pitch_df)) {
  #     return(tibble::tibble(message = "\u26A0\uFE0F No pitch data found for this player."))
  #   }
  #   pitch_df
  # })
  
  output$player_photo <- renderUI({
    req(selected_player_row())
    img_url <- selected_player_row()$photo
    
    if (!is.na(img_url) && nzchar(img_url)) {
      tags$img(src = img_url, width = "100%", style = "border-radius: 8px;")
    } else {
      tags$p("⚠️ No photo available.")
    }
  })
  
  
}

shinyApp(ui = ui, server = server)

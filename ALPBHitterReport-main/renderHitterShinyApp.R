library(shiny)
library(shinyWidgets)
library(bslib)
#library(daterangepicker)
library(ggplot2)
#library(viridis)
library(dplyr)
library(tidyverse)
library(sportyR)
library(shinycssloaders)
#library(DT)

# HELPER FUNCTIONS

# function to create a card container with a header

card_w_header <- function(title, body) {
  div(class = "card",
      div(class = "card-header bg-info text-white text-center font-weight-bold", title),
      div(class = "card-body d-flex justify-content-center", body)
  )
}

# Function to create a card without a header
card_w_no_header <- function(body) {
  div(class = "card-body d-flex justify-content-center", body)
}

# function to create a card container without a header but with a slight inset

card_w_inset_and_no_header <- function(body) {
  div(class = "card",
      div(class = "card-body d-inline-flex justify-content-center", body)
  )
}

# UI layout
ui <- page_fluid(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")),
  
  layout_columns(
    card_w_no_header(selectizeInput("player", "Search for player..", choices = NULL, options = list(placeholder = 'Type to search...', onInitialize = I('function() { this.setValue(""); }')))),
    card_w_no_header(prettyRadioButtons("pitcherThrows", "Select pitcher hand...", choices = c("v RHP", "v LHP"), inline = TRUE, status = "primary", fill = TRUE)),
    card_w_no_header(uiOutput("dateSlider")),
    card_w_no_header(actionButton("createPDF", "Create PDF", class = "btn btn-primary btn-primary bg-info")),
    row_heights = c(1, 1, 1)
  ),
  layout_columns(
    uiOutput("batter_bio_card"),
    uiOutput("basic_stats"),
    col_widths = c(4, 8)
  ),
  layout_columns(
    plotOutput("spray_chart"),
    layout_columns(
      uiOutput("advanced_stats_swing"),
      uiOutput("percentile_swing"),
      uiOutput("advanced_stats_launch"),
      col_widths = c(12, 12)
    )
  ),
  
  # create the layout for the heatmaps with the filterable sidebar
  
  layout_columns(id = "heatPitchInput",
    selectizeInput("heatPitchType", NULL, choices = NULL, options = list(onInitialize = I('function() { this.setValue(""); }'))),
    card_w_header("Heatmaps", plotOutput("heatmaps")),
    col_widths = c(2, 10)
  )
  
)

# Server function to handle all of the processing on the backend

server <- function(input, output, session) {
  
  # observe events to see when we should display the page loading spinner
  
  showPageSpinner(image = 'https://i.ibb.co/cJqZwRT/3dgifmaker77215.gif')
  
  observeEvent(input$player, {
    showPageSpinner(image = 'https://i.ibb.co/cJqZwRT/3dgifmaker77215.gif')
  })
  
  observeEvent(input$pitcherThrows, {
    showPageSpinner(image = 'https://i.ibb.co/cJqZwRT/3dgifmaker77215.gif')
  })
  
  observeEvent(input$dateRange, {
    showPageSpinner(image = 'https://i.ibb.co/cJqZwRT/3dgifmaker77215.gif')
  })
  
  observeEvent(input$heatPitchType, {
    showPageSpinner(image = 'https://i.ibb.co/cJqZwRT/3dgifmaker77215.gif')
  })
  
  # Grab Trackman Data to Start Building Visuals
  
  source('~/ALPB-Pitching-Widget1/getTrackmanData.R')
  
  trackman_data <- get_trackman_data()
  trackman_data$Date <- as.Date(trackman_data$Date)
  
  # get a list of all of the unique players in the data and update the dropdown options with them
  
  player_list <- setNames(trackman_data$BatterId, paste0(trackman_data$Batter, " (", trackman_data$BatterId, ")"))
  
  updateSelectizeInput(session, "player", choices = player_list, server = TRUE)
  
  # Create the date slider output dynamically
  output$dateSlider <- renderUI({
    earliest_date <- min(trackman_data$Date, na.rm = TRUE)  # Get the earliest date
    latest_date <- max(trackman_data$Date, na.rm = TRUE)  # Current date
    sliderInput("dateRange", "Select date range:", 
                min = earliest_date, 
                max = latest_date, 
                value = c(earliest_date, latest_date), 
                timeFormat = "%Y-%m-%d")
  })
  
  # Create a reactive expression for the selected player's data (will dynamically update UI when selections are changed)
  # not needed if using Batter ID as the primary identifer key
  # NO LONGER USING THIS METHOD BUT LEAVING HERE FOR POSTERITY IF NEEDS TO REVERT
  # NOW IT IS DONE BY PASSING IN ALL THE DATA AND FILTERING IN THE FUNCTIONS
  
  # selected_player_name <- reactive({
  #   req(input$player)  # Ensure a player is selected
  #   trackman_data %>% filter(Batter == input$player) %>% pull(Batter) %>% unique() %>% slice_head(n = 1) %>% dplyr::select(BatterId, BatterSide, BatterTeam) 
  # })
  
  # filter the data based on the selected player, pitcher handedness, and date range
  
  # selected_player_data <- reactive({
  #   req(input$player, input$pitcherThrows, input$dateRange)
  #   data <- trackman_data %>%
  #     filter(BatterId == input$player) %>%
  #     filter(PitcherThrows == ifelse(input$pitcherThrows == "v RHP", "Right", "Left")) %>%
  #     filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
  # })
  
  ## Build component 1 - Basic Batter Info Card
  
  source('getBasicBatterInfo.R')
  
  # Create the batter_bio_card as a reactive expression to dynamically update
  
  batter_bio_card <- reactive({
    req(input$player)  # Ensure a player is selected
    
    # normally could pass the selected_player_data object
    # but do this just in case no rows are returned due to filters
    # could be fixed with just passing in a batter ID which might
    # be the best approach once we get API access
    
    # player_data <- trackman_data %>% filter(BatterId == input$player)
    
    # Assuming get_batter_info_card expects a data.frame/tibble
    get_batter_info_card(trackman_data, input$player)
  })
  
  # Render the batter info table in a card
  
  output$batter_bio_card <- renderUI({
    out <- renderTable(batter_bio_card(), align = 'ccc')
    card_w_header('Batter Info', out)
  })
  
  ## Build component 1.5 - Generate Percentiles
  
  source('getPercentiles.R')
  
  basic_percentiles <- get_percentiles(trackman_data, "basic")
  
  advanced_percentiles <- get_percentiles(trackman_data, "advanced") 
  
  ## Build component 2 - Basic Stats
  
  source('getBasicStats.R')
  
  # Create basic_stats as a reactive expression to dynamically update
  
  basic_stats <- reactive({
    # req(selected_player_data())
    
    req(input$player)
    get_basic_stats(trackman_data, basic_percentiles, input$player, ifelse(input$pitcherThrows == "v RHP", "Right", "Left"), input$dateRange[1], input$dateRange[2])
  })
  
  # Render the basic stats table in a card
  
  output$basic_stats <- renderUI({
    
    out <- renderTable(basic_stats(), align = 'cccccc')
    card_w_header('Counting Stats', out)
  })
  
  ## Build component 3 - Advanced Stats

  source('getAdvancedStats.R')

  # Create advanced_stats as a reactive expression to dynamically update
  
  advanced_stats <- reactive({
    req(input$player)
    get_advanced_stats(trackman_data, advanced_percentiles, input$player, ifelse(input$pitcherThrows == "v RHP", "Right", "Left"), input$dateRange[1], input$dateRange[2])

  })
  
  # percentile_advanced_stats <- reactive({get_sliders(trackman_data, "max_ev", input$player)})
  # output$percentile_swing <- renderUI({
  #   out_percSwing <- renderTable(percentile_advanced_stats())
  #   card_w_header('Percentile Stats', out_percSwing)
  # })
  
  # render the advanced stats table in respective cards
  
  output$advanced_stats_swing <- renderUI({
    
    out_swing <- renderTable(advanced_stats()$swing_stats, align = 'cccc')
    card_w_header('Swing Stats', out_swing)
  })
  
  # Render the advanced launch stats
  output$advanced_stats_launch <- renderUI({
    
    out_launch <- renderTable(advanced_stats()$launch_stats, align = 'cccc')
    card_w_header('Launch Stats', out_launch)
  })
  
  # extra advanced stats if we want to stratify and be more specific in the future
  
  # output$advanced_stats_ev <- renderUI({
  #   out_ev <- renderTable(advanced_stats()$ev_stats, align = 'cccc')
  #   card_w_header('EV Stats', out_ev)
  # })
  
  # output$advanced_stats_launch <- renderUI({
  #   out_launch <- renderTable(advanced_stats()$launch_stats, align = 'ccccc')
  #   card_w_header('Launch Stats', out_launch)
  # })
  # 
  # output$advanced_stats_direction <- renderUI({
  #   out_direction <- renderTable(advanced_stats()$direction_stats, align = 'ccc')
  #   card_w_header('Direction Stats', out_direction)
  # })

  ## Build component 4 - Spray Chart
  
  source('getSprayChart.R')
  
  # Create the spray_chart_plot as a reactive expression to dynamically adjust
  
  spray_chart_plot <- reactive({
    req(input$player)
    get_spray_chart(trackman_data, input$player, ifelse(input$pitcherThrows == "v RHP", "Right", "Left"), input$dateRange[1], input$dateRange[2])
  })
  
  # Render the spray chart (no need for a card)
  
  output$spray_chart <- renderPlot({
    
    spray_chart_plot()
  })
  
  ## Build component 5 - Heatmaps

  source('getHeatmaps.R')

  # Create the heatmaps as a reactive expressions based on a dropdown
  
  updateSelectizeInput(session, "heatPitchType", choices = c("FB", "SL", "CB", "CH"), server = TRUE)
  
  heatmap_plots <- reactive({
    req(input$player, input$heatPitchType)
    get_batter_heatmaps(trackman_data, input$player, ifelse(input$pitcherThrows == "v RHP", "Right", "Left"), input$dateRange[1], input$dateRange[2], input$heatPitchType)
  })

  # Render the heatmaps in plot form (will be put in a card in the UI)
  
  output$heatmaps <- renderPlot({
    heatmap_plots()
  })
  
  ## Build component 6 - PDF of the Report
  
  source('getPDFReport.R')
  
  # when the Create PDF button is clicked, show the page spinner and call the rendering function
  # once the PDF is done rendering, show a modal explaining that it has been created and downloaded
  ui <- page_fluid(
    # Existing UI components
    
    # Add Create PDF Button
    downloadButton("create_pdf", "Create PDF")
  )
  
  observeEvent(input$createPDF, {
    showPageSpinner(image = 'https://i.imghippo.com/files/ZDFuo1724952209.gif')
    get_report_pdf(trackman_data, basic_percentiles, advanced_percentiles, input$player, input$dateRange[1], input$dateRange[2])
    showModal(modalDialog(
      title = "Create PDF Report",
      "A PDF of the Hitter Report has been downloaded.",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close")
      )
    ))
    hidePageSpinner()
  })
}

# use the create UI and server to run the app!

shinyApp(ui = ui, server = server)


####### EXTRA CODE FOR TESTING #########

# tags$head(
#   tags$style(HTML("
#   #card_one {
#     margin-bottom:0px;
#   }
#   #card_two {
#     margin-bottom:-50px;
#   }
#   #card_three {
#     margin-bottom:-50px;
#   }
#   #card_four {
#     margin-top:-50px;
#   }
#   #card_five {
#     margin-top:-50px;
#   }
#   #card_six {
#     margin-top:-50px;
#   }"))
# ),


# layout_columns(
#   uiOutput("advanced_stats_launch"),
#   uiOutput("advanced_stats_direction"),
#   col_widths = c(12, 12),
# )


# layout_columns(
#   selectizeInput("heatPitchType", "", choices = NULL, options = list(placeholder = '', onInitialize = I('function() { this.setValue(""); }'))),
#   plotOutput("heatmaps"),
#   col_widths = c(1, 11)
# )

# layout_columns(
#   card_w_no_header(selectizeInput("player", "Search for player..", choices = NULL, options = list(placeholder = 'Type to search...', onInitialize = I('function() { this.setValue(""); }')))),
#   card_w_no_header(prettyRadioButtons("pitcherThrows", "Select pitcher hand...", choices = c("v RHP", "v LHP"), inline = TRUE, status = "primary", fill = TRUE)),
#   card_w_no_header(dateRangeInput("dateRange", "Pick date range...", start = paste0(format(Sys.Date(), "%Y"),"-01-01"), end = Sys.Date(), min = "2000-01-01", max = Sys.Date(), separator = " - ", format = "yyyy-mm-dd", startview = "month", language = "en", width = "100%")),
#   row_heights = c(1,1,1)
# ),
# layout_columns(
#   uiOutput("batter_bio_card"),
#   uiOutput("basic_stats"),
#   col_widths = c(4,8)
# ),
# layout_columns(
#   uiOutput("advanced_stats_swing"),
#   card_w_no_header("Batter Spray Chart"),
#   col_widths = c(6,6)
# ),
# card_w_inset_and_no_header("Heatmaps")

# Render all of the heatmaps with a for loop

# heatmap_categories <- c("EV > 95", "Whiff", "Chase")
#
# pitch_types <- c("Four-Seam", "Slider", "Curveball") # WHICH PITCH TYPE COLUMN DO WE WANT TO USE? DO WE WANT TO SHOW ALL PITCHES THROWN OR ONLY ONES OVER A THRESHOLD??
# pitch_types <- pitch_types[!is.na(pitch_types) & pitch_types != ""]
#
# for (pitch in pitch_types) {
#   for (type in heatmap_categories) {
#     output[[paste0(pitch, "_", type)]] <- renderPlot({
#       heatmap_plots(pitch, type)
#     })
#   }
# }

# # Generate and render heatmaps
# # heatmap_outputs <- c("EV", "Whiff", "Chase")
# # pitch_types <- c("FB", "SL", "CH", "CB")
# # 
# # for (output_type in heatmap_outputs) {
# #   for (pitch in pitch_types) {
# #     output_name <- paste0("heatmap_", pitch, "_", output_type)
# #     output[[output_name]] <- renderPlot({
# #       plot_heatmap(generate_heatmap_data(), paste(pitch, output_type))
# #     })
# #   }
# # }
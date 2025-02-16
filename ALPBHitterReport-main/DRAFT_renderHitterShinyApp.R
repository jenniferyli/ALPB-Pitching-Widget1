library(shiny)
library(shinyWidgets)
library(bslib)
library(daterangepicker)
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyverse)
library(sportyR)
library(shinycssloaders)

card <- function(body, title) {
  div(class = "card",
      div(class = "card-header bg-success text-white text-center font-weight-bold", title),
      div(class = "card-body d-flex justify-content-center", body)
  )
}

ui <- fluidPage(
  #theme = bs_theme(version = 4),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")),
  
  # Top row
  fluidRow(
    column(3, 
           selectizeInput("player", "Search for player..", 
                          choices = NULL, 
                          options = list(
                            placeholder = 'Type to search...',
                            onInitialize = I('function() { this.setValue(""); }')
                          ))
    ),
    column(3, 
           prettyRadioButtons("pitcherThrows", "",
                              choices = c("v RHP", "v LHP"),
                              inline = TRUE,
                              status = "primary",
                              fill = TRUE)
    ),
    column(6, 
           dateRangeInput("dateRange", "Pick Date Range",
                          start = paste0(format(Sys.Date(), "%Y"),"-01-01"), end = Sys.Date(),
                          min = "2000-01-01", max = Sys.Date(),
                          separator = " - ", format = "yyyy-mm-dd",
                          startview = "month", language = "en",
                          width = "100%")
    )
  ),
  
  # Second row
  fluidRow(
    column(3, 
           uiOutput("batter_bio_card")
    ),
    column(9,
           wellPanel(
             fluidRow(
               column(4, textOutput("avg")),
               column(4, textOutput("obp")),
               column(4, textOutput("ops"))
             ),
             fluidRow(
               column(4, textOutput("k_perc")),
               column(4, textOutput("bb_perc")),
               column(4, textOutput("hr"))
             )
           )
    )
  ),
  
  # Third row
  fluidRow(
    column(3,
           wellPanel(
             textOutput("whiffPct"),
             textOutput("chasePct"),
             textOutput("zSwing"),
             textOutput("fpSwing")
           )
    ),
    column(3,
           wellPanel(
             textOutput("maxEV"),
             textOutput("nthEV"),
             textOutput("medLA")
           )
    ),
    column(6, plotOutput("spray_chart", height = "200px", width = "100%"))
  ),
  
  fluidRow(
    column(3,
           wellPanel(
             textOutput("gbPct"),
             textOutput("ldPct"),
             textOutput("popPct"),
             textOutput("fbPct")
           )
    ),
    column(3,
           wellPanel(
             textOutput("pullPct"),
             textOutput("centerPct"),
             textOutput("oppoPct")
           )
    ),
    #column(9, plotOutput("spray_chart", height = "200px", width = "100%"))
  ),
  
  # Fourth row (heatmaps)
  fluidRow(
    column(12, plotOutput("heatmaps", height = "400px", width = "100%"))
  )
  # fluidRow(
  #   column(12,
  #          wellPanel(
  #            fluidRow(
  #              column(3, ""),
  #              column(3, h4("EV > 95")),
  #              column(3, h4("Whiff")),
  #              column(3, h4("Chase"))
  #            ),
  #            fluidRow(
  #              column(3, h4("FB")),
  #              column(3, h4("FB")),
  #              column(3, h4("FB")),
  #              column(3, h4("FB"))
  #            ),
  #            fluidRow(
  #              column(3, h4("FB")),
  #              column(3, h4("FB")),
  #              column(3, h4("FB")),
  #              column(3, h4("FB"))
  #            ),
  #            fluidRow(
  #              column(3, h4("CB")),
  #              column(3, h4("FB")),
  #              column(3,h4("FB")),
  #              column(3, h4("FB"))
  #            )
  #          )
  #   )
  # )
)

server <- function(input, output, session) {
  
  showPageSpinner(image = 'https://i.imghippo.com/files/ZDFuo1724952209.gif')
  
  observeEvent(input$player, {
    showPageSpinner(image = 'https://i.imghippo.com/files/ZDFuo1724952209.gif')
  })
  
  observeEvent(input$pitcherThrows, {
    showPageSpinner(image = 'https://i.imghippo.com/files/ZDFuo1724952209.gif')
  })
  
  observeEvent(input$dateRange, {
    showPageSpinner(image = 'https://i.imghippo.com/files/ZDFuo1724952209.gif')
  })
  
  ## Grab Trackman Data to Start Building Visuals
  
  source('~/Desktop/ALPB Hitter Report/getTrackmanData.R')
  trackman_data <- get_trackman_data()
  
  # Simulated player list - replace with your actual player data
  player_list <- unique(trackman_data$Batter)
  
  updateSelectizeInput(session, "player", choices = player_list, server = TRUE)
  
  # Create a reactive expression for the selected player's data
  selected_player_name <- reactive({
    req(input$player)  # Ensure a player is selected
    trackman_data %>% filter(Batter == input$player)
  })
  
  selected_player_data <- reactive({
    req(input$player, input$pitcherThrows, input$dateRange)
    trackman_data %>%
      filter(Batter == input$player) %>%
      filter(PitcherThrows == ifelse(input$pitcherThrows == "v RHP", "Right", "Left")) %>%
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
  })
  
  ## Build component 1 - Basic Batter Info Card
  
  source('~/Desktop/ALPB Hitter Report/getBasicBatterInfo.R')
  
  # Create the batter_bio_card as a reactive expression
  batter_bio_card <- reactive({
    req(selected_player_name())
    get_batter_info_card(selected_player_name())
  })
  
  # Render the batter_bio_card
  output$batter_bio_card <- renderUI({
    card(batter_bio_card(), "Batter Info")
  })
  
  ## Build component 2 - Basic Stats
  
  source('~/Desktop/ALPB Hitter Report/getBasicStats.R')
  
  # Create basic_stats as a reactive expression
  basic_stats <- reactive({
    req(selected_player_data())
    get_basic_stats(selected_player_data())
  })
  
  output$avg <- renderText({ paste("Avg:", basic_stats()$avg) })
  output$obp <- renderText({ paste("OBP:", basic_stats()$obp) })
  output$ops <- renderText({ paste("OPS:", basic_stats()$ops) })
  output$k_perc <- renderText({ paste("K%:", basic_stats()$k_perc) })
  output$bb_perc <- renderText({ paste("BB%:", basic_stats()$bb_perc) })
  output$hr <- renderText({ paste("HR:", basic_stats()$hr) })
  
  ## Build component 3 - Advanced Stats
  
  source('~/Desktop/ALPB Hitter Report/getAdvancedStats.R')
  
  # Create advanced_stats as a reactive expression
  advanced_stats <- reactive({
    req(selected_player_data())
    get_advanced_stats(selected_player_data())
  })
  
  output$whiffPct <- renderText({ paste("Whiff %:", advanced_stats()$whiff_pct) })
  output$chasePct <- renderText({ paste("Chase %:", advanced_stats()$chase_pct) })
  output$zSwing <- renderText({ paste("Z-Swing %:", advanced_stats()$z_swing) })
  output$fpSwing <- renderText({ paste("FP-Swing %:", advanced_stats()$fp_swing) })
  
  output$maxEV <- renderText({ paste("Max EV:", advanced_stats()$max_ev) })
  output$nthEV <- renderText({ paste("90th %tile EV:", advanced_stats()$nth_ev) })
  output$medLA <- renderText({ paste("Median LA:", advanced_stats()$med_la) })
  
  output$gbPct <- renderText({ paste("GroundBall %:", advanced_stats()$ground_ball_pct) })
  output$ldPct <- renderText({ paste("LineDrive %:", advanced_stats()$line_drive_pct) })
  output$popPct <- renderText({ paste("PopUp %:", advanced_stats()$pop_up_pct) })
  output$fbPct <- renderText({ paste("FlyBall %:", advanced_stats()$fly_ball_pct) })
  
  output$pullPct <- renderText({ paste("Pull %:", advanced_stats()$pull_pct) })
  output$centerPct <- renderText({ paste("Center %:", advanced_stats()$center_pct) })
  output$oppoPct <- renderText({ paste("Oppo %:", advanced_stats()$oppo_pct) })
  
  ## Build component 4 - Spray Chart
  
  source('~/Desktop/ALPB Hitter Report/getSprayChart.R')
  
  # Create the spray_chart_plot as a reactive expression
  spray_chart_plot <- reactive({
    req(selected_player_data())
    get_spray_chart(selected_player_data())
  })
  
  # Render the spray_chart
  output$spray_chart <- renderPlot({
    spray_chart_plot()
  })
  
  ## Build component 5 - Heatmaps
  
  source('~/Desktop/ALPB Hitter Report/getHeatmaps.R')

  # Create the heatmaps as a reactive expressions
  heatmap_plots <- reactive({
      req(selected_player_data())
      get_batter_heatmaps(selected_player_data())
    })

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
  
  output$heatmaps <- renderPlot({
    heatmap_plots()
  })
  
  # Generate and render heatmaps
  # heatmap_outputs <- c("EV", "Whiff", "Chase")
  # pitch_types <- c("FB", "SL", "CH", "CB")
  # 
  # for (output_type in heatmap_outputs) {
  #   for (pitch in pitch_types) {
  #     output_name <- paste0("heatmap_", pitch, "_", output_type)
  #     output[[output_name]] <- renderPlot({
  #       plot_heatmap(generate_heatmap_data(), paste(pitch, output_type))
  #     })
  #   }
  # }
}

shinyApp(ui = ui, server = server)

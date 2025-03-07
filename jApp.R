library(shiny)
library(shinythemes)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
# HELPER FUNCTIONS
# Directly Copied from Alex Nath's code

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
# get trackman data
get_data <- function() {
  folder_path <- "ALPBHitterReport-main/07data"
  
  # Get a list of all CSV files in the data folder
  
  combined_df<- list()
  for(i in 1:23){
    file_number <- sprintf("%02d", i)
    file_path_1 <- file.path(folder_path, file_number, "CSV")
    csv_files <- list.files(path = file_path_1, pattern = "*.csv", full.names = TRUE)
    combined_df <- append(combined_df, csv_files)
  }
  df <- combined_df %>%
    lapply(read.csv) %>%
    bind_rows()

  return(df)
}

# add_table <- function(){
#   df <-get_data()
#   df_selected <- df %>%
#     select(column1, column2)
#   ggplot(df_selected, aes(x = column1, y = column2)) +
#     geom_point() +
#     labs(title = "Scatter Plot of column1 vs column2", x = "Column 1", y = "Column 2") +
#     theme_minimal()
# }


# ui
ui <- fluidPage(
  theme = shinytheme("flatly"),

  titlePanel("ALPB Pitcher Data"),
  fluidRow(   
    column(5, card_w_no_header(selectizeInput("selected_pitcher", "Choose a Pitcher:", choices = NULL, options = list(placeholder = 'Type to search...', onInitialize = I('function() { this.setValue(""); }')))),
           textOutput("pitcher_hand"),
           textOutput("pitcher_id"),
           textOutput("pitcher_team")),
    # column(6, card_w_no_header(prettyRadioButtons("batterHand", "Select batter hitting hand...", choices = c("v RHB", "v LHB"), inline = TRUE, status = "primary", fill = TRUE)))
    column(7, card_w_header("Season Stats", tableOutput("season_log")))
  ),
  fluidRow(
    column(12, 
           card_w_header("Gamesplits:", tableOutput("game_log"))  # Placeholder table in full width
    )
  ),
  
  fluidRow(
    column(6,  # Main content takes up 2/3 of the screen
           card_w_header("Beraks", plotOutput("scatterPlot", height = "300px")) # Correct way to set height for plotOutput
    ),
    column(6, 
           card_w_header("Heat Maps", plotOutput("heatmaps", height = "300px")) # Correct way to set height for plotOutput
    )
  ),
  

  fluidRow(
    column(3,  # Sidebar takes up 1/3 of the screen
           # textOutput("pitcher_hand"),
           # textOutput("pitcher_id"),
           radioButtons("break_type", "Select Break Type:",
                        choices = c("Vertical Break" = "InducedVertBreak",
                                    "Horizontal Break" = "HorzBreak"),
                        selected = "InducedVertBreak")

    ),
    column(3,  # Sidebar takes up 1/3 of the screen
           # textOutput("pitcher_hand"),
           # textOutput("pitcher_id"),
           
           radioButtons("tag_choice", "Select Pitch Tagging Method:",
                        choices = c("Human Tagged" = "TaggedPitchType",
                                    "Machine Tagged" = "AutoPitchType"),
                        selected = "TaggedPitchType")
           
    ),
    column(6, 
           card_w_no_header(selectizeInput("pitch_type", "Choose a pitch type:", choices = NULL, options = list(placeholder = 'Type to search...', onInitialize = I('function() { this.setValue(""); }')))),
    )
  )
  
)


# server
server <- function(input, output, session) {
  
  #select pitcher
  observe({
    updateSelectInput(session, "selected_pitcher", 
                      choices = unique(get_data()$Pitcher)) 
  })
  observe({
    updateSelectInput(session, "pitch_type", 
                      choices = unique(get_data()$AutoPitchType)) 
  })
  
  filtered_data <- reactive({
    req(input$selected_pitcher)
    get_data() %>% filter(Pitcher == input$selected_pitcher)
  })
  
  #render text for pitcher hand
  output$pitcher_hand <- renderText({
    data <- filtered_data()
    if (nrow(data) > 0) {
      paste("Pitcher Hand:", data$PitcherThrows[1])
    } else {
      "No data available for this pitcher."
    }
  })
  
  #render text for pitcher ID 
  output$pitcher_id <- renderText({
    data <- filtered_data()
    if (nrow(data) > 0) {
      paste("Pitcher ID:", data$PitcherId[1])
    } else {
      "No data available for this pitcher."
    }
  })
  output$pitcher_team <- renderText({
    data <- filtered_data()
    if (nrow(data) > 0) {
      paste("Pitcher Team:", data$PitcherTeam[1])
    } else {
      "No data available for this pitcher."
    }
  })
  # In your server function
  
  # Create placeholder data frame
  placeholder_data_1 <- data.frame(
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
    WHIP = round(runif(3, 1.10, 1.50), 2)
  )
    
  # Render the table using renderTable
  output$game_log <- renderTable({
      placeholder_data_1
  })
  
  placeholder_data_2 <- data.frame(
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

  # Render the table using renderTable
  output$season_log <- renderTable({
    placeholder_data_2
  })
  # Scatter plot for Break vs RelSpeed
  output$scatterPlot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) > 0) {
      #Tag_name =  get(input$tag_choice)
      data$TagStatus <- ifelse(data[[input$tag_choice]] == "Undefined" | is.na(data[[input$tag_choice]]), "Untagged", as.character(data[[input$tag_choice]]))
      ggplot(data, aes(x = RelSpeed, y = get(input$break_type), color = TagStatus)) +
        geom_point(alpha = 0.7, size = 2) +
        labs(title = paste(input$break_type, "vs. Velocity" ),
             x = "Velocity",
             y = input$break_type) +
        theme_minimal() +
        scale_color_manual("Pitch Tag", values = c("Fastball" = "red", 
                                      "Four-Seam" = "red",
                                      "Changeup" = "blue", 
                                      "ChangeUp" = "blue", 
                                      "Sinker" = "green", 
                                      "Curveball" = "yellow", 
                                      "Slider" = "purple", 
                                      "Splitter" = "black", 
                                      "Cutter" = "pink",
                                      "Untagged" = "gray")) # Custom colors (optional)
    }
  })
  source('getHeatmap.R')
  
  # Create the heatmaps as a reactive expressions based on a dropdown
  
  # updateSelectizeInput(session, "heatPitchType", choices = c("FB", "SL", "CB", "CH"), server = TRUE)
  
  heatmap_plots <- reactive({
    data <- filtered_data()
    req(data)
    build_heatmap(data)
  })
  
  # Render the heatmaps in plot form (will be put in a card in the UI)
  
  output$heatmaps <- renderPlot({
    heatmap_plots()
  })

  
}

shinyApp(ui, server)


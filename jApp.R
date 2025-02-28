library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)

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
    column(4,  # Sidebar takes up 1/3 of the screen
           selectInput("selected_pitcher", "Choose a Pitcher:", choices = NULL),
           textOutput("pitcher_hand"),
           textOutput("pitcher_id"),
           radioButtons("break_type", "Choose Break Type:",
                        choices = c("Vertical Break" = "InducedVertBreak", 
                                    "Horizontal Break" = "HorzBreak"),
                        selected = "InducedVertBreak"),
           radioButtons("tag_choice", "Select Pitch Type Tagging:",
                        choices = c("Human Tagged" = "TaggedPitchType", 
                                    "Machine Tagged" = "AutoPitchType"),
                        selected = "TaggedPitchType")
    
    ),
    
    column(8,  # Main content takes up 2/3 of the screen
           #tableOutput("pitcher_data"),
           plotOutput("scatterPlot", height = "400px")# Half-screen scatter plot
           
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
  
  # Scatter plot for Break vs RelSpeed
  output$scatterPlot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) > 0) {
      #Tag_name =  get(input$tag_choice)
      #data$TagStatus <- ifelse(is.na(data[[input$tag_choice]]), "Untagged", as.character(data[[input$tag_choice]]))
      ggplot(data, aes(x = RelSpeed, y = get(input$break_type), color = get(input$tag_choice))) +
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

  
}

shinyApp(ui, server)


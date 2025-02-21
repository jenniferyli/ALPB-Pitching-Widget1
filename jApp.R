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

# ui
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Pitcher Data Analysis"),
  
  fluidRow(
    column(4,  # Sidebar takes up 1/3 of the screen
           selectInput("selected_pitcher", "Choose a Pitcher:", choices = NULL),
           textOutput("pitcher_hand"),
           textOutput("pitcher_id")
    ),
    
    column(8,  # Main content takes up 2/3 of the screen
           #tableOutput("pitcher_data"),
           plotOutput("scatterPlot", height = "400px")  # Half-screen scatter plot
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
  
  # Scatter plot for RelSpeed vs. VertBreak
  output$scatterPlot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) > 0) {
      ggplot(data, aes(x = RelSpeed, y = VertBreak)) +
        geom_point(color = "blue", alpha = 0.7, size = 2) +
        labs(title = "Velocity vs. VertBreak",
             x = "Velocity",
             y = "Vertical Break") +
        theme_minimal()
    }
  })

  
}

shinyApp(ui, server)


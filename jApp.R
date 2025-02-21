library(shiny)
library(shinythemes)
library(dplyr)

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
  
  selectInput("selected_pitcher", "Choose a Pitcher:", choices = NULL),  
  textOutput("pitcher_hand"),
  tableOutput("pitcher_id"),
  tableOutput("pitcher_data")
  
)

# server
server <- function(input, output, session) {
  
  observe({
    updateSelectInput(session, "selected_pitcher", 
                      choices = unique(get_data()$Pitcher)) 
  })
  
  filtered_data <- reactive({
    req(input$selected_pitcher)
    get_data() %>% filter(Pitcher == input$selected_pitcher)
  })
  
  output$pitcher_hand <- renderText({
    data <- filtered_data()
    if (nrow(data) > 0) {
      paste("Pitcher Hand:", data$PitcherThrows[1])
    } else {
      "No data available for this pitcher."
    }
  })
  
  output$pitcher_id <- renderText({
    data <- filtered_data()
    if (nrow(data) > 0) {
      paste("Pitcher ID:", data$PitcherId[1])
    } else {
      "No data available for this pitcher."
    }
  })
  
  output$pitcher_data <- renderTable({
    filtered_data()
  })
  
}

shinyApp(ui, server)


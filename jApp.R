library(shiny)
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

# ui
ui <- fluidPage(
  selectInput("selected_pitcher", "Choose a Pitcher:", choices = NULL),  
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
  
  output$pitcher_data <- renderTable({
    filtered_data()
  })
}

shinyApp(ui, server)

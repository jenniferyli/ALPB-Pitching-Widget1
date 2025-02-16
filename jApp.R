library(shiny)
library(dplyr)

# get trackman data
get_data <- function() {
  folder_path <- "7.01"
  csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
  
  combined_df <- csv_files %>%
    lapply(read.csv) %>%
    bind_rows()
  
  return(combined_df)
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

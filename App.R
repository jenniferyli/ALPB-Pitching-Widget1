glibrary(shiny)

#see if this works
# Fluid page sets up basic visual structure
ui <- fluidPage(
  verbatimTextOutput("summary"),
  tableOutput("table")
)

server <- function(input, output, session) {
  # Load the dataset from the CSV file
  dataset <- reactive({
    read.csv("Data/clipperMagazine.csv")
  })
  
  output$table <- renderTable({
    dataset()
  })
}

shinyApp(ui, server)

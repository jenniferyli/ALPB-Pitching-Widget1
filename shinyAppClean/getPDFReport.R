library(rmarkdown)

# Function to generate a blank PDF
get_blank_pdf <- function(name, date1, date2) {
  
  # Define the player name and date range for the header
  # player_name <- "John Doe"  # Replace with the actual player name if you want
  # date_range_start <- "2023-01-01"
  # date_range_end <- "2023-12-31"
  # 
  # Create a header for the report
  report_header <- list(name, date1, date2)
  
  # Render the PDF using the R Markdown template
  rmarkdown::render("PDFReportFormat.Rmd", output_format = "pdf_document",
                    output_file = paste0(getwd(), "/", name, "_Hitter_Report.pdf"),
                    params = list(report_header = report_header))
  
  # render("renderHitterPDFReport.Rmd", 
  #        output_format = "pdf_document", 
  #        encoding = "UTF-8", 
  #        output_file = paste0(getwd(), "/", player_name, "_Blank_Hitter_Report.pdf"),
  #        params = list(report_header = report_header))
}

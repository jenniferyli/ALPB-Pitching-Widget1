library(rmarkdown)

# Function to generate a blank PDF
get_blank_pdf <- function(name, date1, date2) {
  
 
  # report_header <- list(name, date1, date2)
  # 
  # # Render the PDF using the R Markdown template
  # rmarkdown::render("PDFReportFormat.Rmd", output_format = "pdf_document",
  #                   output_file = paste0(getwd(), "/", name, "_Hitter_Report.pdf"),
  #                   params = list(report_header = report_header))
  
  report_header <- list(name = name, date1 = date1, date2 = date2)
  
  output_path <- tempfile(fileext = ".pdf")
  
  rmarkdown::render("PDFReportFormat.Rmd",
                    output_format = "pdf_document",
                    output_file = output_path,
                    params = list(report_header = report_header),
                    envir = new.env(parent = globalenv()))  # prevent param conflicts
  
  return(output_path)
  
 
}

# library(rmarkdown)
# library(ggplot2)
# library(patchwork)
# 
# # Function to generate a blank PDF
# get_blank_pdf <- function(df, name, date1, date2) {
#   
#  
#   # report_header <- list(name, date1, date2)
#   # 
#   # # Render the PDF using the R Markdown template
#   # rmarkdown::render("PDFReportFormat.Rmd", output_format = "pdf_document",
#   #                   output_file = paste0(getwd(), "/", name, "_Hitter_Report.pdf"),
#   #                   params = list(report_header = report_header))
#   
#   report_header <- list(name = name, date1 = date1, date2 = date2)
#   
#   output_path <- tempfile(fileext = ".pdf")
#   source('getGraphs.R')
#   HI_graph <- build_graph_with_title(df, "horz_break", "induced_vert_break", "auto_pitch_type")
#   VI_graph <- build_graph_with_title(df, "rel_speed", "induced_vert_break", "auto_pitch_type")
#   VH_graph <- build_graph_with_title(df, "rel_speed", "horz_break", "auto_pitch_type")
#   
#   
#   combined_plot <- HI_graph + VI_graph + VH_graph + 
#     plot_layout(ncol = 3, 
#                 guides = "collect") &   
#     theme(
#       legend.position = "bottom",     
#       legend.title = element_text(size = 7), 
#       legend.text = element_text(size = 6)    
#     )
#   
#   source('pitchSplit.R')
#   splitDF <- get_pitch_type_percentages(df)
#   
#   # rmarkdown::render("PDFReportFormat.Rmd",
#   #                   output_format = "pdf_document",
#   #                   output_file = output_path,
#   #                   params = list(report_header = report_header,
#   #                                 combined_plot = combined_plot,
#   #                                 pitch_split = splitDF
#   #                   ),
#   #                   envir = new.env(parent = globalenv())) 
#   
#   rmarkdown::render(
#     input = file.path(getwd(), "PDFReportFormat.Rmd"),
#     output_format = "pdf_document",
#     output_file = output_path,
#     params = list(
#       report_header = report_header,
#       combined_plot = combined_plot,
#       pitch_split = splitDF
#     ),
#     envir = new.env(parent = globalenv())
#   )
#   
#   
#   
#   return(output_path)
#   
#  
# }

library(rmarkdown)
library(ggplot2)
library(patchwork)

# Function to generate a  PDF
get_pdf_working <- function(df, name, date1, date2) {
  # Validate data first
  if (is.null(df) || nrow(df) == 0) {
    warning("No data available for PDF generation")
    return(NULL)
  }
  
  report_header <- list(name = name, date1 = date1, date2 = date2)
  output_path <- tempfile(fileext = ".pdf")
  
  source('getGraphs.R')
  source('pitchSplit.R')
  
  HI_graph <- build_graph_with_title(df, "horz_break", "induced_vert_break", "auto_pitch_type")
  VI_graph <- build_graph_with_title(df, "rel_speed", "induced_vert_break", "auto_pitch_type")
  VH_graph <- build_graph_with_title(df, "rel_speed", "horz_break", "auto_pitch_type")
  
  combined_plot <- HI_graph + VI_graph + VH_graph +
    plot_layout(ncol = 3, guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 6)
    )
  
  splitDF <- get_pitch_type_percentages(df, "auto_pitch_type")
  
  rmarkdown::render(
    input = file.path(getwd(), "PDFReportFormat.Rmd"),
    output_format = "pdf_document",
    output_file = output_path,
    params = list(
      report_header = report_header,
      combined_plot = combined_plot,
      pitch_split = splitDF
    ),
    envir = new.env(parent = globalenv())
  )
  
  return(output_path)
}


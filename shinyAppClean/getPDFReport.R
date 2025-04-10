library(rmarkdown)
library(ggplot2)
library(patchwork)

# Function to generate a blank PDF
get_blank_pdf <- function(df, name, date1, date2) {
  
 
  # report_header <- list(name, date1, date2)
  # 
  # # Render the PDF using the R Markdown template
  # rmarkdown::render("PDFReportFormat.Rmd", output_format = "pdf_document",
  #                   output_file = paste0(getwd(), "/", name, "_Hitter_Report.pdf"),
  #                   params = list(report_header = report_header))
  
  report_header <- list(name = name, date1 = date1, date2 = date2)
  
  output_path <- tempfile(fileext = ".pdf")
  source('getGraphs.R')
  HI_graph <- build_graph_with_title(df, "horz_break", "induced_vert_break", "auto_pitch_type")
  VI_graph <- build_graph_with_title(df, "rel_speed", "induced_vert_break", "auto_pitch_type")
  VH_graph <- build_graph_with_title(df, "rel_speed", "horz_break", "auto_pitch_type")
  
  # combined_plot <- HI_graph + VI_graph + VH_graph + plot_layout(ncol = 3)
  
  # combined_plot <- HI_graph + VI_graph + VH_graph + plot_layout(ncol = 3) +
  #   theme(
  #     legend.position = "bottom",  # Move the legend below the plots
  #   )
  
  combined_plot <- HI_graph + VI_graph + VH_graph + 
    plot_layout(ncol = 3, 
                guides = "collect") &   # Collects the legends from all plots
    theme(
      legend.position = "bottom",     # Position the legend below all plots
      legend.title = element_text(size = 7),  # Customize legend title size
      legend.text = element_text(size = 6)     # Customize legend text size
    )
  
  
  rmarkdown::render("PDFReportFormat.Rmd",
                    output_format = "pdf_document",
                    output_file = output_path,
                    params = list(report_header = report_header,
                                  combined_plot = combined_plot
                    ),
                    envir = new.env(parent = globalenv()))  # prevent param conflicts
  
  return(output_path)
  
 
}

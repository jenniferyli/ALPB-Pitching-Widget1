library(ggplot2)
library(shiny)
# library(dplyr)  # Optional
library(hash)

h <- hash() 
h[["induced_vert_break"]] <- "Induced Vertical Break (inches)"
h[["horz_break"]] <- "Horizontal Break (inches)"
h[["rel_speed"]] <- "Velocity (mph)"

build_graph <- function (filtered_df, x_axis, y_axis, tag) {
  
  if (!is.null(filtered_df) && nrow(filtered_df) > 0 &&
      x_axis %in% names(filtered_df) &&
      y_axis %in% names(filtered_df)) {
    
    filtered_df$TagStatus <- ifelse(filtered_df[[tag]] == "Undefined" | is.na(filtered_df[[tag]]),
                           "Untagged", as.character(filtered_df[[tag]]))
    
    plot <- ggplot(filtered_df, aes(x = .data[[x_axis]], y = .data[[y_axis]], color = TagStatus)) +
      geom_point(alpha = 0.7, size = 2) +
      labs(
        x = h[[x_axis]],
        y = h[[y_axis]]) +
      theme_minimal() +
      scale_color_manual("Pitch Tag", values = c(
        "Fastball" = "red", 
        "Four-Seam" = "red",
        "Changeup" = "blue", 
        "ChangeUp" = "blue", 
        "Sinker" = "green", 
        "Curveball" = "brown", 
        "Slider" = "purple", 
        "Splitter" = "black", 
        "Cutter" = "pink",
        "Untagged" = "gray"
      ))
  }
  return(plot)
}
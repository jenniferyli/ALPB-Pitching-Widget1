# Load necessary libraries
library(ggplot2)
library(MASS)
library(dplyr)

# Function to create a heatmap for hard-hit balls (ExitSpeed > 95)
build_heatmap <- function(filtered_df) {
  
  # Check if there is data to plot, otherwise return a blank strike zone
  if (nrow(filtered_df) == 0) {
    heatmap_viz <- ggplot() +
      coord_equal(xlim = c((-16/12), (16/12)), ylim = c(1, 4)) +
      theme_minimal() +
      geom_rect(aes(xmin = -(10/12), xmax = (10/12), ymin = 1.5, ymax = 3.5), 
                fill = NA, color = "black", linetype = "solid") +  # strike zone outline
      theme(legend.position = "none",
            legend.title = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks.length = unit(0, "pt"),
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            plot.title = element_text(face = 'bold', hjust = 0.5, vjust = 0, size = 22))
    
    return(heatmap_viz)
  }
  
  # Filter out rows where PlateLocSide or PlateLocHeight is NA or infinite
  filtered_df <- filtered_df %>%
    filter(!is.na(PlateLocSide), !is.na(PlateLocHeight), 
           is.finite(PlateLocSide), is.finite(PlateLocHeight))
  
  # Check if the data is empty after filtering
  if (nrow(filtered_df) == 0) {
    return(ggplot() + 
             labs(title = "No valid data to plot"))
  }
  
  # Define a grid over the entire plot range
  x_grid <- seq(-1.5, 1.5, length.out = 100)
  y_grid <- seq(0, 4, length.out = 100)
  
  # Create a 2D kernel density estimate (KDE)
  kde <- kde2d(filtered_df$PlateLocSide, filtered_df$PlateLocHeight, h = 1, n = 100, lims = c(range(x_grid), range(y_grid)))
  
  # Expand these density estimates into a dataframe and filter out densities that are very sparse
  df <- expand.grid(x = kde$x, y = kde$y) %>%
    mutate(z = as.vector(kde$z)) %>%
    filter(z > 0.001)
  
  # Build the heatmap with gradient colors based on how large the density is by location
  heatmap_viz <- ggplot(df, aes(x = x, y = y, fill = z)) +
    geom_tile(color = "white") +
    geom_raster(aes(alpha = ifelse(z > 0.001, 1, 0)), interpolate = TRUE) +  
    scale_fill_gradientn(colors = c("white", "white", "blue", "green", "yellow", "red"), 
                         values = scales::rescale(c(0, 0.25, 0.5, 1)), 
                         guide = "colorbar") +
    scale_alpha_identity() +
    coord_equal(xlim = c((-16/12), (16/12)), ylim = c(1, 4)) +
    theme_minimal() +
    geom_rect(aes(xmin = -(10/12), xmax = (10/12), ymin = 1.5, ymax = 3.5), 
              fill = NA, color = "black", linetype = "solid") +  # strike zone outline
    theme(legend.position = "none",
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks.length = unit(0, "pt"),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          plot.title = element_text(face = 'bold', hjust = 0.5, vjust = 0, size = 22))
  
  return(heatmap_viz)
}

# # Function to create the heatmap for hard-hit balls (ExitSpeed > 95)
# get_batter_heatmaps <- function(data, pitcher) {
#   
#   # Filter data based on selected player, pitcher throws, and date range
#   user_filtered_data <- data %>%
#     filter(Pitcher == pitcher) %>%
#     # filter(PitcherThrows == throws) %>%
#     # filter(Date >= date_low & Date <= date_high)
#   
#   # Filter for hard-hit balls with ExitSpeed > 95 for selected pitch type
#   # hard_hit_balls <- user_filtered_data %>%
#   #   filter(!is.na(ExitSpeed) & !is.na(PlateLocHeight) & (PitchCall == "InPlay")) %>%
#   #   filter(ExitSpeed > 95) %>%
#   #   filter(AutoPitchType == heat_pitch_type)  # Filter based on selected pitch type
#   
#   # Generate the heatmap using the filtered data
#   heatmap <- build_heatmap(user_filtered_data)
#   
#   return(heatmap)
# }

batter_heatmaps_low <- function(batter_id, data) {
  
  # Load required library
  library(ggplot2)
  library(MASS)
  library(dplyr)
  
  # Assuming your data is in a dataframe called 'pitches'
  # Filter for batted balls with exit velocity > 95 mph
  hard_hit_balls <- X20240614_ClipperMagazine_1 %>%
    filter(PitchCall == "InPlay", ExitSpeed > 95)
  
  # Define a coarser grid over the entire plot range for faster computation
  x_grid <- seq(-1.5, 1.5, length.out = 50)
  y_grid <- seq(0, 4, length.out = 50)
  
  # Create a 2D kernel density estimate
  kde <- MASS::kde2d(hard_hit_balls$PlateLocSide, hard_hit_balls$PlateLocHeight, 
                     n = 50, lims = c(range(x_grid), range(y_grid)))
  
  # Create a dataframe for plotting
  df <- data.frame(expand.grid(x = kde$x, y = kde$y), z = as.vector(kde$z))
  df <- df %>% filter(z > 0.001)
  
  # Create the refined heatmap
  ggplot(df, aes(x = x, y = y, fill = z)) +
    geom_tile(color = "white", aes(alpha = ifelse(z > 0.001, 1, 0.5))) +  
    scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), 
                         values = scales::rescale(c(0, 0.25, 0.5, 1)), 
                         guide = "colorbar") +
    scale_alpha_identity() +  # Apply transparency
    coord_equal() +
    xlim(-1.5, 1.5) +  # Adjust these limits based on your data
    ylim(0, 4) +       # Adjust these limits based on your data
    labs(x = "Horizontal Location", y = "Vertical Location", 
         title = "Strike Zone Heatmap",
         subtitle = "Kernel Density Estimate for Exit Velocities > 95 mph") +
    theme_minimal() +
    geom_rect(aes(xmin = -0.85, xmax = 0.85, ymin = 1.5, ymax = 3.5), 
              fill = NA, color = "black", linetype = "dashed") +  # strike zone outline
    theme(legend.position = "right",
          legend.title = element_blank())
  
}

